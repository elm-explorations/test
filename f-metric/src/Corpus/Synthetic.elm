module Corpus.Synthetic exposing (cases)

{-| Synthetic corpus cases, each shaped to probe a different weakness a
generation strategy can have.

  - `needle` — exactly one input (out of many) violates the property. Rewards
    strategies that cover a small domain systematically.
  - `boundary` — the defect sits at the edge of a range or at a size boundary.
    Rewards strategies that try edges deliberately rather than uniformly.
  - `structural` — the input has to have a particular _shape_ (a repeat, a
    nesting) rather than a particular value.
  - `deep` — only _large_ inputs violate the property. This is the category that
    punishes any strategy biased towards small inputs, so it's the one to watch
    when a change makes the small cases look great.
  - `filter` — the fuzzer rejects values, which exercises a separate code path
    in the fuzz loop.

-}

import Corpus.Case as Case exposing (Case)
import Fuzz
import Set


cases : List Case
cases =
    [ needleInRange
    , needlePairDiagonal
    , needleExactList
    , boundaryRangeTop

    -- Falsified by the first value: measures simplifying, not search.
    , Case.shrinkOnly boundaryLastChunk
    , Case.shrinkOnly structuralAdjacentDedupe
    , structuralJustNothing
    , deepListSum
    , deepBigInt
    , deepLongString
    , filterMultipleOfFour
    ]



-- NEEDLE


needleInRange : Case
needleInRange =
    Case.fails
        { name = "needle/int-in-range"
        , category = "needle"
        , minima = [ "42" ]
        , budget = Nothing
        }
        (Fuzz.intRange 0 1000)
        (\n -> n /= 42)


needlePairDiagonal : Case
needlePairDiagonal =
    Case.fails
        { name = "needle/pair-diagonal"
        , category = "needle"
        , minima = [ "(0,0)" ]
        , budget = Nothing
        }
        (Fuzz.pair (Fuzz.intRange 0 50) (Fuzz.intRange 0 50))
        (\( a, b ) -> a /= b)


needleExactList : Case
needleExactList =
    Case.fails
        { name = "needle/exact-list"
        , category = "needle"
        , minima = [ "[1,2,3]" ]
        , budget = Nothing
        }
        (Fuzz.listOfLengthBetween 0 4 (Fuzz.intRange 0 3))
        (\xs -> xs /= [ 1, 2, 3 ])



-- BOUNDARY


{-| The defect sits at the very top of the range, where uniform sampling spends
1/101 of its runs.
-}
boundaryRangeTop : Case
boundaryRangeTop =
    Case.fails
        { name = "boundary/range-top"
        , category = "boundary"
        , minima = [ "100" ]
        , budget = Nothing
        }
        (Fuzz.intRange 0 100)
        (\n -> n /= 100)


{-| A `chunk` that silently drops the final, partial chunk. Any input whose
length isn't a multiple of the chunk size falsifies "chunking preserves
elements", so `[0]` is the optimal counterexample and every strategy should find
it almost immediately. Included as a control: if this case ever regresses,
something is badly wrong.
-}
boundaryLastChunk : Case
boundaryLastChunk =
    let
        chunkDroppingRemainder : Int -> List Int -> List (List Int)
        chunkDroppingRemainder size xs =
            if List.length xs < size then
                []

            else
                List.take size xs :: chunkDroppingRemainder size (List.drop size xs)
    in
    Case.fails
        { name = "boundary/last-chunk"
        , category = "boundary"
        , minima = [ "[0]" ]
        , budget = Nothing
        }
        (Fuzz.list (Fuzz.intRange 0 9))
        (\xs -> List.length (List.concat (chunkDroppingRemainder 3 xs)) == List.length xs)



-- STRUCTURAL


{-| A deduplicating function that only removes _adjacent_ duplicates. Needs a
list where equal elements are separated by a different one, so no single value
and no sorted list can expose it.
-}
structuralAdjacentDedupe : Case
structuralAdjacentDedupe =
    let
        dedupeAdjacent : List Int -> List Int
        dedupeAdjacent xs =
            case xs of
                a :: b :: rest ->
                    if a == b then
                        dedupeAdjacent (b :: rest)

                    else
                        a :: dedupeAdjacent (b :: rest)

                _ ->
                    xs
    in
    Case.fails
        { name = "structural/adjacent-dedupe"
        , category = "structural"

        {- Simplifying often stops at `[1,0,1]`, which is shortlex-larger. That's
           a real shortfall, so we score against the actual optimum rather than
           accepting what we currently produce.
        -}
        , minima = [ "[0,1,0]" ]
        , budget = Nothing
        }
        (Fuzz.list (Fuzz.intRange 0 3))
        (\xs -> List.length (dedupeAdjacent xs) == Set.size (Set.fromList xs))


structuralJustNothing : Case
structuralJustNothing =
    Case.fails
        { name = "structural/just-nothing"
        , category = "structural"
        , minima = [ "Just Nothing" ]
        , budget = Nothing
        }
        (Fuzz.maybe (Fuzz.maybe Fuzz.bool))
        (\m -> m /= Just Nothing)



-- DEEP


deepListSum : Case
deepListSum =
    Case.fails
        { name = "deep/list-sum"
        , category = "deep"
        , minima = []
        , budget = Nothing
        }
        (Fuzz.list (Fuzz.intRange 0 9))
        (\xs -> List.sum xs < 100)


deepBigInt : Case
deepBigInt =
    Case.fails
        { name = "deep/big-int"
        , category = "deep"
        , minima = []
        , budget = Nothing
        }
        Fuzz.int
        (\n -> n < 1000000)


{-| Note the explicit length range: `Fuzz.string` is `stringOfLengthBetween 0 10`,
so a threshold above 10 would make this case impossible rather than merely hard.
-}
deepLongString : Case
deepLongString =
    Case.fails
        { name = "deep/long-string"
        , category = "deep"
        , minima = []
        , budget = Nothing
        }
        (Fuzz.stringOfLengthBetween 0 40)
        (\s -> String.length s < 25)



-- FILTER


{-| Keep the predicate generous. `Fuzz.filter` gives up after 16 consecutive
rejections and fails the whole test, and over thousands of runs even a mildly
selective predicate hits that. A 25%-rejection predicate makes a spurious
failure vanishingly unlikely; `\n -> n > 15` over `intRange 0 20` (76%
rejection) fails spuriously within a couple of thousand runs.
-}
filterMultipleOfFour : Case
filterMultipleOfFour =
    Case.fails
        { name = "filter/multiple-of-four"
        , category = "filter"
        , minima = [ "42" ]
        , budget = Nothing
        }
        (Fuzz.intRange 0 100 |> Fuzz.filter (\n -> modBy 4 n /= 0))
        (\n -> n /= 42)
