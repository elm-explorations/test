module Corpus.ShrinkingChallenge exposing (cases)

{-| The <https://github.com/jlink/shrinking-challenge> suite, as corpus cases.

These are ported from `tests/src/ShrinkingChallengeTests.elm`, which already
encodes each challenge as (fuzzer, falsifiable property, known-optimal
counterexample) — exactly what an F-metric case needs. Keep the two in sync:
if the expected minima change there, they should change here.

The challenges were designed to stress _simplifying_, not _detection_, so
several of them are falsified on the first few runs. They still earn their place
here because of the counterexample-quality score, and because a few of them
(`difference*`, `coupling`) are genuinely hard to even detect.

One deliberate divergence from the test suite: where `simplifiesTowardsMany`
lists extra values with a "TODO check why this didn't get shrunk further"
comment, we leave those out of `minima`. They document where simplifying
currently falls short, and the point of the quality score is to notice when that
changes — so scoring against them would hide exactly what we want to see.
`challenge/coupling` therefore sits well below 100% optimal today, on purpose.

-}

import Corpus.Case as Case exposing (Case)
import Fuzz exposing (Fuzzer)
import Set


cases : List Case
cases =
    -- `Case.shrinkOnly` marks the challenges that the very first generated
    -- value or two already falsifies: they measure simplifying, not search.
    [ Case.shrinkOnly reverse
    , Case.shrinkOnly largeUnionList
    , calculator
    , Case.shrinkOnly lengthList
    , difference1
    , difference2
    , difference3
    , Case.shrinkOnly binHeap
    , Case.shrinkOnly coupling
    , deletion
    , Case.shrinkOnly distinct
    , Case.shrinkOnly nestedLists
    ]


reverse : Case
reverse =
    Case.fails
        { name = "challenge/reverse"
        , category = "challenge"
        , minima = [ "[0,1]" ]
        , budget = Nothing
        }
        (Fuzz.list Fuzz.int)
        (\list -> list == List.reverse list)


largeUnionList : Case
largeUnionList =
    Case.fails
        { name = "challenge/large-union-list"
        , category = "challenge"
        , minima = [ "[[0,1,-1,2,-2]]" ]
        , budget = Nothing
        }
        (Fuzz.list (Fuzz.list Fuzz.int))
        (\lists -> Set.size (Set.fromList (List.concat lists)) <= 4)


type CalcExpr
    = Int Int
    | Add CalcExpr CalcExpr
    | Div CalcExpr CalcExpr


calculator : Case
calculator =
    let
        exprFuzzer : Int -> Fuzzer CalcExpr
        exprFuzzer maxDepth =
            if maxDepth <= 0 then
                Fuzz.map Int Fuzz.int

            else
                let
                    subExprFuzzer =
                        exprFuzzer (maxDepth - 1)
                in
                Fuzz.oneOf
                    [ Fuzz.map Int Fuzz.int
                    , Fuzz.map2 Add subExprFuzzer subExprFuzzer
                    , Fuzz.map2 Div subExprFuzzer subExprFuzzer
                    ]

        noDivisionByLiteralZero : CalcExpr -> Bool
        noDivisionByLiteralZero expr =
            case expr of
                Div _ (Int 0) ->
                    False

                Int _ ->
                    True

                Add a b ->
                    noDivisionByLiteralZero a
                        && noDivisionByLiteralZero b

                Div a b ->
                    noDivisionByLiteralZero a
                        && noDivisionByLiteralZero b

        eval : CalcExpr -> Maybe Int
        eval expr =
            case expr of
                Int i ->
                    Just i

                Add a b ->
                    Maybe.map2 (+)
                        (eval a)
                        (eval b)

                Div a b ->
                    Maybe.map2
                        (\a_ b_ ->
                            if b_ == 0 then
                                Nothing

                            else
                                Just <| a_ // b_
                        )
                        (eval a)
                        (eval b)
                        |> Maybe.andThen identity
    in
    Case.fails
        { name = "challenge/calculator"
        , category = "challenge"
        , minima =
            [ "Div (Int 0) (Add (Int 0) (Int 0))"
            , "Div (Int 0) (Div (Int 0) (Int 1))"
            ]
        , budget = Nothing
        }
        (exprFuzzer 5 |> Fuzz.filter noDivisionByLiteralZero)
        (\expr -> eval expr /= Nothing)


lengthList : Case
lengthList =
    Case.fails
        { name = "challenge/length-list"
        , category = "challenge"
        , minima = [ "[900]" ]
        , budget = Nothing
        }
        (Fuzz.intRange 1 100
            |> Fuzz.andThen
                (\len -> Fuzz.listOfLength len (Fuzz.intRange 0 1000))
        )
        -- The fuzzer never produces an empty list, so the default is unreachable.
        (\list -> Maybe.withDefault 0 (List.maximum list) < 900)


difference1 : Case
difference1 =
    Case.fails
        { name = "challenge/difference1"
        , category = "challenge"
        , minima = [ "(10,10)" ]
        , budget = Just 10000
        }
        (Fuzz.pair (Fuzz.intAtLeast 0) (Fuzz.intAtLeast 0))
        (\( x, y ) -> x < 10 || x /= y)


difference2 : Case
difference2 =
    Case.fails
        { name = "challenge/difference2"
        , category = "challenge"
        , minima = [ "(10,6)" ]
        , budget = Just 5000
        }
        (Fuzz.pair (Fuzz.intAtLeast 0) (Fuzz.intAtLeast 0))
        (\( x, y ) ->
            let
                absDiff =
                    abs (x - y)
            in
            x < 10 || absDiff < 1 || absDiff > 4
        )


difference3 : Case
difference3 =
    Case.fails
        { name = "challenge/difference3"
        , category = "challenge"
        , minima = [ "(10,9)" ]
        , budget = Just 5000
        }
        (Fuzz.pair (Fuzz.intAtLeast 0) (Fuzz.intAtLeast 0))
        (\( x, y ) -> x < 10 || abs (x - y) /= 1)


type Heap
    = Heap Int (Maybe Heap) (Maybe Heap)


binHeap : Case
binHeap =
    let
        heapFuzzer : Int -> Fuzzer Heap
        heapFuzzer depth =
            if depth <= 0 then
                Fuzz.map (\i -> Heap i Nothing Nothing) Fuzz.int

            else
                Fuzz.map3 Heap
                    Fuzz.int
                    (Fuzz.maybe (heapFuzzer (depth - 1)))
                    (Fuzz.maybe (heapFuzzer (depth - 1)))

        toList : Heap -> List Int
        toList heap =
            let
                go : List Int -> List Heap -> List Int
                go acc stack =
                    case stack of
                        [] ->
                            List.reverse acc

                        (Heap n left right) :: hs ->
                            go
                                (n :: acc)
                                (List.filterMap identity [ left, right ] ++ hs)
            in
            go [] [ heap ]

        mergeHeaps : Maybe Heap -> Maybe Heap -> Maybe Heap
        mergeHeaps left right =
            case ( left, right ) of
                ( Nothing, _ ) ->
                    right

                ( _, Nothing ) ->
                    left

                ( Just (Heap ln lleft lright), Just (Heap rn rleft rright) ) ->
                    Just <|
                        if ln <= rn then
                            Heap ln (mergeHeaps lright right) lleft

                        else
                            Heap rn (mergeHeaps rright left) rleft

        wrongToSortedList : Heap -> List Int
        wrongToSortedList (Heap n left right) =
            n
                :: (mergeHeaps left right
                        |> Maybe.map toList
                        |> Maybe.withDefault []
                   )
    in
    Case.fails
        { name = "challenge/bin-heap"
        , category = "challenge"
        , minima =
            [ "Heap 1 Nothing (Just (Heap 0 Nothing Nothing))"
            , "Heap 0 Nothing (Just (Heap -1 Nothing Nothing))"
            , "Heap 0 (Just (Heap -1 Nothing Nothing)) Nothing"
            ]
        , budget = Nothing
        }
        (heapFuzzer 4)
        (\heap ->
            let
                l1 =
                    toList heap

                l2 =
                    wrongToSortedList heap
            in
            (l2 == List.sort l2) && (List.sort l1 == l2)
        )


coupling : Case
coupling =
    let
        getAt : Int -> List a -> Maybe a
        getAt index list =
            if index < 0 then
                Nothing

            else
                list
                    |> List.drop index
                    |> List.head
    in
    Case.fails
        { name = "challenge/coupling"
        , category = "challenge"
        , minima = [ "[1,0]" ]
        , budget = Nothing
        }
        (Fuzz.list (Fuzz.intRange 0 10)
            |> Fuzz.filter
                (\l ->
                    let
                        length =
                            List.length l
                    in
                    List.all (\i -> i < length) l
                )
        )
        (\list ->
            list
                |> List.indexedMap
                    (\i x ->
                        if i /= x then
                            getAt x list /= Just i

                        else
                            True
                    )
                |> List.all identity
        )


deletion : Case
deletion =
    let
        go : a -> List a -> List a -> List a -> List a
        go badX next prev orig =
            case next of
                [] ->
                    orig

                x :: rest ->
                    if x == badX then
                        List.reverse prev ++ rest

                    else
                        go badX rest (x :: prev) orig

        removeFirst : a -> List a -> List a
        removeFirst badX xs =
            go badX xs [] xs
    in
    Case.fails
        { name = "challenge/deletion"
        , category = "challenge"
        , minima = [ "([0,0],0)" ]
        , budget = Nothing
        }
        (Fuzz.listOfLengthBetween 1 100 Fuzz.int
            |> Fuzz.andThen
                (\list ->
                    Fuzz.pair
                        (Fuzz.constant list)
                        (Fuzz.oneOfValues list)
                )
        )
        (\( list, el ) -> not (List.member el (removeFirst el list)))


distinct : Case
distinct =
    Case.fails
        { name = "challenge/distinct"
        , category = "challenge"
        , minima =
            [ "[0,1,2]"
            , "[0,1,-1]"
            , "[0,-1,1]"
            ]
        , budget = Nothing
        }
        (Fuzz.list Fuzz.int)
        (\list -> Set.size (Set.fromList list) < 3)


nestedLists : Case
nestedLists =
    Case.fails
        { name = "challenge/nested-lists"
        , category = "challenge"
        , minima = [ "[[0,0,0,0,0,0,0,0,0,0,0]]" ]
        , budget = Nothing
        }
        (Fuzz.listOfLengthBetween 0 20 (Fuzz.listOfLengthBetween 0 20 Fuzz.int))
        (\lists -> List.sum (List.map List.length lists) <= 10)
