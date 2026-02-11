module Fuzz.InputCorpus exposing (Input, InputCorpus, add, generator, init, isEmpty)

import MicroRandomExtra
import Random exposing (Generator)
import RandomRun exposing (RandomRun)
import Test.Coverage.EdgeHitCounts exposing (BucketedEdgeHitCounts)


{-| TODO: do we need IDs and Dict ID Input, to allow efficiently removing /
moving inputs across categories?
-}
type alias InputCorpus =
    { favoredFresh : List Input
    , favoredUsed : List Input
    , otherFresh : List Input
    , otherUsed : List Input
    }


init : InputCorpus
init =
    { favoredFresh = []
    , favoredUsed = []
    , otherFresh = []
    , otherUsed = []
    }


isEmpty : InputCorpus -> Bool
isEmpty corpus =
    (corpus.favoredFresh == [])
        && (corpus.favoredUsed == [])
        && (corpus.otherFresh == [])
        && (corpus.otherUsed == [])


type alias Input =
    { randomRun : RandomRun
    , durationMs : Float
    , bucketedEdgeHitCounts : BucketedEdgeHitCounts
    }


add : RandomRun -> Float -> BucketedEdgeHitCounts -> InputCorpus -> InputCorpus
add randomRun durationMs bucketedEdgeHitCounts corpus =
    let
        input : Input
        input =
            { randomRun = randomRun
            , durationMs = durationMs
            , bucketedEdgeHitCounts = bucketedEdgeHitCounts
            }
    in
    { otherFresh = input :: corpus.otherFresh

    -- the rest, unchanged (PERF: not using record update)
    , favoredFresh = corpus.favoredFresh
    , favoredUsed = corpus.favoredUsed
    , otherUsed = corpus.otherUsed
    }


randomElement : List a -> Maybe (Generator (Maybe a))
randomElement list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            Just (MicroRandomExtra.choose list)


ifNotEmpty : Float -> List a -> Maybe ( Float, Generator (Maybe a) )
ifNotEmpty n xs =
    randomElement xs
        |> Maybe.map (\gen -> ( n, gen ))


generator : InputCorpus -> Generator (Maybe ( RandomRun, BucketedEdgeHitCounts ))
generator corpus =
    if isEmpty corpus then
        Random.constant Nothing

    else
        Random.weighted
            ( {- Don't pick an item from the corpus.
                 Instead let the fuzzer generate a random one.
              -}
              1
            , Random.constant Nothing
            )
            ((if corpus.favoredFresh /= [] then
                [ ifNotEmpty 98 corpus.favoredFresh
                , ifNotEmpty 1 corpus.favoredUsed
                , ifNotEmpty 0.7 corpus.otherFresh
                , ifNotEmpty 0.3 corpus.otherUsed
                ]

              else
                [ ifNotEmpty 70 corpus.favoredUsed
                , ifNotEmpty 25 corpus.otherFresh
                , ifNotEmpty 5 corpus.otherUsed
                ]
             )
                |> List.filterMap identity
            )
            |> Random.andThen identity
            |> Random.andThen
                (\maybeInput ->
                    case maybeInput of
                        Nothing ->
                            Random.constant Nothing

                        Just input ->
                            mutate input.randomRun
                                |> Random.map
                                    (\mutatedRandomRun ->
                                        Just
                                            ( mutatedRandomRun
                                            , input.bucketedEdgeHitCounts
                                            )
                                    )
                )


mutate : RandomRun -> Generator RandomRun
mutate randomRun =
    -- TODO mutate
    Random.constant ()
