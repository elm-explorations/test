module Doodle exposing (..)


type alias PreMutationInput =
    { randomRun : RandomRun
    , bucketedEdgeHitCounts : Maybe BucketedEdgeHitCounts
    , durationMs : Maybe Float
    }


type alias QueueInput =
    { randomRun : RandomRun
    , previousBucketedEdgeHitCounts : Maybe BucketedEdgeHitCounts
    , previousDurationMs : Maybe Float
    }


type alias CorpusInput =
    { randomRun : RandomRun
    , bucketedEdgeHitCounts : BucketedEdgeHitCounts
    , durationMs : Float
    }


type alias Corpus =
    { favoredFresh : List CorpusInput
    , favoredUsed : List CorpusInput
    , otherFresh : List CorpusInput
    , otherUsed : List CorpusInput
    }


type alias Failure a =
    { randomRun : RandomRun
    , value : a
    , message : String
    }


type alias Constants a =
    { fuzzer : Fuzzer a
    , test : a -> Result String ()
    }


type alias State =
    { queue : List QueueInput
    , corpus : Corpus
    , foundFailures : List Failure
    , seed : Random.Seed
    }


step : Constants a -> State -> State
step constants state =
    case state.queue of
        [] ->
            generateOrMutate constants state

        input :: restOfQueue ->
            processInput constants { state | queue = restOfQueue } input


generateOrMutate : Constants a -> State -> State
generateOrMutate constants state =
    let
        ( ( input, newCorpus ), newSeed ) =
            Random.step
                (pickInput constants.fuzzer state.corpus)
                state.seed

        mutatedInputs : List QueueInput
        mutatedInputs =
            mutate input
    in
    mainLoop constants { state | queue = mutatedInputs, seed = newSeed }


processInput : Constants a -> State -> QueueInput -> State
processInput constants state input =
    case runFuzzer constants.fuzzer of
        Nothing ->
            state

        Just value ->
            let
                _ =
                    Coverage.resetCoverage ()
            in
            let
                testResult : Result String ()
                testResult =
                    constants.test value
            in
            let
                coverage =
                    Coverage.getCoverage ()
            in
            if isInteresting input testResult coverage then
                { state
                    | corpus =
                        state.corpus
                            |> addIntoCorpus input value testResult coverage
                    , foundFailures =
                        case testResult of
                            Ok () ->
                                state.foundFailures

                            Err message ->
                                state.foundFailures
                                    |> addFailure input.randomRun value message
                }

            else
                state


isInteresting : Input -> Result String () -> Coverage -> Bool
isInteresting input testResult coverage =
    Result.isErr testResult
        || Debug.todo "something about new covered paths and bucketed coverage"


pickInput : Fuzzer a -> Corpus -> Generator ( PreMutationInput, Corpus )
pickInput fuzzer corpus =
    if isCorpusEmpty corpus then
        generateFromFuzzer fuzzer corpus

    else
        Random.frequency
            [ ( 1, generateFromFuzzer fuzzer corpus )
            , ( 9, pickFromCorpus corpus )
            ]


generateFromFuzzer : Fuzzer a -> Corpus -> Generator ( PreMutationInput, Corpus )
generateFromFuzzer fuzzer corpus =
    todo fuzzer
        |> Random.map (\input -> ( input, corpus ))


pickFromCorpus : Corpus -> Generator ( PreMutationInput, Corpus )
pickFromCorpus corpus =
    -- TODO do we need to throw away the value?
    if corpus.favoredFresh == [] then
        Random.frequency
            [ Debug.todo "prefer favored fresh" ]

    else
        Random.frequency
            [ Debug.todo "pick from anywhere" ]


mutate : PreMutationInput -> List QueueInput
mutate input =
    Debug.todo "deterministically mutate the input"
