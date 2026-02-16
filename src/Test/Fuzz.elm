module Test.Fuzz exposing (fuzzTest)

import DebugConfig
import Dict exposing (Dict)
import Fuzz.InputCorpus exposing (Input, InputCorpus)
import Fuzz.Internal exposing (Fuzzer)
import GenResult exposing (GenResult(..))
import MicroDictExtra as Dict
import MicroListExtra as List
import MicroMaybeExtra as Maybe
import PRNG
import Random
import RandomRun exposing (RandomRun)
import Simplify
import Test.Coverage exposing (EdgeCoverage)
import Test.Coverage.EdgeHitCounts exposing (BucketedEdgeHitCounts)
import Test.Distribution exposing (DistributionReport(..))
import Test.Distribution.Internal exposing (Distribution(..), ExpectedDistribution(..))
import Test.Expectation exposing (Expectation(..))
import Test.Internal exposing (Test(..), blankDescriptionFailure)
import Test.Runner.Distribution
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| Reject always-failing tests because of bad names or invalid fuzzers.
-}
fuzzTest : Distribution a -> Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzTest distribution fuzzer untrimmedDesc getExpectation =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        blankDescriptionFailure

    else
        ElmTestVariant__Labeled desc <| validatedFuzzTest desc fuzzer getExpectation distribution


{-| Knowing that the fuzz test isn't obviously invalid, run the test and package up the results.
-}
validatedFuzzTest : String -> Fuzzer a -> (a -> Expectation) -> Distribution a -> Test
validatedFuzzTest desc fuzzer getExpectation distribution =
    ElmTestVariant__FuzzTest
        (\seed runs ->
            let
                _ =
                    if DebugConfig.shouldLogFuzzTests then
                        Debug.log "running fuzz test" desc

                    else
                        desc
            in
            let
                runResult : RunResult
                runResult =
                    fuzzLoop
                        { fuzzer = fuzzer
                        , testFn = getExpectation
                        , initialSeed = seed
                        , runsNeeded = runs
                        , distribution = distribution
                        }
                        (initLoopState seed distribution)
            in
            case runResult.failure of
                Nothing ->
                    [ Pass { distributionReport = runResult.distributionReport } ]

                Just failure ->
                    [ { failure
                        | expectation =
                            failure.expectation
                                |> Test.Expectation.withDistributionReport runResult.distributionReport
                      }
                        |> formatExpectation
                    ]
        )


type alias Failure =
    { given : Maybe String
    , expectation : Expectation
    }


type alias LoopConstants a =
    { fuzzer : Fuzzer a
    , testFn : a -> Expectation
    , initialSeed : Random.Seed
    , runsNeeded : Int
    , distribution : Distribution a
    }


type alias LoopState =
    { runsElapsed : Int
    , distributionCount : Maybe (Dict (List String) Int)
    , nextPowerOfTwo : Int
    , failure : Maybe Failure
    , currentSeed : Random.Seed
    , inputCorpus : InputCorpus
    }


initLoopState : Random.Seed -> Distribution a -> LoopState
initLoopState initialSeed distribution =
    let
        initialDistributionCount : Maybe (Dict (List String) Int)
        initialDistributionCount =
            Test.Distribution.Internal.getDistributionLabels distribution
                |> Maybe.map
                    (\labels ->
                        labels
                            |> List.map (\( label, _ ) -> ( [ label ], 0 ))
                            |> Dict.fromList
                    )
    in
    { runsElapsed = 0
    , distributionCount = initialDistributionCount
    , nextPowerOfTwo = 1
    , failure = Nothing
    , currentSeed = initialSeed
    , inputCorpus = Fuzz.InputCorpus.init
    }


{-| Runs fuzz tests repeatedly and returns information about distribution and possible failure.

The loop algorithm is roughly:

    if any failure:
        end with failure

    else if not enough tests ran (elapsed < total):
        run `total - elapsed` tests (short-circuiting on failure)
        loop

    else if doesn't need distribution check:
        end with success

    else if all labels sufficiently covered:
        end with success

    else if any label not sufficiently covered:
        set failure
        end with failure

    else:
        run `2^nextPowerOfTwo` tests (short-circuiting on failure)
        increment `nextPowerOfTwo`
        loop

-}
fuzzLoop : LoopConstants a -> LoopState -> RunResult
fuzzLoop c state =
    case state.failure of
        Just failure ->
            -- If the test fails, it still is useful to report the distribution even if we didn't do the statistical check for ExpectDistribution.
            -- For this reason we try to create DistributionToReport even in case of ExpectDistribution.
            { distributionReport =
                case state.distributionCount of
                    Nothing ->
                        NoDistribution

                    Just distributionCount ->
                        DistributionToReport
                            { distributionCount = includeCombinationsInBaseCounts distributionCount
                            , runsElapsed = state.runsElapsed
                            }
            , failure = Just failure
            }

        Nothing ->
            if state.runsElapsed < c.runsNeeded then
                let
                    newState : LoopState
                    newState =
                        runNTimesOrUntilFailure (c.runsNeeded - state.runsElapsed) c state
                in
                fuzzLoop c newState

            else
                case c.distribution of
                    NoDistributionNeeded ->
                        { distributionReport = NoDistribution
                        , failure = Nothing
                        }

                    ReportDistribution _ ->
                        case state.distributionCount of
                            Nothing ->
                                -- Shouldn't happen, we're in the ReportDistribution case. This indicates a bug in `initLoopState`.
                                distributionBugRunResult

                            Just distributionCount ->
                                { distributionReport =
                                    DistributionToReport
                                        { distributionCount = includeCombinationsInBaseCounts distributionCount
                                        , runsElapsed = state.runsElapsed
                                        }
                                , failure = Nothing
                                }

                    ExpectDistribution _ ->
                        let
                            normalizedDistributionCount : Maybe (Dict (List String) Int)
                            normalizedDistributionCount =
                                Maybe.map includeCombinationsInBaseCounts state.distributionCount
                        in
                        if allSufficientlyCovered c state normalizedDistributionCount then
                            {- Success! Well, almost. Now we need to check the Zero and MoreThanZero cases.

                               Unfortunately I don't see a good way of using the statistical test for this,
                               so we'll just hope the amount of tests we've done so far suffices.
                            -}
                            case findBadZeroRelatedCase c state normalizedDistributionCount of
                                Nothing ->
                                    case normalizedDistributionCount of
                                        Nothing ->
                                            -- Shouldn't happen, we're in the ReportDistribution case. This indicates a bug in `initLoopState`.
                                            distributionBugRunResult

                                        Just distributionCount ->
                                            { distributionReport =
                                                DistributionCheckSucceeded
                                                    { distributionCount = distributionCount
                                                    , runsElapsed = state.runsElapsed
                                                    }
                                            , failure = Nothing
                                            }

                                Just failedLabel ->
                                    distributionFailRunResult normalizedDistributionCount failedLabel

                        else
                            case findInsufficientlyCoveredLabel c state normalizedDistributionCount of
                                Nothing ->
                                    let
                                        newState : LoopState
                                        newState =
                                            runNTimesOrUntilFailure (2 ^ state.nextPowerOfTwo) c state
                                    in
                                    fuzzLoop c { newState | nextPowerOfTwo = newState.nextPowerOfTwo + 1 }

                                Just failedLabel ->
                                    distributionFailRunResult normalizedDistributionCount failedLabel


type alias DistributionFailure =
    { label : String
    , actualPercentage : Float
    , expectedDistribution : ExpectedDistribution
    , runsElapsed : Int
    , distributionCount : Dict (List String) Int
    }


allSufficientlyCovered : LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Bool
allSufficientlyCovered c state normalizedDistributionCount =
    Maybe.map2 Tuple.pair
        normalizedDistributionCount
        (Test.Distribution.Internal.getExpectedDistributions c.distribution)
        |> Maybe.andThen
            (\( distributionCount, expectedDistributions ) ->
                let
                    expectedDistributions_ : Dict String ExpectedDistribution
                    expectedDistributions_ =
                        Dict.fromList expectedDistributions
                in
                distributionCount
                    -- Needs normalized distribution count:
                    |> Dict.toList
                    |> List.filterMap
                        (\( labels, count ) ->
                            case labels of
                                [ onlyLabel ] ->
                                    Just ( onlyLabel, count )

                                _ ->
                                    Nothing
                        )
                    |> Maybe.traverse
                        (\( labels, count ) ->
                            Dict.get labels expectedDistributions_
                                |> Maybe.map (\expectedDistribution -> ( labels, count, expectedDistribution ))
                        )
                    |> Maybe.map
                        (List.all
                            (\( _, count, expectedDistribution ) ->
                                case expectedDistribution of
                                    -- Zero and MoreThanZero will get checked in the Success case
                                    Zero ->
                                        True

                                    MoreThanZero ->
                                        True

                                    AtLeast n ->
                                        Test.Distribution.Internal.sufficientlyCovered state.runsElapsed count (n / 100)
                            )
                        )
            )
        -- `Nothing` means something went wrong. We're answering the question "are all labels sufficiently covered?" and so the way to fail here is `False`.
        |> Maybe.withDefault False


findBadZeroRelatedCase : LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Maybe DistributionFailure
findBadZeroRelatedCase c state normalizedDistributionCount =
    Maybe.map2 Tuple.pair
        normalizedDistributionCount
        (Test.Distribution.Internal.getExpectedDistributions c.distribution)
        |> Maybe.andThen
            (\( distributionCount, expectedDistributions ) ->
                expectedDistributions
                    |> List.find
                        (\( label, expectedDistribution ) ->
                            case expectedDistribution of
                                Zero ->
                                    -- TODO short-circuit Zero sooner: as soon as we increment its counter, during runNTimes.
                                    Dict.get [ label ] distributionCount
                                        -- TODO it would be better if we returned a bug failure here instead of failing with a dummy value
                                        |> Maybe.withDefault 1
                                        |> (/=) 0

                                MoreThanZero ->
                                    Dict.get [ label ] distributionCount
                                        -- TODO it would be better if we returned a bug failure here instead of failing with a dummy value
                                        |> Maybe.withDefault 0
                                        |> (==) 0

                                AtLeast _ ->
                                    False
                        )
                    |> Maybe.andThen
                        (\( label, expectedDistribution ) ->
                            Dict.get [ label ] distributionCount
                                |> Maybe.map
                                    (\count ->
                                        { label = label
                                        , actualPercentage = toFloat count * 100 / toFloat state.runsElapsed
                                        , expectedDistribution = expectedDistribution
                                        , runsElapsed = state.runsElapsed
                                        , distributionCount = distributionCount
                                        }
                                    )
                        )
            )


findInsufficientlyCoveredLabel : LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Maybe DistributionFailure
findInsufficientlyCoveredLabel c state normalizedDistributionCount =
    Maybe.map2 Tuple.pair
        normalizedDistributionCount
        (Test.Distribution.Internal.getExpectedDistributions c.distribution)
        |> Maybe.andThen
            (\( distributionCount, expectedDistributions ) ->
                let
                    expectedDistributions_ : Dict String ExpectedDistribution
                    expectedDistributions_ =
                        Dict.fromList expectedDistributions
                in
                -- TODO loop ExpectedDistributions instead of looping the label combinations?
                distributionCount
                    -- Needs normalized distribution count:
                    |> Dict.toList
                    |> List.filterMap
                        (\( labels, count ) ->
                            case labels of
                                [ onlyLabel ] ->
                                    Dict.get onlyLabel expectedDistributions_
                                        |> Maybe.map (\expectedDistribution -> ( onlyLabel, count, expectedDistribution ))

                                _ ->
                                    Nothing
                        )
                    |> List.find
                        (\( _, count, expectedDistribution ) ->
                            case expectedDistribution of
                                Zero ->
                                    False

                                MoreThanZero ->
                                    False

                                AtLeast n ->
                                    Test.Distribution.Internal.insufficientlyCovered state.runsElapsed count (n / 100)
                        )
                    |> Maybe.map
                        (\( label, count, expectedDistribution ) ->
                            { label = label
                            , actualPercentage = toFloat count * 100 / toFloat state.runsElapsed
                            , expectedDistribution = expectedDistribution
                            , runsElapsed = state.runsElapsed
                            , distributionCount = distributionCount
                            }
                        )
            )


distributionFailRunResult : Maybe (Dict (List String) Int) -> DistributionFailure -> RunResult
distributionFailRunResult normalizedDistributionCount failedLabel =
    case normalizedDistributionCount of
        Nothing ->
            -- Shouldn't happen, we're in the ExpectDistribution case. This indicates a bug in `initLoopState`.
            distributionBugRunResult

        Just distributionCount ->
            { distributionReport =
                DistributionCheckFailed
                    { distributionCount = distributionCount
                    , runsElapsed = failedLabel.runsElapsed
                    , badLabel = failedLabel.label
                    , badLabelPercentage = failedLabel.actualPercentage
                    , expectedDistribution = Test.Distribution.Internal.expectedDistributionToString failedLabel.expectedDistribution
                    }
            , failure = Just <| distributionInsufficientFailure failedLabel
            }


distributionBugRunResult : RunResult
distributionBugRunResult =
    { distributionReport = NoDistribution
    , failure =
        Just
            { given = Nothing
            , expectation =
                Test.Expectation.fail
                    { description = "elm-test distribution collection bug"
                    , reason = Invalid DistributionBug
                    }
            }
    }


distributionInsufficientFailure : DistributionFailure -> Failure
distributionInsufficientFailure failure =
    { given = Nothing
    , expectation =
        Test.Expectation.fail
            { description =
                """Distribution of label "{LABEL}" was insufficient:
  expected:  {EXPECTED_PERCENTAGE}
  got:       {ACTUAL_PERCENTAGE}.

(Generated {RUNS} values.)"""
                    |> String.replace "{LABEL}" failure.label
                    |> String.replace "{EXPECTED_PERCENTAGE}" (formatExpectedDistribution failure.expectedDistribution)
                    |> String.replace "{ACTUAL_PERCENTAGE}" (Test.Distribution.Internal.formatPct failure.actualPercentage)
                    |> String.replace "{RUNS}" (String.fromInt failure.runsElapsed)
            , reason = Invalid DistributionInsufficient
            }
    }


runNTimesOrUntilFailure : Int -> LoopConstants a -> LoopState -> LoopState
runNTimesOrUntilFailure times c state =
    if times <= 0 || state.failure /= Nothing then
        state

    else
        runNTimesOrUntilFailure (times - 1) c (runOnce c state)


generateOrPickFromCorpus :
    Fuzzer a
    -> LoopState
    ->
        ( Maybe RandomRun
        , Maybe BucketedEdgeHitCounts
        , Random.Seed
        )
generateOrPickFromCorpus fuzzer state =
    let
        ( corpusInput, newSeed ) =
            Random.step
                (Fuzz.InputCorpus.generator state.inputCorpus)
                state.currentSeed
    in
    case corpusInput of
        Nothing ->
            ( Nothing
            , Nothing
            , newSeed
            )

        Just ( randomRun, previousInputBucketedEdgeHitCounts ) ->
            ( Just randomRun
            , Just previousInputBucketedEdgeHitCounts
            , newSeed
            )


{-| Generate a fuzzed value (fully random or picked from , test it, record the simplified test failure if any
and optionally categorize the value.
-}
runOnce : LoopConstants a -> LoopState -> LoopState
runOnce c state =
    let
        {- It's possible this value was picked from  from a mutated RandomRun from
           some previous interesting corpus input.

           In that case we need the previous input's coverage (edge hit
           counts), to see if the new input's coverage hit some paths way more
           often.
        -}
        ( randomRun, previousInputBucketedEdgeHitCounts, nextSeed ) =
            generateOrPickFromCorpus c.fuzzer state

        ( maybeFailure, newDistributionCounter, newCorpus ) =
            case genResult of
                Rejected { reason } ->
                    ( Just
                        { given = Nothing
                        , expectation =
                            Test.Expectation.fail
                                { description = reason
                                , reason = Invalid InvalidFuzzer
                                }
                        }
                    , state.distributionCount
                    , state.inputCorpus
                    )

                Generated { prng, value } ->
                    let
                        _ =
                            {- This will make sure we start collecting instrumented edges
                               with a clean slate if we're collecting edges at all: we might
                               not be running in an instrumented context, in which case
                               this will be a no-op.

                               We're specifically interested in the `expectation = c.testFn value`
                               below - that's going to call the contents of the tests,
                               and give us coverage of the tested "application" code.
                            -}
                            Test.Coverage.resetEdgeCoverage ()
                    in
                    let
                        expectation : Expectation
                        expectation =
                            c.testFn value
                    in
                    let
                        edgeCoverage : EdgeCoverage
                        edgeCoverage =
                            Test.Coverage.getEdgeCoverage ()
                    in
                    {- The three let-exprs above are intentionally separate:
                       that way we're sure nothing else runs in between them
                       and pollutes the coverage data.
                    -}
                    let
                        failureMessage : Maybe String
                        failureMessage =
                            expectation
                                |> Test.Expectation.getReason
                                |> Maybe.map Test.Internal.toString

                        newBucketed : BucketedEdgeHitCounts
                        newBucketed =
                            Test.Coverage.EdgeHitCounts.bucketed edgeCoverage.edgeHitCounts

                        isInterestingDueToBucketChange : Bool
                        isInterestingDueToBucketChange =
                            case previousInputBucketedEdgeHitCounts of
                                Nothing ->
                                    False

                                Just previousBucketed ->
                                    Test.Coverage.EdgeHitCounts.isImprovementOver
                                        previousBucketed
                                        newBucketed

                        isRunInterestingForCorpus : Bool
                        isRunInterestingForCorpus =
                            {- TODO investigate: we could also think about
                               whether the test finished too slowly and prevent
                               it from entering the corpus. But instead, for
                               now, we're relying on the corpus culling
                               (marking the most performant "base" of the
                               corpus as favored).

                               For skipping (even failing) inputs based on the
                               runtime being too long, we'd need to know a
                               baseline of how long a test _should_ take.
                               Unsure what that would be right now.
                            -}
                            {- Failing tests are automatically interesting:
                               we like test failures!
                            -}
                            (failureMessage /= Nothing)
                                || {- If we've hit a new code path, we'd like
                                      to keep its RandomRun around in the corpus,
                                      even if it didn't cause the test to fail.
                                   -}
                                   (edgeCoverage.newPaths > 0)
                                || {- Change in paths covered doesn't need to
                                      be strictly "found a completely new
                                      path", we also care about whether the
                                      input hit a certain edge more often than
                                      the input it was mutated from.

                                      This is done in buckets:
                                      1,2,3,4-7,8-15,16-31,32-127,128+.

                                      So if it changed from 9 to 11, we don't care,
                                      but if it changed from 9 to 18, we do!
                                   -}
                                   isInterestingDueToBucketChange

                        newCorpus_ : InputCorpus
                        newCorpus_ =
                            if isRunInterestingForCorpus then
                                state.inputCorpus
                                    |> Fuzz.Mutate.deterministicallyMutate randomRun
                                        (\newRun ->
                                            Fuzz.InputCorpus.add
                                                newRun
                                                edgeCoverage.durationMs
                                                newBucketed
                                        )

                            else
                                state.inputCorpus

                        randomRun : RandomRun
                        randomRun =
                            PRNG.getRun prng

                        failure : Maybe Failure
                        failure =
                            testGeneratedValue
                                { getExpectation = c.testFn
                                , fuzzer = c.fuzzer
                                , randomRun = randomRun
                                , value = value
                                , expectation = expectation
                                }

                        distributionCounter : Maybe (Dict (List String) Int)
                        distributionCounter =
                            Maybe.map2
                                (\labels old ->
                                    let
                                        foundLabels : List String
                                        foundLabels =
                                            labels
                                                |> List.filterMap
                                                    (\( label, predicate ) ->
                                                        if predicate value then
                                                            Just label

                                                        else
                                                            Nothing
                                                    )
                                    in
                                    Dict.increment foundLabels old
                                )
                                (Test.Distribution.Internal.getDistributionLabels c.distribution)
                                state.distributionCount
                    in
                    ( failure, distributionCounter, newCorpus_ )
    in
    { state
        | failure = maybeFailure
        , distributionCount = newDistributionCounter
        , currentSeed = nextSeed
        , runsElapsed = state.runsElapsed + 1
        , inputCorpus = newCorpus
    }


includeCombinationsInBaseCounts : Dict (List String) Int -> Dict (List String) Int
includeCombinationsInBaseCounts distribution =
    distribution
        |> Dict.map
            (\labels count ->
                case labels of
                    [ single ] ->
                        let
                            combinations : List Int
                            combinations =
                                distribution
                                    |> Dict.filter (\k _ -> List.length k > 1 && List.member single k)
                                    |> Dict.values
                        in
                        count + List.sum combinations

                    _ ->
                        count
            )


formatExpectedDistribution : ExpectedDistribution -> String
formatExpectedDistribution expected =
    case expected of
        Zero ->
            "exactly 0%"

        MoreThanZero ->
            "more than 0%"

        AtLeast n ->
            Test.Distribution.Internal.formatPct n


type alias RunResult =
    { distributionReport : DistributionReport
    , failure : Maybe Failure
    }


testGeneratedValue : Simplify.State a -> Maybe Failure
testGeneratedValue state =
    case state.expectation of
        Pass _ ->
            Nothing

        Fail _ ->
            Just <| findSimplestFailure state


findSimplestFailure : Simplify.State a -> Failure
findSimplestFailure state =
    let
        ( simplestValue, _, expectation ) =
            Simplify.simplify state
    in
    { given = Just <| Test.Internal.toString simplestValue
    , expectation = expectation
    }


formatExpectation : Failure -> Expectation
formatExpectation { given, expectation } =
    case given of
        Nothing ->
            expectation

        Just given_ ->
            Test.Expectation.withGiven given_ expectation
