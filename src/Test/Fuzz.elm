module Test.Fuzz exposing (fuzzTest)

import DebugConfig
import Dict exposing (Dict)
import Fuzz.Internal exposing (Fuzzer)
import GenResult exposing (GenResult(..))
import MicroDictExtra as Dict
import MicroListExtra as List
import PRNG
import Random
import Simplify
import Test.Distribution exposing (DistributionReport(..))
import Test.Distribution.Internal exposing (Distribution(..), ExpectedDistribution(..))
import Test.Expectation exposing (Expectation(..))
import Test.Internal exposing (Test(..), blankDescriptionFailure)
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| Reject always-failing tests because of bad names or invalid fuzzers.
-}
fuzzTest : String -> Distribution a -> Fuzzer a -> (a -> Expectation) -> Test
fuzzTest untrimmedDesc distribution fuzzer getExpectation =
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
                    Pass runResult.distributionReport

                Just failure ->
                    formatExpectation
                        failure.given
                        (Test.Expectation.withDistributionReport runResult.distributionReport failure.expectation)
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
    }


initLoopState : Random.Seed -> Distribution a -> LoopState
initLoopState initialSeed distribution =
    let
        initialDistributionCount : Maybe (Dict (List String) Int)
        initialDistributionCount =
            Test.Distribution.Internal.getDistributionLabels distribution
                |> Maybe.map
                    (\labels ->
                        List.foldl
                            (\( label, _ ) dict -> Dict.insert [ label ] 0 dict)
                            Dict.empty
                            labels
                    )
    in
    { runsElapsed = 0
    , distributionCount = initialDistributionCount
    , nextPowerOfTwo = 1
    , failure = Nothing
    , currentSeed = initialSeed
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
                        Fuzz.Internal.noDistribution

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
                        runNTimes (c.runsNeeded - state.runsElapsed) c state
                in
                fuzzLoop c newState

            else
                case c.distribution of
                    NoDistributionNeeded ->
                        { distributionReport = Fuzz.Internal.noDistribution
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
                                            runNTimes (2 ^ state.nextPowerOfTwo) c state
                                    in
                                    fuzzLoop c { newState | nextPowerOfTwo = newState.nextPowerOfTwo + 1 }

                                Just failedLabel ->
                                    distributionFailRunResult normalizedDistributionCount failedLabel


type alias DistributionFailure =
    { label : String
    , actualPercentage : Float
    , expectedDistribution : ExpectedDistribution
    , runsElapsed : Int
    }


allSufficientlyCovered : LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Bool
allSufficientlyCovered c state normalizedDistributionCount =
    case normalizedDistributionCount of
        Nothing ->
            False

        Just distributionCount ->
            case Test.Distribution.Internal.getExpectedDistributions c.distribution of
                Nothing ->
                    False

                Just expectedDistributions ->
                    -- Needs normalized distribution count:
                    Dict.foldr
                        (\labels count soFar ->
                            case labels of
                                [ onlyLabel ] ->
                                    soFar && isLabelSufficientlyCovered state.runsElapsed expectedDistributions onlyLabel count

                                _ ->
                                    soFar
                        )
                        True
                        distributionCount


isLabelSufficientlyCovered : Int -> Dict String ExpectedDistribution -> String -> Int -> Bool
isLabelSufficientlyCovered runsElapsed expectedDistributions labels count =
    case Dict.get labels expectedDistributions of
        Nothing ->
            -- `Nothing` means something went wrong. We're answering the question "are all labels sufficiently covered?" and so the way to fail here is `False`.
            False

        Just expectedDistribution ->
            case expectedDistribution of
                -- Zero and MoreThanZero will get checked in the Success case
                Zero ->
                    True

                MoreThanZero ->
                    True

                AtLeast n ->
                    Test.Distribution.Internal.sufficientlyCovered runsElapsed count (n / 100)


findBadZeroRelatedCase : LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Maybe DistributionFailure
findBadZeroRelatedCase c state normalizedDistributionCount =
    case normalizedDistributionCount of
        Nothing ->
            Nothing

        Just distributionCount ->
            case Test.Distribution.Internal.getExpectedDistributionsAsList c.distribution of
                Nothing ->
                    Nothing

                Just expectedDistributions ->
                    expectedDistributions
                        |> List.find
                            (\( expectedDistribution, label, _ ) ->
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
                            (\( expectedDistribution, label, _ ) ->
                                Dict.get [ label ] distributionCount
                                    |> Maybe.map
                                        (\count ->
                                            { label = label
                                            , actualPercentage = toFloat count * 100 / toFloat state.runsElapsed
                                            , expectedDistribution = expectedDistribution
                                            , runsElapsed = state.runsElapsed
                                            }
                                        )
                            )


findInsufficientlyCoveredLabel : LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Maybe DistributionFailure
findInsufficientlyCoveredLabel c state normalizedDistributionCount =
    case normalizedDistributionCount of
        Nothing ->
            Nothing

        Just distributionCount ->
            case Test.Distribution.Internal.getExpectedDistributions c.distribution of
                Nothing ->
                    Nothing

                Just expectedDistributions ->
                    -- TODO loop ExpectedDistributions instead of looping the label combinations?
                    distributionCount
                        -- Needs normalized distribution count:
                        |> Dict.toList
                        |> List.findMap
                            (\( labels, count ) ->
                                case labels of
                                    [ onlyLabel ] ->
                                        case Dict.get onlyLabel expectedDistributions of
                                            Just Zero ->
                                                Nothing

                                            Just MoreThanZero ->
                                                Nothing

                                            Just ((AtLeast n) as expectedDistribution) ->
                                                if Test.Distribution.Internal.insufficientlyCovered state.runsElapsed count (n / 100) then
                                                    Just
                                                        { label = onlyLabel
                                                        , actualPercentage = toFloat count * 100 / toFloat state.runsElapsed
                                                        , expectedDistribution = expectedDistribution
                                                        , runsElapsed = state.runsElapsed
                                                        }

                                                else
                                                    Nothing

                                            Nothing ->
                                                Nothing

                                    _ ->
                                        Nothing
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
    { distributionReport = Fuzz.Internal.noDistribution
    , failure =
        Just
            { given = Nothing
            , expectation =
                Test.Expectation.Fail
                    { given = Nothing
                    , distributionReport = Fuzz.Internal.noDistribution
                    , description = "elm-test distribution collection bug"
                    , reason = Invalid DistributionBug
                    }
            }
    }


distributionInsufficientFailure : DistributionFailure -> Failure
distributionInsufficientFailure failure =
    { given = Nothing
    , expectation =
        Test.Expectation.Fail
            { given = Nothing
            , distributionReport = Fuzz.Internal.noDistribution
            , description =
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


{-| Short-circuits on failure.
-}
runNTimes : Int -> LoopConstants a -> LoopState -> LoopState
runNTimes times c state =
    if times <= 0 || state.failure /= Nothing then
        state

    else
        runNTimes (times - 1) c (runOnce c state)


{-| Generate a fuzzed value, test it, record the simplified test failure if any
and optionally categorize the value.
-}
runOnce : LoopConstants a -> LoopState -> LoopState
runOnce c state =
    let
        genResult : GenResult a
        genResult =
            Fuzz.Internal.generate
                (PRNG.random state.currentSeed)
                c.fuzzer

        maybeNextSeed : Maybe Random.Seed
        maybeNextSeed =
            genResult
                |> GenResult.getPrng
                |> PRNG.getSeed

        nextSeed : Random.Seed
        nextSeed =
            case maybeNextSeed of
                Just seed ->
                    seed

                Nothing ->
                    stepSeed state.currentSeed

        ( maybeFailure, newDistributionCounter ) =
            case genResult of
                Rejected { reason } ->
                    ( Just
                        { given = Nothing
                        , expectation =
                            Test.Expectation.Fail
                                { given = Nothing
                                , distributionReport = Fuzz.Internal.noDistribution
                                , description = reason
                                , reason = Invalid InvalidFuzzer
                                }
                        }
                    , state.distributionCount
                    )

                Generated { prng, value } ->
                    let
                        failure : Maybe Failure
                        failure =
                            testGeneratedValue
                                { getExpectation = c.testFn
                                , fuzzer = c.fuzzer
                                , randomRun = PRNG.getRun prng
                                , value = value
                                , expectation = c.testFn value
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
                    ( failure, distributionCounter )
    in
    { failure = maybeFailure
    , distributionCount = newDistributionCounter
    , currentSeed = nextSeed
    , runsElapsed = state.runsElapsed + 1
    , nextPowerOfTwo = state.nextPowerOfTwo
    }


includeCombinationsInBaseCounts : Dict (List String) Int -> Dict (List String) Int
includeCombinationsInBaseCounts distribution =
    distribution
        |> Dict.map
            (\labels count ->
                case labels of
                    [ single ] ->
                        Dict.foldr
                            (\k value sum ->
                                if List.hasMultipleItems k && List.member single k then
                                    value + sum

                                else
                                    sum
                            )
                            count
                            distribution

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


{-| Random.next is private ¯\_(ツ)\_/¯
-}
stepSeed : Random.Seed -> Random.Seed
stepSeed seed =
    seed
        |> Random.step (Random.int 0 0)
        |> Tuple.second


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


formatExpectation : Maybe String -> Expectation -> Expectation
formatExpectation given expectation =
    case given of
        Nothing ->
            expectation

        Just given_ ->
            Test.Expectation.withGiven given_ expectation
