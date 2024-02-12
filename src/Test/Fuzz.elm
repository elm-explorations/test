module Test.Fuzz (fuzzTest) where

import Dict (Dict)
import Dict as Dict
import Fuzz.Internal (Fuzzer)
import Fuzz.Internal as Fuzz.Internal
import GenResult (GenResult(..))
import GenResult as GenResult
import MicroDictExtra as Dict

import MicroListExtra as List

import MicroMaybeExtra as Maybe

import PRNG as PRNG
import Random as Random
import Simplify as Simplify
import Test.Distribution (DistributionReport(..))
import Test.Distribution as Test.Distribution
import Test.Distribution.Internal (Distribution(..), ExpectedDistribution(..))
import Test.Distribution.Internal as Test.Distribution.Internal
import Test.Expectation (Expectation(..))
import Test.Expectation as Test.Expectation
import Test.Internal (Test(..), blankDescriptionFailure)
import Test.Internal as Test.Internal
import Test.Runner.Distribution as Test.Runner.Distribution
import Test.Runner.Failure (InvalidReason(..), Reason(..))
import Test.Runner.Failure as Test.Runner.Failure


{-| Reject always-failing tests because of bad names or invalid fuzzers.
-}
fuzzTest :: Distribution a -> Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzTest distribution fuzzer untrimmedDesc getExpectation =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        blankDescriptionFailure

    else
        ElmTestVariant__Labeled desc <| validatedFuzzTest fuzzer getExpectation distribution


{-| Knowing that the fuzz test isn't obviously invalid, run the test and package up the results.
-}
validatedFuzzTest :: Fuzzer a -> (a -> Expectation) -> Distribution a -> Test
validatedFuzzTest fuzzer getExpectation distribution =
    ElmTestVariant__FuzzTest
        (\seed runs ->
            let
                runResult :: RunResult
                runResult =
                    fuzzLoop
                        { fuzzer : fuzzer
                        , testFn : getExpectation
                        , initialSeed : seed
                        , runsNeeded : runs
                        , distribution : distribution
                        }
                        (initLoopState seed distribution)
            in
            case runResult.failure of
                Nothing ->
                    [ Pass { distributionReport : runResult.distributionReport } ]

                Just failure ->
                    [ ( failure { expectation =
                            failure.expectation
                                |> Test.Expectation.withDistributionReport runResult.distributionReport
                       })
                        |> formatExpectation
                    ]
        )


type Failure =
    { given :: Maybe String
    , expectation :: Expectation
    }


type LoopConstants a =
    { fuzzer :: Fuzzer a
    , testFn :: a -> Expectation
    , initialSeed :: Random.Seed
    , runsNeeded :: Int
    , distribution :: Distribution a
    }


type LoopState =
    { runsElapsed :: Int
    , distributionCount :: Maybe (Dict (List String) Int)
    , nextPowerOfTwo :: Int
    , failure :: Maybe Failure
    , currentSeed :: Random.Seed
    }


initLoopState :: Random.Seed -> Distribution a -> LoopState
initLoopState initialSeed distribution =
    let
        initialDistributionCount :: Maybe (Dict (List String) Int)
        initialDistributionCount =
            Test.Distribution.Internal.getDistributionLabels distribution
                |> Maybe.map
                    (\labels ->
                        labels
                            |> List.map (\( label, _ ) -> ( [ label ], 0 ))
                            |> Dict.fromList
                    )
    in
    { runsElapsed : 0
    , distributionCount : initialDistributionCount
    , nextPowerOfTwo : 1
    , failure : Nothing
    , currentSeed : initialSeed
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
fuzzLoop :: LoopConstants a -> LoopState -> RunResult
fuzzLoop c state =
    case state.failure of
        Just failure ->
            -- If the test fails, it still is useful to report the distribution even if we didn't do the statistical check for ExpectDistribution.
            -- For this reason we try to create DistributionToReport even in case of ExpectDistribution.
            { distributionReport :
                case state.distributionCount of
                    Nothing ->
                        NoDistribution

                    Just distributionCount ->
                        DistributionToReport
                            { distributionCount : includeCombinationsInBaseCounts distributionCount
                            , runsElapsed : state.runsElapsed
                            }
            , failure : Just failure
            }

        Nothing ->
            if state.runsElapsed < c.runsNeeded then
                let
                    newState :: LoopState
                    newState =
                        runNTimes (c.runsNeeded - state.runsElapsed) c state
                in
                fuzzLoop c newState

            else
                case c.distribution of
                    NoDistributionNeeded ->
                        { distributionReport : NoDistribution
                        , failure : Nothing
                        }

                    ReportDistribution _ ->
                        case state.distributionCount of
                            Nothing ->
                                -- Shouldn't happen, we're in the ReportDistribution case. This indicates a bug in `initLoopState`.
                                distributionBugRunResult

                            Just distributionCount ->
                                { distributionReport :
                                    DistributionToReport
                                        { distributionCount : includeCombinationsInBaseCounts distributionCount
                                        , runsElapsed : state.runsElapsed
                                        }
                                , failure : Nothing
                                }

                    ExpectDistribution _ ->
                        let
                            normalizedDistributionCount :: Maybe (Dict (List String) Int)
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
                                            { distributionReport :
                                                DistributionCheckSucceeded
                                                    { distributionCount : distributionCount
                                                    , runsElapsed : state.runsElapsed
                                                    }
                                            , failure : Nothing
                                            }

                                Just failedLabel ->
                                    distributionFailRunResult normalizedDistributionCount failedLabel

                        else
                            case findInsufficientlyCoveredLabel c state normalizedDistributionCount of
                                Nothing ->
                                    let
                                        newState :: LoopState
                                        newState =
                                            runNTimes (2 ^ state.nextPowerOfTwo) c state
                                    in
                                    fuzzLoop c ( newState { nextPowerOfTwo = newState.nextPowerOfTwo + 1  })

                                Just failedLabel ->
                                    distributionFailRunResult normalizedDistributionCount failedLabel


type DistributionFailure =
    { label :: String
    , actualPercentage :: Float
    , expectedDistribution :: ExpectedDistribution
    , runsElapsed :: Int
    , distributionCount :: Dict (List String) Int
    }


allSufficientlyCovered :: LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Bool
allSufficientlyCovered c state normalizedDistributionCount =
    Maybe.map2 Tuple.pair
        normalizedDistributionCount
        (Test.Distribution.Internal.getExpectedDistributions c.distribution)
        |> Maybe.andThen
            (\{a:distributionCount, b:expectedDistributions } ->
                let
                    expectedDistributions_ :: Dict String ExpectedDistribution
                    expectedDistributions_ =
                        Dict.fromList expectedDistributions
                in
                distributionCount
                    -- Needs normalized distribution count:
                    |> Dict.toList
                    |> List.filterMap
                        (\{a:labels, b:count } ->
                            case labels of
                                [ onlyLabel ] ->
                                    Just {a:onlyLabel, b:count }

                                _ ->
                                    Nothing
                        )
                    |> Maybe.traverse
                        (\{a:labels, b:count } ->
                            Dict.get labels expectedDistributions_
                                |> Maybe.map (\expectedDistribution -> {a:labels, b:count, c:expectedDistribution })
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


findBadZeroRelatedCase :: LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Maybe DistributionFailure
findBadZeroRelatedCase c state normalizedDistributionCount =
    Maybe.map2 Tuple.pair
        normalizedDistributionCount
        (Test.Distribution.Internal.getExpectedDistributions c.distribution)
        |> Maybe.andThen
            (\{a:distributionCount, b:expectedDistributions } ->
                expectedDistributions
                    |> List.find
                        (\{a:label, b:expectedDistribution } ->
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
                        (\{a:label, b:expectedDistribution } ->
                            Dict.get [ label ] distributionCount
                                |> Maybe.map
                                    (\count ->
                                        { label : label
                                        , actualPercentage : toFloat count * 100 / toFloat state.runsElapsed
                                        , expectedDistribution : expectedDistribution
                                        , runsElapsed : state.runsElapsed
                                        , distributionCount : distributionCount
                                        }
                                    )
                        )
            )


findInsufficientlyCoveredLabel :: LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Maybe DistributionFailure
findInsufficientlyCoveredLabel c state normalizedDistributionCount =
    Maybe.map2 Tuple.pair
        normalizedDistributionCount
        (Test.Distribution.Internal.getExpectedDistributions c.distribution)
        |> Maybe.andThen
            (\{a:distributionCount, b:expectedDistributions } ->
                let
                    expectedDistributions_ :: Dict String ExpectedDistribution
                    expectedDistributions_ =
                        Dict.fromList expectedDistributions
                in
                -- TODO loop ExpectedDistributions instead of looping the label combinations?
                distributionCount
                    -- Needs normalized distribution count:
                    |> Dict.toList
                    |> List.filterMap
                        (\{a:labels, b:count } ->
                            case labels of
                                [ onlyLabel ] ->
                                    Dict.get onlyLabel expectedDistributions_
                                        |> Maybe.map (\expectedDistribution -> {a:onlyLabel, b:count, c:expectedDistribution })

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
                        (\{a:label, b:count, c:expectedDistribution } ->
                            { label : label
                            , actualPercentage : toFloat count * 100 / toFloat state.runsElapsed
                            , expectedDistribution : expectedDistribution
                            , runsElapsed : state.runsElapsed
                            , distributionCount : distributionCount
                            }
                        )
            )


distributionFailRunResult :: Maybe (Dict (List String) Int) -> DistributionFailure -> RunResult
distributionFailRunResult normalizedDistributionCount failedLabel =
    case normalizedDistributionCount of
        Nothing ->
            -- Shouldn't happen, we're in the ExpectDistribution case. This indicates a bug in `initLoopState`.
            distributionBugRunResult

        Just distributionCount ->
            { distributionReport :
                DistributionCheckFailed
                    { distributionCount : distributionCount
                    , runsElapsed : failedLabel.runsElapsed
                    , badLabel : failedLabel.label
                    , badLabelPercentage : failedLabel.actualPercentage
                    , expectedDistribution : Test.Distribution.Internal.expectedDistributionToString failedLabel.expectedDistribution
                    }
            , failure : Just <| distributionInsufficientFailure failedLabel
            }


distributionBugRunResult :: RunResult
distributionBugRunResult =
    { distributionReport : NoDistribution
    , failure :
        Just
            { given : Nothing
            , expectation :
                Test.Expectation.fail
                    { description : "elm-test distribution collection bug"
                    , reason : Invalid DistributionBug
                    }
            }
    }


distributionInsufficientFailure :: DistributionFailure -> Failure
distributionInsufficientFailure failure =
    { given : Nothing
    , expectation :
        Test.Expectation.fail
            { description :
                """Distribution of label "{LABEL}" was insufficient:
  expected:  {EXPECTED_PERCENTAGE}
  got:       {ACTUAL_PERCENTAGE}.

(Generated {RUNS} values.)"""
                    |> String.replace "{LABEL}" failure.label
                    |> String.replace "{EXPECTED_PERCENTAGE}" (formatExpectedDistribution failure.expectedDistribution)
                    |> String.replace "{ACTUAL_PERCENTAGE}" (Test.Distribution.Internal.formatPct failure.actualPercentage)
                    |> String.replace "{RUNS}" (String.fromInt failure.runsElapsed)
            , reason : Invalid DistributionInsufficient
            }
    }


{-| Short-circuits on failure.
-}
runNTimes :: Int -> LoopConstants a -> LoopState -> LoopState
runNTimes times c state =
    if times <= 0 || state.failure /= Nothing then
        state

    else
        runNTimes (times - 1) c (runOnce c state)


{-| Generate a fuzzed value, test it, record the simplified test failure if any
and optionally categorize the value.
-}
runOnce :: LoopConstants a -> LoopState -> LoopState
runOnce c state =
    let
        genResult :: GenResult a
        genResult =
            Fuzz.Internal.generate
                (PRNG.random state.currentSeed)
                c.fuzzer

        maybeNextSeed :: Maybe Random.Seed
        maybeNextSeed =
            genResult
                |> GenResult.getPrng
                |> PRNG.getSeed

        nextSeed :: Random.Seed
        nextSeed =
            case maybeNextSeed of
                Just seed ->
                    seed

                Nothing ->
                    stepSeed state.currentSeed

        {a:maybeFailure, b:newDistributionCounter } =
            case genResult of
                Rejected { reason } ->
                    ( Just
                        { given : Nothing
                        , expectation :
                            Test.Expectation.fail
                                { description : reason
                                , reason : Invalid InvalidFuzzer
                                }
                        }
                    , state.distributionCount
                    )

                Generated { prng, value } ->
                    let
                        failure :: Maybe Failure
                        failure =
                            testGeneratedValue
                                { getExpectation : c.testFn
                                , fuzzer : c.fuzzer
                                , randomRun : PRNG.getRun prng
                                , value : value
                                , expectation : c.testFn value
                                }

                        distributionCounter :: Maybe (Dict (List String) Int)
                        distributionCounter =
                            Maybe.map2
                                (\labels old ->
                                    let
                                        foundLabels :: List String
                                        foundLabels =
                                            labels
                                                |> List.filterMap
                                                    (\{a:label, b:predicate } ->
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
                    {a:failure, b:distributionCounter }
    in
    { state
        | failure = maybeFailure
        , distributionCount : newDistributionCounter
        , currentSeed : nextSeed
        , runsElapsed : state.runsElapsed + 1
    }


includeCombinationsInBaseCounts :: Dict (List String) Int -> Dict (List String) Int
includeCombinationsInBaseCounts distribution =
    distribution
        |> Dict.map
            (\labels count ->
                case labels of
                    [ single ] ->
                        let
                            combinations :: List Int
                            combinations =
                                distribution
                                    |> Dict.filter (\k _ -> List.length k > 1 && List.member single k)
                                    |> Dict.values
                        in
                        count + List.sum combinations

                    _ ->
                        count
            )


formatExpectedDistribution :: ExpectedDistribution -> String
formatExpectedDistribution expected =
    case expected of
        Zero ->
            "exactly 0%"

        MoreThanZero ->
            "more than 0%"

        AtLeast n ->
            Test.Distribution.Internal.formatPct n


type RunResult =
    { distributionReport :: DistributionReport
    , failure :: Maybe Failure
    }


{-| Random.next is private ¯\_(ツ)\_/¯
-}
stepSeed :: Random.Seed -> Random.Seed
stepSeed seed =
    seed
        |> Random.step (Random.int 0 0)
        |> Tuple.second


testGeneratedValue :: Simplify.State a -> Maybe Failure
testGeneratedValue state =
    case state.expectation of
        Pass _ ->
            Nothing

        Fail _ ->
            Just <| findSimplestFailure state


findSimplestFailure :: Simplify.State a -> Failure
findSimplestFailure state =
    let
        ( simplestValue, _, expectation ) =
            Simplify.simplify state
    in
    { given : Just <| Test.Internal.toString simplestValue
    , expectation : expectation
    }


formatExpectation :: Failure -> Expectation
formatExpectation { given, expectation } =
    case given of
        Nothing ->
            expectation

        Just given_ ->
            Test.Expectation.withGiven given_ expectation
