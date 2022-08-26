module Test.Fuzz exposing (fuzzTest)

import Dict exposing (Dict)
import Fuzz.Internal exposing (Fuzzer)
import GenResult exposing (GenResult(..))
import MicroDictExtra as Dict
import MicroListExtra as List
import MicroMaybeExtra as Maybe
import PRNG
import Random
import Simplify
import Test.Coverage exposing (CoverageReport(..))
import Test.Coverage.Internal exposing (Coverage(..), ExpectedCoverage(..))
import Test.Expectation exposing (Expectation(..))
import Test.Internal exposing (Test(..), blankDescriptionFailure)
import Test.Runner.Coverage
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| Reject always-failing tests because of bad names or invalid fuzzers.
-}
fuzzTest : Coverage a -> Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzTest coverage fuzzer untrimmedDesc getExpectation =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        blankDescriptionFailure

    else
        ElmTestVariant__Labeled desc <| validatedFuzzTest fuzzer getExpectation coverage


{-| Knowing that the fuzz test isn't obviously invalid, run the test and package up the results.
-}
validatedFuzzTest : Fuzzer a -> (a -> Expectation) -> Coverage a -> Test
validatedFuzzTest fuzzer getExpectation coverage =
    ElmTestVariant__FuzzTest
        (\seed runs ->
            let
                runResult : RunResult
                runResult =
                    fuzzLoop
                        { fuzzer = fuzzer
                        , testFn = getExpectation
                        , initialSeed = seed
                        , runsNeeded = runs
                        , coverage = coverage
                        }
                        (initLoopState seed coverage)
            in
            case runResult.failure of
                Nothing ->
                    [ Pass { coverageReport = runResult.coverageReport } ]

                Just failure ->
                    [ { failure
                        | expectation =
                            failure.expectation
                                |> Test.Expectation.withCoverageReport runResult.coverageReport
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
    , coverage : Coverage a
    }


type alias LoopState =
    { runsElapsed : Int
    , coverageCount : Maybe (Dict (List String) Int)
    , nextPowerOfTwo : Int
    , failure : Maybe Failure
    , currentSeed : Random.Seed
    }


initLoopState : Random.Seed -> Coverage a -> LoopState
initLoopState initialSeed coverage =
    let
        initialCoverageCount : Maybe (Dict (List String) Int)
        initialCoverageCount =
            Test.Coverage.Internal.getCoverageLabels coverage
                |> Maybe.map
                    (\labels ->
                        labels
                            |> List.map (\( label, _ ) -> ( [ label ], 0 ))
                            |> Dict.fromList
                    )
    in
    { runsElapsed = 0
    , coverageCount = initialCoverageCount
    , nextPowerOfTwo = 1
    , failure = Nothing
    , currentSeed = initialSeed
    }


{-| Runs fuzz tests repeatedly and returns information about coverage and possible failure.

The loop algorithm is roughly:

    if any failure:
        end with failure

    else if not enough tests ran (elapsed < total):
        run `total - elapsed` tests (short-circuiting on failure)
        loop

    else if doesn't need coverage check:
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
            -- If the test fails, it still is useful to report the coverage even if we didn't do the statistical check for ExpectCoverage.
            -- For this reason we try to create CoverageToReport even in case of ExpectCoverage.
            { coverageReport =
                case state.coverageCount of
                    Nothing ->
                        NoCoverage

                    Just coverageCount ->
                        CoverageToReport
                            { coverageCount = includeCombinationsInBaseCounts coverageCount
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
                case c.coverage of
                    NoCoverageNeeded ->
                        { coverageReport = NoCoverage
                        , failure = Nothing
                        }

                    ReportCoverage _ ->
                        case state.coverageCount of
                            Nothing ->
                                -- Shouldn't happen, we're in the ReportCoverage case. This indicates a bug in `initLoopState`.
                                coverageBugRunResult

                            Just coverageCount ->
                                { coverageReport =
                                    CoverageToReport
                                        { coverageCount = includeCombinationsInBaseCounts coverageCount
                                        , runsElapsed = state.runsElapsed
                                        }
                                , failure = Nothing
                                }

                    ExpectCoverage _ ->
                        let
                            normalizedCoverageCount : Maybe (Dict (List String) Int)
                            normalizedCoverageCount =
                                Maybe.map includeCombinationsInBaseCounts state.coverageCount
                        in
                        if allSufficientlyCovered c state normalizedCoverageCount then
                            {- Success! Well, almost. Now we need to check the Zero and MoreThanZero cases.

                               Unfortunately I don't see a good way of using the statistical test for this,
                               so we'll just hope the amount of tests we've done so far suffices.
                            -}
                            case findBadZeroRelatedCase c state normalizedCoverageCount of
                                Nothing ->
                                    case normalizedCoverageCount of
                                        Nothing ->
                                            -- Shouldn't happen, we're in the ReportCoverage case. This indicates a bug in `initLoopState`.
                                            coverageBugRunResult

                                        Just coverageCount ->
                                            { coverageReport =
                                                CoverageCheckSucceeded
                                                    { coverageCount = coverageCount
                                                    , runsElapsed = state.runsElapsed
                                                    }
                                            , failure = Nothing
                                            }

                                Just failedLabel ->
                                    coverageFailRunResult normalizedCoverageCount failedLabel

                        else
                            case findInsufficientlyCoveredLabel c state normalizedCoverageCount of
                                Nothing ->
                                    let
                                        newState : LoopState
                                        newState =
                                            runNTimes (2 ^ state.nextPowerOfTwo) c state
                                    in
                                    fuzzLoop c { newState | nextPowerOfTwo = newState.nextPowerOfTwo + 1 }

                                Just failedLabel ->
                                    coverageFailRunResult normalizedCoverageCount failedLabel


type alias CoverageFailure =
    { label : String
    , actualPercentage : Float
    , expectedCoverage : ExpectedCoverage
    , runsElapsed : Int
    , coverageCount : Dict (List String) Int
    }


allSufficientlyCovered : LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Bool
allSufficientlyCovered c state normalizedCoverageCount =
    Maybe.map2 Tuple.pair
        normalizedCoverageCount
        (Test.Coverage.Internal.getExpectedCoverages c.coverage)
        |> Maybe.andThen
            (\( coverageCount, expectedCoverages ) ->
                let
                    expectedCoverages_ : Dict String ExpectedCoverage
                    expectedCoverages_ =
                        Dict.fromList expectedCoverages
                in
                coverageCount
                    -- Needs normalized coverage count:
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
                            Dict.get labels expectedCoverages_
                                |> Maybe.map (\expectedCoverage -> ( labels, count, expectedCoverage ))
                        )
                    |> Maybe.map
                        (List.all
                            (\( _, count, expectedCoverage ) ->
                                case expectedCoverage of
                                    -- Zero and MoreThanZero will get checked in the Success case
                                    Zero ->
                                        True

                                    MoreThanZero ->
                                        True

                                    AtLeast n ->
                                        Test.Coverage.Internal.sufficientlyCovered state.runsElapsed count (n / 100)
                            )
                        )
            )
        -- `Nothing` means something went wrong. We're answering the question "are all labels sufficiently covered?" and so the way to fail here is `False`.
        |> Maybe.withDefault False


findBadZeroRelatedCase : LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Maybe CoverageFailure
findBadZeroRelatedCase c state normalizedCoverageCount =
    Maybe.map2 Tuple.pair
        normalizedCoverageCount
        (Test.Coverage.Internal.getExpectedCoverages c.coverage)
        |> Maybe.andThen
            (\( coverageCount, expectedCoverages ) ->
                expectedCoverages
                    |> List.find
                        (\( label, expectedCoverage ) ->
                            case expectedCoverage of
                                Zero ->
                                    -- TODO short-circuit Zero sooner: as soon as we increment its counter, during runNTimes.
                                    Dict.get [ label ] coverageCount
                                        -- TODO it would be better if we returned a bug failure here instead of failing with a dummy value
                                        |> Maybe.withDefault 1
                                        |> (/=) 0

                                MoreThanZero ->
                                    Dict.get [ label ] coverageCount
                                        -- TODO it would be better if we returned a bug failure here instead of failing with a dummy value
                                        |> Maybe.withDefault 0
                                        |> (==) 0

                                AtLeast _ ->
                                    False
                        )
                    |> Maybe.andThen
                        (\( label, expectedCoverage ) ->
                            Dict.get [ label ] coverageCount
                                |> Maybe.map
                                    (\count ->
                                        { label = label
                                        , actualPercentage = toFloat count * 100 / toFloat state.runsElapsed
                                        , expectedCoverage = expectedCoverage
                                        , runsElapsed = state.runsElapsed
                                        , coverageCount = coverageCount
                                        }
                                    )
                        )
            )


findInsufficientlyCoveredLabel : LoopConstants a -> LoopState -> Maybe (Dict (List String) Int) -> Maybe CoverageFailure
findInsufficientlyCoveredLabel c state normalizedCoverageCount =
    Maybe.map2 Tuple.pair
        normalizedCoverageCount
        (Test.Coverage.Internal.getExpectedCoverages c.coverage)
        |> Maybe.andThen
            (\( coverageCount, expectedCoverages ) ->
                let
                    expectedCoverages_ : Dict String ExpectedCoverage
                    expectedCoverages_ =
                        Dict.fromList expectedCoverages
                in
                -- TODO loop ExpectedCoverages instead of looping the label combinations?
                coverageCount
                    -- Needs normalized coverage count:
                    |> Dict.toList
                    |> List.filterMap
                        (\( labels, count ) ->
                            case labels of
                                [ onlyLabel ] ->
                                    Dict.get onlyLabel expectedCoverages_
                                        |> Maybe.map (\expectedCoverage -> ( onlyLabel, count, expectedCoverage ))

                                _ ->
                                    Nothing
                        )
                    |> List.find
                        (\( _, count, expectedCoverage ) ->
                            case expectedCoverage of
                                Zero ->
                                    False

                                MoreThanZero ->
                                    False

                                AtLeast n ->
                                    Test.Coverage.Internal.insufficientlyCovered state.runsElapsed count (n / 100)
                        )
                    |> Maybe.map
                        (\( label, count, expectedCoverage ) ->
                            { label = label
                            , actualPercentage = toFloat count * 100 / toFloat state.runsElapsed
                            , expectedCoverage = expectedCoverage
                            , runsElapsed = state.runsElapsed
                            , coverageCount = coverageCount
                            }
                        )
            )


coverageFailRunResult : Maybe (Dict (List String) Int) -> CoverageFailure -> RunResult
coverageFailRunResult normalizedCoverageCount failedLabel =
    case normalizedCoverageCount of
        Nothing ->
            -- Shouldn't happen, we're in the ExpectCoverage case. This indicates a bug in `initLoopState`.
            coverageBugRunResult

        Just coverageCount ->
            { coverageReport =
                CoverageCheckFailed
                    { coverageCount = coverageCount
                    , runsElapsed = failedLabel.runsElapsed
                    , badLabel = failedLabel.label
                    , badLabelPercentage = failedLabel.actualPercentage
                    , expectedCoverage = Test.Coverage.Internal.expectedCoverageToString failedLabel.expectedCoverage
                    }
            , failure = Just <| coverageInsufficientFailure failedLabel
            }


coverageBugRunResult : RunResult
coverageBugRunResult =
    { coverageReport = NoCoverage
    , failure =
        Just
            { given = Nothing
            , expectation =
                Test.Expectation.fail
                    { description = "elm-test coverage collection bug"
                    , reason = Invalid CoverageBug
                    }
            }
    }


coverageInsufficientFailure : CoverageFailure -> Failure
coverageInsufficientFailure failure =
    { given = Nothing
    , expectation =
        Test.Expectation.fail
            { description =
                """{TABLE}

Coverage of label "{LABEL}" was insufficient:
  expected:  {EXPECTED_PERCENTAGE}
  got:       {ACTUAL_PERCENTAGE}.

(Generated {RUNS} values.)"""
                    |> String.replace "{TABLE}" (Test.Runner.Coverage.formatTable failure)
                    |> String.replace "{LABEL}" failure.label
                    |> String.replace "{EXPECTED_PERCENTAGE}" (formatExpectedCoverage failure.expectedCoverage)
                    |> String.replace "{ACTUAL_PERCENTAGE}" (Test.Coverage.Internal.formatPct failure.actualPercentage)
                    |> String.replace "{RUNS}" (String.fromInt failure.runsElapsed)
            , reason = Invalid CoverageInsufficient
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

        ( maybeFailure, newCoverageCounter ) =
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
                    , state.coverageCount
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

                        coverageCounter : Maybe (Dict (List String) Int)
                        coverageCounter =
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
                                (Test.Coverage.Internal.getCoverageLabels c.coverage)
                                state.coverageCount
                    in
                    ( failure, coverageCounter )
    in
    { state
        | failure = maybeFailure
        , coverageCount = newCoverageCounter
        , currentSeed = nextSeed
        , runsElapsed = state.runsElapsed + 1
    }


includeCombinationsInBaseCounts : Dict (List String) Int -> Dict (List String) Int
includeCombinationsInBaseCounts coverage =
    coverage
        |> Dict.map
            (\labels count ->
                case labels of
                    [ single ] ->
                        let
                            combinations : List Int
                            combinations =
                                coverage
                                    |> Dict.filter (\k _ -> List.length k > 1 && List.member single k)
                                    |> Dict.values
                        in
                        count + List.sum combinations

                    _ ->
                        count
            )


formatExpectedCoverage : ExpectedCoverage -> String
formatExpectedCoverage expected =
    case expected of
        Zero ->
            "exactly 0%"

        MoreThanZero ->
            "more than 0%"

        AtLeast n ->
            Test.Coverage.Internal.formatPct n


type alias RunResult =
    { coverageReport : CoverageReport
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


formatExpectation : Failure -> Expectation
formatExpectation { given, expectation } =
    case given of
        Nothing ->
            expectation

        Just given_ ->
            Test.Expectation.withGiven given_ expectation
