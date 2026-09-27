module Test.RunnerV2 exposing
    ( toTests, Tests, getUnitTests, getFuzzTests, getExcludedDueToSkip, getExcludedDueToOnly
    , UnitTest, getUnitTestTag, getUnitTestLabels, runUnitTest, runUnitTestWithUnbufferedLogs
    , UnitTestExpectation(..), UnitTestFailData, getUnitTestFailDescription, getUnitTestFailReason
    , FuzzTest, getFuzzTestTag, getFuzzTestLabels, getFuzzTestRuns, runFuzzTest, runFuzzTestWithUnbufferedLogs
    , FuzzTestExpectation(..), getFuzzTestPassDistributionReport, FuzzTestPassData, FuzzTestFailData, getFuzzTestFailDescription, getFuzzTestFailReason, getFuzzTestFailDistributionReport, getFuzzTestFailGiven, getFuzzTestFailFuzzerInts
    , identifyTest, tagTest
    , getDebugLogsBeforeFirstTestRun
    )

{-| This is an "experts only" module that exposes functions needed to run tests.
A typical user will use an existing runner library for Node or the browser,
which is implemented using this interface. A list of these runners
can be found in the [README](.).

This module supersedes the deprecated [Test.Runner](Test.Runner) module.


## Consume tests

@docs toTests, Tests, getUnitTests, getFuzzTests, getExcludedDueToSkip, getExcludedDueToOnly


## Unit Tests

@docs UnitTest, getUnitTestTag, getUnitTestLabels, runUnitTest, runUnitTestWithUnbufferedLogs
@docs UnitTestExpectation, UnitTestFailData, getUnitTestFailDescription, getUnitTestFailReason


## Fuzz Tests

@docs FuzzTest, getFuzzTestTag, getFuzzTestLabels, getFuzzTestRuns, runFuzzTest, runFuzzTestWithUnbufferedLogs
@docs FuzzTestExpectation, getFuzzTestPassDistributionReport, FuzzTestPassData, FuzzTestFailData, getFuzzTestFailDescription, getFuzzTestFailReason, getFuzzTestFailDistributionReport, getFuzzTestFailGiven, getFuzzTestFailFuzzerInts


## Runner test operations

@docs identifyTest, tagTest


## Debug logs

@docs getDebugLogsBeforeFirstTestRun

-}

import Array exposing (Array)
import Random
import RandomRun exposing (RandomRun)
import Task exposing (Task)
import Test exposing (Test)
import Test.Distribution exposing (DistributionReport(..))
import Test.Expectation exposing (Expectation(..))
import Test.Internal as Internal
import Test.Internal.DebugLogs
import Test.Runner.Failure exposing (Reason(..))


{-| This type contains flat arrays of tests and some metadata.

`Tests` is opaque for future extensibility. It contains the following:

  - `unitTests : Array UnitTest`
  - `fuzzTests : Array FuzzTest`
  - `excludedDueToSkip : Int`
  - `excludedDueToOnly : Maybe Int`

Use the various `get*` functions to access each field.

The lists of unit tests and fuzz tests only include tests that should be run,
after taking [`skip`](Test#skip) and [`only`](Test#only) into account.
The `excludedDueToSkip` and `excludedDueToOnly` fields tell if any
`skip` and/or `only` reduced the number of tests returned, and by how much.
A runner could fail the test run if `skip` or `only` was used.

-}
type Tests
    = Tests TestsData


type alias TestsData =
    { unitTests : Array UnitTest
    , fuzzTests : Array FuzzTest
    , excludedDueToSkip : Int
    , excludedDueToOnly : Maybe Int
    }


{-| Get unit tests.
-}
getUnitTests : Tests -> Array UnitTest
getUnitTests (Tests testsData) =
    testsData.unitTests


{-| Get fuzz tests.
-}
getFuzzTests : Tests -> Array FuzzTest
getFuzzTests (Tests testsData) =
    testsData.fuzzTests


{-| Get how many tests were excluded due to `skip`.
If `skip` wasn’t used at all, this returns 0.
-}
getExcludedDueToSkip : Tests -> Int
getExcludedDueToSkip (Tests testsData) =
    testsData.excludedDueToSkip


{-| Get how many tests were excluded due to `only`.

  - If `only` wasn’t used at all, this returns `Nothing`.
  - If `only` was used but didn’t exclude anything, this returns `Just 0`.
  - If `only` and excluded `n` tests, this returns `Just n`.
  - If a test is excluded both due to `skip` _and_ `only`, it is only counted as excluded due to `skip`.

-}
getExcludedDueToOnly : Tests -> Maybe Int
getExcludedDueToOnly (Tests testsData) =
    testsData.excludedDueToOnly


{-| A unit test.

`UnitTest` is opaque for future extensibility. It contains the following:

  - `tag : String`. The tag is set via [`tagTest`](#tagTest) and used by runners to cache test results.
  - `labels : List String`. The list starts with the test name, and then contains each [`describe`](Test#describe) up the hierarchy.
  - `thunk : () -> UnitTestExpectation`. This is the function originally passed to [`Test.test`](Test#test).

Use the various `getUnitTest*` functions to access each field, as well as [`runUnitTest`](#runUnitTest) to run it.

-}
type UnitTest
    = UnitTest
        { tag : String
        , labels : List String
        , thunk : () -> Expectation
        }


{-| Get the [tag](#tagTest) of the test.
-}
getUnitTestTag : UnitTest -> String
getUnitTestTag (UnitTest data) =
    data.tag


{-| Get the labels of the test.
-}
getUnitTestLabels : UnitTest -> List String
getUnitTestLabels (UnitTest data) =
    data.labels


{-| Run the unit test. Returns the test result, the duration it took to run in milliseconds,
and captured debug logs.

The debug logs are joined to one string with a newline as separator and with a trailing newline.

-}
runUnitTest : UnitTest -> Task x ( UnitTestExpectation, Float, String )
runUnitTest (UnitTest data) =
    Test.Internal.DebugLogs.runTestWithDurationAndCollectDebugLogs
        Test.Internal.DebugLogs.modeCollect
        data.thunk
        (\expectation duration debugLogs _ ->
            ( toUnitTestExpectation expectation, duration, debugLogs )
        )


{-| Like `runUnitTest`, but if `Debug.log` is used it writes directly to the console
instead of buffering. This is useful if a test ends up in an infinite loop and you would
like to see logs as the test runs.

The returned `Bool` says whether `Debug.log` was used at all while running the test.

Note: The logs are printed to stderr, not stdout like you might be used with
`Platform.worker` programs running in Node.js. This is because test runners typically
offer structured output on stdout, and don’t want to mix that with debug logs.

-}
runUnitTestWithUnbufferedLogs : UnitTest -> Task x ( UnitTestExpectation, Float, Bool )
runUnitTestWithUnbufferedLogs (UnitTest data) =
    Test.Internal.DebugLogs.runTestWithDurationAndCollectDebugLogs
        Test.Internal.DebugLogs.modeConsoleLog
        data.thunk
        (\expectation duration _ debugLogUsed ->
            ( toUnitTestExpectation expectation, duration, debugLogUsed )
        )


{-| A fuzz test. It’s similar to a unit test but has more data, and running
it requires more arguments.

`FuzzTest` is opaque for future extensibility. It contains the following:

  - `tag : String`. The tag is set via [`tagTest`](#tagTest) and used by runners to cache test results.
  - `labels : List String`. The list starts with the test name, and then contains each [`describe`](Test#describe) up the hierarchy.
  - `runs : Maybe Int`. Contains the specified number of fuzz runs if [`fuzzWith`](Test#fuzzWith) was used.
  - `thunk : Random.Seed -> Int -> List Int -> FuzzTestExpectation`. This function runs the fuzz test.

Use the various `getFuzzTest*` functions to access each field, as well as [`runFuzzTest`](#runFuzzTest) to run it.

-}
type FuzzTest
    = FuzzTest
        { tag : String
        , labels : List String
        , runs : Maybe Int
        , thunk : Random.Seed -> Int -> List Int -> Test.Expectation.FuzzTestExpectation
        }


{-| Get the [tag](#tagTest) of the test.
-}
getFuzzTestTag : FuzzTest -> String
getFuzzTestTag (FuzzTest data) =
    data.tag


{-| Get the labels of the test.
-}
getFuzzTestLabels : FuzzTest -> List String
getFuzzTestLabels (FuzzTest data) =
    data.labels


{-| Get the the specified number of fuzz runs if [`fuzzWith`](Test#fuzzWith) was used.
-}
getFuzzTestRuns : FuzzTest -> Maybe Int
getFuzzTestRuns (FuzzTest data) =
    data.runs


{-| Run the fuzz test. Returns the test result, and the duration it took to run in milliseconds,
and captured debug logs. When it comes to the debug logs, you need to know:

  - If the test failed, the debug logs are from the run that caused the returned test failure.

  - If the test passed, the debug logs are either empty, or a hardcoded message saying:

    > For passing fuzz tests, Debug.log is not shown, since showing logs from lots of runs is pretty confusing.
    > Tip: Use Debug.todo to fail a test from anywhere if you want some logs to appear.

  - The debug logs are joined to one string with a newline as separator and with a trailing newline.

It requires a few arguments:

  - The initial seed.
  - The number of fuzz runs.
  - A list of “fuzzer ints” from a previous run. If non-empty,
    the test will first be run with input based on those ints,
    which can quickly reproduce a previous failure. If that run
    succeeds (or the ints are no longer valid), the test continues
    with a regular random run.

-}
runFuzzTest : FuzzTest -> Random.Seed -> Int -> List Int -> Task x ( FuzzTestExpectation, Float, String )
runFuzzTest (FuzzTest data) seed runs fuzzerInts =
    Test.Internal.DebugLogs.runTestWithDurationAndCollectDebugLogs
        Test.Internal.DebugLogs.modeIgnore
        (\() -> data.thunk seed runs fuzzerInts)
        (\expectation duration _ debugLogUsed ->
            case expectation of
                Test.Expectation.FuzzTestPass distributionReport ->
                    ( FuzzTestPass (FuzzTestPassData distributionReport)
                    , duration
                    , if debugLogUsed then
                        Test.Internal.DebugLogs.noDebugLogsForPassingFuzzTests

                      else
                        ""
                    )

                Test.Expectation.FuzzTestFail failData ->
                    ( FuzzTestFail
                        (FuzzTestFailData
                            { description = failData.description
                            , reason = failData.reason
                            , distributionReport = failData.distributionReport
                            , given = failData.given
                            , fuzzerInts = RandomRun.toList failData.randomRun
                            }
                        )
                    , duration
                    , Test.Internal.DebugLogs.rerunFailureToCollectDebugLogs failData.rerunFailure
                    )
        )


{-| Like `runFuzzTest`, but if `Debug.log` is used it writes directly to the console
instead of buffering. This is useful if a test ends up in an infinite loop and you would
like to see logs as the test runs, or if you’d like to see logs from all the runs of the
test, not just the failing one (if any).

The returned `Bool` says whether `Debug.log` was used at all while running the test.

Note: The logs are printed to stderr, not stdout like you might be used with
`Platform.worker` programs running in Node.js. This is because test runners typically
offer structured output on stdout, and don’t want to mix that with debug logs.

-}
runFuzzTestWithUnbufferedLogs : FuzzTest -> Random.Seed -> Int -> List Int -> Task x ( FuzzTestExpectation, Float, Bool )
runFuzzTestWithUnbufferedLogs (FuzzTest data) seed runs fuzzerInts =
    Test.Internal.DebugLogs.runTestWithDurationAndCollectDebugLogs
        Test.Internal.DebugLogs.modeConsoleLog
        (\() -> data.thunk seed runs fuzzerInts)
        (\expectation duration _ debugLogUsed ->
            ( toFuzzTestExpectation expectation, duration, debugLogUsed )
        )


{-| A unit test either passes, or fails with a description and reason.
-}
type UnitTestExpectation
    = UnitTestPass
    | UnitTestFail UnitTestFailData


{-| Information about a unit test failure.

`UnitTestFailData` is opaque for future extensibility. It contains the following:

  - `description : String`. The failure as described by the [`Expect`](Expect) module.
  - `reason : Reason`. See the [`Reason`](Test.Runner.Failure#Reason) type.

Use the various `get*` functions to access each field.

-}
type UnitTestFailData
    = UnitTestFailData
        { description : String
        , reason : Reason
        }


{-| Get the failure description.
-}
getUnitTestFailDescription : UnitTestFailData -> String
getUnitTestFailDescription (UnitTestFailData data) =
    data.description


{-| Get the failure reason.
-}
getUnitTestFailReason : UnitTestFailData -> Reason
getUnitTestFailReason (UnitTestFailData data) =
    data.reason


{-| A fuzz test either passes or fails. In both cases there can be a distribution report.
In case of failures, there is a description and reason just like for unit tests, but also
a few more fuzz-specific fields – see [FuzzTestFailData](#FuzzTestFailData).
-}
type FuzzTestExpectation
    = FuzzTestPass FuzzTestPassData
    | FuzzTestFail FuzzTestFailData


{-| `FuzzTestFailData` is opaque for future extensibility. It contains the following:

  - `distributionReport : DistributionReport`. See the [`DistributionReport`](Test.Distribution#DistributionReport) type.

Use the various `get*` functions to access each field.

-}
type FuzzTestPassData
    = FuzzTestPassData DistributionReport


{-| Get the distribution report from a passing test.
-}
getFuzzTestPassDistributionReport : FuzzTestPassData -> DistributionReport
getFuzzTestPassDistributionReport (FuzzTestPassData distributionReport) =
    distributionReport


{-| Information about a fuzz test failure.

`FuzzTestFailData` is opaque for future extensibility. It contains the following:

  - `description : String`. The failure as described by the [`Expect`](Expect) module.
  - `reason : Reason`. See the [`Reason`](Test.Runner.Failure#Reason) type.
  - `distributionReport : DistributionReport`. See the [`DistributionReport`](Test.Distribution#DistributionReport) type.
  - `given : Maybe String`. This is the input to the test function that caused the failure, formatted with `Debug.toString`.
    A fuzz test can in unusual circumstances fail to even produce a `given` value, which is why it is `Maybe`.
  - `fuzzerInts : List Int`. This is the internal fuzzer state that produced `given`. A runner can pass this
    to [runFuzzTest](#runFuzzTest) to reproduce a previous failure.

Use the various `getFuzzTestFail*` functions to access each field.

-}
type FuzzTestFailData
    = FuzzTestFailData
        { description : String
        , reason : Reason
        , distributionReport : DistributionReport
        , given : Maybe String
        , fuzzerInts : List Int
        }


{-| Get the failure description.
-}
getFuzzTestFailDescription : FuzzTestFailData -> String
getFuzzTestFailDescription (FuzzTestFailData data) =
    data.description


{-| Get the failure reason.
-}
getFuzzTestFailReason : FuzzTestFailData -> Reason
getFuzzTestFailReason (FuzzTestFailData data) =
    data.reason


{-| Get the distribution report from a failing test.
-}
getFuzzTestFailDistributionReport : FuzzTestFailData -> DistributionReport
getFuzzTestFailDistributionReport (FuzzTestFailData data) =
    data.distributionReport


{-| Get the value that caused the test to fail.

This is the input to the test function that caused the failure, formatted with `Debug.toString`.
A fuzz test can in unusual circumstances fail to even produce a `given` value, which is why it is `Maybe`.

-}
getFuzzTestFailGiven : FuzzTestFailData -> Maybe String
getFuzzTestFailGiven (FuzzTestFailData data) =
    data.given


{-| Get the internal fuzzer state that produced the test input that caused the failure.

A runner can pass this to [runFuzzTest](#runFuzzTest) to reproduce a previous failure.

-}
getFuzzTestFailFuzzerInts : FuzzTestFailData -> List Int
getFuzzTestFailFuzzerInts (FuzzTestFailData data) =
    data.fuzzerInts


{-| This lets runners tag tests with an identifier, which can be used to implement
caching of test results.
-}
tagTest : String -> Test -> Test
tagTest tag test =
    Internal.ElmTestVariant__Tagged tag test
        |> Internal.wrapTestVariant


{-| This function allows runners to find exposed values of type `Test` without
having to implement type inference. A runners can collect _all_ exposed values
from a module, and then use this function to filter out the actual tests.
-}
identifyTest : a -> Maybe Test
identifyTest =
    Internal.identifyTest


{-| Turns a nested [`Test`](Test#Test) (from [`Test.test`](Test#test), [`Test.fuzz`](Test#fuzz) etc.)
into a flat structure that is easily consumable by runners.

Runners can collect all exposed `Test` values, join them up into
one single `Test` and then give it to this function. After that
they can start executing tests.

-}
toTests : Test -> Tests
toTests test =
    Tests (toTestsHelper "" [] test)


toTestsHelper : String -> List String -> Test -> TestsData
toTestsHelper tag labels test =
    case Internal.unwrapTestVariant test of
        Internal.ElmTestVariant__UnitTest thunk ->
            { unitTests =
                Array.push
                    (UnitTest
                        { tag = tag
                        , labels = labels
                        , thunk = thunk
                        }
                    )
                    Array.empty
            , fuzzTests = Array.empty
            , excludedDueToSkip = 0
            , excludedDueToOnly = Nothing
            }

        Internal.ElmTestVariant__FuzzTest maybeRuns thunk ->
            { unitTests = Array.empty
            , fuzzTests =
                Array.push
                    (FuzzTest
                        { tag = tag
                        , labels = labels
                        , thunk = thunk
                        , runs = maybeRuns
                        }
                    )
                    Array.empty
            , excludedDueToSkip = 0
            , excludedDueToOnly = Nothing
            }

        Internal.ElmTestVariant__Labeled label subTest ->
            toTestsHelper tag (label :: labels) subTest

        Internal.ElmTestVariant__Tagged newTag subTest ->
            toTestsHelper newTag labels subTest

        Internal.ElmTestVariant__Skipped subTest ->
            { unitTests = Array.empty
            , fuzzTests = Array.empty
            , excludedDueToSkip = countSkipped subTest
            , excludedDueToOnly = Nothing
            }

        Internal.ElmTestVariant__Only subTest ->
            let
                sub =
                    toTestsHelper tag labels subTest
            in
            -- As an optimization, if `excludedDueToOnly` is already
            -- the correct value (`Just n`), skip creating a new record.
            if sub.excludedDueToOnly /= Nothing then
                sub

            else
                -- Not using record update for performance.
                { unitTests = sub.unitTests
                , fuzzTests = sub.fuzzTests
                , excludedDueToSkip = sub.excludedDueToSkip
                , excludedDueToOnly = Just 0
                }

        Internal.ElmTestVariant__Batch subTests ->
            subTests
                |> List.foldl
                    (\subTest acc ->
                        let
                            sub =
                                toTestsHelper tag labels subTest

                            excludedDueToSkip =
                                acc.excludedDueToSkip + sub.excludedDueToSkip
                        in
                        -- Using nested `case` to avoid allocating a tuple.
                        case acc.excludedDueToOnly of
                            Just accOnly ->
                                case sub.excludedDueToOnly of
                                    Just subOnly ->
                                        { unitTests = Array.append acc.unitTests sub.unitTests
                                        , fuzzTests = Array.append acc.fuzzTests sub.fuzzTests
                                        , excludedDueToSkip = excludedDueToSkip
                                        , excludedDueToOnly = Just (accOnly + subOnly)
                                        }

                                    Nothing ->
                                        { unitTests = acc.unitTests
                                        , fuzzTests = acc.fuzzTests
                                        , excludedDueToSkip = excludedDueToSkip
                                        , excludedDueToOnly = Just (accOnly + Array.length sub.unitTests + Array.length sub.fuzzTests)
                                        }

                            Nothing ->
                                case sub.excludedDueToOnly of
                                    Just subOnly ->
                                        { unitTests = sub.unitTests
                                        , fuzzTests = sub.fuzzTests
                                        , excludedDueToSkip = excludedDueToSkip
                                        , excludedDueToOnly = Just (subOnly + Array.length acc.unitTests + Array.length acc.fuzzTests)
                                        }

                                    Nothing ->
                                        { unitTests = Array.append acc.unitTests sub.unitTests
                                        , fuzzTests = Array.append acc.fuzzTests sub.fuzzTests
                                        , excludedDueToSkip = excludedDueToSkip
                                        , excludedDueToOnly = Nothing
                                        }
                    )
                    { unitTests = Array.empty
                    , fuzzTests = Array.empty
                    , excludedDueToSkip = 0
                    , excludedDueToOnly = Nothing
                    }


countSkipped : Test -> Int
countSkipped test =
    case Internal.unwrapTestVariant test of
        Internal.ElmTestVariant__UnitTest _ ->
            1

        Internal.ElmTestVariant__FuzzTest _ _ ->
            1

        Internal.ElmTestVariant__Labeled _ subTest ->
            countSkipped subTest

        Internal.ElmTestVariant__Tagged _ subTest ->
            countSkipped subTest

        Internal.ElmTestVariant__Skipped subTest ->
            countSkipped subTest

        Internal.ElmTestVariant__Only subTest ->
            countSkipped subTest

        Internal.ElmTestVariant__Batch tests ->
            List.foldl (\subTest sum -> countSkipped subTest + sum) 0 tests


toUnitTestExpectation : Expectation -> UnitTestExpectation
toUnitTestExpectation expectation =
    case expectation of
        Pass _ ->
            UnitTestPass

        Fail { failData } ->
            UnitTestFail (UnitTestFailData failData)


toFuzzTestExpectation : Test.Expectation.FuzzTestExpectation -> FuzzTestExpectation
toFuzzTestExpectation expectation =
    case expectation of
        Test.Expectation.FuzzTestPass distributionReport ->
            FuzzTestPass (FuzzTestPassData distributionReport)

        Test.Expectation.FuzzTestFail data ->
            FuzzTestFail
                (FuzzTestFailData
                    { description = data.description
                    , reason = data.reason
                    , distributionReport = data.distributionReport
                    , given = data.given
                    , fuzzerInts = RandomRun.toList data.randomRun
                    }
                )


{-| It’s possible to write code like this:

    defaultShape =
        makeShape 3
            |> Debug.log "shape"

    myTest =
        test "my test" <|
            \() ->
                defaultShape.sides
                    |> Expect.equal 3

In this case, `defaultShape` will be evaluated before the test is run!
This is due to Elm being an eager language and what the generated JavaScript looks like.

This means that when a test runner starts running its Elm code, there might already
be a few debug logs made. This task lets you retrieve them.

If you set `globalThis.elmTestPrintDebugLogsBeforeFirstTestToConsole` to a truthy
value, these debug logs will be printed to the console, and the returned `DebugLogs`
here will be empty. Runners might want to do this when using the
[runUnitTestWithUnbufferedLogs](#runUnitTestWithUnbufferedLogs) and
[runFuzzTestWithUnbufferedLogs](#runFuzzTestWithUnbufferedLogs) functions,
to consistently print all debug logs to the console.

-}
getDebugLogsBeforeFirstTestRun : Task x String
getDebugLogsBeforeFirstTestRun =
    Test.Internal.DebugLogs.getDebugLogsBeforeFirstTestRun
