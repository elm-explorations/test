module Runner.String (Summary, run, runWithOptions) where

{-| String Runner

Run a test and present its results as a nicely-formatted String, along with
a count of how many tests passed and failed.

Note that this always uses an initial seed of 902101337, since it can't do effects.

@docs Summary, run, runWithOptions

-}

import Dict (Dict)
import Dict as Dict
import Expect (Expectation)
import Expect as Expect
import Random as Random
import Runner.String.Distribution as Runner.String.Distribution
import Runner.String.Format as Runner.String.Format
import Test (Test)
import Test as Test
import Test.Runner (Runner, SeededRunners(..))
import Test.Runner as Test.Runner


{-| The output string, the number of passed tests,
and the number of failed tests.
-}
type Summary =
    { output :: String
    , passed :: Int
    , failed :: Int
    , autoFail :: Maybe String
    }


toOutput :: Summary -> SeededRunners -> Summary
toOutput summary seededRunners =
    let
        render =
            List.foldl toOutputHelp
    in
    case seededRunners of
        Plain runners ->
            render ( summary { autoFail = Nothing  }) runners

        Only runners ->
            render ( summary { autoFail = Just "Test.only was used"  }) runners

        Skipping runners ->
            render ( summary { autoFail = Just "Test.skip was used"  }) runners

        Invalid message ->
            { output : message, passed : 0, failed : 0, autoFail : Nothing }


toOutputHelp :: Runner -> Summary -> Summary
toOutputHelp runner summary =
    {-
       let
           _ =
               Debug.log "==================" ()
       in
       let
           _ =
               Debug.log "TEST" runner.labels
       in
    -}
    runner.run {}
        |> List.foldl (fromExpectation runner.labels) summary


fromExpectation :: List String -> Expectation -> Summary -> Summary
fromExpectation labels expectation summary =
    let
        distributionReport :: Maybe String
        distributionReport =
            expectation
                |> Test.Runner.getDistributionReport
                |> Runner.String.Distribution.report labels

        summaryWithDistribution :: Summary
        summaryWithDistribution =
            case distributionReport of
                Nothing ->
                    summary

                Just distribution ->
                    ( summary { output =
                            summary.output
                                <> "\n\n"
                                <> distribution
                                <> "\n"
                     })
    in
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            ( summaryWithDistribution { passed = summaryWithDistribution.passed + 1  })

        Just { given, description, reason } ->
            let
                message =
                    Runner.String.Format.format description reason

                prefix =
                    case given of
                        Nothing ->
                            ""

                        Just g ->
                            "Given " <> g <> "\n\n"

                newOutput =
                    "\n\n"
                        <> outputLabels labels
                        <> "\n"
                        <> (prefix <> indentLines message)
                        <> "\n"
            in
            { summaryWithDistribution
                | output = summaryWithDistribution.output <> newOutput
                , failed : summaryWithDistribution.failed + 1
                , passed : summaryWithDistribution.passed
            }


outputLabels :: List String -> String
outputLabels labels =
    labels
        |> Test.Runner.formatLabels ((<>) "↓ ") ((<>) "✗ ")
        |> String.join "\n"


defaultSeed :: Random.Seed
defaultSeed =
    Random.initialSeed 902101337


defaultRuns :: Int
defaultRuns =
    100


wrap :: String -> String -> String
wrap delimiter string =
    delimiter <> string <> delimiter


indentLines :: String -> String
indentLines str =
    str
        |> String.split "\n"
        |> List.map ((<>) "    ")
        |> String.join "\n"


{-| Run a test and return a Summary.

Fuzz tests use a default run count of 100, and a fixed initial seed.

-}
run :: Test -> Summary
run =
    runWithOptions defaultRuns defaultSeed


{-| Run a test and return a Summary.
-}
runWithOptions :: Int -> Random.Seed -> Test -> Summary
runWithOptions runs seed test =
    let
        seededRunners =
            Test.Runner.fromTest runs seed test
    in
    toOutput
        { output : ""
        , passed : 0
        , failed : 0
        , autoFail : Just "no tests were run"
        }
        seededRunners
