module Test.Runner.String exposing (Summary, run, runWithOptions)

{-| # String Runner

Run a test and present its results as a nicely-formatted String, along with
a count of how many tests passed and failed.

This is a quick way to get decent test outputs which can then be presented in
various different environments. See `Test.Runner.Log` for an example.

@docs Summary, run, runWithOptions
-}

import Random.Pcg as Random
import Test exposing (Test)
import Expect exposing (Expectation)
import String
import Test.Runner exposing (Runner(..))


{-| The output string, the number of passed tests,
and the number of failed tests.
-}
type alias Summary =
    { output : String, passed : Int, failed : Int }


toOutput : Summary -> Runner -> Summary
toOutput =
    flip (toOutputHelp [])


toOutputHelp : List String -> Runner -> Summary -> Summary
toOutputHelp labels runner summary =
    case runner of
        Runnable runnable ->
            let
                output =
                    String.join "\n\n" [ summary.output, outputLabels labels ]
            in
                Test.Runner.run runnable
                    |> List.foldl fromExpectation { summary | output = output }

        Labeled label subRunner ->
            toOutputHelp (label :: labels) subRunner summary

        Batch runners ->
            List.foldl (toOutputHelp labels) summary runners


fromExpectation : Expectation -> Summary -> Summary
fromExpectation expectation summary =
    case Expect.getFailure expectation of
        Nothing ->
            { summary | passed = summary.passed + 1 }

        Just message ->
            { output =
                summary.output ++ "\n\n" ++ indentLines message ++ "\n"
            , failed = summary.failed + 1
            , passed = summary.passed
            }


outputLabels : List String -> String
outputLabels labels =
    case List.filter (not << String.isEmpty) labels of
        [] ->
            ""

        first :: rest ->
            rest
                |> List.map ((++) "↓ ")
                |> (::) ("✗ " ++ first)
                |> List.reverse
                |> String.join "\n"


defaultSeed : Random.Seed
defaultSeed =
    Random.initialSeed 4295183


defaultRuns : Int
defaultRuns =
    100


indentLines : String -> String
indentLines str =
    str
        |> String.split "\n"
        |> List.map ((++) "    ")
        |> String.join "\n"


{-| Run a test and return a tuple of the output message and the number of
tests that failed.

Fuzz tests use a default run count of 100, and a fixed initial seed.
-}
run : Test -> Summary
run =
    runWithOptions defaultRuns defaultSeed


{-| Run a test and return a tuple of the output message and the number of
tests that failed.
-}
runWithOptions : Int -> Random.Seed -> Test -> Summary
runWithOptions runs seed test =
    test
        |> Test.Runner.fromTest runs seed
        |> toOutput { output = "", passed = 0, failed = 0 }
