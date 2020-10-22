module Test.Fuzz exposing (Meta, fuzzTest)

import Dict exposing (Dict)
import Fuzz exposing (Fuzzer)
import Fuzz.Internal exposing (ValidFuzzer)
import Lazy.List
import Random exposing (Generator)
import RoseTree exposing (RoseTree(..))
import Test.Expectation exposing (Expectation(..))
import Test.Internal as Internal exposing (Test(..), blankDescriptionFailure, failNow)
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


type alias Meta a =
    { log : String -> a -> a }


noopMeta : Meta a
noopMeta =
    { log = \_ a -> a
    }


{-| Reject always-failing tests because of bad names or invalid fuzzers.
-}
fuzzTest : Fuzzer a -> String -> (Meta x -> a -> Expectation) -> Test
fuzzTest fuzzer untrimmedDesc getExpectation =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        blankDescriptionFailure

    else
        case fuzzer of
            Err reason ->
                failNow
                    { description = reason
                    , reason = Invalid InvalidFuzzer
                    }

            Ok validFuzzer ->
                -- Preliminary checks passed; run the fuzz test
                ElmTestVariant__Labeled desc <| validatedFuzzTest validFuzzer getExpectation


{-| Knowing that the fuzz test isn't obviously invalid, run the test and package up the results.
-}
validatedFuzzTest : ValidFuzzer a -> (Meta x -> a -> Expectation) -> Test
validatedFuzzTest fuzzer getExpectation =
    ElmTestVariant__FuzzTest
        (\seed runs ->
            case runAllFuzzIterations fuzzer getExpectation seed runs |> Dict.toList of
                [] ->
                    [ Pass ]

                failures ->
                    List.map formatExpectation failures
        )


type alias Failures =
    Dict String Expectation


{-| Runs the specified number of fuzz tests and returns a dictionary of simplified failures.
-}
runAllFuzzIterations : ValidFuzzer a -> (Meta x -> a -> Expectation) -> Random.Seed -> Int -> Failures
runAllFuzzIterations fuzzer getExpectation initialSeed totalRuns =
    let
        ( failures, _ ) =
            runOneFuzzIteration totalRuns fuzzer getExpectation ( Dict.empty, initialSeed )
    in
    failures


{-| Generate a fuzzed value, test it, and record the simplified test failure if any.
-}
runOneFuzzIteration : Int -> ValidFuzzer a -> (Meta x -> a -> Expectation) -> ( Failures, Random.Seed ) -> ( Failures, Random.Seed )
runOneFuzzIteration runsLeft fuzzer getExpectation ( failures, currentSeed ) =
    if Dict.isEmpty failures == False || runsLeft <= 1 then
        -- short-circuit on failure so we don't spam 100 copies of all Debug.log messages if they all hit an error
        ( failures, currentSeed )

    else
        let
            ( rosetree, nextSeed ) =
                Random.step fuzzer currentSeed

            newFailures =
                case testGeneratedValue rosetree getExpectation of
                    Nothing ->
                        -- test passed, nothing to change
                        failures

                    Just ( k, v ) ->
                        -- record test failure
                        Dict.insert k v failures
        in
        runOneFuzzIteration (runsLeft - 1) fuzzer getExpectation ( newFailures, nextSeed )


{-| Run a function whose inputs are the same as its outputs a given number of times. Requires the initial state to pass
in and returns the final state. This generic combinator extracts the "run n times" logic from our test running code.
-}
foldUntil : Int -> a -> (a -> a) -> a
foldUntil remainingRuns initialState f =
    if remainingRuns <= 1 then
        initialState

    else
        foldUntil (remainingRuns - 1) (f initialState) f


{-| Given a rosetree -- a root to test and branches of simplifications -- run the test and perform simplification if it fails.
-}
testGeneratedValue : RoseTree a -> (Meta x -> a -> Expectation) -> Maybe ( String, Expectation )
testGeneratedValue rosetree getExpectation =
    case getExpectation noopMeta (RoseTree.root rosetree) of
        Pass ->
            Nothing

        failedExpectation ->
            Just <| findSimplestFailure rosetree getExpectation failedExpectation


{-| Knowing that the rosetree's root already failed, finds the key and value of the simplest failure.
-}
findSimplestFailure : RoseTree a -> (Meta x -> a -> Expectation) -> Expectation -> ( String, Expectation )
findSimplestFailure (Rose failingValue branches) getExpectation oldExpectation =
    case Lazy.List.headAndTail branches of
        Just ( (Rose possiblyFailingValue _) as firstChild, otherChildren ) ->
            case getExpectation noopMeta possiblyFailingValue of
                -- recurse "horizontally" on other simplifications of the last known failing value
                -- discard simplifications of the passing value (the _)
                Pass ->
                    findSimplestFailure (Rose failingValue otherChildren) getExpectation oldExpectation

                -- recurse downward on simplifications of the newly-found failing value
                -- discard simplifications of the previous failing value (otherChildren)
                newExpectation ->
                    findSimplestFailure firstChild getExpectation newExpectation

        -- base case: we cannot simplify any more
        Nothing ->
            let
                _ =
                    getExpectation { log = Debug.log } failingValue
            in
            ( Internal.toString failingValue, oldExpectation )


formatExpectation : ( String, Expectation ) -> Expectation
formatExpectation ( given, expectation ) =
    Test.Expectation.withGiven given expectation
