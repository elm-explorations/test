module Test.Fuzz exposing (fuzzTest)

import Fuzz.Internal exposing (Fuzzer)
import GenResult exposing (GenResult(..))
import PRNG
import Random
import Simplify
import Test.Expectation exposing (Expectation(..))
import Test.Internal exposing (Test(..), blankDescriptionFailure)
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| Reject always-failing tests because of bad names or invalid fuzzers.
-}
fuzzTest : Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzTest fuzzer untrimmedDesc getExpectation =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        blankDescriptionFailure

    else
        ElmTestVariant__Labeled desc <| validatedFuzzTest fuzzer getExpectation


{-| Knowing that the fuzz test isn't obviously invalid, run the test and package up the results.
-}
validatedFuzzTest : Fuzzer a -> (a -> Expectation) -> Test
validatedFuzzTest fuzzer getExpectation =
    ElmTestVariant__FuzzTest
        (\seed runs ->
            case runUntilFailure fuzzer getExpectation seed runs of
                Nothing ->
                    [ Pass ]

                Just failure ->
                    [ formatExpectation failure ]
        )


type alias Failure =
    { given : Maybe String
    , expectation : Expectation
    }


{-| Runs the specified number of fuzz tests and returns a dictionary of simplified failures.
-}
runUntilFailure : Fuzzer a -> (a -> Expectation) -> Random.Seed -> Int -> Maybe Failure
runUntilFailure fuzzer getExpectation initialSeed totalRuns =
    runOneFuzzIteration fuzzer getExpectation
        |> foldUntil
            totalRuns
            (\( failure, _ ) -> failure /= Nothing)
            ( Nothing, initialSeed )
        -- throw away the random seed
        |> Tuple.first


{-| Generate a fuzzed value, test it, and record the simplified test failure if any.
-}
runOneFuzzIteration : Fuzzer a -> (a -> Expectation) -> ( Maybe Failure, Random.Seed ) -> ( Maybe Failure, Random.Seed )
runOneFuzzIteration fuzzer getExpectation ( _, currentSeed ) =
    let
        genResult : GenResult a
        genResult =
            Fuzz.Internal.generate
                (PRNG.random currentSeed)
                fuzzer

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
                    stepSeed currentSeed

        maybeFailure : Maybe Failure
        maybeFailure =
            case genResult of
                Rejected { reason } ->
                    Just
                        { given = Nothing
                        , expectation =
                            Test.Expectation.fail
                                { description = reason
                                , reason = Invalid InvalidFuzzer
                                }
                        }

                Generated { prng, value } ->
                    testGeneratedValue
                        { getExpectation = getExpectation
                        , fuzzer = fuzzer
                        , randomRun = PRNG.getRun prng
                        , value = value
                        , expectation = getExpectation value
                        }
    in
    ( maybeFailure, nextSeed )


{-| Random.next is private ¯\_(ツ)\_/¯
-}
stepSeed : Random.Seed -> Random.Seed
stepSeed seed =
    seed
        |> Random.step (Random.int 0 0)
        |> Tuple.second


{-| Run a function whose inputs are the same as its outputs a given number of times. Requires the initial state to pass
in and returns the final state. This generic combinator extracts the "run n times" logic from our test running code.
-}
foldUntil : Int -> (a -> Bool) -> a -> (a -> a) -> a
foldUntil remainingRuns endingCondition initialState f =
    if remainingRuns <= 1 || endingCondition initialState then
        initialState

    else
        foldUntil (remainingRuns - 1) endingCondition (f initialState) f


testGeneratedValue : Simplify.State a -> Maybe Failure
testGeneratedValue state =
    case state.expectation of
        Pass ->
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
