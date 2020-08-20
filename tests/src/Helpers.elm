module Helpers exposing
    ( canGenerate
    , canGenerateSatisfying
    , canGenerateSatisfyingWith
    , canGenerateWith
    , cannotGenerate
    , cannotGenerateSatisfying
    , cannotGenerateSatisfyingWith
    , different
    , doesNotReject
    , expectPass
    , expectSimplifiesTo
    , expectTestToFail
    , expectToFail
    , passes
    , randomSeedFuzzer
    , rejects
    , same
    , simplifiesTowards
    , simplifiesTowardsMany
    , simplifiesTowardsManyWith
    , simplifiesTowardsWith
    , succeeded
    , testFailing
    , testFailingWith
    , testSimplifyingWith
    , testStringLengthIsPreserved
    )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Set
import Test exposing (Test)
import Test.Runner exposing (Runner, SeededRunners)
import Test.Runner.Failure exposing (Reason(..))


defaultRuns : { runs : Int }
defaultRuns =
    { runs = 1000 }


{-| To test simplifying, we have to fail some tests so we can simplify their inputs.
The best place we found for storing the expected last state(s) of the simplifying procedure is the description field, which is why we have this function here.
Previously, we (ab)used Expect.true for this, but since that was removed, here we are.
-}
expectSimplifiesTo : String -> Bool -> Expectation
expectSimplifiesTo label a =
    Expect.equal True a |> Expect.onFail label


testSimplifyingWith : { runs : Int } -> Test -> Test
testSimplifyingWith { runs } test =
    let
        handleFailure { given, description } =
            case given of
                Nothing ->
                    [ "Expected this test to have a given value!"
                    , "Description:"
                    , "  " ++ description
                    ]
                        |> String.join "\n"
                        |> Err

                Just g ->
                    if String.contains g description then
                        Ok ()

                    else
                        Err <| "Got simplified value " ++ g ++ " but expected " ++ description

        seed =
            Random.initialSeed 2242652938

        runners =
            test
                |> Test.Runner.fromTest runs seed
                |> getRunners
    in
    case runners of
        [ runner ] ->
            let
                label =
                    runner.labels
                        |> List.head
                        |> Maybe.withDefault "No labels??"
            in
            Test.test label <|
                \() ->
                    runner.run ()
                        |> List.head
                        |> Maybe.map (passToFail handleFailure)
                        |> Maybe.withDefault (Expect.fail "Somehow `testSimplifyingWith` had multiple tests inside")

        _ ->
            Debug.todo "Unexpected number of test runners in `testSimplifyingWith`"


testFailing : Test -> Test
testFailing test =
    testFailingWith defaultRuns test


testFailingWith : { runs : Int } -> Test -> Test
testFailingWith { runs } test =
    let
        handleFailure { given, description } =
            case given of
                Nothing ->
                    [ "Expected this test to have a given value!"
                    , "Description:"
                    , "  " ++ description
                    ]
                        |> String.join "\n"
                        |> Err

                Just g ->
                    Ok ()

        seed =
            Random.initialSeed 2242652938

        runners =
            test
                |> Test.Runner.fromTest runs seed
                |> getRunners
    in
    case runners of
        [ runner ] ->
            let
                label =
                    runner.labels
                        |> List.head
                        |> Maybe.withDefault "No labels??"
            in
            Test.test label <|
                \() ->
                    runner.run ()
                        |> List.head
                        |> Maybe.map (passToFail handleFailure)
                        |> Maybe.withDefault (Expect.fail "Somehow `testFailingWith` had multiple tests inside")

        _ ->
            Debug.todo "Unexpected number of test runners in `testFailingWith`"


expectPass : a -> Expectation
expectPass _ =
    Expect.pass


testStringLengthIsPreserved : List String -> Expectation
testStringLengthIsPreserved strings =
    strings
        |> List.map String.length
        |> List.sum
        |> Expect.equal (String.length (List.foldl (++) "" strings))


expectToFail : Expectation -> Expectation
expectToFail expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            Expect.fail "Expected the test to fail, but it passed!"

        Just _ ->
            Expect.pass


expectTestToFail : Test -> Expectation
expectTestToFail test =
    let
        seed =
            Random.initialSeed 2242652938
    in
    test
        |> Test.Runner.fromTest 100 seed
        |> getRunners
        |> List.concatMap (\{ run } -> run ())
        |> List.map (\expectation () -> expectToFail expectation)
        |> (\expectations -> Expect.all expectations ())


succeeded : Expectation -> Bool
succeeded expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            True

        Just _ ->
            False


passToFail :
    ({ reason : Reason
     , description : String
     , given : Maybe String
     }
     -> Result String ()
    )
    -> Expectation
    -> Expectation
passToFail f expectation =
    let
        result =
            case Test.Runner.getFailureReason expectation of
                Nothing ->
                    Err "Nope!"

                Just record ->
                    f record
    in
    case result of
        Ok () ->
            Expect.pass

        Err message ->
            Expect.fail message


getRunners : SeededRunners -> List Runner
getRunners seededRunners =
    case seededRunners of
        Test.Runner.Plain runners ->
            runners

        Test.Runner.Only runners ->
            runners

        Test.Runner.Skipping runners ->
            runners

        Test.Runner.Invalid _ ->
            []


{-| get a good distribution of random seeds, and don't simplify our seeds!
-}
randomSeedFuzzer : Fuzzer Random.Seed
randomSeedFuzzer =
    Fuzz.intRange 0 0xFFFFFFFF
        |> Fuzz.map Random.initialSeed


same : Expectation -> Expectation -> Expectation
same a b =
    case ( Test.Runner.getFailureReason a, Test.Runner.getFailureReason b ) of
        ( Nothing, Nothing ) ->
            Expect.pass

        ( Just _, Just _ ) ->
            Expect.pass

        ( reasonA, reasonB ) ->
            Expect.equal reasonA reasonB
                |> Expect.onFail "expected both arguments to fail, or both to succeed"


different : Expectation -> Expectation -> Expectation
different a b =
    case ( Test.Runner.getFailureReason a, Test.Runner.getFailureReason b ) of
        ( Nothing, Just _ ) ->
            Expect.pass

        ( Just _, Nothing ) ->
            Expect.pass

        ( Nothing, Nothing ) ->
            Expect.fail "expected only one argument to fail, but both passed"

        ( Just reasonA, Just reasonB ) ->
            [ reasonA, reasonB ]
                |> Expect.equal []
                |> Expect.onFail "expected only one argument to fail"


passes : String -> Fuzzer a -> (a -> Bool) -> Test
passes label fuzzer fn =
    passesWith defaultRuns label fuzzer fn


passesWith : { runs : Int } -> String -> Fuzzer a -> (a -> Bool) -> Test
passesWith runs label fuzzer fn =
    Test.fuzzWith runs fuzzer label (fn >> Expect.equal True)


canGenerateSatisfying : String -> Fuzzer a -> (a -> Bool) -> Test
canGenerateSatisfying label fuzzer fn =
    canGenerateSatisfyingWith defaultRuns label fuzzer fn


canGenerateSatisfyingWith : { runs : Int } -> String -> Fuzzer a -> (a -> Bool) -> Test
canGenerateSatisfyingWith runs label fuzzer fn =
    testFailingWith runs <|
        Test.fuzz fuzzer
            ("Can generate satisfying: " ++ label)
            (\fuzzedValue ->
                (not <| fn fuzzedValue)
                    |> Expect.equal True
            )


cannotGenerateSatisfying : String -> Fuzzer a -> (a -> Bool) -> Test
cannotGenerateSatisfying label fuzzer fn =
    cannotGenerateSatisfyingWith defaultRuns label fuzzer fn


cannotGenerateSatisfyingWith : { runs : Int } -> String -> Fuzzer a -> (a -> Bool) -> Test
cannotGenerateSatisfyingWith runs label fuzzer fn =
    passesWith runs
        ("Cannot generate satisfying: " ++ label)
        fuzzer
        (not << fn)


cannotGenerate : a -> Fuzzer a -> Test
cannotGenerate value fuzzer =
    let
        valueString =
            Debug.toString value
    in
    passes ("Cannot generate " ++ valueString)
        fuzzer
        (\v -> v /= value)


canGenerate : a -> Fuzzer a -> Test
canGenerate value fuzzer =
    canGenerateWith defaultRuns value fuzzer


canGenerateWith : { runs : Int } -> a -> Fuzzer a -> Test
canGenerateWith runs value fuzzer =
    let
        valueString =
            Debug.toString value
    in
    testSimplifyingWith runs <|
        Test.fuzz fuzzer
            ("Can generate " ++ valueString)
            (\fuzzedValue ->
                (fuzzedValue /= value)
                    |> expectSimplifiesTo valueString
            )


simplifiesTowards : String -> a -> Fuzzer a -> (a -> Bool) -> Test
simplifiesTowards =
    simplifiesTowardsWith defaultRuns


simplifiesTowardsWith : { runs : Int } -> String -> a -> Fuzzer a -> (a -> Bool) -> Test
simplifiesTowardsWith runs label value fuzzer fn =
    let
        valueString =
            Debug.toString value
    in
    testSimplifyingWith runs <|
        Test.fuzz fuzzer
            ("[" ++ label ++ "] Simplifies towards " ++ valueString)
            (\fuzzedValue ->
                fn fuzzedValue
                    |> expectSimplifiesTo valueString
            )


simplifiesTowardsMany : String -> List a -> Fuzzer a -> (a -> Bool) -> Test
simplifiesTowardsMany =
    simplifiesTowardsManyWith defaultRuns


simplifiesTowardsManyWith : { runs : Int } -> String -> List a -> Fuzzer a -> (a -> Bool) -> Test
simplifiesTowardsManyWith runs label values fuzzer fn =
    let
        valuesString =
            values
                |> List.map Debug.toString
                |> String.join "|"
    in
    testSimplifyingWith runs <|
        Test.fuzz fuzzer
            ("[" ++ label ++ "] Simplifies towards one of " ++ valuesString)
            (\fuzzedValue ->
                fn fuzzedValue
                    |> expectSimplifiesTo valuesString
            )


rejects : String -> Fuzzer a -> String -> Test
rejects label fuzzer expectedReason =
    Test.fuzz randomSeedFuzzer ("Rejects: " ++ label) <|
        \seed ->
            case Random.step (Test.Runner.fuzz fuzzer) seed of
                ( Err reason, _ ) ->
                    reason
                        |> Expect.equal expectedReason

                _ ->
                    Expect.fail "Fuzzer generated a value instead of being marked as invalid"


doesNotReject : String -> Fuzzer a -> Test
doesNotReject label fuzzer =
    passes ("Does not reject: " ++ label)
        fuzzer
        (\_ -> True)
