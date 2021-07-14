module Helpers exposing
    ( canGenerate
    , canGenerateSatisfying
    , canGenerateWith
    , cannotGenerateSatisfying
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
    , testSimplifying
    , testStringLengthIsPreserved
    )

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Set
import Test exposing (Test, fuzz)
import Test.Runner exposing (Runner, SeededRunners)
import Test.Runner.Failure exposing (Reason(..))


{-| To test simplifying, we have to fail some tests so we can simplify their inputs.
The best place we found for storing the expected last state(s) of the simplifying procedure is the description field, which is why we have this function here.
Previously, we (ab)used Expect.true for this, but since that was removed, here we are.
-}
expectSimplifiesTo : String -> Bool -> Expectation
expectSimplifiesTo label a =
    Expect.equal True a |> Expect.onFail label


testSimplifying : Int -> Test -> Test
testSimplifying runs test =
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
    in
    test
        |> Test.Runner.fromTest runs seed
        |> getRunners
        |> List.head
        |> Maybe.map
            (\runner ->
                Test.test
                    (List.head runner.labels
                        |> Maybe.withDefault "Failed"
                    )
                <|
                    \() ->
                        runner.run
                            |> (\run -> run ())
                            |> passToFail handleFailure
            )
        |> Maybe.withDefault (Test.test "Failed" <| \() -> Expect.fail "Failed")


testFailing : Test -> Test
testFailing test =
    let
        handleFailure { given, description } =
            case given of
                Nothing ->
                    Err "Expected this test to have a given value!"

                Just g ->
                    Ok ()

        seed =
            Random.initialSeed 2242652938
    in
    test
        |> Test.Runner.fromTest 1000 seed
        |> getRunners
        |> List.head
        |> Maybe.map
            (\runner ->
                Test.test
                    (List.head runner.labels
                        |> Maybe.withDefault "Failed"
                    )
                <|
                    \() ->
                        runner.run
                            |> (\run -> run ())
                            |> passToFail handleFailure
            )
        |> Maybe.withDefault (Test.test "Failed" <| \() -> Expect.fail "Failed")


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
            Expect.fail "Expected this test to fail, but it passed!"

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
        |> List.map (.run >> (\run -> run ()))
        |> List.map expectToFail
        |> List.map always
        |> Expect.all
        |> (\all -> all ())


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
                    Err "Expected this test to fail, but it passed!"

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
    fuzz fuzzer label (fn >> Expect.equal True)


canGenerateSatisfying : String -> Fuzzer a -> (a -> Bool) -> Test
canGenerateSatisfying label fuzzer fn =
    testFailing <|
        fuzz fuzzer
            ("Can generate satisfying: " ++ label)
            (\fuzzedValue ->
                (not <| fn fuzzedValue)
                    |> Expect.equal True
            )


cannotGenerateSatisfying : String -> Fuzzer a -> (a -> Bool) -> Test
cannotGenerateSatisfying label fuzzer fn =
    passes ("Cannot generate satisfying: " ++ label)
        fuzzer
        (not << fn)


canGenerate : a -> Fuzzer a -> Test
canGenerate =
    canGenerateWith { runs = 100 }


canGenerateWith : { runs : Int } -> a -> Fuzzer a -> Test
canGenerateWith { runs } value fuzzer =
    let
        valueString =
            Debug.toString value
    in
    testSimplifying runs <|
        fuzz fuzzer
            ("Can generate " ++ valueString)
            (\fuzzedValue ->
                (fuzzedValue /= value)
                    |> expectSimplifiesTo valueString
            )


simplifiesTowards : String -> a -> Fuzzer a -> (a -> Bool) -> Test
simplifiesTowards =
    simplifiesTowardsWith { runs = 100 }


simplifiesTowardsWith : { runs : Int } -> String -> a -> Fuzzer a -> (a -> Bool) -> Test
simplifiesTowardsWith { runs } label value fuzzer fn =
    let
        valueString =
            Debug.toString value
    in
    testSimplifying runs <|
        fuzz fuzzer
            ("[" ++ label ++ "] Simplifies towards " ++ valueString)
            (\fuzzedValue ->
                fn fuzzedValue
                    |> expectSimplifiesTo valueString
            )


simplifiesTowardsMany : String -> List a -> Fuzzer a -> (a -> Bool) -> Test
simplifiesTowardsMany =
    simplifiesTowardsManyWith { runs = 100 }


simplifiesTowardsManyWith : { runs : Int } -> String -> List a -> Fuzzer a -> (a -> Bool) -> Test
simplifiesTowardsManyWith { runs } label values fuzzer fn =
    let
        valuesString =
            values
                |> List.map Debug.toString
                |> String.join "|"
    in
    testSimplifying runs <|
        fuzz fuzzer
            ("[" ++ label ++ "] Simplifies towards one of " ++ valuesString)
            (\fuzzedValue ->
                fn fuzzedValue
                    |> expectSimplifiesTo valuesString
            )


rejects : String -> Fuzzer a -> String -> Test
rejects label fuzzer expectedReason =
    fuzz randomSeedFuzzer ("Rejects: " ++ label) <|
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
