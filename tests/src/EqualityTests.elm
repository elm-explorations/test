module EqualityTests exposing (equalityTests)

import Array
import Dict
import Expect exposing (FloatingPointTolerance(..), NaNBehavior(..))
import Fuzz
import Helpers exposing (expectToFail)
import Random
import Set
import Test exposing (Test, describe, test)
import Test.Runner
import Test.Runner.Failure exposing (Reason(..))


equalityTests : Test
equalityTests =
    describe "equality"
        [ equalTests
        , equalWithNumbersTests
        , notTests
        ]


equalTests : Test
equalTests =
    describe "Expect.equal"
        [ describe "compares structurally"
            [ test "records" <|
                \() ->
                    { name = "Anne", pets = [ "cat" ] }
                        |> Expect.equal { name = "Anne", pets = [ "cat" ] }
            , test "custom types" <|
                \() ->
                    Just (Ok [ 'a' ])
                        |> Expect.equal (Just (Ok [ 'a' ]))
            , test "dicts built in different orders" <|
                \() ->
                    Dict.fromList [ ( 1, "one" ), ( 2, "two" ), ( 3, "three" ) ]
                        |> Expect.equal
                            (Dict.empty
                                |> Dict.insert 3 "three"
                                |> Dict.insert 1 "one"
                                |> Dict.insert 2 "two"
                            )
            , test "dicts nested in a record" <|
                \() ->
                    { counts = Dict.fromList [ ( "a", 1 ) ] }
                        |> Expect.equal { counts = Dict.fromList [ ( "a", 1 ) ] }
            , test "long lists don't overflow the stack" <|
                \() ->
                    List.range 1 100000
                        |> Expect.equal (List.range 1 100000)
            , test "NaN is not equal to itself" <|
                \() ->
                    { value = 0 / 0 }
                        |> Expect.equal { value = 0 / 0 }
                        |> expectToFail
            ]
        , describe "reports a diff of"
            [ test "lists" <|
                \() ->
                    [ 1, 2, 4, 6 ]
                        |> Expect.equal [ 1, 2, 5 ]
                        |> expectReason "ListDiff"
                            (\reason ->
                                case reason of
                                    ListDiff expected actual ->
                                        Just ( expected, actual )

                                    _ ->
                                        Nothing
                            )
                            (Just ( [ "1", "2", "5" ], [ "1", "2", "4", "6" ] ))
            , test "arrays" <|
                \() ->
                    Array.fromList [ 1, 2, 4 ]
                        |> Expect.equal (Array.fromList [ 1, 2, 5 ])
                        |> expectReason "ListDiff"
                            (\reason ->
                                case reason of
                                    ListDiff expected actual ->
                                        Just ( expected, actual )

                                    _ ->
                                        Nothing
                            )
                            (Just ( [ "1", "2", "5" ], [ "1", "2", "4" ] ))
            , test "sets" <|
                \() ->
                    Set.fromList [ 1, 2, 4, 6 ]
                        |> Expect.equal (Set.fromList [ 1, 2, 5 ])
                        |> expectReason "CollectionDiff"
                            (\reason ->
                                case reason of
                                    CollectionDiff { extra, missing } ->
                                        Just ( extra, missing )

                                    _ ->
                                        Nothing
                            )
                            (Just ( [ "4", "6" ], [ "5" ] ))
            , test "dicts" <|
                \() ->
                    Dict.fromList [ ( 1, "one" ), ( 2, "too" ) ]
                        |> Expect.equal (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])
                        |> expectReason "CollectionDiff"
                            (\reason ->
                                case reason of
                                    CollectionDiff { extra, missing } ->
                                        Just ( extra, missing )

                                    _ ->
                                        Nothing
                            )
                            (Just ( [ "(2,\"too\")" ], [ "(2,\"two\")" ] ))
            , test "anything else" <|
                \() ->
                    { name = "Anne" }
                        |> Expect.equal { name = "Bob" }
                        |> expectReason "Equality"
                            (\reason ->
                                case reason of
                                    Equality expected actual ->
                                        Just ( expected, actual )

                                    _ ->
                                        Nothing
                            )
                            (Just ( "{ name = \"Bob\" }", "{ name = \"Anne\" }" ))
            ]
        ]


equalWithNumbersTests : Test
equalWithNumbersTests =
    describe "Expect.equalWithNumbers"
        [ test "compares bare floats like Expect.within does" <|
            \() ->
                3.14
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0.01) pi
        , test "compares floats nested in a data structure" <|
            \() ->
                { pi = 3.14, tau = 6.28 }
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0.01) { pi = pi, tau = 2 * pi }
        , test "fails when a nested float is outside the tolerance" <|
            \() ->
                { pi = 3.14 }
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0.0001) { pi = pi }
                    |> expectToFail
        , test "honors relative tolerance" <|
            \() ->
                [ 100000 * 3.14 ]
                    |> Expect.equalWithNumbers NaNsNeverEqual (Relative 0.001) [ 100000 * pi ]
        , test "with zero tolerance, behaves like Expect.equal" <|
            \() ->
                { pi = 3.14 }
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0) { pi = 3.14 }
        , test "NaNsAlwaysEqual makes NaNs equal" <|
            \() ->
                { value = 0 / 0 }
                    |> Expect.equalWithNumbers NaNsAlwaysEqual (Absolute 0) { value = 0 / 0 }
        , test "NaNsNeverEqual keeps NaNs unequal" <|
            \() ->
                { value = 0 / 0 }
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0) { value = 0 / 0 }
                    |> expectToFail
        , test "NaN is not equal to a number, even with a huge tolerance" <|
            \() ->
                (0 / 0)
                    |> Expect.equalWithNumbers NaNsAlwaysEqual (Absolute 1.0e100) 1
                    |> expectToFail
        , test "infinities are equal to themselves" <|
            \() ->
                [ 1 / 0, -1 / 0 ]
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0) [ 1 / 0, -1 / 0 ]
        , test "infinities of different signs aren't equal" <|
            \() ->
                (1 / 0)
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 1.0e100) (-1 / 0)
                    |> expectToFail
        , test "compares floats inside dicts" <|
            \() ->
                Dict.fromList [ ( "pi", 3.14 ) ]
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0.01) (Dict.fromList [ ( "pi", pi ) ])
        , test "fails on a negative tolerance" <|
            \() ->
                1
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute -1) 1
                    |> expectToFail
        ]


notTests : Test
notTests =
    describe "Expect.not"
        [ test "passes when the inverted expectation fails" <|
            \() ->
                90 + 10 |> Expect.equal 11 |> Expect.not
        , test "fails when the inverted expectation passes" <|
            \() ->
                90 + 10 |> Expect.equal 100 |> Expect.not |> expectToFail
        , test "inverts expectations that take a tolerance" <|
            \() ->
                3.14
                    |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0.0001) pi
                    |> Expect.not
        , test "inverts comparisons" <|
            \() ->
                1 |> Expect.greaterThan 2 |> Expect.not
        , test "inverts one-argument expectations" <|
            \() ->
                Ok 1 |> Expect.err |> Expect.not
        , test "inverts Expect.all" <|
            \() ->
                Expect.all [ Expect.pass, 1 |> Expect.equal 2 ]
                    |> Expect.not
        , test "inverting twice is a no-op for a pass" <|
            \() ->
                1 |> Expect.equal 1 |> Expect.not |> Expect.not
        , test "inverting twice is a no-op for a failure" <|
            \() ->
                1 |> Expect.equal 2 |> Expect.not |> Expect.not |> expectToFail
        , test "reports the inverted expectation's message" <|
            \() ->
                1
                    |> Expect.equal 1
                    |> Expect.not
                    |> expectReason "Equality"
                        (\reason ->
                            case reason of
                                Equality expected actual ->
                                    Just ( expected, actual )

                                _ ->
                                    Nothing
                        )
                        (Just ( "1", "1" ))
        , test "inverting a fuzz failure and back keeps its given" <|
            \() ->
                case fuzzFailure of
                    Nothing ->
                        Expect.fail "Expected the fuzz test to produce a failure"

                    Just expectation ->
                        case Test.Runner.getFailureReason (Expect.not (Expect.not expectation)) of
                            Just { given } ->
                                given |> Expect.equal (Just "42")

                            Nothing ->
                                Expect.fail "Expected the expectation to fail, but it passed!"
        , test "reports the inverted expectation's description" <|
            \() ->
                case Test.Runner.getFailureReason (Expect.not (Expect.equal 1 1)) of
                    Just { description } ->
                        description |> Expect.equal "Expect.not (Expect.equal)"

                    Nothing ->
                        Expect.fail "Expected the expectation to fail, but it passed!"
        , test "says something useful when it can't describe the pass" <|
            \() ->
                case Test.Runner.getFailureReason (Expect.not Expect.pass) of
                    Just { reason } ->
                        reason |> Expect.equal Custom

                    Nothing ->
                        Expect.fail "Expected the expectation to fail, but it passed!"
        , describe "doesn't invert invalid expectations into passes"
            [ test "empty Expect.all" <|
                \() ->
                    Expect.all [] |> Expect.not |> expectToFail
            , test "empty Expect.oneOf" <|
                \() ->
                    Expect.oneOf [] |> Expect.not |> expectToFail
            , test "equating two floats" <|
                \() ->
                    1.5 |> Expect.equal 1.5 |> Expect.not |> expectToFail
            , test "negative tolerance" <|
                \() ->
                    1
                        |> Expect.equalWithNumbers NaNsNeverEqual (Absolute -1) 2
                        |> Expect.not
                        |> expectToFail
            ]
        ]


{-| A failing fuzz test's expectation, which the runner has tagged with the
input it failed on.
-}
fuzzFailure : Maybe Expect.Expectation
fuzzFailure =
    Test.fuzz "always fails" (Fuzz.constant 42) (\n -> n |> Expect.equal 0)
        |> Test.Runner.fromTest 1 (Random.initialSeed 0)
        |> (\seededRunners ->
                case seededRunners of
                    Test.Runner.Plain [ runner ] ->
                        List.head (runner.run ())

                    _ ->
                        Nothing
           )


expectReason :
    String
    -> (Reason -> Maybe a)
    -> Maybe a
    -> Expect.Expectation
    -> Expect.Expectation
expectReason expectedReasonName toDetails expectedDetails expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            Expect.fail "Expected the expectation to fail, but it passed!"

        Just { reason } ->
            case toDetails reason of
                Nothing ->
                    Expect.fail
                        ("Expected a "
                            ++ expectedReasonName
                            ++ " reason, but got: "
                            ++ Debug.toString reason
                        )

                details ->
                    Expect.equal expectedDetails details
