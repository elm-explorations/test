module RunnerTests exposing (all)

import Expect
import Fuzz exposing (..)
import Helpers exposing (expectPass)
import Random
import Test exposing (..)
import Test.Runner exposing (SeededRunners(..))


all : Test
all =
    Test.concat
        [ fromTest ]


toSeededRunners : Test -> SeededRunners
toSeededRunners =
    Test.Runner.fromTest 5 (Random.initialSeed 42)


fromTest : Test
fromTest =
    describe "Test.Runner.fromTest"
        [ describe "test length"
            [ fuzz2 "only positive tests runs are valid" int int <|
                \runs intSeed ->
                    case Test.Runner.fromTest runs (Random.initialSeed intSeed) passing of
                        Invalid str ->
                            if runs > 0 then
                                Expect.fail ("Expected a run count of " ++ String.fromInt runs ++ " to be valid, but was invalid with this message: " ++ Debug.toString str)

                            else
                                Expect.pass

                        val ->
                            if runs > 0 then
                                Expect.pass

                            else
                                Expect.fail ("Expected a run count of " ++ String.fromInt runs ++ " to be invalid, but was valid with this value: " ++ Debug.toString val)
            , test "an only inside another only should ignore the non-only siblings" <|
                \_ ->
                    let
                        suite =
                            describe "three tests"
                                [ test "passes" expectPass
                                , Test.only <|
                                    describe "two tests"
                                        [ test "fails" testImpl
                                        , Test.only (test "is an only" testImpl)
                                        ]
                                ]
                    in
                    case toSeededRunners suite of
                        Only runners ->
                            runners
                                |> List.map (.labels >> List.reverse)
                                |> Expect.equal [ [ "three tests", "two tests", "is an only" ] ]

                        val ->
                            Expect.fail ("Expected SeededRunner to be Only, but was " ++ Debug.toString val)
            , test "should keep all only tests spread among siblings" <|
                \_ ->
                    let
                        suite =
                            describe "root"
                                [ test "A" expectPass
                                , describe "B"
                                    [ Test.only (test "1" testImpl)
                                    ]
                                , Test.only <|
                                    describe "C"
                                        [ test "1" testImpl
                                        , Test.only (test "2" testImpl)
                                        , describe "3"
                                            [ test "X" testImpl
                                            , Test.only (test "Y" testImpl)
                                            , test "Z" testImpl
                                            ]
                                        ]
                                , describe "D"
                                    [ Test.only (test "1" testImpl)
                                    ]
                                ]
                    in
                    case toSeededRunners suite of
                        Only runners ->
                            runners
                                |> List.map (.labels >> List.reverse)
                                |> Expect.equal
                                    [ [ "root", "B", "1" ]
                                    , [ "root", "C", "2" ]
                                    , [ "root", "C", "3", "Y" ]
                                    , [ "root", "D", "1" ]
                                    ]

                        val ->
                            Expect.fail ("Expected SeededRunner to be Only, but was " ++ Debug.toString val)
            , test "a skip inside an only takes effect" <|
                \_ ->
                    let
                        suite =
                            describe "three tests"
                                [ test "passes" expectPass
                                , Test.only <|
                                    describe "two tests"
                                        [ test "fails" testImpl
                                        , Test.skip (test "is skipped" testImpl)
                                        ]
                                ]
                    in
                    case toSeededRunners suite of
                        Only runners ->
                            runners
                                |> List.map (.labels >> List.reverse)
                                |> Expect.equal [ [ "three tests", "two tests", "fails" ] ]

                        val ->
                            Expect.fail ("Expected SeededRunner to be Only, but was " ++ Debug.toString val)
            , test "an only inside a skip has no effect" <|
                \_ ->
                    let
                        suite =
                            describe "three tests"
                                [ test "passes" expectPass
                                , Test.skip <|
                                    describe "two tests"
                                        [ test "fails" testImpl
                                        , Test.only (test "is skipped" testImpl)
                                        ]
                                ]
                    in
                    case toSeededRunners suite of
                        Skipping runners ->
                            runners
                                |> List.map (.labels >> List.reverse)
                                |> Expect.equal [ [ "three tests", "passes" ] ]

                        val ->
                            Expect.fail ("Expected SeededRunner to be Skipping, but was " ++ Debug.toString val)
            , test "a test that uses only is an Only summary" <|
                \_ ->
                    case toSeededRunners (Test.only <| test "passes" expectPass) of
                        Only runners ->
                            runners
                                |> List.map (.labels >> List.reverse)
                                |> Expect.equal [ [ "passes" ] ]

                        val ->
                            Expect.fail ("Expected SeededRunner to be Only, but was " ++ Debug.toString val)
            , test "a skip inside another skip has no effect" <|
                \_ ->
                    let
                        suite =
                            describe "three tests"
                                [ test "passes" expectPass
                                , Test.skip <|
                                    describe "two tests"
                                        [ test "fails" testImpl
                                        , Test.skip (test "is skipped" testImpl)
                                        ]
                                ]
                    in
                    case toSeededRunners suite of
                        Skipping runners ->
                            runners
                                |> List.map (.labels >> List.reverse)
                                |> Expect.equal [ [ "three tests", "passes" ] ]

                        val ->
                            Expect.fail ("Expected SeededRunner to be Skipping, but was " ++ Debug.toString val)
            , test "a pair of tests where one uses skip is a Skipping summary" <|
                \_ ->
                    let
                        suite =
                            describe "two tests"
                                [ test "passes" expectPass
                                , Test.skip (test "fails" testImpl)
                                ]
                    in
                    case toSeededRunners suite of
                        Skipping runners ->
                            runners
                                |> List.map (.labels >> List.reverse)
                                |> Expect.equal [ [ "two tests", "passes" ] ]

                        val ->
                            Expect.fail ("Expected SeededRunner to be Skipping, but was " ++ Debug.toString val)
            , test "when all tests are skipped, we get an empty Skipping summary" <|
                \_ ->
                    case toSeededRunners (Test.skip <| test "passes" expectPass) of
                        Skipping runners ->
                            runners
                                |> List.length
                                |> Expect.equal 0

                        val ->
                            Expect.fail ("Expected SeededRunner to be Skipping, but was " ++ Debug.toString val)
            , test "a test that does not use only or skip is a Plain summary" <|
                \_ ->
                    case toSeededRunners (test "passes" expectPass) of
                        Plain runners ->
                            runners
                                |> List.map (.labels >> List.reverse)
                                |> Expect.equal [ [ "passes" ] ]

                        val ->
                            Expect.fail ("Expected SeededRunner to be Plain, but was " ++ Debug.toString val)
            ]
        ]


passing : Test
passing =
    test "A passing test" expectPass


{-| Dummy test implementation.
-}
testImpl : () -> Expect.Expectation
testImpl () =
    Expect.pass
