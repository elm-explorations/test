module Tests exposing (all)

import Expect exposing (FloatingPointTolerance(..))
import FloatWithinTests exposing (floatWithinTests)
import Fuzz exposing (..)
import FuzzerTests exposing (fuzzerTests)
import Helpers exposing (..)
import Random
import RunnerTests
import Simplify
import SimplifyTests
import Test exposing (..)
import Test.Html.EventTests
import Test.Html.ExampleAppTests
import Test.Html.Query.CustomNodeTests
import Test.Html.Query.MarkdownTests
import Test.Html.QueryTests
import Test.Html.SelectorTests
import Test.Runner


all : Test
all =
    Test.concat
        [ readmeExample
        , regressions
        , testTests
        , expectationTests
        , fuzzerTests
        , floatWithinTests
        , SimplifyTests.all
        , RunnerTests.all
        , elmHtmlTests
        ]


elmHtmlTests : Test
elmHtmlTests =
    describe "elm-html-test"
        [ Test.Html.QueryTests.all
        , Test.Html.Query.MarkdownTests.all
        , Test.Html.Query.CustomNodeTests.all
        , Test.Html.SelectorTests.all
        , Test.Html.EventTests.all
        , Test.Html.ExampleAppTests.all
        ]


readmeExample : Test
readmeExample =
    describe "The String module"
        [ describe "String.reverse"
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                    Expect.equal palindrome (String.reverse palindrome)
            , test "reverses a known string" <|
                \_ ->
                    "ABCDEFG"
                        |> String.reverse
                        |> Expect.equal "GFEDCBA"
            , fuzz string "restores the original string if you run it again" <|
                \randomlyGeneratedString ->
                    randomlyGeneratedString
                        |> String.reverse
                        |> String.reverse
                        |> Expect.equal randomlyGeneratedString
            ]
        ]


expectationTests : Test
expectationTests =
    describe "Expectations"
        [ describe "Expect.err"
            [ test "passes on Err _" <|
                \_ ->
                    Err 12 |> Expect.err
            , test "passes on Ok _" <|
                \_ ->
                    Ok 12
                        |> Expect.err
                        |> expectToFail
            ]
        , describe "Expect.all"
            [ test "fails with empty list" <|
                \_ ->
                    "dummy subject"
                        |> Expect.all []
                        |> expectToFail
            ]
        , describe "Expect.equal"
            [ test "fails when equating two floats (see #230)" <|
                \_ ->
                    1.41
                        |> Expect.equal 1.41
                        |> expectToFail
            , test "succeeds when equating two ints" <|
                \_ -> 141 |> Expect.equal 141
            ]

        -- , describe "Expect.equal on unicode strings should show pretty output"
        --     [ test "ascii" <|
        --         \_ -> "😻🙀👻" |> Expect.equal "🙀👻😻🙈"
        --     , test "ascii space vs nbsp" <|
        --         \_ -> "asd qwe" |> Expect.equal "asd\u{00a0}qwe"
        --     , test "ascii only" <|
        --         \_ -> "asd qwe" |> Expect.equal "asd dwe"
        --     , test "newline diff" <|
        --         \_ -> "first\u{000a}second" |> Expect.equal "first\r\nsecond"
        --     , test "long lines" <|
        --         \_ -> "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in scelerisque arcu. Curabitur cursus efficitur turpis sed porttitor. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nunc eu cursus ex. Proin accumsan quam quis dui semper finibus. Nunc vel nibh at tellus finibus rhoncus eu eget dolor. Sed eget neque ut lorem imperdiet fermentum. 😻 Morbi iaculis ante euismod, vulputate velit ut, varius velit. Nulla tempus dapibus mattis. In tempus, nisi a porta lobortis, nulla lacus iaculis quam, vel euismod magna risus at tortor. Integer porta urna odio. Nulla pellentesque dictum maximus. Donec auctor urna nec tortor imperdiet varius." |> Expect.equal "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque in scelerisque arcu. Curabitur cursus efficitur turpis sed porttitor. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Nunc eu cursus ex. Proin accumsan quam quis dui semper finibus. Nunc vel nibh at tellus finibus rhoncus eu eget dolor. Sed eget neque ut lorem imperdiet fermentum. Morbi iaculis ante euismod, vulputate velit ut, varius velit. Nulla tempus dapibus mattis. In tempus, nisi a porta lobortis, nulla lacus iaculis quam, vel euismod magna risus at tortor. Integer porta urna odio. Nulla pellentesque dictum maximus. Donec auctor urna nec tortor imperdiet varius."
        --     ]
        ]


regressions : Test
regressions =
    describe "regression tests"
        [ fuzz (intRange 1 32) "for elm-community/elm-test #39" <|
            \positiveInt ->
                positiveInt
                    |> Expect.greaterThan 0
        , test "for elm-community/elm-test #127" <|
            {- If fuzz tests actually run 100 times, then asserting that no number
               in 1..8 equals 5 fails with 0.999998 probability. If they only run
               once, or stop after a duplicate due to #127, then it's much more
               likely (but not guaranteed) that the 5 won't turn up. See #128.
               (Issue numbers refer to elm-community/elm-test.)
            -}
            \() ->
                fuzz
                    (custom (Random.int 1 8) Simplify.simplest)
                    "fuzz tests run 100 times"
                    (Expect.notEqual 5)
                    |> expectTestToFail
        ]


testTests : Test
testTests =
    describe "functions that create tests"
        [ describe "describe"
            [ test "fails with empty list" <|
                \() ->
                    describe "x" []
                        |> expectTestToFail
            , test "fails with empty description" <|
                \() ->
                    describe "" [ test "x" expectPass ]
                        |> expectTestToFail
            ]
        , describe "test"
            [ test "fails with empty name" <|
                \() ->
                    test "" expectPass
                        |> expectTestToFail
            ]
        , describe "fuzz"
            [ test "fails with empty name" <|
                \() ->
                    fuzz Fuzz.bool "" expectPass
                        |> expectTestToFail
            ]
        , describe "fuzzWith"
            [ test "fails with fewer than 1 run" <|
                \() ->
                    fuzzWith { runs = 0 } Fuzz.bool "nonpositive" expectPass
                        |> expectTestToFail
            , test "fails with empty name" <|
                \() ->
                    fuzzWith { runs = 1 } Fuzz.bool "" expectPass
                        |> expectTestToFail
            ]
        , describe "Test.todo"
            [ test "causes test failure" <|
                \() ->
                    todo "a TODO test fails"
                        |> expectTestToFail
            , test "Passes are not TODO"
                (\_ -> Expect.pass |> Test.Runner.isTodo |> Expect.equal False)
            , test "Simple failures are not TODO" <|
                \_ ->
                    Expect.fail "reason" |> Test.Runner.isTodo |> Expect.equal False
            ]
        , identicalNamesAreRejectedTests
        ]


identicalNamesAreRejectedTests : Test
identicalNamesAreRejectedTests =
    describe "Identically-named sibling and parent/child tests fail"
        [ test "a describe with two identically named children" <|
            \() ->
                describe "x"
                    [ test "foo" expectPass
                    , test "foo" expectPass
                    ]
                    |> expectTestToFail
        , test "a describe with the same name as a child test" <|
            \() ->
                describe "A"
                    [ test "A" expectPass ]
                    |> expectTestToFail
        , test "a describe with the same name as a child describe fails" <|
            \() ->
                describe "A"
                    [ describe "A"
                        [ test "x" expectPass ]
                    ]
                    |> expectTestToFail
        , test "a describe with the same name as a sibling describe fails" <|
            \() ->
                Test.concat
                    [ describe "A" [ test "x" expectPass ]
                    , describe "A" [ test "y" expectPass ]
                    ]
                    |> expectTestToFail
        , test "a describe with the same name as a de facto sibling describe fails" <|
            \() ->
                Test.concat
                    [ Test.concat
                        [ describe "A" [ test "x" expectPass ]
                        ]
                    , describe "A" [ test "y" expectPass ]
                    ]
                    |> expectTestToFail
        , test "a describe with the same name as a de facto sibling describe fails (2)" <|
            \() ->
                Test.concat
                    [ Test.concat
                        [ describe "A" [ test "x" expectPass ]
                        ]
                    , Test.concat
                        [ describe "A" [ test "y" expectPass ]
                        ]
                    ]
                    |> expectTestToFail
        ]
