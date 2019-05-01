module FuzzerTests exposing (fuzzerTests)

import Expect
import Fuzz exposing (..)
import Helpers exposing (..)
import Random
import Test exposing (..)
import Test.Runner
import Validate


fuzzerTests : Test
fuzzerTests =
    describe "Fuzzer methods that use Debug.crash don't call it"
        [ describe "FuzzN (uses use pair or triple) testing string length properties"
            [ fuzz2 string string "fuzz2" <|
                \a b ->
                    testStringLengthIsPreserved [ a, b ]
            , fuzz3 string string string "fuzz3" <|
                \a b c ->
                    testStringLengthIsPreserved [ a, b, c ]
            ]
        , fuzz
            (intRange 1 6)
            "intRange"
            (Expect.greaterThan 0)
        , fuzz
            (frequency [ ( 1, intRange 1 6 ), ( 1, intRange 1 20 ) ])
            "Fuzz.frequency"
            (Expect.greaterThan 0)
        , fuzz (result string int) "Fuzz.result" <| \r -> Expect.pass
        , describe "Whitebox testing using Fuzz.Internal"
            [ fuzz randomSeedFuzzer "the same value is generated with and without simplifying" <|
                \seed ->
                    let
                        step gen =
                            Random.step gen seed

                        aFuzzer =
                            triple
                                ( pair ( list int, array float )
                                , pair
                                    ( maybe bool
                                    , result unit char
                                    )
                                , pair
                                    ( triple
                                        ( percentage
                                        , map2 (+) int int
                                        , frequency [ ( 1, constant True ), ( 3, constant False ) ]
                                        )
                                    , triple ( intRange 0 100, floatRange -51 pi, map abs int )
                                    )
                                )
                                |> Test.Runner.fuzz

                        valNoSimplify =
                            aFuzzer |> Result.map (Random.map Tuple.first >> step >> Tuple.first)

                        valWithSimplify =
                            aFuzzer |> Result.map (step >> Tuple.first >> Tuple.first)
                    in
                    Expect.equal valNoSimplify valWithSimplify
            , simplifyingTests
            , manualFuzzerTests
            , unicodeStringFuzzerTests
            ]
        ]


simplifyingTests : Test
simplifyingTests =
    let
        -- To test simplifying, we have to fail some tests so we can simplify their inputs.
        -- The best place we found for storing the expected last state(s) of the simplifying procedure is the description field, which is why we have this function here.
        -- Previously, we (ab)used Expect.true for this, but since that was removed, here we are.
        expectTrueAndExpectSimplifyResultToEqualString label a =
            Expect.equal True a |> Expect.onFail label
    in
    testSimplifying <|
        describe "tests that fail intentionally to test simplifying"
            [ fuzz2 int int "Every pair of ints has a zero" <|
                \i j ->
                    (i == 0)
                        || (j == 0)
                        |> expectTrueAndExpectSimplifyResultToEqualString "(1,1)"
            , fuzz3 int int int "Every triple of ints has a zero" <|
                \i j k ->
                    (i == 0)
                        || (j == 0)
                        || (k == 0)
                        |> expectTrueAndExpectSimplifyResultToEqualString "(1,1,1)"
            , fuzz (list int) "All lists are sorted" <|
                \aList ->
                    let
                        checkPair l =
                            case l of
                                a :: b :: more ->
                                    if a > b then
                                        False

                                    else
                                        checkPair (b :: more)

                                _ ->
                                    True
                    in
                    checkPair aList |> expectTrueAndExpectSimplifyResultToEqualString "[1,0]|[0,-1]"
            ]


type alias SimplifyResult a =
    Maybe ( a, Test.Runner.Simplifiable a )


initialSimplifyResult : Fuzzer a -> Random.Seed -> SimplifyResult a
initialSimplifyResult fuzzer seed =
    case Test.Runner.fuzz fuzzer of
        Ok generator ->
            Random.step generator seed
                |> Tuple.first
                |> Just

        Err _ ->
            Nothing


manualFuzzerTests : Test
manualFuzzerTests =
    describe "Test.Runner.{fuzz, simplify}"
        [ fuzz randomSeedFuzzer "Claim there are no even numbers" <|
            \seed ->
                let
                    -- fuzzer is guaranteed to produce an even number
                    fuzzer =
                        Fuzz.intRange 2 10000
                            |> Fuzz.map
                                (\n ->
                                    if failsTest n then
                                        n

                                    else
                                        n + 1
                                )

                    failsTest n =
                        (n |> modBy 2) == 0

                    pair =
                        initialSimplifyResult fuzzer seed

                    unfold acc maybePair =
                        case maybePair of
                            Just ( valN, simplifyN ) ->
                                if failsTest valN then
                                    unfold (valN :: acc) (Test.Runner.simplify False simplifyN)

                                else
                                    unfold acc (Test.Runner.simplify True simplifyN)

                            Nothing ->
                                acc
                in
                unfold [] pair
                    |> Expect.all
                        [ List.all failsTest >> Expect.equal True >> Expect.onFail "Not all elements were even"
                        , List.head
                            >> Maybe.map (Expect.all [ Expect.lessThan 5, Expect.atLeast 0 ])
                            >> Maybe.withDefault (Expect.fail "Did not cause failure")
                        , List.reverse >> List.head >> Expect.equal (Maybe.map Tuple.first pair)
                        ]
        , fuzz randomSeedFuzzer "No strings contain the letter e" <|
            \seed ->
                let
                    -- fuzzer is guaranteed to produce a string with the letter e
                    fuzzer =
                        map2 (\pre suf -> pre ++ "e" ++ suf) string string

                    failsTest =
                        String.contains "e"

                    pair =
                        initialSimplifyResult fuzzer seed

                    unfold acc maybePair =
                        case maybePair of
                            Just ( valN, simplifyN ) ->
                                if failsTest valN then
                                    unfold (valN :: acc) (Test.Runner.simplify False simplifyN)

                                else
                                    unfold acc (Test.Runner.simplify True simplifyN)

                            Nothing ->
                                acc
                in
                unfold [] pair
                    |> Expect.all
                        [ List.all failsTest >> Expect.equal True >> Expect.onFail "Not all contained the letter e"
                        , List.head >> Expect.equal (Just "e")
                        , List.reverse >> List.head >> Expect.equal (Maybe.map Tuple.first pair)
                        ]
        , fuzz randomSeedFuzzer "List simplifier finds the smallest counter example" <|
            \seed ->
                let
                    fuzzer : Fuzzer (List Int)
                    fuzzer =
                        Fuzz.list Fuzz.int

                    allEven : List Int -> Bool
                    allEven xs =
                        List.all (\x -> (x |> modBy 2) == 0) xs

                    initialSimplify : SimplifyResult (List Int)
                    initialSimplify =
                        initialSimplifyResult fuzzer seed

                    simplify : Maybe (List Int) -> SimplifyResult (List Int) -> Maybe (List Int)
                    simplify simplified lastSimplify =
                        case lastSimplify of
                            Just ( valN, simplifyN ) ->
                                simplify
                                    (if allEven valN then
                                        simplified

                                     else
                                        Just valN
                                    )
                                    (Test.Runner.simplify (allEven valN) simplifyN)

                            Nothing ->
                                simplified
                in
                case simplify Nothing initialSimplify of
                    Just simplified ->
                        Expect.equal [ 1 ] simplified

                    Nothing ->
                        Expect.pass
        ]


whitespaceTest : Test
whitespaceTest =
    describe "fuzzing whitespace (taken from rtfeldman/elm-validate, which crashed when this first ran)"
        [ fuzz whitespace "whitespace characters are blank" <|
            \str ->
                str
                    |> Validate.isBlank
                    |> Expect.equal True
                    >> Expect.onFail "Validate.isBlank should consider whitespace blank"
        , fuzz2 whitespace whitespace "non-whitespace characters mean it's not blank" <|
            \prefix suffix ->
                (prefix ++ "_" ++ suffix)
                    |> Validate.isBlank
                    |> Expect.equal False
                    >> Expect.onFail "Validate.isBlank shouldn't consider strings containing non-whitespace characters blank"
        ]


email : Test
email =
    describe "email"
        [ test "empty string is not a valid email" <|
            \() ->
                ""
                    |> Validate.isValidEmail
                    |> Expect.equal False
                    >> Expect.onFail "Validate.isValidEmail should have considered empty string blank"
        , test "valid email is valid" <|
            \() ->
                "foo@bar.com"
                    |> Validate.isValidEmail
                    |> Expect.equal True
                    >> Expect.onFail "Validate.isValidEmail should have considered foo@bar.com a valid email address"
        ]


whitespace : Fuzzer String
whitespace =
    [ ' ', '\u{00A0}', '\t', '\n' ]
        |> List.map Fuzz.constant
        |> Fuzz.oneOf
        |> Fuzz.list
        |> Fuzz.map String.fromList


unicodeStringFuzzerTests : Test
unicodeStringFuzzerTests =
    describe "unicode string fuzzer" <|
        [ test "generates ascii" <|
            \() ->
                expectTestToFail <|
                    fuzz string "generates ascii" <|
                        \str -> str |> String.contains "E" |> Expect.equal True
        , test "generates whitespace" <|
            \() ->
                expectTestToFail <|
                    fuzz string "generates whitespace" <|
                        \str -> str |> String.contains "\t" |> Expect.equal True
        , test "generates combining diacritical marks" <|
            \() ->
                expectTestToFail <|
                    fuzz string "generates combining diacritical marks" <|
                        \str -> str |> String.contains "̃" |> Expect.equal True
        , test "generates emoji" <|
            \() ->
                expectTestToFail <|
                    fuzz string "generates emoji" <|
                        \str -> str |> String.contains "🔥" |> Expect.equal True
        , test "generates long strings with a single character" <|
            \() ->
                expectTestToFail <|
                    fuzz string "generates long strings with a single character" <|
                        \str ->
                            let
                                countSequentialUniquesAtStart s =
                                    case s of
                                        a :: b :: cs ->
                                            if a == b then
                                                1 + countSequentialUniquesAtStart (b :: cs)

                                            else
                                                0

                                        _ ->
                                            0
                            in
                            str |> String.toList |> countSequentialUniquesAtStart |> (\x -> x < 7) |> Expect.equal True
        ]
