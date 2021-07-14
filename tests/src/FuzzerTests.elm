module FuzzerTests exposing (fuzzerTests)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Helpers exposing (..)
import Random
import Set
import Test exposing (..)
import Test.Runner exposing (Simplifiable)


fuzzerTests : Test
fuzzerTests =
    describe "Fuzzer tests"
        [ describe "FuzzN (uses use pair or triple) testing string length properties"
            [ fuzz2 string string "fuzz2" <|
                \a b ->
                    testStringLengthIsPreserved [ a, b ]
            , fuzz3 string string string "fuzz3" <|
                \a b c ->
                    testStringLengthIsPreserved [ a, b, c ]
            ]
        , describe "Whitebox testing using Fuzz.Internal"
            [ manualFuzzerTests
            , unicodeStringFuzzerTests
            ]
        , fuzzerSpecificationTests
        ]


type alias SimplifyResult a =
    Maybe ( a, Test.Runner.Simplifiable a )


initialSimplifyResult : Fuzzer a -> Random.Seed -> SimplifyResult a
initialSimplifyResult fuzzer seed =
    Random.step (Test.Runner.fuzz fuzzer) seed
        |> Tuple.first
        |> Result.toMaybe


manualFuzzerTests : Test
manualFuzzerTests =
    describe "Test.Runner.{fuzz, simplify}"
        [ fuzz randomSeedFuzzer "Claim there are no even numbers" <|
            \seed ->
                let
                    -- fuzzer is guaranteed to produce an even number
                    fuzzer : Fuzzer Int
                    fuzzer =
                        Fuzz.intRange 1 10000
                            |> Fuzz.map ((*) 2)

                    getExpectation : Int -> Expectation
                    getExpectation n =
                        modBy 2 n
                            |> Expect.notEqual 0

                    pair : Maybe ( Int, Simplifiable Int )
                    pair =
                        initialSimplifyResult fuzzer seed

                    finalValue : Maybe Int
                    finalValue =
                        pair
                            |> Maybe.andThen (Test.Runner.simplify getExpectation)
                            |> Maybe.map Tuple.first
                in
                finalValue
                    |> Expect.equal (Just 2)
        , fuzz randomSeedFuzzer "No strings contain the letter e" <|
            \seed ->
                let
                    -- fuzzer is guaranteed to produce a string with the letter e
                    fuzzer : Fuzzer String
                    fuzzer =
                        map2 (\pre suf -> pre ++ "e" ++ suf) string string

                    getExpectation : String -> Expectation
                    getExpectation string =
                        if String.contains "e" string then
                            Expect.fail "String had 'e' in it"

                        else
                            Expect.pass

                    pair : Maybe ( String, Simplifiable String )
                    pair =
                        initialSimplifyResult fuzzer seed

                    finalValue : Maybe String
                    finalValue =
                        pair
                            |> Maybe.andThen (Test.Runner.simplify getExpectation)
                            |> Maybe.map Tuple.first
                in
                finalValue
                    |> Maybe.map
                        (Expect.all
                            [ \string ->
                                if String.contains "e" string then
                                    Expect.pass

                                else
                                    Expect.fail "Didn't have 'e' in it"
                            , Expect.equal "e"
                            ]
                        )
                    |> Maybe.withDefault (Expect.fail "no final value")
        ]


unicodeStringFuzzerTests : Test
unicodeStringFuzzerTests =
    describe "unicode string fuzzer" <|
        -- These tests are a bit hard to read. Sorry about that.
        --
        -- The tools we have at our disposal are:
        -- - Forall (∀) in the form of normal fuzz tests
        -- - Exists not (∃𝑥¬) in the form of expectTestToFail
        --
        -- so with these tools we made these statistical tests:
        --
        -- `exists (fuzzed string) such that ((fuzzed string) contains (specific string))` -- what we want to test
        -- <=> (¬¬𝑥 <=> 𝑥) (since the only tool for Exists we have is Exists not, we negate the body to counter that negation)
        -- `exists (fuzzed string) such that (not (not ((fuzzed string) contains (specific string))))` -- what we actually test here
        -- where
        -- `expectTestsToFail x` <=> `exists (fuzzed string) such that (not x)`
        -- so what our fuzz tests should looks like is
        -- `(not ((fuzzed string) contains (specific string)))`
        [ test "generates ascii" <|
            \() ->
                expectTestToFail <|
                    fuzz string "generates ascii" <|
                        \str -> str |> String.contains "E" |> Expect.equal False
        , test "generates whitespace" <|
            \() ->
                expectTestToFail <|
                    fuzz string "generates whitespace" <|
                        \str -> str |> String.contains "\t" |> Expect.equal False
        , test "generates combining diacritical marks" <|
            \() ->
                expectTestToFail <|
                    fuzz string "generates combining diacritical marks" <|
                        \str -> str |> String.contains "̃" |> Expect.equal False
        , test "generates emoji" <|
            \() ->
                expectTestToFail <|
                    fuzz string "generates emoji" <|
                        \str -> str |> String.contains "🔥" |> Expect.equal False
        , test "the String.reverse bug that prevented us from releasing unicode string fuzzers in August 2017 is now fixed" <|
            -- if characters that span more than one utf-16 character work, this version of the unicode string fuzzer is good to go
            \() -> "🔥" |> String.reverse |> Expect.equal "🔥"

        --, test "String.reverse implements unicode string reversing correctly" <|
        --    -- String.reverse still doesn't properly implement unicode string reversing, so combining emojis like skin tones or families break
        --    -- Here's a test that should pass, since these emoji families are supposed to be counted as single elements when reversing the string. When I'm writing this, I instead get a per-character string reversal, which renders as four emojis after each other "👦👦👩👩" (plus a bunch of non-printable characters in-between).
        --    \() -> "👩‍👩‍👦‍👦" |> String.reverse |> Expect.equal "👩‍👩‍👦‍👦"
        ]


fuzzerSpecificationTests : Test
fuzzerSpecificationTests =
    Test.describe "Fuzz.*"
        [ describe "Tough examples"
            [ simplifiesTowards "redistributed additive pair"
                ( 1, 1000 )
                (Fuzz.pair
                    (Fuzz.intRange 0 1000)
                    (Fuzz.intRange 0 1000)
                )
                (\( m, n ) -> m + n <= 1000)
            , simplifiesTowards "list written in flip-a-coin way"
                [ 1001 ]
                (Fuzz.list (Fuzz.intRange 0 10000))
                (\list -> List.sum list <= 1000)
            , simplifiesTowards "list written in length-first way"
                -- TODO this has problems with run [2,0,1001]
                --                        == value [0,1001]
                [ 1001 ]
                (Fuzz.intRange 0 10
                    |> Fuzz.andThen
                        (\length ->
                            let
                                go : Int -> List Int -> Fuzzer (List Int)
                                go todo acc =
                                    if todo <= 0 then
                                        Fuzz.constant (List.reverse acc)

                                    else
                                        Fuzz.intRange 0 10000
                                            |> Fuzz.andThen (\item -> go (todo - 1) (item :: acc))
                            in
                            go length []
                        )
                )
                (\list -> List.sum list <= 1000)
            ]
        , describe "Fuzzers"
            [ describe "bool"
                [ canGenerate False Fuzz.bool
                , canGenerate True Fuzz.bool
                , simplifiesTowards "simplest" False Fuzz.bool fullySimplify
                , simplifiesTowards "next simplest" True Fuzz.bool (\v -> v == False)
                ]
            , describe "order"
                [ canGenerate LT Fuzz.order
                , canGenerate EQ Fuzz.order
                , canGenerate GT Fuzz.order
                , simplifiesTowards "simplest" LT Fuzz.order fullySimplify
                , simplifiesTowards "next simplest" EQ Fuzz.order (\x -> x == LT)
                ]
            , describe "unit"
                [ canGenerate () Fuzz.unit
                , simplifiesTowards "()" () Fuzz.unit fullySimplify
                ]
            , describe "constant"
                [ passes "Returns what you give it - Int"
                    (Fuzz.constant 42)
                    (\v -> v == 42)
                , passes "Returns what you give it - different Int"
                    (Fuzz.constant 999)
                    (\v -> v == 999)
                , passes "Returns what you give it - Bool"
                    (Fuzz.constant True)
                    (\v -> v == True)
                , simplifiesTowards "42" 42 (Fuzz.constant 42) fullySimplify
                ]
            , describe "maybe"
                [ canGenerateSatisfying "Just" (Fuzz.maybe Fuzz.unit) ((/=) Nothing)
                , canGenerateSatisfying "Nothing" (Fuzz.maybe Fuzz.unit) ((==) Nothing)
                , simplifiesTowards "simplest" Nothing (Fuzz.maybe Fuzz.int) fullySimplify
                , simplifiesTowards "non-Nothing" (Just 0) (Fuzz.maybe Fuzz.int) (\x -> x == Nothing)
                ]
            , describe "result"
                [ canGenerateSatisfying "Ok"
                    (Fuzz.result Fuzz.unit Fuzz.unit)
                    (Result.toMaybe >> (/=) Nothing)
                , canGenerateSatisfying "Err"
                    (Fuzz.result Fuzz.unit Fuzz.unit)
                    (Result.toMaybe >> (==) Nothing)
                , simplifiesTowards "simplest"
                    (Err 0)
                    (Fuzz.result Fuzz.int Fuzz.int)
                    fullySimplify
                , simplifiesTowards "non-Err"
                    (Ok 0)
                    (Fuzz.result Fuzz.int Fuzz.int)
                    (\x -> Result.toMaybe x == Nothing)
                ]
            , describe "pair"
                [ simplifiesTowards "Every pair of ints has a zero"
                    ( 1, 1 )
                    (Fuzz.pair Fuzz.int Fuzz.int)
                    (\( i, j ) -> (i == 0) || (j == 0))
                ]
            , describe "triple"
                [ simplifiesTowards "Every triple of ints has a zero"
                    ( 1, 1, 1 )
                    (Fuzz.triple Fuzz.int Fuzz.int Fuzz.int)
                    (\( i, j, k ) -> (i == 0) || (j == 0) || (k == 0))
                ]
            , describe "map"
                (let
                    fuzzer : Fuzzer Int
                    fuzzer =
                        Fuzz.int
                            |> Fuzz.map (\n -> n * 2)
                 in
                 [ passes "Any number * 2 = even number" fuzzer (\n -> modBy 2 n == 0)
                 , simplifiesTowards "simplest" 0 fuzzer fullySimplify
                 , simplifiesTowards "non-zero" 2 fuzzer (\n -> n == 0)
                 ]
                )
            , describe "intRange"
                [ passes "Smaller range"
                    (Fuzz.intRange -5 5)
                    (\n -> n >= -5 && n <= 5)
                , cannotGenerateSatisfying "Smaller range"
                    (Fuzz.intRange -5 5)
                    (\n -> n < -5 && n > 5)
                , passes "Dice"
                    (Fuzz.intRange 1 6)
                    (\n -> n > 0)
                , simplifiesTowards "(-,+) simplest" 0 (Fuzz.intRange -5 5) fullySimplify
                , simplifiesTowards "(-,+) non-zero" 1 (Fuzz.intRange -5 5) (\n -> n == 0)
                , simplifiesTowards "(0,+) simplest" 0 (Fuzz.intRange 0 5) fullySimplify
                , simplifiesTowards "(0,+) non-zero" 1 (Fuzz.intRange 0 5) (\n -> n == 0)
                , simplifiesTowards "(+,+) simplest" 1 (Fuzz.intRange 1 5) fullySimplify
                , simplifiesTowards "(+,+) non-low" 2 (Fuzz.intRange 1 5) (\n -> n == 1)
                , simplifiesTowards "(-,0) simplest" 0 (Fuzz.intRange -5 0) fullySimplify
                , simplifiesTowards "(-,0) non-zero" -1 (Fuzz.intRange -5 0) (\n -> n == 0)
                , simplifiesTowards "(-,-) simplest" -1 (Fuzz.intRange -5 -1) fullySimplify
                , simplifiesTowards "(-,-) non-high" -2 (Fuzz.intRange -5 -1) (\n -> n == -1)
                , rejects "min > max"
                    (Fuzz.intRange 5 -5)
                    "Fuzz.intRange was given a lower bound of 5 which is greater than the upper bound, -5."
                ]
            , describe "int"
                [ cannotGenerateSatisfying "any Infinity"
                    Fuzz.int
                    (isInfinite << toFloat)
                , cannotGenerateSatisfying "NaN"
                    Fuzz.int
                    (isNaN << toFloat)
                , simplifiesTowards "simplest" 0 Fuzz.int fullySimplify
                , simplifiesTowards "non-zero" 1 Fuzz.int (\n -> n == 0)
                , simplifiesTowards "negative" -1 Fuzz.int (\n -> n >= 0)
                ]
            , describe "percentage"
                [ passes "Range 0..1"
                    Fuzz.percentage
                    (\p -> p >= 0 && p <= 1)
                , cannotGenerateSatisfying "any Infinity" Fuzz.percentage isInfinite
                , cannotGenerateSatisfying "NaN" Fuzz.percentage isNaN
                , simplifiesTowards "simplest" 0 Fuzz.percentage fullySimplify
                , simplifiesTowards "non-zero" 2.2204460492503136e-16 Fuzz.percentage (\v -> v == 0)
                , simplifiesTowards "non-zero non-one, specific threshold #1"
                    0.25
                    Fuzz.percentage
                    (\v -> v == 1 || v < 0.25)
                , simplifiesTowards "non-zero non-one, specific threshold #2"
                    0.25000000000000006
                    Fuzz.percentage
                    (\v -> v == 1 || v <= 0.25)
                ]
            , describe "char"
                [ passes "Range 32..126"
                    Fuzz.char
                    (\char ->
                        let
                            code =
                                Char.toCode char
                        in
                        code >= 32 && code <= 126
                    )
                , simplifiesTowards "simplest" ' ' Fuzz.char fullySimplify
                , simplifiesTowards "next simplest" '!' Fuzz.char (\c -> c == ' ')
                , simplifiesTowards "above A" 'B' Fuzz.char (\c -> Char.toCode c <= Char.toCode 'A')
                ]
            , describe "string"
                [ canGenerateWith { runs = 1000 } "" Fuzz.string
                , canGenerateSatisfying "non-empty string"
                    -- TODO flaky test: the probability isn't great without preferring empty string
                    Fuzz.string
                    (not << String.isEmpty)
                , simplifiesTowards "simplest" "" Fuzz.string fullySimplify
                , simplifiesTowardsWith { runs = 1000 } "next simplest" " " Fuzz.string (\x -> x == "")
                , simplifiesTowardsWith { runs = 1000 }
                    "alpha"
                    "A"
                    Fuzz.string
                    (\x -> x == "" || not (String.all Char.isAlpha x))
                ]
            , describe "oneOfValues"
                [ canGenerate 1 (Fuzz.oneOfValues [ 1, 42 ])
                , canGenerate 42 (Fuzz.oneOfValues [ 1, 42 ])
                , cannotGenerateSatisfying "not in list"
                    (Fuzz.oneOfValues [ 1, 42 ])
                    (\n -> not <| List.member n [ 1, 42 ])
                , passes "One value -> picks it"
                    (Fuzz.oneOfValues [ 42 ])
                    (\n -> n == 42)
                , simplifiesTowards "simplest" 42 (Fuzz.oneOfValues [ 42, 1, 999 ]) fullySimplify
                , simplifiesTowards "next simplest" 1 (Fuzz.oneOfValues [ 42, 1, 999 ]) (\x -> x == 42)
                , rejects "empty list"
                    (Fuzz.oneOfValues [])
                    "Fuzz.oneOfValues: You must provide at least one item."
                ]
            , describe "oneOf"
                (let
                    fuzzer : Fuzzer Int
                    fuzzer =
                        Fuzz.oneOf
                            [ Fuzz.intRange -2 0
                            , Fuzz.constant 2
                            ]

                    constFuzzer : Fuzzer Int
                    constFuzzer =
                        Fuzz.oneOf
                            [ Fuzz.constant 42
                            , Fuzz.constant 1
                            , Fuzz.constant 999
                            ]
                 in
                 [ canGenerate -2 fuzzer
                 , canGenerate -1 fuzzer
                 , canGenerate 0 fuzzer
                 , canGenerate 2 fuzzer
                 , cannotGenerateSatisfying "not in the range"
                    fuzzer
                    (\n -> not <| List.member n [ -2, -1, 0, 2 ])
                 , passes "One fuzzer -> picks it"
                    (Fuzz.oneOf [ Fuzz.constant 42 ])
                    (\n -> n == 42)
                 , simplifiesTowards "simplest" 42 constFuzzer fullySimplify
                 , simplifiesTowards "next simplest" 1 constFuzzer (\x -> x == 42)
                 , rejects "empty list"
                    (Fuzz.oneOf [])
                    "Fuzz.oneOf: You must provide at least one item."
                 ]
                )
            , describe "frequencyValues"
                (let
                    fuzzer : Fuzzer Int
                    fuzzer =
                        Fuzz.frequencyValues
                            [ ( 0.3, 1 )
                            , ( 0.7, 42 )
                            ]

                    simplifyFuzzer : Fuzzer Int
                    simplifyFuzzer =
                        Fuzz.frequencyValues
                            [ ( 1, 42 )
                            , ( 2, 1 )
                            , ( 3, 999 )
                            ]
                 in
                 [ canGenerate 1 fuzzer
                 , canGenerate 42 fuzzer
                 , cannotGenerateSatisfying "not in the range"
                    fuzzer
                    (\n -> not <| List.member n [ 1, 42 ])
                 , passes "One value -> picks it"
                    (Fuzz.frequencyValues [ ( 0.7, 42 ) ])
                    (\n -> n == 42)
                 , simplifiesTowards "simplest" 42 simplifyFuzzer fullySimplify
                 , simplifiesTowards "next simplest" 1 simplifyFuzzer (\x -> x == 42)
                 , rejects "empty list"
                    (Fuzz.frequencyValues [])
                    "Fuzz.frequencyValues: You must provide at least one frequency pair with weight greater than 0."
                 , rejects "zero weight"
                    (Fuzz.frequencyValues [ ( 0, () ) ])
                    "Fuzz.frequencyValues: You must provide at least one frequency pair with weight greater than 0."
                 , doesNotReject "zero and non-zero weight"
                    (Fuzz.frequencyValues
                        [ ( 0, () )
                        , ( 1, () )
                        ]
                    )
                 , rejects "negative weight"
                    (Fuzz.frequencyValues [ ( -1, () ) ])
                    "Fuzz.frequencyValues: No frequency weights can be less than 0."
                 ]
                )
            , describe "frequency"
                (let
                    fuzzer : Fuzzer Int
                    fuzzer =
                        Fuzz.frequency
                            [ ( 0.3, Fuzz.intRange -2 0 )
                            , ( 0.7, Fuzz.constant 2 )
                            ]

                    simplifyFuzzer : Fuzzer Int
                    simplifyFuzzer =
                        Fuzz.frequency
                            [ ( 1, Fuzz.constant 42 )
                            , ( 2, Fuzz.constant 1 )
                            , ( 3, Fuzz.constant 999 )
                            ]
                 in
                 [ canGenerate -2 fuzzer
                 , canGenerate -1 fuzzer
                 , canGenerate 0 fuzzer
                 , canGenerate 2 fuzzer
                 , cannotGenerateSatisfying "not in the range"
                    fuzzer
                    (\n -> not <| List.member n [ -2, -1, 0, 2 ])
                 , passes "One fuzzer -> picks it"
                    (Fuzz.frequency [ ( 0.7, Fuzz.constant 42 ) ])
                    (\n -> n == 42)
                 , passes "One of two dice"
                    (Fuzz.frequency
                        [ ( 1, Fuzz.intRange 1 6 )
                        , ( 1, Fuzz.intRange 1 20 )
                        ]
                    )
                    (\n -> n > 0)
                 , simplifiesTowards "simplest" 42 simplifyFuzzer fullySimplify
                 , simplifiesTowards "next simplest" 1 simplifyFuzzer (\x -> x == 42)
                 , rejects "empty list"
                    (Fuzz.frequency [])
                    "Fuzz.frequency: You must provide at least one frequency pair with weight greater than 0."
                 , rejects "zero weight"
                    (Fuzz.frequency [ ( 0, Fuzz.unit ) ])
                    "Fuzz.frequency: You must provide at least one frequency pair with weight greater than 0."
                 , doesNotReject "zero and non-zero weight"
                    (Fuzz.frequency
                        [ ( 0, Fuzz.unit )
                        , ( 1, Fuzz.unit )
                        ]
                    )
                 , rejects "negative weight"
                    (Fuzz.frequency [ ( -1, Fuzz.unit ) ])
                    "Fuzz.frequency: No frequency weights can be less than 0."
                 ]
                )
            , describe "list"
                [ canGenerateWith { runs = 1000 } [] (Fuzz.list Fuzz.unit)
                , canGenerateSatisfying "non-empty list"
                    (Fuzz.list Fuzz.unit)
                    (not << List.isEmpty)
                , simplifiesTowards "simplest" [] (Fuzz.list Fuzz.int) fullySimplify
                , simplifiesTowardsWith { runs = 2000 } "next simplest" [ 0 ] (Fuzz.list Fuzz.int) (\x -> x == [])
                , simplifiesTowards "All lists are sorted"
                    [ 0, -1 ]
                    (Fuzz.list Fuzz.int)
                    (\list -> list == List.sort list)
                ]
            , describe "listOfLength"
                [ passes "always length 3"
                    (Fuzz.listOfLength 3 Fuzz.unit)
                    (\list -> List.length list == 3)
                , passes "negative length -> empty list"
                    (Fuzz.listOfLength -3 Fuzz.unit)
                    List.isEmpty
                , simplifiesTowards "simplest" [ 0, 0, 0 ] (Fuzz.listOfLength 3 Fuzz.int) fullySimplify
                , simplifiesTowards "next simplest" [ 0, 0, 1 ] (Fuzz.listOfLength 3 Fuzz.int) (\x -> x == [ 0, 0, 0 ])
                ]
            , describe "listOfLengthBetween"
                [ passes "always in range"
                    (Fuzz.listOfLengthBetween 2 5 Fuzz.unit)
                    (\list ->
                        let
                            length =
                                List.length list
                        in
                        length >= 2 && length <= 5
                    )
                , simplifiesTowards "simplest" [ 0, 0 ] (Fuzz.listOfLengthBetween 2 5 Fuzz.int) fullySimplify
                , simplifiesTowards "next simplest" [ 0, 1 ] (Fuzz.listOfLengthBetween 2 5 Fuzz.int) (\x -> x == [ 0, 0 ])
                , doesNotReject "swapped arguments" (Fuzz.listOfLengthBetween 5 -5 Fuzz.unit)
                ]
            , describe "array"
                [ canGenerateWith { runs = 1000 } Array.empty (Fuzz.array Fuzz.unit)
                , canGenerateSatisfying "non-empty array"
                    (Fuzz.array Fuzz.unit)
                    (not << Array.isEmpty)
                , simplifiesTowards "simplest" Array.empty (Fuzz.array Fuzz.int) fullySimplify
                , simplifiesTowards "next simplest" (Array.fromList [ 0 ]) (Fuzz.array Fuzz.int) (\x -> Array.isEmpty x)
                ]
            , describe "andThen"
                [ passes "integer defined by another integer"
                    (Fuzz.intRange 0 5
                        |> Fuzz.andThen
                            (\m ->
                                Fuzz.pair
                                    (Fuzz.constant m)
                                    (Fuzz.intRange m (m + 10))
                            )
                    )
                    (\( m, n ) -> m <= n && n <= m + 10)
                , simplifiesTowards "list of booleans"
                    [ False, False, False ]
                    (Fuzz.bool
                        |> Fuzz.andThen (\bool -> Fuzz.list (Fuzz.constant bool))
                    )
                    (\list -> List.length list < 3)
                , simplifiesTowards "LHS: constant int"
                    0
                    (Fuzz.int
                        |> Fuzz.andThen Fuzz.constant
                    )
                    fullySimplify
                , simplifiesTowards "RHS: list of ints"
                    []
                    (Fuzz.int
                        |> Fuzz.andThen (\x -> Fuzz.list (Fuzz.constant x))
                    )
                    fullySimplify
                , simplifiesTowardsWith { runs = 1000 }
                    "can ignore LHS"
                    [ 0, 0, 0 ]
                    (Fuzz.int
                        |> Fuzz.andThen (\_ -> Fuzz.list Fuzz.int)
                    )
                    (\x -> List.length x < 3)
                , simplifiesTowards "both LHS and RHS at the same time"
                    [ 0, 0, 0 ]
                    (Fuzz.int
                        |> Fuzz.andThen (\x -> Fuzz.list (Fuzz.constant x))
                    )
                    (\x -> List.length x < 3)
                , simplifiesTowards
                    "rectangles"
                    [ [ 'a', 'b' ] ]
                    (Fuzz.intRange 0 4
                        |> Fuzz.andThen
                            (\len ->
                                Fuzz.list
                                    (Fuzz.listOfLength len
                                        (Fuzz.oneOfValues [ 'a', 'b' ])
                                    )
                            )
                    )
                    (\lists -> List.head lists /= Just [ 'a', 'b' ])
                ]
            , describe "weightedBool"
                [ canGenerate False (Fuzz.weightedBool 0.5)
                , canGenerate True (Fuzz.weightedBool 0.5)
                , passes "0 = always False"
                    (Fuzz.weightedBool 0)
                    (\bool -> bool == False)
                , passes "1 = always True"
                    (Fuzz.weightedBool 1)
                    (\bool -> bool == True)
                , passes "<0 clamps to 0"
                    (Fuzz.weightedBool -0.5)
                    (\bool -> bool == False)
                , passes ">1 clamps to 1"
                    (Fuzz.weightedBool 1.5)
                    (\bool -> bool == True)
                , simplifiesTowards "simplest" False (Fuzz.weightedBool 0.5) fullySimplify
                , simplifiesTowards "non-False" True (Fuzz.weightedBool 0.5) (\x -> x == False)
                ]
            , describe "float"
                [ cannotGenerateSatisfying "NaN" Fuzz.float isNaN
                , cannotGenerateSatisfying "Infinity" Fuzz.float isInfinite
                , canGenerateSatisfying "negative" Fuzz.float (\f -> f < 0)
                , canGenerateSatisfying "positive" Fuzz.float (\f -> f > 0)
                , simplifiesTowards "simplest" 0 Fuzz.float fullySimplify
                , simplifiesTowards "next simplest" 1 Fuzz.float (\x -> x == 0)
                , simplifiesTowards "simplest non-int"
                    -- TODO ~janiczek: hmmm... should prefer simple fractions first...
                    0.5
                    Fuzz.float
                    (\x -> String.toInt (String.fromFloat x) /= Nothing)
                , simplifiesTowards "simplest negative" -1 Fuzz.float (\x -> x >= 0)
                ]
            , describe "floatRange"
                [ passes "Smaller range"
                    (Fuzz.floatRange -5 5)
                    (\n -> n >= -5 && n <= 5)
                , cannotGenerateSatisfying "Smaller range"
                    (Fuzz.floatRange -5 5)
                    (\n -> n < -5 && n > 5)
                , simplifiesTowards "(-,+) simplest" 0 (Fuzz.floatRange -5 5) fullySimplify
                , simplifiesTowards "(-,+) non-zero" 1.1102230246251567e-15 (Fuzz.floatRange -5 5) (\n -> n == 0)
                , simplifiesTowards "(0,+) simplest" 0 (Fuzz.floatRange 0 5) fullySimplify
                , simplifiesTowards "(0,+) non-zero" 1.1102230246251567e-15 (Fuzz.floatRange 0 5) (\n -> n == 0)
                , simplifiesTowards "(+,+) simplest" 1 (Fuzz.floatRange 1 5) fullySimplify
                , simplifiesTowards "(+,+) non-low" 1.0000000000000009 (Fuzz.floatRange 1 5) (\n -> n == 1)
                , simplifiesTowards "(-,0) simplest" 0 (Fuzz.floatRange -5 0) fullySimplify
                , simplifiesTowards "(-,0) non-zero" -1.1102230246251567e-15 (Fuzz.floatRange -5 0) (\n -> n == 0)
                , simplifiesTowards "(-,-) simplest" -1 (Fuzz.floatRange -5 -1) fullySimplify
                , simplifiesTowards "(-,-) non-high" -1.0000000000000009 (Fuzz.floatRange -5 -1) (\n -> n == -1)
                , rejects "min > max"
                    (Fuzz.floatRange 5 -5)
                    "Fuzz.floatRange was given a lower bound of 5 which is greater than the upper bound, -5."
                ]
            , describe "invalid"
                [ rejects "with user's message" (Fuzz.invalid "boo") "boo"
                ]
            , todo "filter"
            ]
        ]


{-| An user test function that makes the simplifier simplify the value fully.
-}
fullySimplify : a -> Bool
fullySimplify _ =
    False
