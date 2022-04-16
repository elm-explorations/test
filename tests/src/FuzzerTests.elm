module FuzzerTests exposing (fuzzerTests)

import Array
import Expect exposing (Expectation)
import Fuzz exposing (..)
import Helpers exposing (..)
import Random
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
        , testRunnerModuleTests
        , fuzzerSpecificationTests
        ]


testRunnerModuleTests : Test
testRunnerModuleTests =
    describe "Test.Runner.{fuzz,simplify}"
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
                        Random.step (Test.Runner.fuzz fuzzer) seed
                            |> Tuple.first
                            |> Result.toMaybe

                    finalValue : Maybe Int
                    finalValue =
                        (case pair |> Maybe.andThen (Test.Runner.simplify getExpectation) of
                            Nothing ->
                                -- couldn't simplify (generated the minimal value straight away)
                                pair

                            Just fullySimplified ->
                                Just fullySimplified
                        )
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
                        Random.step (Test.Runner.fuzz fuzzer) seed
                            |> Tuple.first
                            |> Result.toMaybe

                    finalValue : Maybe String
                    finalValue =
                        (case pair |> Maybe.andThen (Test.Runner.simplify getExpectation) of
                            Nothing ->
                                -- couldn't simplify (generated the minimal value straight away)
                                pair

                            Just fullySimplified ->
                                Just fullySimplified
                        )
                            |> Maybe.map Tuple.first
                in
                finalValue
                    |> Maybe.map (Expect.equal "e")
                    |> Maybe.withDefault (Expect.fail "no final value")
        ]


fuzzerSpecificationTests : Test
fuzzerSpecificationTests =
    Test.describe "Fuzz.*"
        [ describe "Tough examples"
            [ simplifiesTowardsWith { runs = 5000 }
                "redistributed additive pair"
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
                [ 1001 ]
                (Fuzz.intRange 0 10
                    |> Fuzz.andThen
                        (\length ->
                            let
                                go : Int -> List Int -> Fuzzer (List Int)
                                go left acc =
                                    if left <= 0 then
                                        Fuzz.constant (List.reverse acc)

                                    else
                                        Fuzz.intRange 0 10000
                                            |> Fuzz.andThen (\item -> go (left - 1) (item :: acc))
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
                [ describe "lo >= 0 (all non-negative)"
                    [ canGenerateWith { runs = 3000 } 1 (Fuzz.intRange 1 20)
                    , canGenerateWith { runs = 3000 } 10 (Fuzz.intRange 1 20)
                    , canGenerateWith { runs = 3000 } 20 (Fuzz.intRange 1 20)
                    ]
                , describe "hi <= 0 (all non-positive)"
                    [ canGenerateWith { runs = 3000 } -20 (Fuzz.intRange -20 -1)
                    , canGenerateWith { runs = 3000 } -10 (Fuzz.intRange -20 -1)
                    , canGenerateWith { runs = 3000 } -1 (Fuzz.intRange -20 -1)
                    ]
                , describe "mixed case (lo < 0 && hi > 0)"
                    [ canGenerateWith { runs = 3000 } -20 (Fuzz.intRange -20 20)
                    , canGenerateWith { runs = 3000 } -10 (Fuzz.intRange -20 20)
                    , canGenerateWith { runs = 3000 } 0 (Fuzz.intRange -20 20)
                    , canGenerateWith { runs = 3000 } 10 (Fuzz.intRange -20 20)
                    , canGenerateWith { runs = 3000 } 20 (Fuzz.intRange -20 20)
                    ]
                , passes "Smaller range"
                    (Fuzz.intRange -5 5)
                    (\n -> n >= -5 && n <= 5)
                , canGenerate -5 (Fuzz.intRange -5 5)
                , canGenerate 0 (Fuzz.intRange -5 5)
                , canGenerate 5 (Fuzz.intRange -5 5)
                , cannotGenerateSatisfying "Smaller range"
                    (Fuzz.intRange -5 5)
                    (\n -> n < -5 && n > 5)
                , passes "min > max -> automatic swap"
                    (Fuzz.intRange 5 -5)
                    (\n -> n >= -5 && n <= 5)
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
                , canGenerateSatisfying "upper 50% of 0xFFFFFFFF"
                    (Fuzz.intRange 0 0xFFFFFFFF)
                    (\n -> n >= 0x7FFFFFFF)
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
            , describe "intAtLeast"
                [ cannotGenerateSatisfying "below the limit" (Fuzz.intAtLeast 5) (\n -> n < 5)
                , canGenerateSatisfying "above the limit" (Fuzz.intAtLeast 5) (\n -> n > 5)
                , cannotGenerateSatisfying "above 2^32 - 1" (Fuzz.intAtLeast 5) (\n -> n > 2 ^ 32 - 1)
                ]
            , describe "intAtMost"
                [ canGenerateSatisfying "below the limit" (Fuzz.intAtMost 5) (\n -> n < 5)
                , cannotGenerateSatisfying "above the limit" (Fuzz.intAtMost 5) (\n -> n > 5)
                , cannotGenerateSatisfying "below -(2^32 - 1)" (Fuzz.intAtMost 5) (\n -> n < -(2 ^ 32 - 1))
                ]
            , describe "percentage"
                [ passes "Range 0..1"
                    Fuzz.percentage
                    (\p -> p >= 0 && p < 1)
                , canGenerate 0 Fuzz.percentage
                , canGenerate (1 - 2 ^ -52) Fuzz.percentage -- just below 1
                , cannotGenerate 1 Fuzz.percentage
                , cannotGenerateSatisfying "any Infinity" Fuzz.percentage isInfinite
                , cannotGenerateSatisfying "NaN" Fuzz.percentage isNaN
                , simplifiesTowards "simplest = lower limit = zero" 0 Fuzz.percentage fullySimplify
                , simplifiesTowards "non-zero = upper limit = just below 1" (1 - 2 ^ -52) Fuzz.percentage (\v -> v == 0)
                , simplifiesTowards "non-zero non-one, doesn't shrink nicely"
                    0.25000000000000006
                    Fuzz.percentage
                    (\v -> v == 1 - 2 ^ -52 || v <= 0.25)
                ]
            , describe "asciiChar"
                [ passes "Range 32..126"
                    Fuzz.asciiChar
                    (\char ->
                        let
                            code =
                                Char.toCode char
                        in
                        code >= 32 && code <= 126
                    )
                , simplifiesTowards "simplest" ' ' Fuzz.asciiChar fullySimplify
                , simplifiesTowards "next simplest" '!' Fuzz.asciiChar (\c -> c == ' ')
                , simplifiesTowards "above A" 'B' Fuzz.asciiChar (\c -> Char.toCode c <= Char.toCode 'A')
                , cannotGenerate '🔥' Fuzz.asciiChar
                ]
            , describe "char"
                [ canGenerateSatisfying "Alpha" Fuzz.char Char.isAlpha
                , canGenerateSatisfying "Digit" Fuzz.char Char.isDigit
                , canGenerate '\t' Fuzz.char
                , canGenerate (Char.fromCode 0x0303) Fuzz.char
                , canGenerate '🔥' Fuzz.char
                , canGenerateSatisfyingWith { runs = 3000 }
                    "Unicode above the special cases"
                    Fuzz.char
                    (\c -> Char.toCode c > 0x0001F525)
                , simplifiesTowards "simplest" ' ' Fuzz.char fullySimplify
                , simplifiesTowardsMany "next simplest"
                    -- lowest representatives of all the special-case groups
                    [ '!'
                    , '\t'
                    , Char.fromCode 0x0302
                    , '🌈'
                    ]
                    Fuzz.char
                    (\c -> c == ' ')
                ]
            , describe "asciiString"
                [ canGenerate "" Fuzz.asciiString
                , canGenerateSatisfying "non-empty string"
                    Fuzz.asciiString
                    (not << String.isEmpty)
                , simplifiesTowards "simplest" "" Fuzz.asciiString fullySimplify
                , simplifiesTowards "next simplest" " " Fuzz.asciiString (\x -> x == "")
                , simplifiesTowards "alpha"
                    "A"
                    Fuzz.asciiString
                    (\x -> x == "" || String.all (not << Char.isAlpha) x)
                , canGenerateSatisfying "Alpha" Fuzz.asciiString (String.any Char.isAlpha)
                , canGenerateSatisfying "Digit" Fuzz.asciiString (String.any Char.isDigit)
                , canGenerateSatisfying "whitespace" Fuzz.asciiString (String.contains " ")
                , cannotGenerateSatisfying "combining diacritical marks"
                    Fuzz.asciiString
                    -- going a roundabout way about this to keep Vim from being confused about unclosed strings
                    (String.contains (String.fromChar (Char.fromCode 0x0303)))
                , cannotGenerateSatisfying "emoji" Fuzz.asciiString (String.contains "🔥")
                , cannotGenerateSatisfying "non-printables below Space"
                    Fuzz.asciiString
                    (String.any (\c -> Char.toCode c < 32))
                , cannotGenerateSatisfying "anything above ~"
                    Fuzz.asciiString
                    (String.any (\c -> Char.toCode c > 126))
                ]
            , describe "string"
                [ canGenerate "" Fuzz.string
                , canGenerateSatisfying "non-empty string"
                    Fuzz.string
                    (not << String.isEmpty)
                , simplifiesTowards "simplest" "" Fuzz.string fullySimplify
                , simplifiesTowards "next simplest" " " Fuzz.string (\x -> x == "")
                , simplifiesTowards "alpha"
                    "A"
                    Fuzz.string
                    (\x -> x == "" || String.all (not << Char.isAlpha) x)
                , canGenerateSatisfying "Alpha" Fuzz.string (String.any Char.isAlpha)
                , canGenerateSatisfying "Digit" Fuzz.string (String.any Char.isDigit)
                , canGenerateSatisfying "whitespace" Fuzz.string (String.contains "\t")
                , canGenerateSatisfying "combining diacritical marks"
                    Fuzz.string
                    -- going a roundabout way about this to keep Vim from being confused about unclosed strings
                    (String.contains (String.fromChar (Char.fromCode 0x0303)))
                , canGenerateSatisfying "emoji" Fuzz.string (String.contains "🔥")
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
                , simplifiesTowardsMany "All lists are sorted"
                    [ [ 1, 0 ]
                    , [ 0, -1 ]
                    ]
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
            , describe "niceFloat"
                [ cannotGenerateSatisfying "NaN" Fuzz.niceFloat isNaN
                , cannotGenerateSatisfying "Infinity" Fuzz.niceFloat isInfinite
                , canGenerateSatisfying "negative" Fuzz.niceFloat (\f -> f < 0)
                , canGenerateSatisfying "positive" Fuzz.niceFloat (\f -> f > 0)
                , simplifiesTowards "simplest" 0 Fuzz.niceFloat fullySimplify
                , simplifiesTowards "next simplest" 1 Fuzz.niceFloat (\x -> x == 0)
                , simplifiesTowards "simplest outside 0 or 1" -1 Fuzz.niceFloat (\x -> x == 0 || x == 1)
                , simplifiesTowards "simplest non-int"
                    1.5
                    Fuzz.niceFloat
                    (\x -> String.toInt (String.fromFloat x) /= Nothing)
                , simplifiesTowards "simplest negative" -1 Fuzz.niceFloat (\x -> x >= 0)
                ]
            , describe "float"
                [ canGenerateSatisfying "NaN" Fuzz.float isNaN
                , canGenerateSatisfying "Infinity" Fuzz.float isInfinite
                , canGenerateSatisfying "negative" Fuzz.float (\f -> f < 0)
                , canGenerateSatisfying "positive" Fuzz.float (\f -> f > 0)
                , simplifiesTowardsMany "simplest"
                    [ 0
                    , -1 / 0
                    , 1 / 0
                    , 0 / 0
                    ]
                    Fuzz.float
                    fullySimplify
                , simplifiesTowardsMany "next simplest"
                    [ 1 / 0
                    , -1 / 0
                    , 0 / 0
                    , 1
                    ]
                    Fuzz.float
                    (\x -> x == 0)
                , simplifiesTowards "simplest outside 0, infinities and NaN" 1 Fuzz.float (\x -> x == 0 || isInfinite x || isNaN x)
                , simplifiesTowards "simplest non-int"
                    1.5
                    Fuzz.float
                    (\x -> isInfinite x || isNaN x || String.toInt (String.fromFloat x) /= Nothing)
                , simplifiesTowardsMany "simplest negative"
                    [ -1
                    , -1 / 0
                    ]
                    Fuzz.float
                    (\x -> x >= 0)
                ]
            , describe "floatAtLeast"
                [ describe "n <= 0"
                    [ cannotGenerateSatisfying "below the limit" (Fuzz.floatAtLeast -5) (\n -> n < -5)
                    , canGenerateSatisfying "above the limit" (Fuzz.floatAtLeast -5) (\n -> n > -5)
                    , canGenerate -5 (Fuzz.floatAtLeast -5)
                    , canGenerate 0 (Fuzz.floatAtLeast -5)
                    , canGenerate (1 / 0) (Fuzz.floatAtLeast -5)
                    , cannotGenerate (0 / 0) (Fuzz.floatAtLeast -5)
                    ]
                , describe "n > 0"
                    [ cannotGenerateSatisfying "below the limit" (Fuzz.floatAtLeast 5) (\n -> n < 5)
                    , canGenerateSatisfying "above the limit" (Fuzz.floatAtLeast 5) (\n -> n > 5)
                    , canGenerate 5 (Fuzz.floatAtLeast 5)
                    , canGenerate (1 / 0) (Fuzz.floatAtLeast 5)
                    , cannotGenerate (0 / 0) (Fuzz.floatAtLeast 5)
                    ]
                ]
            , describe "floatAtMost"
                [ describe "n < 0"
                    [ canGenerateSatisfying "below the limit" (Fuzz.floatAtMost -5) (\n -> n < -5)
                    , cannotGenerateSatisfying "above the limit" (Fuzz.floatAtMost -5) (\n -> n > -5)
                    , canGenerate -5 (Fuzz.floatAtMost -5)
                    , canGenerate (-1 / 0) (Fuzz.floatAtMost -5)
                    , cannotGenerate (0 / 0) (Fuzz.floatAtMost -5)
                    ]
                , describe "n >= 0"
                    [ canGenerateSatisfying "below the limit" (Fuzz.floatAtMost 5) (\n -> n < 5)
                    , cannotGenerateSatisfying "above the limit" (Fuzz.floatAtMost 5) (\n -> n > 5)
                    , canGenerate 5 (Fuzz.floatAtMost 5)
                    , canGenerate 0 (Fuzz.floatAtMost 5)
                    , canGenerate (-1 / 0) (Fuzz.floatAtMost 5)
                    , cannotGenerate (0 / 0) (Fuzz.floatAtMost 5)
                    ]
                ]
            , describe "floatRange"
                [ passes "Smaller range"
                    (Fuzz.floatRange -5 5)
                    (\n -> n >= -5 && n <= 5)
                , cannotGenerateSatisfying "Smaller range"
                    (Fuzz.floatRange -5 5)
                    (\n -> n < -5 && n > 5)
                , simplifiesTowards "(-,+) simplest" 0 (Fuzz.floatRange -5 5) fullySimplify
                , simplifiesTowards "(-,+) non-zero" -5 (Fuzz.floatRange -5 5) (\n -> n == 0)
                , simplifiesTowards "(-,+) non-zero non-low" 5 (Fuzz.floatRange -5 5) (\n -> n == 0 || n == -5)
                , simplifiesTowards "(0,+) simplest" 0 (Fuzz.floatRange 0 5) fullySimplify
                , simplifiesTowards "(0,+) non-zero" 1.1102230246251567e-15 (Fuzz.floatRange 0 5) (\n -> n == 0)
                , simplifiesTowards "(+,+) simplest" 1 (Fuzz.floatRange 1 5) fullySimplify
                , simplifiesTowards "(+,+) non-low" 5 (Fuzz.floatRange 1 5) (\n -> n == 1)
                , simplifiesTowards "(-,0) simplest" -5 (Fuzz.floatRange -5 0) fullySimplify
                , simplifiesTowards "(-,0) non-low" 0 (Fuzz.floatRange -5 0) (\n -> n == -5)
                , simplifiesTowards "(-,-) simplest" -5 (Fuzz.floatRange -5 -1) fullySimplify
                , simplifiesTowards "(-,-) non-low" -1 (Fuzz.floatRange -5 -1) (\n -> n == -5)
                , passes "min > max -> automatic swap"
                    (Fuzz.floatRange 5 -5)
                    (\n -> n >= -5 && n <= 5)
                ]
            , describe "invalid"
                [ rejects "with user's message" (Fuzz.invalid "boo") "boo"
                ]
            , describe "filter" <|
                let
                    {- We're using a more complicated (at least, naming and
                       readability wise) example than isEven to make it less
                       likely to randomly hit 15 even numbers in a row...
                       (that _has_ happened...)
                    -}
                    isDivBy5 : Int -> Bool
                    isDivBy5 n =
                        modBy 5 n == 0

                    intsNotDivBy5 : Fuzzer Int
                    intsNotDivBy5 =
                        Fuzz.int
                            |> Fuzz.filter (not << isDivBy5)
                in
                [ rejects "impossible predicate (always False)"
                    (Fuzz.int |> Fuzz.filter (\_ -> False))
                    "Too many values were filtered out"
                , passes "trivial predicate (always True) doesn't reject"
                    (Fuzz.int |> Fuzz.filter (\_ -> True))
                    (\_ -> True)
                , canGenerateSatisfyingWith { runs = 5000 } "not divisible by 5" intsNotDivBy5 (not << isDivBy5)
                , cannotGenerateSatisfyingWith { runs = 5000 } "divisible by 5" intsNotDivBy5 isDivBy5
                ]
            ]
        ]


{-| An user test function that makes the simplifier simplify the value fully.
-}
fullySimplify : a -> Bool
fullySimplify _ =
    False
