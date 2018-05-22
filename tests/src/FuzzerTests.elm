module FuzzerTests exposing (fuzzerTests)

import Expect
import Fuzz exposing (..)
import Helpers exposing (..)
import Lazy.List
import Random
import RoseTree
import Test exposing (..)
import Test.Internal as Internal
import Test.Runner


die : Fuzzer Int
die =
    Fuzz.intRange 1 6


fuzzerTests : Test
fuzzerTests =
    describe "Fuzzer methods that use Debug.crash don't call it"
        [ describe "FuzzN (uses tupleN) testing string length properties"
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
            [ fuzz randomSeedFuzzer "the same value is generated with and without shrinking" <|
                \seed ->
                    let
                        step gen =
                            Random.step gen seed

                        aFuzzer =
                            tuple3
                                ( tuple ( list int, array float )
                                , tuple
                                    ( maybe bool
                                    , result unit char
                                    )
                                , tuple
                                    ( tuple3
                                        ( percentage
                                        , map2 (+) int int
                                        , frequency [ ( 1, constant True ), ( 3, constant False ) ]
                                        )
                                    , tuple3 ( intRange 0 100, floatRange -51 pi, map abs int )
                                    )
                                )

                        valNoShrink =
                            aFuzzer |> Result.map (Random.map RoseTree.root >> step >> Tuple.first)

                        valWithShrink =
                            aFuzzer |> Result.map (step >> Tuple.first >> RoseTree.root)
                    in
                    Expect.equal valNoShrink valWithShrink
            , shrinkingTests
            , manualFuzzerTests
            ]
        ]


shrinkingTests : Test
shrinkingTests =
    testShrinking <|
        describe "tests that fail intentionally to test shrinking"
            [ fuzz2 int int "Every pair of ints has a zero" <|
                \i j ->
                    (i == 0)
                        || (j == 0)
                        |> Expect.true "(1,1)"
            , fuzz3 int int int "Every triple of ints has a zero" <|
                \i j k ->
                    (i == 0)
                        || (j == 0)
                        || (k == 0)
                        |> Expect.true "(1,1,1)"
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
                    checkPair aList |> Expect.true "[1,0]|[0,-1]"
            ]


type alias ShrinkResult a =
    Maybe ( a, Test.Runner.Shrinkable a )


manualFuzzerTests : Test
manualFuzzerTests =
    describe "Test.Runner.{fuzz, shrink}"
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
                        Random.step (Test.Runner.fuzz fuzzer) seed
                            |> Tuple.first
                            |> Just

                    unfold acc maybePair =
                        case maybePair of
                            Just ( valN, shrinkN ) ->
                                if failsTest valN then
                                    unfold (valN :: acc) (Test.Runner.shrink False shrinkN)

                                else
                                    unfold acc (Test.Runner.shrink True shrinkN)

                            Nothing ->
                                acc
                in
                unfold [] pair
                    |> Expect.all
                        [ List.all failsTest >> Expect.true "Not all elements were even"
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
                        Random.step (Test.Runner.fuzz fuzzer) seed
                            |> Tuple.first
                            |> Just

                    unfold acc maybePair =
                        case maybePair of
                            Just ( valN, shrinkN ) ->
                                if failsTest valN then
                                    unfold (valN :: acc) (Test.Runner.shrink False shrinkN)

                                else
                                    unfold acc (Test.Runner.shrink True shrinkN)

                            Nothing ->
                                acc
                in
                unfold [] pair
                    |> Expect.all
                        [ List.all failsTest >> Expect.true "Not all contained the letter e"
                        , List.head >> Expect.equal (Just "e")
                        , List.reverse >> List.head >> Expect.equal (Maybe.map Tuple.first pair)
                        ]
        , fuzz randomSeedFuzzer "List shrinker finds the smallest counter example" <|
            \seed ->
                let
                    fuzzer : Fuzzer (List Int)
                    fuzzer =
                        Fuzz.list Fuzz.int

                    allEven : List Int -> Bool
                    allEven xs =
                        List.all (\x -> (x |> modBy 2) == 0) xs

                    initialShrink : ShrinkResult (List Int)
                    initialShrink =
                        Random.step (Test.Runner.fuzz fuzzer) seed
                            |> Tuple.first
                            |> Just

                    shrink : Maybe (List Int) -> ShrinkResult (List Int) -> Maybe (List Int)
                    shrink shrunken lastShrink =
                        case lastShrink of
                            Just ( valN, shrinkN ) ->
                                shrink
                                    (if allEven valN then
                                        shrunken

                                     else
                                        Just valN
                                    )
                                    (Test.Runner.shrink (allEven valN) shrinkN)

                            Nothing ->
                                shrunken
                in
                case shrink Nothing initialShrink of
                    Just shrunken ->
                        Expect.equal [ 1 ] shrunken

                    Nothing ->
                        Expect.pass
        ]
