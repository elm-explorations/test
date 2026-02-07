module RandomRunTests exposing (all)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import RandomRun exposing (Chunk, RandomRun)
import Test exposing (Test)


all : Test
all =
    Test.describe "RandomRun"
        [ negativeValuesTests
        , Test.describe "Properties"
            [ isEmptyTests
            , lengthTests
            , compareTests
            , equalTests
            , toListTests
            , getTests
            , setTests
            , updateTests
            , appendTests
            , replaceTests
            , nextChoiceTests
            , deleteChunkTests
            , replaceChunkWithZeroTests
            , sortChunkTests
            , swapChunksTests
            , swapIfOutOfOrderTests
            ]
        ]


negativeValuesTests : Test
negativeValuesTests =
    Test.describe "behaviour of negative values"
        [ Test.test "append -1" <|
            \() ->
                RandomRun.empty
                    |> RandomRun.append -1
                    |> RandomRun.get 0
                    |> Expect.equal (Just 0)
        , Test.test "set -1" <|
            \() ->
                RandomRun.empty
                    |> RandomRun.append 0
                    |> RandomRun.set 0 -1
                    |> RandomRun.get 0
                    |> Expect.equal (Just 0)
        , Test.test "update (\\_ -> -1)" <|
            \() ->
                RandomRun.empty
                    |> RandomRun.append 0
                    |> RandomRun.update 0 (\_ -> -1)
                    |> RandomRun.get 0
                    |> Expect.equal (Just 0)
        , Test.test "replace [(0,-1)]" <|
            \() ->
                RandomRun.empty
                    |> RandomRun.append 0
                    |> RandomRun.replace [ ( 0, -1 ) ]
                    |> RandomRun.get 0
                    |> Expect.equal (Just 0)
        ]


isEmptyTests : Test
isEmptyTests =
    Test.describe "isEmpty"
        [ Test.test "isEmpty empty" <|
            \_ ->
                RandomRun.isEmpty RandomRun.empty
                    |> Expect.equal True
                    |> Expect.onFail "empty RandomRun should be reported as empty (isEmpty)"
        ]


lengthTests : Test
lengthTests =
    Test.describe "length"
        [ Test.test "length empty == 0" <|
            \_ ->
                RandomRun.length RandomRun.empty
                    |> Expect.equal 0
        , Test.fuzz randomRunFuzzer "isEmpty r ⇔ length r == 0" <|
            \r ->
                Expect.equal
                    (RandomRun.isEmpty r)
                    (RandomRun.length r == 0)
        ]


compareTests : Test
compareTests =
    Test.describe "compare"
        [ Test.test "compare empty empty == EQ" <|
            \_ ->
                RandomRun.compare RandomRun.empty RandomRun.empty
                    |> Expect.equal EQ
        , Test.fuzz randomRunFuzzer "compare r r == EQ" <|
            \r ->
                RandomRun.compare r r
                    |> Expect.equal EQ
        , Test.fuzz randomRunFuzzer "if isEmpty r then compare r empty == EQ else compare r empty == GT" <|
            \r ->
                let
                    compareResult =
                        RandomRun.compare r RandomRun.empty
                in
                if RandomRun.isEmpty r then
                    compareResult |> Expect.equal EQ

                else
                    compareResult |> Expect.equal GT
        , Test.fuzz2 randomRunFuzzer randomRunFuzzer "isEmpty r1 => compare r1 r2 /= GT" <|
            \r1 r2 ->
                RandomRun.isEmpty r1
                    |> implies (RandomRun.compare r1 r2 |> Expect.notEqual GT)
        , Test.fuzz2 randomRunFuzzer randomRunFuzzer "length ordering vs compare r1 r2" <|
            \r1 r2 ->
                case compare (RandomRun.length r1) (RandomRun.length r2) of
                    LT ->
                        RandomRun.compare r1 r2
                            |> Expect.equal LT

                    GT ->
                        RandomRun.compare r1 r2
                            |> Expect.equal GT

                    EQ ->
                        Expect.pass
        ]


equalTests : Test
equalTests =
    Test.describe "equal"
        [ Test.fuzz randomRunFuzzer "isEmpty r ⇔ equal r empty" <|
            \r ->
                Expect.equal
                    (RandomRun.isEmpty r)
                    (RandomRun.equal r RandomRun.empty)
        , Test.fuzz2 randomRunFuzzer randomRunFuzzer "length r1 != length r2 => not (equal r1 r2)" <|
            \r1 r2 ->
                (RandomRun.length r1 /= RandomRun.length r2)
                    |> implies
                        (RandomRun.equal r1 r2
                            |> Expect.equal False
                            |> Expect.onFail "runs with different lengths must not be equal"
                        )
        , Test.fuzz2 randomRunFuzzer randomRunFuzzer "equal r1 r2 ⇔ compare r1 r2 == EQ" <|
            \r1 r2 ->
                Expect.equal
                    (RandomRun.equal r1 r2)
                    (RandomRun.compare r1 r2 == EQ)
        ]


toListTests : Test
toListTests =
    Test.describe "toList"
        [ Test.test "toList empty == []" <|
            \_ ->
                RandomRun.toList RandomRun.empty
                    |> Expect.equal []
        , Test.fuzz randomRunFuzzer "List.length (toList r) == length r" <|
            \r ->
                RandomRun.toList r
                    |> List.length
                    |> Expect.equal (RandomRun.length r)
        , Test.fuzz2 randomRunFuzzer randomRunFuzzer "equal r1 r2 ⇔ toList r1 == toList r2" <|
            \r1 r2 ->
                Expect.equal (RandomRun.equal r1 r2) (RandomRun.toList r1 == RandomRun.toList r2)
        ]


getTests : Test
getTests =
    Test.describe "get"
        [ Test.fuzz indexFuzzer "get i empty == Nothing" <|
            \i ->
                RandomRun.get i RandomRun.empty
                    |> Expect.equal Nothing
        , Test.fuzz2 indexFuzzer randomRunFuzzer "i >= length r ⇔ get i r == Nothing" <|
            \i r ->
                Expect.equal
                    (i >= RandomRun.length r)
                    (RandomRun.get i r == Nothing)
        , Test.fuzz3 indexFuzzer randomRunFuzzer randomRunFuzzer "equal r1 r2 => get i r1 == get i r2" <|
            \i r1 r2 ->
                RandomRun.equal r1 r2
                    |> implies
                        (Expect.equal
                            (RandomRun.get i r1)
                            (RandomRun.get i r2)
                        )
        , Test.fuzz2 indexFuzzer randomRunFuzzer "get i r == List.getAt i (toList r)" <|
            \i r ->
                Expect.equal
                    (RandomRun.get i r)
                    (listGetAt i (RandomRun.toList r))
        ]


setTests : Test
setTests =
    Test.describe "set"
        [ Test.fuzz2 indexFuzzer (Fuzz.intRange 0 Random.maxInt) "isEmpty (set i v empty)" <|
            \i v ->
                RandomRun.set i v RandomRun.empty
                    |> RandomRun.isEmpty
                    |> Expect.equal True
                    |> Expect.onFail "set on empty should still yield an empty run"
        , Test.fuzz3 indexFuzzer (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "length r == length (set i v r)" <|
            \i v r ->
                RandomRun.length r
                    |> Expect.equal (RandomRun.length (RandomRun.set i v r))
        , Test.fuzz3 indexFuzzer (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "i < length r => get i (set i v r) == Just v" <|
            \i v r ->
                (i < RandomRun.length r)
                    |> implies
                        (r
                            |> RandomRun.set i v
                            |> RandomRun.get i
                            |> Expect.equal (Just v)
                        )
        ]


updateTests : Test
updateTests =
    Test.describe "update"
        [ Test.fuzz2 indexFuzzer updateFnFuzzer "isEmpty (update i fn empty)" <|
            \i fn ->
                RandomRun.update i fn RandomRun.empty
                    |> RandomRun.isEmpty
                    |> Expect.equal True
                    |> Expect.onFail "update on empty should still yield an empty run"
        , Test.fuzz3 indexFuzzer updateFnFuzzer randomRunFuzzer "length r == length (update i fn r)" <|
            \i fn r ->
                r
                    |> RandomRun.update i fn
                    |> RandomRun.length
                    |> Expect.equal (RandomRun.length r)
        , Test.fuzz3 indexFuzzer (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "set i v r == update i (always v) r" <|
            \i v r ->
                Expect.equal
                    (r
                        |> RandomRun.set i v
                        |> RandomRun.toList
                    )
                    (r
                        |> RandomRun.update i (always v)
                        |> RandomRun.toList
                    )
        ]


appendTests : Test
appendTests =
    Test.describe "append"
        [ Test.fuzz (Fuzz.intRange 0 Random.maxInt) "toList (append v empty) == [v]" <|
            \v ->
                RandomRun.append v RandomRun.empty
                    |> RandomRun.toList
                    |> Expect.equal [ v ]
        , Test.fuzz2 (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "length (append v r) == length r + 1" <|
            \v r ->
                RandomRun.length (RandomRun.append v r)
                    |> Expect.equal (RandomRun.length r + 1)
        , Test.fuzz2 (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "compare r (append v r) == LT" <|
            \v r ->
                RandomRun.compare r (RandomRun.append v r)
                    |> Expect.equal LT
        , Test.fuzz2 (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "not (equal r (append v r))" <|
            \v r ->
                RandomRun.equal r (RandomRun.append v r)
                    |> Expect.equal False
                    |> Expect.onFail "appending a value must make the run different from the original"
        , Test.fuzz2 (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "List.head (List.reverse (toList (append v r))) == Just v" <|
            \v r ->
                RandomRun.append v r
                    |> RandomRun.toList
                    |> List.reverse
                    |> List.head
                    |> Expect.equal (Just v)
        , Test.fuzz2 (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "get (length r) (append v r) == Just v" <|
            \v r ->
                RandomRun.get (RandomRun.length r) (RandomRun.append v r)
                    |> Expect.equal (Just v)
        ]


replaceTests : Test
replaceTests =
    Test.describe "replace"
        [ Test.fuzz2 indexFuzzer (Fuzz.intRange 0 Random.maxInt) "isEmpty (replace [(i,v)] empty)" <|
            \i v ->
                RandomRun.empty
                    |> RandomRun.replace [ ( i, v ) ]
                    |> RandomRun.isEmpty
                    |> Expect.equal True
                    |> Expect.onFail "replace on empty should still yield an empty run"
        , Test.fuzz2 replaceListFuzzer randomRunFuzzer "length r == length (replace isvs r)" <|
            \isvs r ->
                RandomRun.length r
                    |> Expect.equal (RandomRun.length (RandomRun.replace isvs r))
        , Test.fuzz2 replaceListFuzzer randomRunFuzzer "replace isvs r == fold set one by one" <|
            \isvs r ->
                let
                    bySet =
                        List.foldl (\( i, v ) acc -> RandomRun.set i v acc) r isvs
                in
                RandomRun.toList (RandomRun.replace isvs r)
                    |> Expect.equal (RandomRun.toList bySet)
        , Test.fuzz2 replaceListFuzzer randomRunFuzzer "replace isvs r == fold replace one by one" <|
            \isvs r ->
                let
                    byReplace =
                        List.foldl (\( i, v ) acc -> RandomRun.replace [ ( i, v ) ] acc) r isvs
                in
                RandomRun.toList (RandomRun.replace isvs r)
                    |> Expect.equal (RandomRun.toList byReplace)
        ]


nextChoiceTests : Test
nextChoiceTests =
    Test.describe "nextChoice"
        [ Test.test "nextChoice empty == Nothing" <|
            \_ ->
                RandomRun.nextChoice RandomRun.empty
                    |> Expect.equal Nothing
        , Test.fuzz randomRunFuzzer "isEmpty r ⇔ nextChoice r == Nothing" <|
            \r ->
                Expect.equal
                    (RandomRun.isEmpty r)
                    (RandomRun.nextChoice r == Nothing)
        , Test.fuzz randomRunFuzzer "nextChoice r .next == List.head (toList r) && .rest == List.tail (toList r)" <|
            \r ->
                case
                    ( RandomRun.nextChoice r
                    , List.head (RandomRun.toList r)
                    , List.tail (RandomRun.toList r)
                    )
                of
                    ( Nothing, Nothing, _ ) ->
                        Expect.pass

                    ( Just ( next, restRun ), Just h, Just t ) ->
                        Expect.all
                            [ \_ -> Expect.equal next h
                            , \_ -> Expect.equal (RandomRun.toList restRun) t
                            ]
                            ()

                    _ ->
                        Expect.fail "nextChoice and list head/tail mismatch"
        , Test.fuzz randomRunFuzzer "nextChoice r .next == get 0 r" <|
            \r ->
                case RandomRun.nextChoice r of
                    Nothing ->
                        Expect.pass

                    Just ( next, _ ) ->
                        Expect.equal (RandomRun.get 0 r) (Just next)
        , Test.fuzz2 (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "not (isEmpty r) => (nextChoice (set 0 v r)).next == Just v" <|
            \v r ->
                not (RandomRun.isEmpty r)
                    |> implies
                        (r
                            |> RandomRun.set 0 v
                            |> RandomRun.nextChoice
                            |> Maybe.map Tuple.first
                            |> Expect.equal (Just v)
                        )
        , Test.fuzz (Fuzz.intRange 0 Random.maxInt) "(nextChoice (append v empty)).next == Just v" <|
            \v ->
                RandomRun.empty
                    |> RandomRun.append v
                    |> RandomRun.nextChoice
                    |> Maybe.map Tuple.first
                    |> Expect.equal (Just v)
        , Test.fuzz2 (Fuzz.intRange 0 Random.maxInt) randomRunFuzzer "not (isEmpty r) => (nextChoice (replace [(0,v)] r)).next == Just v" <|
            \v r ->
                not (RandomRun.isEmpty r)
                    |> implies
                        (r
                            |> RandomRun.replace [ ( 0, v ) ]
                            |> RandomRun.nextChoice
                            |> Maybe.map Tuple.first
                            |> Expect.equal (Just v)
                        )
        ]


deleteChunkTests : Test
deleteChunkTests =
    Test.describe "deleteChunk"
        [ Test.fuzz chunkFuzzer "isEmpty (deleteChunk c empty)" <|
            \c ->
                RandomRun.deleteChunk c RandomRun.empty
                    |> RandomRun.isEmpty
                    |> Expect.equal True
                    |> Expect.onFail "deleteChunk on empty should still yield an empty run"
        , Test.fuzz2 chunkFuzzer randomRunFuzzer "chunk in bounds and size > 0 => length (deleteChunk c r) < length r else length preserved" <|
            \c r ->
                let
                    len =
                        RandomRun.length r

                    inBounds =
                        c.startIndex + c.size <= len && c.startIndex >= 0

                    shrinks =
                        inBounds && c.size > 0

                    resultLen =
                        RandomRun.length (RandomRun.deleteChunk c r)
                in
                if shrinks then
                    (resultLen < len)
                        |> Expect.equal True
                        |> Expect.onFail "deleting an in-bounds chunk with size > 0 should shorten the run"

                else
                    Expect.equal len resultLen
        , Test.fuzz2 chunkFuzzer randomRunFuzzer "compare (deleteChunk c r) r /= GT" <|
            \c r ->
                RandomRun.compare
                    (RandomRun.deleteChunk c r)
                    r
                    |> Expect.notEqual GT
        ]


replaceChunkWithZeroTests : Test
replaceChunkWithZeroTests =
    Test.describe "replaceChunkWithZero"
        [ Test.fuzz chunkFuzzer "isEmpty (replaceChunkWithZero c empty)" <|
            \c ->
                RandomRun.empty
                    |> RandomRun.replaceChunkWithZero c
                    |> RandomRun.isEmpty
                    |> Expect.equal True
                    |> Expect.onFail "replaceChunkWithZero on empty should still yield an empty run"
        , Test.fuzz2 chunkFuzzer randomRunFuzzer "length r == length (replaceChunkWithZero c r)" <|
            \c r ->
                Expect.equal
                    (RandomRun.length r)
                    (RandomRun.length (RandomRun.replaceChunkWithZero c r))
        , Test.fuzz2 chunkFuzzer randomRunFuzzer "compare (replaceChunkWithZero c r) r /= GT" <|
            \c r ->
                RandomRun.compare
                    (RandomRun.replaceChunkWithZero c r)
                    r
                    |> Expect.notEqual GT
        , Test.fuzz2 chunkFuzzer randomRunFuzzer "for i in chunk range, get i (replaceChunkWithZero c r) == Nothing or Just 0" <|
            \c r ->
                let
                    result =
                        RandomRun.replaceChunkWithZero c r

                    len =
                        RandomRun.length result

                    endExclusive =
                        min (c.startIndex + c.size) len

                    indices =
                        if c.size <= 0 || c.startIndex + c.size > len then
                            []

                        else
                            List.range c.startIndex (endExclusive - 1)
                in
                indices
                    |> List.map
                        (\i ->
                            case RandomRun.get i result of
                                Nothing ->
                                    True

                                Just x ->
                                    x == 0
                        )
                    |> Expect.equal (indices |> List.map (\_ -> True))
        ]


sortChunkTests : Test
sortChunkTests =
    Test.describe "sortChunk"
        [ Test.fuzz chunkFuzzer "isEmpty (sortChunk c empty)" <|
            \c ->
                RandomRun.empty
                    |> RandomRun.sortChunk c
                    |> RandomRun.isEmpty
                    |> Expect.equal True
                    |> Expect.onFail "sortChunk on empty should still yield an empty run"
        , Test.fuzz2 chunkFuzzer randomRunFuzzer "length r == length (sortChunk c r)" <|
            \c r ->
                Expect.equal
                    (RandomRun.length r)
                    (RandomRun.length (RandomRun.sortChunk c r))
        , Test.fuzz2 chunkFuzzer randomRunFuzzer "compare (sortChunk c r) r /= GT" <|
            \c r ->
                RandomRun.compare
                    (RandomRun.sortChunk c r)
                    r
                    |> Expect.notEqual GT
        , Test.fuzz2 chunkFuzzer randomRunFuzzer "chunk range is non-decreasing in sortChunk c r" <|
            \c r ->
                let
                    len =
                        RandomRun.length r
                in
                if c.startIndex + c.size > len then
                    Expect.pass

                else
                    let
                        result =
                            RandomRun.sortChunk c r

                        end =
                            min (c.startIndex + c.size) len

                        indices =
                            List.range c.startIndex (end - 1)

                        values =
                            indices
                                |> List.filterMap (\i -> RandomRun.get i result)
                    in
                    values
                        |> Expect.equal (List.sort values)
        ]


swapChunksTests : Test
swapChunksTests =
    Test.describe "swapChunks"
        [ Test.fuzz2 chunkFuzzer chunkFuzzer "swapChunks c1 c2 empty == Nothing" <|
            \c1 c2 ->
                RandomRun.empty
                    |> RandomRun.swapChunks { leftChunk = c1, rightChunk = c2 }
                    |> Expect.equal Nothing
        , Test.fuzz3 chunkFuzzer chunkFuzzer randomRunFuzzer "when Just, length (swapChunks c1 c2 r) == length r" <|
            \c1 c2 r ->
                case RandomRun.swapChunks { leftChunk = c1, rightChunk = c2 } r of
                    Nothing ->
                        Expect.pass

                    Just newRun ->
                        RandomRun.length newRun
                            |> Expect.equal (RandomRun.length r)
        ]


swapIfOutOfOrderTests : Test
swapIfOutOfOrderTests =
    Test.describe "swapIfOutOfOrder"
        [ Test.fuzz2 indexFuzzer indexFuzzer "swapIfOutOfOrder i1 i2 empty == Nothing" <|
            \i1 i2 ->
                RandomRun.empty
                    |> RandomRun.swapIfOutOfOrder { leftIndex = i1, rightIndex = i2 }
                    |> Expect.equal Nothing
        , Test.fuzz3 indexFuzzer indexFuzzer randomRunFuzzer "when Just, length (swapIfOutOfOrder i1 i2 r) == length r" <|
            \i1 i2 r ->
                case RandomRun.swapIfOutOfOrder { leftIndex = i1, rightIndex = i2 } r of
                    Nothing ->
                        Expect.pass

                    Just { newRun } ->
                        RandomRun.length newRun
                            |> Expect.equal (RandomRun.length r)
        , Test.fuzz3 indexFuzzer indexFuzzer randomRunFuzzer "when Just, compare (swapIfOutOfOrder i1 i2 r) r /= GT" <|
            \i1 i2 r ->
                (i1 <= i2)
                    |> implies
                        (case RandomRun.swapIfOutOfOrder { leftIndex = i1, rightIndex = i2 } r of
                            Nothing ->
                                Expect.pass

                            Just { newRun } ->
                                RandomRun.compare newRun r
                                    |> Expect.notEqual GT
                                    |> Expect.onFail (Debug.toString ( RandomRun.toList r, RandomRun.toList newRun ))
                        )
        ]



-- FUZZERS


valueFuzzer : Fuzzer Int
valueFuzzer =
    Fuzz.intRange 0 Random.maxInt


randomRunMaxLength : Int
randomRunMaxLength =
    10


randomRunFuzzer : Fuzzer RandomRun
randomRunFuzzer =
    Fuzz.listOfLengthBetween 0 randomRunMaxLength valueFuzzer
        |> Fuzz.map (List.foldr RandomRun.append RandomRun.empty)


indexFuzzer : Fuzzer Int
indexFuzzer =
    Fuzz.intRange 0 randomRunMaxLength


chunkFuzzer : Fuzzer Chunk
chunkFuzzer =
    Fuzz.map2 (\startIndex size -> { startIndex = startIndex, size = size })
        indexFuzzer
        indexFuzzer


replaceListFuzzer : Fuzzer (List ( Int, Int ))
replaceListFuzzer =
    Fuzz.listOfLengthBetween 0 5 <|
        Fuzz.pair indexFuzzer valueFuzzer


updateFnFuzzer : Fuzzer (Int -> Int)
updateFnFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant identity
        , Fuzz.constant negate
        , Fuzz.constant ((*) 2)
        , Fuzz.constant (\_ -> 0)
        , Fuzz.constant (\_ -> 999)
        ]



-- HELPERS


listGetAt : Int -> List a -> Maybe a
listGetAt i list =
    list |> List.drop i |> List.head


implies : Expectation -> Bool -> Expectation
implies expectation condition =
    if condition then
        expectation

    else
        Expect.pass
