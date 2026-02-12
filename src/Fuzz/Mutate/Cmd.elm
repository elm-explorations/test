module Fuzz.Mutate.Cmd exposing (DeterministicMutateCmd(..), MutateCmd(..), deterministicallyMutate)

{-| Inspired by:
<https://lcamtuf.blogspot.com/2014/08/binary-fuzzing-strategies-what-works.html>
-}

import Bitwise
import MicroListExtra
import RandomRun exposing (RandomRun)


type MutateCmd
    = Deterministic DeterministicMutateCmd


type DeterministicMutateCmd
    = BitFlip1 -- flip bits in: b......., .b......, ..b....., etc.
    | BitFlip2 -- flip bits in: bb......, .bb....., ..bb...., etc.
    | BitFlip4 -- flip bits in: bbbb...., .bbbb..., ..bbbb.., etc.
    | ByteFlip1 -- flip bytes in: B..., .B.., ..B., ...B
    | ByteFlip2 -- flip bytes in: BB.., .BB., ..BB
    | ByteFlip4 -- flip all 4 bytes in the int32: BBBB
    | Arithmetic1 -- add -35..+35 to bytes in: B..., .B.., ..B., ...B
    | Arithmetic2 -- add -35..+35 to bytes in: BB.., .BB., ..BB
    | Arithmetic4 -- add -35..+35 to the whole int32: BBBB
    | Interesting1 -- rewrite 8 bits with edge cases in: B..., .B.., ..B., ...B
    | Interesting2 -- rewrite 16 bits with edge cases in: BB.., .BB., ..BB
    | Interesting4 -- rewrite the whole int32 with edge cases: BBBB


enabledDeterministicMutations : List DeterministicMutateCmd
enabledDeterministicMutations =
    [ BitFlip1
    , BitFlip2
    , BitFlip4
    , ByteFlip1
    , ByteFlip2
    , ByteFlip4
    , Arithmetic1
    , Arithmetic2
    , Arithmetic4
    , Interesting1
    , Interesting2
    , Interesting4
    ]



{- The following functions could be thought of as

       RandomRun -> List RandomRun

   where we'd later take this list of mutated runs and try each, adding it to
   the corpus or not based on the results.

   The length of the list could get very large though; so we're opting for a
   "callback" style, where each run gets processed as it appears, and we don't
   build the intermediate list at all:

       RandomRun -> (RandomRun -> a -> a) -> a -> a
       --           ^                  ^     ^    ^
       --           callback           ^     ^    state after processing all runs
       --                              ^     init state
       --                              ^
       --                              state after processing one run

-}


deterministicallyMutate : RandomRun -> (RandomRun -> a -> a) -> a -> a
deterministicallyMutate randomRun onMutatedRandomRun initState =
    enabledDeterministicMutations
        |> List.foldl
            (\cmd accState -> deterministicallyMutateWith cmd randomRun onMutatedRandomRun accState)
            initState


deterministicallyMutateWith : DeterministicMutateCmd -> RandomRun -> (RandomRun -> a -> a) -> a -> a
deterministicallyMutateWith cmd randomRun onMutatedRandomRun initState =
    let
        do : (RandomRun -> (RandomRun -> a -> a) -> a -> a) -> a
        do fn =
            fn randomRun onMutatedRandomRun initState
    in
    case cmd of
        BitFlip1 ->
            do <| flipBits 1 1

        BitFlip2 ->
            do <| flipBits 2 1

        BitFlip4 ->
            do <| flipBits 4 1

        ByteFlip1 ->
            do <| flipBits 8 8

        ByteFlip2 ->
            do <| flipBits 16 8

        ByteFlip4 ->
            do negateWholeNumber

        Arithmetic1 ->
            do <| addNumbersToBytes 1

        Arithmetic2 ->
            do <| addNumbersToBytes 2

        Arithmetic4 ->
            do <| addNumbersToBytes 4

        Interesting1 ->
            do <| interesting interesting8bit

        Interesting2 ->
            do <| interesting interesting16bit

        Interesting4 ->
            do <| interesting interesting32bit


{-| Helpful for functions that only mutate each RandomRun int32 in isolation.
-}
onEachRandomRunInt : RandomRun -> (Int -> Int -> a -> a) -> a -> a
onEachRandomRunInt randomRun onInt32 initState =
    randomRun
        |> {- TODO PERF: expose a RandomRun.indexedFoldl function? -} RandomRun.toList
        |> MicroListExtra.indexedFoldl onInt32 initState


flipBits : Int -> Int -> RandomRun -> (RandomRun -> a -> a) -> a -> a
flipBits n step randomRun onMutatedRandomRun initState =
    let
        maxBitIndex =
            31 - n + 1

        go : Int -> Int -> Int -> a -> a
        go bitIndex int32Index int32 accState =
            if bitIndex > maxBitIndex then
                accState

            else
                let
                    mutatedRun =
                        randomRun
                            |> RandomRun.set int32Index (flipBitsAux n bitIndex int32)
                in
                go
                    (bitIndex + step)
                    int32Index
                    int32
                    (onMutatedRandomRun mutatedRun accState)
    in
    onEachRandomRunInt randomRun (go 0) initState


flipBitsAux : Int -> Int -> Int -> Int
flipBitsAux n bitIndex int32 =
    {- Beware: will break at n==32. Use negateWholeNumberAux instead.
       It wouldn't work because all Elm bitwise functions are clamped to 32b,
       and we get outside that range for a moment.

       Algorithm:
       Create a mask with 1 for all bits you want to flip and 0 for the rest
       XOR this mask with the original number
    -}
    let
        mask =
            -- let's say, for n=3 and bitIndex=1
            {- 0b00000001 -}
            1
                |> {- 0b00001000 -} Bitwise.shiftLeftBy n
                |> {- 0b00000111 -} (\powerOf2 -> powerOf2 - 1)
                |> {- 0b00001110 -} Bitwise.shiftLeftBy bitIndex
    in
    Bitwise.xor int32 mask


negateWholeNumber : RandomRun -> (RandomRun -> a -> a) -> a -> a
negateWholeNumber randomRun onMutatedRandomRun initState =
    let
        go : Int -> Int -> a -> a
        go int32Index int32 accState =
            let
                mutatedRun =
                    randomRun
                        |> RandomRun.set int32Index (negateWholeNumberAux int32)
            in
            onMutatedRandomRun mutatedRun accState
    in
    onEachRandomRunInt randomRun go initState


negateWholeNumberAux : Int -> Int
negateWholeNumberAux int32 =
    -- We need to shiftRightZfBy 0 to convert from int to uint.
    {- 0x0000000A = 10 -}
    int32
        |> {- 0xFFFFFFF5 = -11 -} Bitwise.complement
        |> {- 0xFFFFFFF5 = 4294967285 -} Bitwise.shiftRightZfBy 0


addNumbersToBytes : Int -> RandomRun -> (RandomRun -> a -> a) -> a -> a
addNumbersToBytes nBytes randomRun onMutatedRandomRun initState =
    let
        -- AFL numbers
        minAddend =
            -35

        maxAddend =
            35

        maxByteIndex =
            4 - nBytes + 1

        go : Int -> Int -> Int -> Int -> a -> a
        go addend byteIndex int32Index int32 accState =
            if addend > maxAddend then
                accState

            else if byteIndex > maxByteIndex then
                go (addend + 1) 0 int32Index int32 accState

            else
                let
                    newInt32 =
                        addNumbersToBytesAux addend nBytes byteIndex int32
                in
                if newInt32 < 0 then
                    go addend (byteIndex + 1) int32Index int32 accState

                else
                    go addend
                        (byteIndex + 1)
                        int32Index
                        int32
                        (onMutatedRandomRun
                            (randomRun
                                |> RandomRun.set int32Index newInt32
                            )
                            accState
                        )
    in
    onEachRandomRunInt randomRun (go minAddend 0) initState


addNumbersToBytesAux : Int -> Int -> Int -> Int -> Int
addNumbersToBytesAux addend nBytes byteIndex int32 =
    let
        bitIndex =
            8 * byteIndex

        mask =
            -- let's say, for n=2 and byteIndex=1
            {- 0x00000001 -}
            1
                |> {- 0x00000100 -} Bitwise.shiftLeftBy (8 * nBytes)
                |> {- 0x000000FF -} (\powerOf2 -> powerOf2 - 1)
                |> {- 0x00000FF0 -} Bitwise.shiftLeftBy bitIndex

        extractedBytes =
            {- 0x12345678 -}
            int32
                |> {- 0x00000670 -} Bitwise.and mask
                |> {- 0x00000067 -} Bitwise.shiftRightZfBy bitIndex

        withoutExtractedBytes =
            {- 0x12345678 -}
            int32
                |> {- 0x12345008 -} Bitwise.and (Bitwise.complement mask)
    in
    {- 0x000000FF -}
    (extractedBytes + addend)
        |> {- 0x00000FF0 -} Bitwise.shiftLeftBy bitIndex
        |> {- 0x12345FF8 -} Bitwise.or withoutExtractedBytes


interesting : List Int -> RandomRun -> (RandomRun -> a -> a) -> a -> a
interesting numbers randomRun onMutatedRandomRun initState =
    let
        go : Int -> Int -> a -> a
        go int32Index int32 accState =
            numbers
                |> List.foldl
                    (\n accState_ ->
                        let
                            mutatedRun =
                                randomRun
                                    |> RandomRun.set int32Index n
                        in
                        onMutatedRandomRun mutatedRun accState_
                    )
                    accState
    in
    onEachRandomRunInt randomRun go initState


interesting8bit : List Int
interesting8bit =
    [ 0, 1, 16, 32, 64, 100, 127, 128, 255 ]


interesting16bit : List Int
interesting16bit =
    [ 256, 512, 1000, 1024, 4096, 32767, 32768, 65407 ]
        ++ interesting8bit


interesting32bit : List Int
interesting32bit =
    [ 65535, 65536, 100663045, 2147483647, 2147483648, 4194304250, 4294934527 ]
        ++ interesting16bit
