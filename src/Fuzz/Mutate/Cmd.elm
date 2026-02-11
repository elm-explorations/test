module Fuzz.Mutate.Cmd exposing (DeterministicMutateCmd(..), MutateCmd(..))

{-| Inspired by:
<https://lcamtuf.blogspot.com/2014/08/binary-fuzzing-strategies-what-works.html>
-}


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


deterministicallyMutate : RandomRun -> List RandomRun
deterministicallyMutate randomRun =
    enabledDeterministicMutations
        |> MicroListExtra.fastConcatMap (\cmd -> deterministicallyMutateWith cmd randomRun)


deterministicallyMutateWith : DeterministicMutateCmd -> RandomRun -> List RandomRun
deterministicallyMutateWith cmd randomRun =
    case cmd of
        BitFlip1 ->
            flipBits 1 randomRun

        BitFlip2 ->
            flipBits 2 randomRun

        BitFlip4 ->
            flipBits 4 randomRun

        ByteFlip1 ->
            Debug.todo "byte flip 1"

        ByteFlip2 ->
            []

        ByteFlip4 ->
            []

        Arithmetic1 ->
            []

        Arithmetic2 ->
            []

        Arithmetic4 ->
            []

        Interesting1 ->
            []

        Interesting2 ->
            []

        Interesting4 ->
            []


flipBits : Int -> RandomRun -> List RandomRun
flipBits n randomRun =
    List.range 0 (RandomRun.length - 1)
        |> List.concatMap
            (\int32Index ->
                List.range 0 (31 - n + 1)
                    |> List.concatMap
                        (\bitIndex ->
                            randomRun
                                |> RandomRun.update int32Index (flipBitsAux bitIndex)
                        )
            )


flipBitsAux : Int -> Int -> Int
flipBitsAux bitIndex int32 =
    ()



-- Interesting 8bit, 16bit, 32bit values to replace parts of RandomRuns with.
-- Note that interesting1 will be also used for Interesting2 and 4, etc.


interesting8bit : List Int
interesting8bit =
    [ 0, 1, 16, 32, 64, 100, 127, 128, 255 ]


interesting16bit : List Int
interesting16bit =
    [ {- 128, 255, -} 256, 512, 1000, 1024, 4096, 32767, 32768, 65407 ]


interesting32bit : List Int
interesting32bit =
    [ {- 32768, -} 65535, 65536, 100663045, 2147483647, 2147483648, 4194304250, 4294934527 ]
