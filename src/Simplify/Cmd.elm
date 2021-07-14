module Simplify.Cmd exposing
    ( SimplifyCmd(..)
    , cmdsForRun
    )

import MicroListExtra as List
import RandomRun exposing (Chunk, RandomRun)


type SimplifyCmd
    = DeleteChunkAndMaybeDecrementPrevious Chunk
    | ReplaceChunkWithZero Chunk
    | SortChunk Chunk
    | MinimizeChoice { index : Int }
    | RedistributeChoices { leftIndex : Int, rightIndex : Int }


cmdsForRun : RandomRun -> List SimplifyCmd
cmdsForRun run =
    let
        length =
            RandomRun.length run
    in
    List.fastConcat
        [ deletionCmds length
        , zeroCmds length
        , binarySearchCmds length
        , sortCmds length
        , redistributeCmds length
        ]


deletionCmds : Int -> List SimplifyCmd
deletionCmds length =
    chunkCmds
        DeleteChunkAndMaybeDecrementPrevious
        { length = length
        , allowChunksOfSize1 = True
        }


zeroCmds : Int -> List SimplifyCmd
zeroCmds length =
    chunkCmds
        ReplaceChunkWithZero
        { length = length
        , allowChunksOfSize1 = False -- already happens in binary search
        }


sortCmds : Int -> List SimplifyCmd
sortCmds length =
    chunkCmds
        SortChunk
        { length = length
        , allowChunksOfSize1 = False -- doesn't make sense for sorting
        }


binarySearchCmds : Int -> List SimplifyCmd
binarySearchCmds length =
    List.range 0 (length - 1)
        |> List.reverse
        |> List.map (\index -> MinimizeChoice { index = index })


redistributeCmds : Int -> List SimplifyCmd
redistributeCmds length =
    let
        forOffset : Int -> List SimplifyCmd
        forOffset offset =
            if offset >= length then
                []

            else
                List.range 0 (length - 1 - offset)
                    |> List.reverse
                    |> List.map
                        (\leftIndex ->
                            RedistributeChoices
                                { leftIndex = leftIndex
                                , rightIndex = leftIndex + offset
                                }
                        )
    in
    forOffset 3 ++ forOffset 2 ++ forOffset 1


{-| Will generate SimplifyCmds for all chunks of sizes 1,2,4,8 in bounds of the
given RandomRun length.

    chunkCmds
        SortChunk
        { itemsCount = 10, allowChunksOfSize1 = False }
        -->
        [ -- Chunks of size 8
          SortChunk { chunkSize = 8, startIndex = 2 } -- [..XXXXXXXX]
        , SortChunk { chunkSize = 8, startIndex = 1 } -- [.XXXXXXXX.]
        , SortChunk { chunkSize = 8, startIndex = 0 } -- [XXXXXXXX..]

        -- Chunks of size 4
        , SortChunk { chunkSize = 4, startIndex = 6 } -- [......XXXX]
        , SortChunk { chunkSize = 4, startIndex = 5 } -- [.....XXXX.]
        , -- ...
          SortChunk { chunkSize = 4, startIndex = 1 } -- [.XXXX.....]
        , SortChunk { chunkSize = 4, startIndex = 0 } -- [XXXX......]

        -- Chunks of size 2
        , SortChunk { chunkSize = 2, startIndex = 8 } -- [........XX]
        , SortChunk { chunkSize = 2, startIndex = 7 } -- [.......XX.]
        , -- ...
          SortChunk { chunkSize = 2, startIndex = 1 } -- [.XX.......]
        , SortChunk { chunkSize = 2, startIndex = 0 } -- [XX........]
        ]

-}
chunkCmds :
    ({ size : Int, startIndex : Int } -> cmd)
    -> { length : Int, allowChunksOfSize1 : Bool }
    -> List cmd
chunkCmds toCmd { length, allowChunksOfSize1 } =
    let
        initChunkSize : Int
        initChunkSize =
            if allowChunksOfSize1 then
                1

            else
                2

        go : Int -> Int -> List cmd -> List cmd
        go chunkSize startIndex acc =
            if startIndex > length - chunkSize then
                if chunkSize == 8 then
                    acc

                else
                    go (chunkSize * 2) 0 acc

            else
                let
                    newCmd =
                        toCmd
                            { size = chunkSize
                            , startIndex = startIndex
                            }
                in
                go chunkSize (startIndex + 1) (newCmd :: acc)
    in
    go initChunkSize 0 []
