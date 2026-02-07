module RandomRun exposing
    ( Chunk
    , RandomRun
    , append
    , compare
    , deleteChunk
    , empty
    , equal
    , get
    , isEmpty
    , length
    , nextChoice
    , replace
    , replaceChunkWithZero
    , set
    , sortChunk
    , swapChunks
    , swapIfOutOfOrder
    , toList
    , update
    )

import Elm.Kernel.RandomRun


{-| TypedArray in JS kernel
-}
type RandomRun
    = RandomRun Never


type alias Chunk =
    { size : Int
    , startIndex : Int
    }


empty : RandomRun
empty =
    Elm.Kernel.RandomRun.empty


isEmpty : RandomRun -> Bool
isEmpty =
    Elm.Kernel.RandomRun.isEmpty


nextChoice : RandomRun -> Maybe ( Int, RandomRun )
nextChoice =
    Elm.Kernel.RandomRun.nextChoice


append : Int -> RandomRun -> RandomRun
append =
    Elm.Kernel.RandomRun.append


length : RandomRun -> Int
length =
    Elm.Kernel.RandomRun.length


deleteChunk : Chunk -> RandomRun -> RandomRun
deleteChunk =
    Elm.Kernel.RandomRun.deleteChunk


replaceChunkWithZero : Chunk -> RandomRun -> RandomRun
replaceChunkWithZero =
    Elm.Kernel.RandomRun.replaceChunkWithZero


sortChunk : Chunk -> RandomRun -> RandomRun
sortChunk =
    Elm.Kernel.RandomRun.sortChunk


replace : List ( Int, Int ) -> RandomRun -> RandomRun
replace =
    Elm.Kernel.RandomRun.replace


swapChunks :
    { leftChunk : Chunk, rightChunk : Chunk }
    -> RandomRun
    -> Maybe RandomRun
swapChunks =
    Elm.Kernel.RandomRun.swapChunks


swapIfOutOfOrder :
    { leftIndex : Int, rightIndex : Int }
    -> RandomRun
    ->
        Maybe
            { newRun : RandomRun
            , newLeftValue : Int
            , newRightValue : Int
            }
swapIfOutOfOrder =
    Elm.Kernel.RandomRun.swapIfOutOfOrder


get : Int -> RandomRun -> Maybe Int
get =
    Elm.Kernel.RandomRun.get


set : Int -> Int -> RandomRun -> RandomRun
set =
    Elm.Kernel.RandomRun.set


compare : RandomRun -> RandomRun -> Order
compare =
    Elm.Kernel.RandomRun.compare


toList : RandomRun -> List Int
toList =
    Elm.Kernel.RandomRun.toList


update : Int -> (Int -> Int) -> RandomRun -> RandomRun
update =
    Elm.Kernel.RandomRun.update


equal : RandomRun -> RandomRun -> Bool
equal =
    Elm.Kernel.RandomRun.equal
