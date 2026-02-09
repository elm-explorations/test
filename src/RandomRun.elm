module RandomRun exposing
    ( Chunk
    , ReadOnlyRandomRun
    , WriteOnlyRandomRun
    , append
    , compare
    , deleteChunk
    , empty
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
    , switchPhase
    , toList
    , update
    )

import MicroListExtra as List


type WriteOnly
    = WriteOnly Never


type ReadOnly
    = ReadOnly Never


type RandomRun tag
    = RandomRun
        { data : List Int

        -- derived precomputed data:
        , length : Int
        }


type alias WriteOnlyRandomRun =
    RandomRun WriteOnly


type alias ReadOnlyRandomRun =
    RandomRun ReadOnly


type alias Chunk =
    { size : Int
    , startIndex : Int
    }


empty : RandomRun tag
empty =
    RandomRun
        { data = []
        , length = 0
        }


isEmpty : RandomRun tag -> Bool
isEmpty (RandomRun run) =
    run.length == 0


nextChoice : ReadOnlyRandomRun -> Maybe ( Int, ReadOnlyRandomRun )
nextChoice (RandomRun run) =
    case run.data of
        [] ->
            Nothing

        first :: rest ->
            Just
                ( first
                , RandomRun
                    { run
                        | length = run.length - 1
                        , data = rest
                    }
                )


append : Int -> WriteOnlyRandomRun -> WriteOnlyRandomRun
append n (RandomRun run) =
    RandomRun
        { run
            | length = run.length + 1
            , data = max 0 n :: run.data
        }


switchPhase : WriteOnlyRandomRun -> ReadOnlyRandomRun
switchPhase (RandomRun run) =
    RandomRun
        { length = run.length
        , data = List.reverse run.data
        }


isInBounds : Chunk -> RandomRun tag -> Bool
isInBounds { startIndex, size } (RandomRun run) =
    startIndex + size <= run.length


length : RandomRun tag -> Int
length (RandomRun run) =
    run.length


getChunk : Chunk -> ReadOnlyRandomRun -> Maybe (List Int)
getChunk chunk ((RandomRun run) as orig) =
    if isInBounds chunk orig then
        run.data
            |> List.drop chunk.startIndex
            |> List.take chunk.size
            |> Just

    else
        Nothing


deleteChunk : Chunk -> ReadOnlyRandomRun -> ReadOnlyRandomRun
deleteChunk chunk ((RandomRun run) as orig) =
    if isInBounds chunk orig then
        RandomRun
            { run
                | length = run.length - chunk.size
                , data =
                    List.take chunk.startIndex run.data
                        ++ List.drop (chunk.startIndex + chunk.size) run.data
            }

    else
        orig


replaceChunkWithZero : Chunk -> ReadOnlyRandomRun -> ReadOnlyRandomRun
replaceChunkWithZero chunk ((RandomRun run) as orig) =
    if isInBounds chunk orig then
        -- TODO PERF: maybe `replace [...] run` would be faster?
        RandomRun
            { run
                | data =
                    List.fastConcat
                        [ List.take chunk.startIndex run.data
                        , List.repeat chunk.size 0
                        , List.drop (chunk.startIndex + chunk.size) run.data
                        ]
            }

    else
        orig


sortChunk : Chunk -> ReadOnlyRandomRun -> ReadOnlyRandomRun
sortChunk chunk run =
    case getChunk chunk run of
        Nothing ->
            run

        Just chunkData ->
            let
                sortedIndexed : List ( Int, Int )
                sortedIndexed =
                    chunkData
                        |> List.sort
                        |> List.indexedMap
                            (\i value -> ( chunk.startIndex + i, value ))
            in
            replace sortedIndexed run


replace : List ( Int, Int ) -> ReadOnlyRandomRun -> ReadOnlyRandomRun
replace values (RandomRun run) =
    replaceInList values run.length run.data


{-| An optimization to not do data list extraction redundantly.

Expects `list == run.data`
and `len == run.length`

-}
replaceInList : List ( Int, Int ) -> Int -> List Int -> ReadOnlyRandomRun
replaceInList values len list =
    RandomRun
        { length = len
        , data =
            List.foldl
                (\( index, newValue ) accList ->
                    List.setAt index (max 0 newValue) len accList
                )
                list
                values
        }


swapChunks :
    { leftChunk : Chunk, rightChunk : Chunk }
    -> ReadOnlyRandomRun
    -> Maybe ReadOnlyRandomRun
swapChunks { leftChunk, rightChunk } ((RandomRun run) as orig) =
    let
        list =
            run.data
    in
    Maybe.map2
        (\lefts rights ->
            replaceInList
                (List.concat
                    [ List.indexedMap (\i n -> ( rightChunk.startIndex + i, n )) lefts
                    , List.indexedMap (\i n -> ( leftChunk.startIndex + i, n )) rights
                    ]
                )
                run.length
                list
        )
        {- TODO PERF: both of these are doing the List.drop etc. operations
           while we already have that factored out in the `list` var.
           We could factor that operation out `getChunk`?
        -}
        (getChunk leftChunk orig)
        (getChunk rightChunk orig)


swapIfOutOfOrder :
    { leftIndex : Int, rightIndex : Int }
    -> ReadOnlyRandomRun
    ->
        Maybe
            { newRun : ReadOnlyRandomRun
            , newLeftValue : Int
            , newRightValue : Int
            }
swapIfOutOfOrder { leftIndex, rightIndex } ((RandomRun run) as orig) =
    let
        list =
            run.data
    in
    Maybe.map2
        (\left right ->
            if left > right then
                { newRun =
                    replaceInList
                        [ ( leftIndex, right )
                        , ( rightIndex, left )
                        ]
                        run.length
                        list
                , newLeftValue = right
                , newRightValue = left
                }

            else
                { newRun = orig
                , newLeftValue = left
                , newRightValue = right
                }
        )
        (List.getAt leftIndex list)
        (List.getAt rightIndex list)


get : Int -> ReadOnlyRandomRun -> Maybe Int
get index (RandomRun run) =
    run.data
        |> List.getAt index


set : Int -> Int -> ReadOnlyRandomRun -> ReadOnlyRandomRun
set index value ((RandomRun run) as orig) =
    if run.length <= index then
        orig

    else
        RandomRun
            { run
                | data =
                    run.data
                        |> List.setAt index (max 0 value) run.length
            }


sortKey : ReadOnlyRandomRun -> ( Int, List Int )
sortKey (RandomRun run) =
    ( run.length
    , run.data
    )


compare : ReadOnlyRandomRun -> ReadOnlyRandomRun -> Order
compare a b =
    Basics.compare (sortKey a) (sortKey b)


toList : ReadOnlyRandomRun -> List Int
toList (RandomRun run) =
    run.data


update : Int -> (Int -> Int) -> ReadOnlyRandomRun -> ReadOnlyRandomRun
update index fn run =
    case get index run of
        Nothing ->
            run

        Just value ->
            replace [ ( index, fn value ) ] run
