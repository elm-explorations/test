module MicroDictExtra {a:any, b:increment} where

import Dict (Dict)
import Dict as Dict


any :: (k -> v -> Bool) -> Dict k v -> Bool
any pred dict =
    dict
        |> Dict.toList
        |> List.any (\{a:k, b:v } -> pred k v)


increment :: comparable -> Dict comparable Int -> Dict comparable Int
increment key dict =
    dict
        |> Dict.update key
            (\maybeValue ->
                case maybeValue of
                    Nothing ->
                        Just 1

                    Just value ->
                        Just (value + 1)
            )
