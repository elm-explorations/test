module MicroListExtra exposing
    ( fastConcat
    , fastConcatMap
    , getAt
    , setAt
    )


getAt : Int -> List a -> Maybe a
getAt index list =
    if index < 0 then
        Nothing

    else
        list
            |> List.drop index
            |> List.head


setAt : Int -> a -> Int -> List a -> List a
setAt index value length list =
    if length <= index || index < 0 then
        list

    else
        List.take index list
            ++ value
            :: List.drop (index + 1) list


fastConcat : List (List a) -> List a
fastConcat =
    List.foldr (++) []


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap f =
    List.foldr (f >> (++)) []
