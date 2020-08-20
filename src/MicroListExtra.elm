module MicroListExtra exposing
    ( fastConcat
    , fastConcatMap
    , getAt
    , greedyGroupsOf
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


greedyGroupsOf : Int -> List a -> List (List a)
greedyGroupsOf size xs =
    greedyGroupsOfWithStep size size xs


greedyGroupsOfWithStep : Int -> Int -> List a -> List (List a)
greedyGroupsOfWithStep size step list =
    if size <= 0 || step <= 0 then
        []

    else
        let
            go : List a -> List (List a) -> List (List a)
            go xs acc =
                if List.isEmpty xs then
                    List.reverse acc

                else
                    go
                        (List.drop step xs)
                        (List.take size xs :: acc)
        in
        go list []
