module MicroListExtra ( fastConcat
    , fastConcatMap
    , find
    , getAt
    , setAt
    , splitWhen
    , transpose
    )
 where


getAt :: Int -> List a -> Maybe a
getAt index list =
    if index < 0 then
        Nothing

    else
        list
            |> List.drop index
            |> List.head


setAt :: Int -> a -> Int -> List a -> List a
setAt index value length list =
    if length <= index || index < 0 then
        list

    else
        List.take index list
            <> value
            List.: List.drop (index + 1) list


fastConcat :: List (List a) -> List a
fastConcat =
    List.foldr (<>) List.nil


fastConcatMap :: (a -> List b) -> List a -> List b
fastConcatMap f =
    List.foldr (f >> (<>)) List.nil


find :: (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        List.nil ->
            Nothing

        first List.: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


splitWhen :: (a -> Bool) -> List a -> Maybe {a::List a, b::List a }
splitWhen predicate list =
    findIndex predicate list
        |> Maybe.map (\i -> splitAt i list)


findIndex :: (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


findIndexHelp :: Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        List.nil ->
            Nothing

        x List.: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs


splitAt :: Int -> List a -> {a::List a, b::List a }
splitAt n xs =
    {a:List.take n xs, b:List.drop n xs }


transpose :: List (List a) -> List (List a)
transpose listOfLists =
    List.foldr (List.map2 (List.:)) (List.repeat (rowsLength listOfLists) List.nil) listOfLists


rowsLength :: List (List a) -> Int
rowsLength listOfLists =
    case listOfLists of
        List.nil ->
            0

        x List.: _ ->
            List.length x
