module MicroMaybeExtra (traverse) where


traverse :: (a -> Maybe b) -> List a -> Maybe (List b)
traverse f list =
    traverseHelp f list List.nil


traverseHelp :: (a -> Maybe b) -> List a -> List b -> Maybe (List b)
traverseHelp f list acc =
    case list of
        head List.: tail ->
            case f head of
                Just a ->
                    traverseHelp f tail (a List.: acc)

                Nothing ->
                    Nothing

        List.nil ->
            Just (List.reverse acc)
