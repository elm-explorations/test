module MicroArrayExtra exposing (swap)

import Array exposing (Array)


swap : Int -> Int -> Array a -> Array a
swap i j arr =
    if i - j == 0 then
        arr

    else
        case Array.get i arr of
            Just vi ->
                case Array.get j arr of
                    Just vj ->
                        arr
                            |> Array.set i vj
                            |> Array.set j vi

                    Nothing ->
                        arr

            Nothing ->
                arr
