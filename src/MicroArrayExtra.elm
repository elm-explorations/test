module MicroArrayExtra exposing (swap)

import Array exposing (Array)


swap : Int -> Int -> Array a -> Array a
swap i j arr =
    if i == j then
        arr

    else
        case ( Array.get i arr, Array.get j arr ) of
            ( Just vi, Just vj ) ->
                arr
                    |> Array.set i vj
                    |> Array.set j vi

            _ ->
                arr
