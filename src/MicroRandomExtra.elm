module MicroRandomExtra exposing (choose)

import MicroListExtra
import Random exposing (Generator)


choose : List a -> Generator (Maybe a)
choose list =
    if List.isEmpty list then
        Random.constant Nothing

    else
        Random.int 0 (List.length list - 1)
            |> Random.map (\index -> MicroListExtra.getAt index list)
