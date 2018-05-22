module Fuzz.Internal exposing (Fuzzer, map)

import Lazy
import Lazy.List exposing (LazyList)
import Random exposing (Generator)
import RoseTree exposing (RoseTree(..))


type alias Fuzzer a =
    Generator (RoseTree a)


map : (a -> b) -> Fuzzer a -> Fuzzer b
map fn fuzzer =
    (Random.map << RoseTree.map) fn fuzzer


sequenceRoseTree : RoseTree (Generator a) -> Generator (RoseTree a)
sequenceRoseTree (Rose root branches) =
    Random.map2
        Rose
        root
        (Lazy.List.map sequenceRoseTree branches |> sequenceLazyList)


sequenceLazyList : LazyList (Generator a) -> Generator (LazyList a)
sequenceLazyList xs =
    Random.independentSeed
        |> Random.map (runAll xs)


runAll : LazyList (Generator a) -> Random.Seed -> LazyList a
runAll xs seed =
    Lazy.lazy <|
        \_ ->
            case Lazy.force xs of
                Lazy.List.Nil ->
                    Lazy.List.Nil

                Lazy.List.Cons firstGenerator rest ->
                    let
                        ( x, newSeed ) =
                            Random.step firstGenerator seed
                    in
                    Lazy.List.Cons x (runAll rest newSeed)
