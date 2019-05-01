module Fuzz.Internal exposing (Fuzzer, Valid, ValidFuzzer, combineValid, frequencyList, invalidReason, map)

import Lazy
import Lazy.List exposing (LazyList)
import MicroRandomExtra
import Random exposing (Generator)
import RoseTree exposing (RoseTree(..))


type alias Fuzzer a =
    Valid (ValidFuzzer a)


type alias Valid a =
    Result String a


type alias ValidFuzzer a =
    Generator (RoseTree a)


combineValid : List (Valid a) -> Valid (List a)
combineValid valids =
    case valids of
        [] ->
            Ok []

        (Ok x) :: rest ->
            Result.map ((::) x) (combineValid rest)

        (Err reason) :: _ ->
            Err reason


map : (a -> b) -> Fuzzer a -> Fuzzer b
map fn fuzzer =
    (Result.map << Random.map << RoseTree.map) fn fuzzer


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


getValid : Valid a -> Maybe a
getValid valid =
    case valid of
        Ok x ->
            Just x

        Err _ ->
            Nothing


invalidReason : Valid a -> Maybe String
invalidReason valid =
    case valid of
        Ok _ ->
            Nothing

        Err reason ->
            Just reason


{-| Creates a single generator from a list of generators by, once at the start, randomly choosing between:

  - a single element from a single generator,
  - a single one of the generators
  - a pair of the generators, or
  - all of the generators.

It then runs Fuzz.frequency on that subset until we have the desired length list.

-}
frequencyList : Generator Int -> ( Float, Generator a ) -> List ( Float, Generator a ) -> Generator (List a)
frequencyList lengthGenerator pair pairs =
    let
        rConst ( a, b ) =
            ( a, Random.constant ( a, b ) )

        randomGenerator : Generator ( Float, Generator a )
        randomGenerator =
            MicroRandomExtra.frequency (rConst pair) (List.map rConst pairs)

        nonEmptySample a rest =
            MicroRandomExtra.sample (a :: rest) |> Random.map (Maybe.withDefault a)

        generator : Generator (Generator a)
        generator =
            nonEmptySample
                -- single repeated element for a single generator
                (MicroRandomExtra.frequency pair pairs
                    |> Random.map Random.constant
                )
                [ -- single generator
                  randomGenerator
                    |> Random.map Tuple.second

                -- pair of generators
                , Random.map2
                    (\firstGenerator secondGenerator -> MicroRandomExtra.frequency firstGenerator [ secondGenerator ])
                    randomGenerator
                    randomGenerator

                -- all generators
                , MicroRandomExtra.frequency pair pairs
                    |> Random.constant
                ]
                |> Random.andThen identity
    in
    Random.map2 Tuple.pair lengthGenerator generator
        |> Random.andThen (\( len, gen ) -> Random.list len gen)
