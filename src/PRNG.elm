module PRNG exposing
    ( PRNG(..)
    , random, hardcoded
    , getRun, getSeed, getInputCorpus
    )

{-| A way to draw values. There are two ways:

1.  Random: draw genuinely new random values using a Random.Seed. We remember
    drawn values on the side.

2.  Hardcoded: draw predefined values out of a recorded RandomRun. Handy
    when reproducing a failure. This can run out of values to draw, but that
    shouldn't happen during the normal execution.

@docs PRNG
@docs random, hardcoded
@docs getRun, getSeed, getInputCorpus

-}

import Fuzz.InputCorpus exposing (InputCorpus)
import Random
import RandomRun exposing (RandomRun)


type PRNG
    = Random
        { run : RandomRun
        , seed : Random.Seed
        , inputCorpus : InputCorpus
        }
    | Hardcoded
        { wholeRun : RandomRun
        , unusedPart : RandomRun
        }


random : Random.Seed -> InputCorpus -> PRNG
random seed corpus =
    Random
        { run = RandomRun.empty
        , seed = seed
        , inputCorpus = corpus
        }


hardcoded : RandomRun -> PRNG
hardcoded run =
    Hardcoded
        { wholeRun = run
        , unusedPart = run
        }


getRun : PRNG -> RandomRun
getRun prng =
    case prng of
        Random { run } ->
            run

        Hardcoded { wholeRun } ->
            wholeRun


getSeed : PRNG -> Maybe Random.Seed
getSeed prng =
    case prng of
        Random { seed } ->
            Just seed

        Hardcoded _ ->
            Nothing


getInputCorpus : PRNG -> Maybe InputCorpus
getInputCorpus prng =
    case prng of
        Random { inputCorpus } ->
            Just inputCorpus

        Hardcoded _ ->
            Nothing
