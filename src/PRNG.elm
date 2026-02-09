module PRNG exposing (PRNG(..), getRun, getSeed, hardcoded, random)

{-| A way to draw values. There are two ways:

1.  Random: draw genuinely new random values using a Random.Seed. We remember
    drawn values on the side.

2.  Hardcoded: draw predefined values out of a recorded RandomRun. Handy
    when reproducing a failure. This can run out of values to draw, but that
    shouldn't happen during the normal execution.

-}

import Random
import RandomRun exposing (ReadOnlyRandomRun, WriteOnlyRandomRun)


type PRNG
    = Random
        { run : WriteOnlyRandomRun
        , seed : Random.Seed
        }
    | Hardcoded
        { wholeRun : ReadOnlyRandomRun
        , unusedPart : ReadOnlyRandomRun
        }


random : Random.Seed -> PRNG
random seed =
    Random
        { run = RandomRun.empty
        , seed = seed
        }


hardcoded : ReadOnlyRandomRun -> PRNG
hardcoded run =
    Hardcoded
        { wholeRun = run
        , unusedPart = run
        }


getRun : PRNG -> ReadOnlyRandomRun
getRun prng =
    case prng of
        Random { run } ->
            RandomRun.switchPhase run

        Hardcoded { wholeRun } ->
            wholeRun


getSeed : PRNG -> Maybe Random.Seed
getSeed prng =
    case prng of
        Random { seed } ->
            Just seed

        Hardcoded _ ->
            Nothing
