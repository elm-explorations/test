module PRNG exposing (PRNG(..), getRun, getSeed, hardcoded, random)

{-| A way to draw values. There are two ways:

1.  Random: draw genuinely new random values using a Random.Seed. We remember
    drawn values on the side.

2.  Hardcoded: draw predefined values out of a recorded RandomRun. Handy
    when reproducing a failure. This can run out of values to draw, but that
    shouldn't happen during the normal execution.

-}

import Random
import RandomRun exposing (RandomRun)


type PRNG
    = -- PERF: optimized from record to custom type arguments to skip _Utils_update:
      Random RandomRun Random.Seed
    | Hardcoded {- wholeRun: -} RandomRun {- unusedPart: -} RandomRun


random : Random.Seed -> PRNG
random seed =
    Random RandomRun.empty seed


hardcoded : RandomRun -> PRNG
hardcoded run =
    Hardcoded run run


getRun : PRNG -> RandomRun
getRun prng =
    case prng of
        Random run _ ->
            run

        Hardcoded wholeRun _ ->
            wholeRun


getSeed : PRNG -> Maybe Random.Seed
getSeed prng =
    case prng of
        Random _ seed ->
            Just seed

        Hardcoded _ _ ->
            Nothing
