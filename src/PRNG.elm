module PRNG exposing (PRNG(..), getMaxes, getRun, getSeed, hardcoded, random, tracked)

{-| A way to draw values. There are two ways:

1.  Random: draw genuinely new random values using a Random.Seed. We remember
    drawn values on the side.

2.  Hardcoded: draw predefined values out of a recorded RandomRun. Handy
    when reproducing a failure. This can run out of values to draw, but that
    shouldn't happen during the normal execution.

3.  Tracked: like Random, but carrying a record of which parts of the input
    space have already been covered, so a draw can decline them. Also records
    the upper bound of every draw, which is what tells `Occupancy` how many
    children a node has and therefore when it's complete.

    Until something is actually covered this behaves exactly like Random: the
    same seed produces the same values.

-}

import Occupancy exposing (Occupancy)
import Random
import RandomRun exposing (RandomRun)


type PRNG
    = -- PERF: optimized from record to custom type arguments to skip _Utils_update:
      Random RandomRun Random.Seed
    | Hardcoded {- wholeRun: -} RandomRun {- unusedPart: -} RandomRun
    | Tracked {- hereOnwards: -} Occupancy {- run: -} RandomRun {- reversedMaxes: -} (List Int) Random.Seed


random : Random.Seed -> PRNG
random seed =
    Random RandomRun.empty seed


hardcoded : RandomRun -> PRNG
hardcoded run =
    Hardcoded run run


{-| Draw randomly, declining inputs already covered.
-}
tracked : Occupancy -> Random.Seed -> PRNG
tracked occupancy seed =
    Tracked occupancy RandomRun.empty [] seed


getRun : PRNG -> RandomRun
getRun prng =
    case prng of
        Random run _ ->
            run

        Hardcoded wholeRun _ ->
            wholeRun

        Tracked _ run _ _ ->
            run


getSeed : PRNG -> Maybe Random.Seed
getSeed prng =
    case prng of
        Random _ seed ->
            Just seed

        Hardcoded _ _ ->
            Nothing

        Tracked _ _ _ seed ->
            Just seed


{-| The upper bound of every draw made, in order. Only a tracked draw records
these.

Accumulated as a reversed list rather than a `RandomRun`, because `RandomRun`
appends copy the underlying array: recording bounds that way makes every draw pay
a second O(length) copy, which turns into O(length^2) per run for no reason. The
bounds are only read once, when the run finishes.

-}
getMaxes : PRNG -> List Int
getMaxes prng =
    case prng of
        Random _ _ ->
            []

        Hardcoded _ _ ->
            []

        Tracked _ _ reversedMaxes _ ->
            List.reverse reversedMaxes
