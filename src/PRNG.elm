module PRNG exposing (PRNG(..), getRun, getSeed, hardcoded, random)

import Random
import RandomRun exposing (RandomRun)


type PRNG
    = Random
        { run : RandomRun
        , seed : Random.Seed
        }
    | Hardcoded
        { wholeRun : RandomRun
        , unusedPart : RandomRun
        }


random : Random.Seed -> PRNG
random seed =
    Random
        { run = RandomRun.empty
        , seed = seed
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
