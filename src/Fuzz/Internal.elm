module Fuzz.Internal exposing (Fuzzer(..), gen, genR, generate)

{-| This module is here just to hide the `generate` function from the end users
of the library.
-}

import GenResult exposing (GenResult)
import PRNG exposing (PRNG)
import Random
import RandomRun


type Fuzzer a
    = Fuzzer (PRNG -> GenResult a)


generate : PRNG -> Fuzzer a -> GenResult a
generate prng (Fuzzer fuzzer) =
    fuzzer prng


gen : List Int -> Fuzzer a -> GenResult a
gen randomRun fuzzer =
    generate (PRNG.hardcoded (RandomRun.fromList randomRun)) fuzzer


genR : Int -> Fuzzer a -> GenResult a
genR seed fuzzer =
    generate (PRNG.random (Random.initialSeed seed)) fuzzer
