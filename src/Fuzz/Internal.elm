module Fuzz.Internal exposing (Fuzzer(..), generate)

import GenResult exposing (GenResult)
import PRNG exposing (PRNG)


type Fuzzer a
    = Fuzzer (PRNG -> GenResult a)


generate : PRNG -> Fuzzer a -> GenResult a
generate prng (Fuzzer fuzzer) =
    fuzzer prng
