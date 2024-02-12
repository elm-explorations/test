module Fuzz.Internal (Fuzzer(..), generate) where

{-| This module is here just to hide the `generate` function from the end users
of the library.
-}

import GenResult (GenResult)
import GenResult as GenResult
import PRNG (PRNG)
import PRNG as PRNG


data Fuzzer a
    = Fuzzer (PRNG -> GenResult a)


generate :: PRNG -> Fuzzer a -> GenResult a
generate prng (Fuzzer fuzzer) =
    fuzzer prng
