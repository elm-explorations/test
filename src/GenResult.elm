module GenResult (GenResult(..), getPrng) where

{-| A result of a Fuzzer trying to generate a value from a given PRNG.
-}

import PRNG (PRNG)
import PRNG as PRNG


data GenResult a
    = Generated
        { value :: a
        , prng :: PRNG
        }
    | Rejected
        { reason :: String
        , prng :: PRNG
        }


getPrng :: GenResult a -> PRNG
getPrng genResult =
    case genResult of
        Generated { prng } ->
            prng

        Rejected { prng } ->
            prng
