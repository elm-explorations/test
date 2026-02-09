module GenResult exposing (GenResult(..), getPrng)

{-| A result of a Fuzzer trying to generate a value from a given PRNG.
-}

import PRNG exposing (PRNG)
import Test.Coverage.EdgeHitCounts exposing (BucketedEdgeHitCounts)


type GenResult a
    = Generated
        { value : a
        , prng : PRNG
        , previousInputBucketedEdgeHitCounts : Maybe BucketedEdgeHitCounts
        }
    | Rejected
        { reason : String
        , prng : PRNG
        }


getPrng : GenResult a -> PRNG
getPrng genResult =
    case genResult of
        Generated { prng } ->
            prng

        Rejected { prng } ->
            prng
