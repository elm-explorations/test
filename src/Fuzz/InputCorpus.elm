module Fuzz.InputCorpus exposing (Input, InputCorpus)

import Test.Coverage.EdgeHitCounts exposing (BucketedEdgeHitCounts)


type alias InputCorpus =
    { favoredFresh : List Input
    , favoredUsed : List Input
    , otherFresh : List Input
    , otherUsed : List Input
    }


type alias Input =
    { randomRun : RandomRun
    , durationMs : Int
    , -- in case this was mutated from a previous input, let's hold their hit counts to compare later
      previousInputBucketedEdgeHitCounts : BucketedEdgeHitCounts
    }
