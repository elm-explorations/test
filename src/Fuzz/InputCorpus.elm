module Fuzz.InputCorpus exposing (Input, InputCorpus, init, isEmpty)

import RandomRun exposing (RandomRun)
import Test.Coverage.EdgeHitCounts exposing (BucketedEdgeHitCounts)


type alias InputCorpus =
    { favoredFresh : List Input
    , favoredUsed : List Input
    , otherFresh : List Input
    , otherUsed : List Input
    }


init : InputCorpus
init =
    { favoredFresh = []
    , favoredUsed = []
    , otherFresh = []
    , otherUsed = []
    }


isEmpty : InputCorpus -> Bool
isEmpty corpus =
    (corpus.favoredFresh == [])
        && (corpus.favoredUsed == [])
        && (corpus.otherFresh == [])
        && (corpus.otherUsed == [])


type alias Input =
    { randomRun : RandomRun
    , durationMs : Int
    , -- in case this was mutated from a previous input, let's hold their hit counts to compare later
      previousInputBucketedEdgeHitCounts : BucketedEdgeHitCounts
    }
