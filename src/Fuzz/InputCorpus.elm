module Fuzz.InputCorpus exposing (Input, InputCorpus, add, init, isEmpty)

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
    , durationMs : Float
    , bucketedEdgeHitCounts : BucketedEdgeHitCounts
    }


add : RandomRun -> Float -> BucketedEdgeHitCounts -> InputCorpus -> InputCorpus
add randomRun durationMs bucketedEdgeHitCounts corpus =
    let
        input : Input
        input =
            { randomRun = randomRun
            , durationMs = durationMs
            , bucketedEdgeHitCounts = bucketedEdgeHitCounts
            }
    in
    { otherFresh = input :: corpus.otherFresh

    -- the rest, unchanged (PERF: not using record update)
    , favoredFresh = corpus.favoredFresh
    , favoredUsed = corpus.favoredUsed
    , otherUsed = corpus.otherUsed
    }
