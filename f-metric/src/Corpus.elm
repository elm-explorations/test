module Corpus exposing (all)

{-| The whole F-metric corpus. See `Corpus.Case` for what an entry is and how to
add one.
-}

import Corpus.Case exposing (Case)
import Corpus.KnownBugs
import Corpus.ShrinkingChallenge
import Corpus.Synthetic


all : List Case
all =
    Corpus.KnownBugs.cases
        ++ Corpus.Synthetic.cases
        ++ Corpus.ShrinkingChallenge.cases
