module Test.Coverage.SeenHits exposing (SeenHits)

{-| SeenHits track which edges we have seen globally over the course of running
a fuzz test with various inputs.

For what the edges are exactly, look at docs of Test.Coverage.EdgeHitCounts.

This data is later used to find the number of new paths (paths we haven't seen
during any previous test run).

-}

import Elm.Kernel.EdgeCoverage


type SeenHitsJs
    = SeenHitsJs


type alias SeenHits =
    Set Int


fromJs : SeenHitsJs -> SeenHits
fromJs =
    Elm.Kernel.EdgeCoverage.seenHitsFromJs
