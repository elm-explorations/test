module Test.Coverage.SeenHits exposing (SeenHits(..), anotherFn)

{-| SeenHits track which edges we have seen globally over the course of running
a fuzz test with various inputs.

For what the edges are exactly, look at docs of Test.Coverage.EdgeHitCounts.

This data is later used to find the number of new paths (paths we haven't seen
during any previous test run).

-}

import Elm.Kernel.EdgeCoverage
import Set exposing (Set)


type SeenHits
    = SeenHits


anotherFn : SeenHits -> Int
anotherFn =
    Elm.Kernel.EdgeCoverage.anotherFn
