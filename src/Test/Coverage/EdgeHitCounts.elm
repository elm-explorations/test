module Test.Coverage.EdgeHitCounts exposing (EdgeHitCounts, EdgeHitCountsJs, fromJs)

{-| EdgeHitCounts capture directed edges alongside code paths (basic blocks
instrumented with `let _ = Test.Coverage.track <pointId> in ...` by coverage
tooling).

On JS side, EdgeHitCountsJs is a TypedArray (fixed length, contiguous).
Specifically,

    -- new Uint8ClampedArray(65536)



Each index in this array is a possible edge: see docs for Test.Coverage.EdgeId.

The array holds a hit counter for each edge (an 8bit unsigned integer, so 0..255)
that we increment whenever we hit that edge. The clamped array type makes it so
that when we increment a path that was hit 255 times, we stay at 255 and don't
overflow to 0.

We'll get this EdgeHitCounts array for every input tested by a fuzz test, and
compare it against a global set of seen edges. Inputs can then be classified as
interesting (due to new paths uncovered) even if they don't cause a test to
fail. Interesting inputs get saved to the corpus and will later be mutated into
new inputs, which will eventually be picked by the machinery generating fuzz
test inputs.

There's a chance of collisions, but it was deemed acceptable. Can be tweaked
with the array size.

-}

import Elm.Kernel.EdgeCoverage


type EdgeHitCountsJs
    = EdgeHitCountsJs


type alias EdgeHitCounts =
    Dict Int Int


fromJs : EdgeHitCountsJs -> EdgeHitCounts
fromJs =
    Elm.Kerel.EdgeCoverage.edgeHitCountsFromJs
