module Test.Coverage.EdgeId exposing (EdgeId)

{-| Heavily inspired by AFL: <https://lcamtuf.coredump.cx/afl/technical_details.txt>

EdgeId is an unique-ish integer denoting a directed edge in the code path,
to index into EdgeHitCounts and SeenHits.

The edges being directed means that code paths:

  - A -> B -> C -> D
  - A -> C -> B -> D

will be considered different.


## How these are created:

Every interesting "basic block" (a piece of code connected via jumps, like
function calls or conditionals) gets instrumented with a `PointId`: an
uniformly random Int32.

These are useful for line coverage tracking, but also for edge tracking (for
coverage-guided fuzzers):

When hitting the patched code for `Test.Coverage.track pointId`, the previous
and current pointId get converted into a single integer by shifting and XORing:



    {-
       currentLocation = // pointId we just hit, uniformly random 32bit integer
       edge = (currentLocation ^ previousLocation) % 65536; // index into the array!
       previousLocation = currentLocation >> 1; // this gives us the directionality
    -}

-}


type alias EdgeId =
    Int
