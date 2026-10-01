module Occupancy exposing
    ( Occupancy
    , childOf
    , empty
    , exhaustedChildren
    , isExhausted
    , isOpen
    , markCovered
    )

{-| What part of a fuzzer's input space has already been covered.

Every fuzzer bottoms out in `rollDice maxValue _`, so the inputs it can produce
form a tree with `maxValue + 1` children per node. This records which subtrees of
that tree have been completely explored, so a draw can decline them.

That one idea does the work of three separate mechanisms:

  - **No duplicates.** A covered leaf is never offered again, so no input is ever
    tested twice — without hashing runs or keeping a set of them.

  - **Early termination.** When the root is covered there is provably nothing
    left to test, whatever run count was requested.

  - **Partial coverage.** The interesting case. Given

        Fuzz.oneOf
            [ Fuzz.map Err Fuzz.string
            , Fuzz.map Ok Fuzz.bool
            ]

    `oneOf` spends half its runs re-testing `Ok True` and `Ok False`. Here the
    `Ok` subtree is covered after two draws and then stops being offered, so its
    share redistributes to `Err` and a defect in the `Err` branch is found about
    twice as fast. Neither whole-tree enumeration nor per-run deduplication gets
    this: the tree is infinite, so there is nothing to enumerate, and the runs
    aren't duplicates until they have already been generated.

The important property is that **conditioning only happens once something is
actually covered**. Until then the draw is exactly the draw that would have
happened anyway, so fuzzers with large domains behave identically — same seed,
same values, no probe, no mode switch.


## Bounding the cost

Tracking everything would cost memory and time proportional to the number of
runs, for fuzzers that can never exhaust anything. Three bounds prevent that: a
limit on how deep coverage is recorded, on how wide a node may be to be worth
recording, and on how many nodes are recorded in total. All three are deliberately
**shape-agnostic** — none may depend on which path through the tree happened to be
drawn first. See [`markCovered`](#markCovered) and [`maxDepth`](#maxDepth).

-}

import Dict exposing (Dict)
import RandomRun exposing (RandomRun)
import Set exposing (Set)


type Occupancy
    = {- Partly covered, and worth tracking. The set of covered children is
         maintained as we go rather than derived on demand: it's read on every
         draw, and folding the whole child dict each time makes a wide node cost
         O(width) per draw.
      -}
      Partial Int (Dict Int Occupancy) (Set Int)
    | {- Not worth tracking. Never exhausts, children never recorded. -} Open
    | {- Every input reachable from here has been tested. -} Covered


empty : Occupancy
empty =
    Partial 0 Dict.empty Set.empty


{-| Whether this part of the tree is untracked, so a draw need not consult it.
-}
isOpen : Occupancy -> Bool
isOpen occupancy =
    case occupancy of
        Open ->
            True

        _ ->
            False


isExhausted : Occupancy -> Bool
isExhausted occupancy =
    case occupancy of
        Covered ->
            True

        _ ->
            False


{-| The child values at this node that are fully covered, and so must not be
drawn again.

Empty for `Open` and for anything not yet visited, which is the common case —
that's what keeps the cost near zero until coverage actually happens.

-}
exhaustedChildren : Occupancy -> Set Int
exhaustedChildren occupancy =
    case occupancy of
        Partial _ _ covered ->
            covered

        Open ->
            Set.empty

        Covered ->
            Set.empty


{-| Descend to a child, so a draw can carry its position in the tree along with
it instead of walking from the root every time.
-}
childOf : Int -> Int -> Occupancy -> Occupancy
childOf value maxValue occupancy =
    case occupancy of
        Partial _ children _ ->
            case Dict.get value children of
                Just child ->
                    child

                Nothing ->
                    {- Transient: never stored, and has no covered children, so it
                       declines nothing. `markCovered` makes the real decision.
                    -}
                    Partial maxValue Dict.empty Set.empty

        Open ->
            Open

        Covered ->
            Covered


{-| Record that the input described by this run has been tested, and report how
much of the node budget is left.

`maxes` carries the branching factor at each position, which is what lets a node
know when all of its children are accounted for and it can collapse to `Covered`.

-}
markCovered : Int -> RandomRun -> List Int -> Occupancy -> Occupancy
markCovered runs run maxes occupancy =
    case markCoveredHelp runs maxDepth 0 run maxes occupancy of
        Nothing ->
            -- Unchanged, so hand back the same value rather than a copy.
            occupancy

        Just updated ->
            updated


{-| How far down a run coverage is recorded.

Without this the cost of recording a run grows with its length: `Fuzz.string` runs
are tens of draws long and `Fuzz.filter` multiplies that by its retries.

Shallow is enough for what this is for. The coverable parts of a fuzzer are near
the root -- `bool` is one draw, `pair bool bool` two, and the `Ok bool` branch of a
`oneOf` is two. Anything deeper is treated as never exhausting, which is the safe
direction: we decline nothing we shouldn't.

Three also replaces an explicit budget on tracked nodes. That budget existed to
bound products of narrow choices, but with nodes wider than
[`maxTrackedWidth`](#worthTracking) refused outright, depth alone bounds the
structure: at most `8 + 8^2 + 8^3` nodes, and in practice far fewer since nodes are
only created along runs that actually happen. Dropping the counter removes a tuple
allocated on every single run, which measured as a real cost for fuzzers whose runs
are cheap.

-}
maxDepth : Int
maxDepth =
    3


{-| `Nothing` means nothing changed.

This is what keeps the cost flat rather than growing with the number of runs. A
child that is already `Open` or `Covered` cannot change, so recursing into it and
then reinserting it would copy a path through the dictionary on every single run
for no reason -- and for a fuzzer that can never exhaust anything, _every_ run is
that case. Benchmarking put the whole mechanism at 0.16x of baseline on
`filter/even` with this missing, essentially all of it here.

-}
markCoveredHelp : Int -> Int -> Int -> RandomRun -> List Int -> Occupancy -> Maybe Occupancy
markCoveredHelp runs depthLeft index run maxes occupancy =
    case maxes of
        [] ->
            {- Out of bounds to walk. Two different situations, and they mean
               opposite things:

                 - the run ended here too, so this is a leaf and it's now covered;
                 - the run continues, which means recording stopped partway
                   because the draw entered an untracked region. Nothing to record.
            -}
            if index >= RandomRun.length run then
                case occupancy of
                    Covered ->
                        Nothing

                    _ ->
                        Just Covered

            else
                Nothing

        maxValue :: restOfMaxes ->
            case ( RandomRun.get index run, occupancy ) of
                ( Nothing, _ ) ->
                    -- Shouldn't happen: bounds are recorded alongside the run.
                    Nothing

                ( Just _, Open ) ->
                    Nothing

                ( Just _, Covered ) ->
                    Nothing

                ( Just value, Partial _ children covered ) ->
                    if depthLeft <= 0 then
                        -- Too deep to be worth recording.
                        Just Open

                    else if not (worthTracking runs (maxValue + 1)) then
                        {- Checked on the width of *this* node, not of the child
                           we're about to create. This node is the one that
                           accumulates a child per distinct value, so this is
                           where the cost lives. Testing the child instead left a
                           wide node recording a leaf for every value it saw,
                           which measured as a quarter of baseline throughput.
                        -}
                        Just Open

                    else
                        let
                            existing : Occupancy
                            existing =
                                case Dict.get value children of
                                    Just child ->
                                        child

                                    Nothing ->
                                        newChild runs restOfMaxes
                        in
                        case markCoveredHelp runs (depthLeft - 1) (index + 1) run restOfMaxes existing of
                            Nothing ->
                                -- Nothing below changed, so nothing here did.
                                Nothing

                            Just updatedChild ->
                                let
                                    updatedCovered : Set Int
                                    updatedCovered =
                                        if isExhausted updatedChild then
                                            Set.insert value covered

                                        else
                                            covered
                                in
                                if Set.size updatedCovered == maxValue + 1 then
                                    {- Collapsing keeps the structure small, and is
                                       what propagates coverage up to the root.
                                    -}
                                    Just Covered

                                else
                                    Just (Partial maxValue (Dict.insert value updatedChild children) updatedCovered)


{-| Whether a newly discovered node is worth tracking, and what that costs.

Two bounds, both shape-agnostic:

  - **Its own branching factor** has to be coverable within the run count. A
    `uniformInt 0 1000` node needs around `1001 * ln 1001` draws to fill, so
    tracking it means paying for a thousand runs and never collapsing.
  - **A budget on tracked nodes overall**, because branching factor alone says
    nothing about a _product_: `pair (intRange 0 30) (intRange 0 30)` is two
    perfectly narrow nodes with 961 leaves between them.

The obvious third measure — the size of the subtree below the node — is better
than either and unusable as a rule. The subtree below a choice depends on the
choice, so the answer changes with whichever run reaches the node first. For
`oneOf [ Err string, Ok bool ]` an `Err` run would mark the root untracked and
lose the `Ok` branch, which is the one case this mechanism exists for.

-}
newChild : Int -> List Int -> Occupancy
newChild runs restOfMaxes =
    case restOfMaxes of
        [] ->
            {- A leaf, created empty rather than already covered so that the step
               to `Covered` is a real change and propagates: reporting it covered
               on creation makes the parent see "nothing changed" and never add it
               to its covered set, so nothing ever collapses.
            -}
            Partial 0 Dict.empty Set.empty

        childMax :: _ ->
            if worthTracking runs (childMax + 1) then
                Partial childMax Dict.empty Set.empty

            else
                Open


{-| Whether a node this wide is worth keeping coverage records for.

Two conditions, and the second is the one that matters in practice.

_Coverable_: random draws repeat, so covering `n` distinct values takes about
`n * ln n` draws (the coupon collector's problem), not `n`. There is no point
tracking a node that cannot fill within the run count.

_Worth it_: recording coverage means updating a persistent tree, which allocates
along the path it copies. A fuzz run can be as cheap as a third of a microsecond,
and a dozen node allocations cost more than that -- so tracking only pays where
coverage completes almost immediately and the saving is then total.

That second condition is what the measurements insisted on. With only the
coupon-collector test, `pair (intRange 0 30) (intRange 0 30)` and
`intRange 0 100 |> filter ...` both qualify -- 961 and 101 values, both coverable
inside 1000 runs -- and both came out at around a fifth of baseline throughput,
while the domains that matter (`bool`, `order`, `oneOfValues`, and the small
branch of a `oneOf`) are all under eight values and gain 45-90x.

So the width limit is deliberately severe. It gives up mid-sized domains, which we
would otherwise be able to cover completely, in exchange for never making anything
slower.

-}
worthTracking : Int -> Int -> Bool
worthTracking runs size =
    {- `size < 1` is the sentinel a sparse draw records: its generator doesn't
       produce every value in range, so the set of children isn't knowable from the
       bound and the node must never be tracked or declared covered.
    -}
    size >= 1 && size * bitsNeeded size * trackingMargin <= runs


trackingMargin : Int
trackingMargin =
    8


{-| A node wider than this is not tracked. See [`worthTracking`](#worthTracking).
-}
maxTrackedWidth : Int
maxTrackedWidth =
    8


bitsNeeded : Int -> Int
bitsNeeded n =
    bitsNeededHelp n 1


bitsNeededHelp : Int -> Int -> Int
bitsNeededHelp n acc =
    if n <= 1 then
        acc

    else
        bitsNeededHelp ((n + 1) // 2) (acc + 1)
