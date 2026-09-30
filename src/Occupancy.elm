module Occupancy exposing
    ( Occupancy
    , childOf
    , empty
    , exhaustedChildren
    , isExhausted
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
runs, for fuzzers that can never exhaust anything. Two bounds prevent that, and
both are deliberately **shape-agnostic** — neither may depend on which path
through the tree happened to be drawn first. See [`markCovered`](#markCovered).

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
markCovered : Int -> Int -> RandomRun -> List Int -> Occupancy -> ( Occupancy, Int )
markCovered runs nodeBudget run maxes occupancy =
    markCoveredHelp runs nodeBudget (RandomRun.toList run) maxes occupancy


markCoveredHelp : Int -> Int -> List Int -> List Int -> Occupancy -> ( Occupancy, Int )
markCoveredHelp runs nodeBudget run maxes occupancy =
    case ( run, maxes ) of
        ( [], _ ) ->
            -- End of the run: this leaf is now covered.
            ( Covered, nodeBudget )

        ( value :: restOfRun, maxValue :: restOfMaxes ) ->
            case occupancy of
                Open ->
                    ( Open, nodeBudget )

                Covered ->
                    ( Covered, nodeBudget )

                Partial _ children covered ->
                    let
                        ( existing, budgetAfterCreate ) =
                            case Dict.get value children of
                                Just child ->
                                    ( child, nodeBudget )

                                Nothing ->
                                    newChild runs nodeBudget restOfMaxes

                        ( updatedChild, remainingBudget ) =
                            markCoveredHelp runs budgetAfterCreate restOfRun restOfMaxes existing

                        updatedCovered : Set Int
                        updatedCovered =
                            if isExhausted updatedChild then
                                Set.insert value covered

                            else
                                covered
                    in
                    if Set.size updatedCovered == maxValue + 1 then
                        {- Collapsing keeps the structure small, and is what
                           propagates coverage up towards the root. Cheap now that
                           the covered set is maintained: no fold over children.
                        -}
                        ( Covered, remainingBudget )

                    else
                        ( Partial maxValue (Dict.insert value updatedChild children) updatedCovered
                        , remainingBudget
                        )

        ( _ :: _, [] ) ->
            -- Shouldn't happen: a run and its bounds are recorded together.
            ( occupancy, nodeBudget )


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
newChild : Int -> Int -> List Int -> ( Occupancy, Int )
newChild runs nodeBudget restOfMaxes =
    case restOfMaxes of
        [] ->
            -- A leaf: nothing to track below it, and nothing to pay.
            ( Partial 0 Dict.empty Set.empty, nodeBudget )

        childMax :: _ ->
            if nodeBudget <= 0 || not (coverableWithin runs (childMax + 1)) then
                ( Open, nodeBudget )

            else
                ( Partial childMax Dict.empty Set.empty, nodeBudget - 1 )


{-| Whether a space of this many inputs can plausibly be covered by drawing
`runs` times at random.

Random draws repeat, so covering `n` distinct inputs takes about `n * ln n` draws
(the coupon collector's problem), not `n`. Tracking coverage only pays if that
fits in the budget; otherwise we carry the bookkeeping for the whole test and
never exhaust anything.

`n * log2 n` stands in for `n * ln n`, overestimating by about 1.44x, which errs
towards not tracking.

-}
coverableWithin : Int -> Int -> Bool
coverableWithin runs size =
    size <= runs && size * bitsNeeded size <= runs


bitsNeeded : Int -> Int
bitsNeeded n =
    bitsNeededHelp n 1


bitsNeededHelp : Int -> Int -> Int
bitsNeededHelp n acc =
    if n <= 1 then
        acc

    else
        bitsNeededHelp ((n + 1) // 2) (acc + 1)
