module Corpus.KnownBugs exposing (cases)

{-| Replications of defects that property-based testing is known for catching:
data structure implementations with one realistic mistake in them.

These are the cases that carry the F signal. Each needs the fuzzer to produce a
_sequence_ of operations or keys that interact in a particular way, so no single
value falsifies them and the case count actually measures how well generation
searches.

Each case says where its bug comes from. Two are faithful to named, documented
mutations; two are classic bug _classes_ rather than one famous incident, and are
labelled as such.

-}

import Bitwise
import Corpus.Case as Case exposing (Case)
import Dict
import Fuzz exposing (Fuzzer)
import Set


cases : List Case
cases =
    [ bstDeleteTwoChildren
    , rbtMissingRotation
    , intervalsTouchingMerge
    , queueMissingInvariant
    , cacheTruncatedKey
    ]



-- BINARY SEARCH TREE


type Tree
    = Leaf
    | Node Tree Int Tree


treeInsert : Int -> Tree -> Tree
treeInsert key tree =
    case tree of
        Leaf ->
            Node Leaf key Leaf

        Node left k right ->
            if key < k then
                Node (treeInsert key left) k right

            else if key > k then
                Node left k (treeInsert key right)

            else
                tree


treeToList : Tree -> List Int
treeToList tree =
    case tree of
        Leaf ->
            []

        Node left k right ->
            treeToList left ++ (k :: treeToList right)


{-| Deleting a node with two children requires replacing it with the _minimum of
its right subtree_. This implementation uses the right child's key instead, which
is the same thing exactly when that child has no left subtree — so it is correct
for most trees and wrong for the rest.

From the BST suite in John Hughes, _How to Specify It! A Guide to Writing
Properties of Pure Functions_ (2019), where `delete` is the operation whose bugs
need several keys in a specific arrangement before they show.

-}
treeDelete : Int -> Tree -> Tree
treeDelete key tree =
    case tree of
        Leaf ->
            Leaf

        Node left k right ->
            if key < k then
                Node (treeDelete key left) k right

            else if key > k then
                Node left k (treeDelete key right)

            else
                case ( left, right ) of
                    ( Leaf, _ ) ->
                        right

                    ( _, Leaf ) ->
                        left

                    ( _, Node _ rightKey _ ) ->
                        -- The bug.
                        Node left rightKey (treeDelete rightKey right)


bstDeleteTwoChildren : Case
bstDeleteTwoChildren =
    Case.fails
        { name = "bst/delete-two-children"
        , category = "known-bug"

        {- Exposing the bug needs four nodes: a root with two children whose
           right child has a left child. With distinct keys the smallest such
           set is {0,1,2,3} with 1 at the root, and 3 has to be inserted before
           2 (otherwise 2 becomes 3's parent, the right child's left subtree is
           empty, and the buggy replacement happens to be the correct one). That
           leaves three insertion orders, all building the same tree.

           Simplifying currently reaches the right *shape* every time but never
           reduces the keys, stopping at things like `([5,0,7,6],5)` -- so this
           case reports 0% optimal today. That's a real shortfall, not a broken
           expectation.
        -}
        , minima =
            [ "([1,0,3,2],1)"
            , "([1,3,0,2],1)"
            , "([1,3,2,0],1)"
            ]
        , budget = Nothing
        }
        (Fuzz.pair
            (Fuzz.list (Fuzz.intRange 0 40))
            (Fuzz.intRange 0 40)
        )
        (\( keys, target ) ->
            let
                tree : Tree
                tree =
                    List.foldl treeInsert Leaf keys
            in
            -- Model-based: deleting a key should do to the in-order traversal
            -- exactly what filtering it out of the traversal does.
            treeToList (treeDelete target tree)
                == List.filter (\k -> k /= target) (treeToList tree)
        )



-- RED-BLACK TREE


type Color
    = R
    | B


type RBTree
    = E
    | T Color RBTree Int RBTree


{-| Okasaki's `balance` has four rebalancing cases. This one is missing the
left-right case: a black node whose red left child has a red _right_ child. The
three remaining cases still rebalance most insertions, so the invariant only
breaks for particular insertion orders.

This is one of the documented `balance` mutations from the red-black tree suite
used to benchmark property-based testing tools (Okasaki's implementation; the
mutation set is the one in Etna, Shi et al., PLDI 2023).

-}
balance : Color -> RBTree -> Int -> RBTree -> RBTree
balance color left key right =
    case ( color, left, right ) of
        ( B, T R (T R a x b) y c, _ ) ->
            T R (T B a x b) y (T B c key right)

        -- Missing: ( B, T R a x (T R b y c), _ ) -> T R (T B a x b) y (T B c key right)
        ( B, _, T R (T R b y c) z d ) ->
            T R (T B left key b) y (T B c z d)

        ( B, _, T R b y (T R c z d) ) ->
            T R (T B left key b) y (T B c z d)

        _ ->
            T color left key right


rbInsert : Int -> RBTree -> RBTree
rbInsert x tree =
    case ins x tree of
        T _ a y b ->
            T B a y b

        E ->
            E


ins : Int -> RBTree -> RBTree
ins x tree =
    case tree of
        E ->
            T R E x E

        T color a y b ->
            if x < y then
                balance color (ins x a) y b

            else if x > y then
                balance color a y (ins x b)

            else
                tree


{-| No red node may have a red child.
-}
noRedRed : RBTree -> Bool
noRedRed tree =
    case tree of
        E ->
            True

        T R (T R _ _ _) _ _ ->
            False

        T R _ _ (T R _ _ _) ->
            False

        T _ a _ b ->
            noRedRed a && noRedRed b


{-| Every path from the root to a leaf passes the same number of black nodes.
`Nothing` means it doesn't hold.
-}
blackHeight : RBTree -> Maybe Int
blackHeight tree =
    case tree of
        E ->
            Just 0

        T color a _ b ->
            Maybe.andThen
                (\heightA ->
                    Maybe.andThen
                        (\heightB ->
                            if heightA == heightB then
                                Just
                                    (heightA
                                        + (if color == B then
                                            1

                                           else
                                            0
                                          )
                                    )

                            else
                                Nothing
                        )
                        (blackHeight b)
                )
                (blackHeight a)


rbtMissingRotation : Case
rbtMissingRotation =
    Case.fails
        { name = "rbt/missing-rotation"
        , category = "known-bug"

        -- The minimal left-right trigger: insert 2, then 0, then 1.
        , minima = [ "[2,0,1]" ]
        , budget = Nothing
        }
        {- Bounded length on purpose. With `Fuzz.list`'s ~16 elements a random
           key sequence almost always contains a left-right trigger somewhere, and
           the case is falsified by the first value it ever sees. Short sequences
           make finding the trigger the actual work, which is what we're here to
           measure -- and it's how the Etna suites bound their inputs too.
        -}
        (Fuzz.listOfLengthBetween 0 6 (Fuzz.intRange 0 9))
        (\keys ->
            let
                tree : RBTree
                tree =
                    List.foldl rbInsert E keys
            in
            noRedRed tree && blackHeight tree /= Nothing
        )



-- INTERVAL MERGING


{-| Merging a list of closed integer intervals. The fold treats two intervals as
overlapping when `lo < previousHi`, where it should be `<=`: intervals that meet
at exactly one point, like `(1,2)` and `(2,3)`, are left unmerged.

A bug class rather than one famous incident, but a persistent one — off-by-one at
the boundary of a range comparison, where the defect needs two intervals to share
an endpoint _exactly_.

-}
mergeIntervals : List ( Int, Int ) -> List ( Int, Int )
mergeIntervals intervals =
    let
        step : ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
        step ( lo, hi ) acc =
            case acc of
                ( previousLo, previousHi ) :: rest ->
                    if lo < previousHi then
                        -- The bug: should be `lo <= previousHi`.
                        ( previousLo, max previousHi hi ) :: rest

                    else
                        ( lo, hi ) :: acc

                [] ->
                    [ ( lo, hi ) ]
    in
    intervals
        |> List.map
            (\( lo, hi ) ->
                if lo <= hi then
                    ( lo, hi )

                else
                    ( hi, lo )
            )
        |> List.sortBy Tuple.first
        |> List.foldl step []
        |> List.reverse


intervalsTouchingMerge : Case
intervalsTouchingMerge =
    Case.fails
        { name = "intervals/touching-merge"
        , category = "known-bug"

        -- Two degenerate intervals that touch at a point and aren't merged.
        , minima = [ "[(0,0),(0,0)]" ]
        , budget = Nothing
        }
        (Fuzz.list (Fuzz.pair (Fuzz.intRange 0 30) (Fuzz.intRange 0 30)))
        (\intervals ->
            -- No two intervals in the result may overlap or touch.
            let
                merged : List ( Int, Int )
                merged =
                    mergeIntervals intervals
            in
            List.map2 (\( _, previousHi ) ( lo, _ ) -> lo > previousHi)
                merged
                (List.drop 1 merged)
                |> List.all identity
        )



-- BATCHED QUEUE


type Queue
    = Queue (List Int) (List Int)


type QueueOp
    = Push Int
    | Pop


{-| Okasaki's batched queue keeps a front list and a reversed back list, with the
invariant "the front is empty only if the whole queue is". `push` restores it,
`pop` forgets to — so a queue that still holds elements reports itself empty.

A bug class rather than one famous incident, and the textbook motivation for
model-based/stateful property testing: no single operation exposes it, you need
push, push, pop, pop.

-}
queuePush : Int -> Queue -> Queue
queuePush x (Queue front back) =
    checkQueue (Queue front (x :: back))


queuePop : Queue -> Maybe ( Int, Queue )
queuePop (Queue front back) =
    case front of
        x :: rest ->
            -- The bug: should be `Just ( x, checkQueue (Queue rest back) )`.
            Just ( x, Queue rest back )

        [] ->
            Nothing


checkQueue : Queue -> Queue
checkQueue (Queue front back) =
    case front of
        [] ->
            Queue (List.reverse back) []

        _ ->
            Queue front back


queueMissingInvariant : Case
queueMissingInvariant =
    Case.fails
        { name = "queue/missing-invariant"
        , category = "known-bug"

        -- The textbook trigger: two pushes, then two pops.
        , minima = [ "[Push 0,Push 0,Pop,Pop]" ]
        , budget = Nothing
        }
        -- Bounded for the same reason as `rbt/missing-rotation`.
        (Fuzz.listOfLengthBetween 0
            6
            (Fuzz.oneOf
                [ Fuzz.map Push (Fuzz.intRange 0 9)
                , Fuzz.constant Pop
                ]
            )
        )
        (\ops ->
            -- Model-based: run the same operations against a plain list and
            -- require the same sequence of popped values.
            let
                run : List QueueOp -> Queue -> List Int -> Bool
                run remaining queue model =
                    case remaining of
                        [] ->
                            True

                        (Push x) :: rest ->
                            run rest (queuePush x queue) (model ++ [ x ])

                        Pop :: rest ->
                            case ( queuePop queue, model ) of
                                ( Just ( x, newQueue ), expected :: newModel ) ->
                                    (x == expected) && run rest newQueue newModel

                                ( Nothing, [] ) ->
                                    run rest queue model

                                _ ->
                                    False
            in
            run ops (Queue [] []) []
        )



-- TRUNCATED CACHE KEY


{-| A uniform 24-bit integer.

`Fuzz.intRange` buckets above 255 (it draws 4-, 8-, 16- or 32-bit values with
weights 4, 8, 2, 1 and folds them into the range), so the probability of hitting
any particular value in a wide range is awkward to reason about. Composing three
uniform byte draws avoids that: every value in `0..2^24-1` is equally likely, so
the detection probability below is analytic rather than guessed.

-}
uniform24 : Fuzzer Int
uniform24 =
    let
        byte : Fuzzer Int
        byte =
            Fuzz.intRange 0 255
    in
    Fuzz.map3 (\a b c -> a * 65536 + b * 256 + c) byte byte byte


{-| A cache that keys entries on the low 19 bits of a 24-bit id, so two distinct
ids sharing those bits collide and one silently overwrites the other.

Truncating an identifier to fit a narrower field is a real and recurring bug
class. It's here for a second reason too: it's the one case in the corpus tuned so
that **random generation does not always find it**, which is what keeps `found`
from being a constant 100% across the whole corpus.

With ids uniform over 2^24, a key space of 2^19 and 4..12 ids per run, a single
run collides with probability on the order of `28 / 2^19`, so a 10 000-run budget
finds the defect about half the time. Measured at 300 seeds: **55% found**, median
3600 runs when it does. (The 18-bit version of this mask measured 80%, so the knob
is roughly a factor of two in `found` per bit.)

The axis is two-sided from there: a change that searches a wide domain better
pushes `found` up, one that searches worse pushes it down, and either direction is
visible. If it drifts to an extreme, retune the mask rather than this case's
budget -- the budget has to stay comparable across cases for detection rates to
mean anything.

-}
truncatedCacheKey : Int -> Int
truncatedCacheKey id =
    -- The bug: should be the whole id.
    Bitwise.and 0x0007FFFF id


cacheEntryCount : List Int -> Int
cacheEntryCount ids =
    ids
        |> List.foldl (\id acc -> Dict.insert (truncatedCacheKey id) id acc) Dict.empty
        |> Dict.size


cacheTruncatedKey : Case
cacheTruncatedKey =
    Case.fails
        { name = "cache/truncated-key"
        , category = "known-bug"

        {- Left empty on purpose. The optimum is a pair of ids differing only
           above bit 18, and which pair simplifying can reach depends on the run
           it found; `|run|` and `sum(run)` carry the quality signal instead.
        -}
        , minima = []
        , budget = Nothing
        }
        (Fuzz.listOfLengthBetween 4 12 uniform24)
        (\ids ->
            -- Distinct ids must occupy distinct cache slots. Repeated ids are
            -- fine: they drop out of both sides.
            cacheEntryCount ids == Set.size (Set.fromList ids)
        )
