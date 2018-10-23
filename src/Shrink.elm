module Shrink exposing
    ( Shrinker, shrink
    , noShrink, unit, bool, order, int, atLeastInt, float, atLeastFloat, char, atLeastChar, character, string, maybe, result, lazylist, list, array, tuple, tuple3
    , convert, keepIf, dropIf, merge, map, andMap
    )

{-| Library containing a collection of basic shrinkers and helper functions to
make your own.

Shrinking is part of fuzzing, and the provided fuzzers have shrinkers already
built into them. You really only have to write your own shrinkers if you use
`Fuzz.custom`.


## Quick Reference

  - [Shrinking Basics](#shrinking-basics)
  - [Readymade Shrinkers](#readymade-shrinkers)
  - [Functions on Shrinkers](#functions-on-shrinkers)
  - [What are Shrinkers and why do we need them?](#what-are-shrinkers-and-why-do-we-need-them)


## Shrinking Basics

@docs Shrinker, shrink


## Readymade Shrinkers

@docs noShrink, unit, bool, order, int, atLeastInt, float, atLeastFloat, char, atLeastChar, character, string, maybe, result, lazylist, list, array, tuple, tuple3


## Functions on Shrinkers

@docs convert, keepIf, dropIf, merge, map, andMap


## What are Shrinkers and why do we need them?

Fuzzers consist of two parts; a Generator and a Shrinker.

The Generator takes a random Seed as input and returns a random value of
the desired type, based on the Seed. When a test fails on one of those random
values, the shrinker takes the failing value and makes it smaller/simpler for
you so you can guess more easily what property of that value caused the test
to fail.

Shrinking is a way to try and find the "smallest", "simplest" example that
fails, in order to give the tester better feedback on what went wrong.

Shrinkers are functions that, given a failing value, offer "smaller", "simpler"
values to test against.


### What is "small" (or "simple")?

That's kind of arbitrary, and depends on what kind of values you're fuzzing.
When you write your own Shrinker, you decide what is small for the kind of data
you're testing with.

Let's say I'm writing a Fuzzer for binary trees:

    -- randomly-generated binary trees might soon become unreadable
    type Tree a
        = Node (Tree a) (Tree a)
        | Leaf a

Now let's say its random Generator produced the following tree that makes the
test fail:

    Node
        (Node
            (Node
                (Node
                    (Leaf 888)
                    (Leaf 9090)
                )
                (Node
                    (Leaf -1)
                    (Node
                        (Leaf 731)
                        (Node
                            (Leaf 9621)
                            (Leaf -12)
                        )
                    )
                )
            )
            (Node
                (Leaf -350)
                (Leaf 124)
            )
        )
        (Node
            (Leaf 45)
            (Node
                (Leaf 123)
                (Node
                    (Leaf 999111)
                    (Leaf -148148)
                )
            )
        )

This is a pretty big tree, there are many nodes and leaves, and it's difficult
to tell which is responsible for the failing. If we don't attempt to shrink it,
the developer will have a hard time pointing out why it fails.

Now let's pass it through a shrinker, and test the resulting value until we find
this new value that still fails the test:

    Leaf -1

Nice, looks like a negative number in a `Leaf` could be the issue.


### How does shrinking work?

A shrinker takes a value and returns a short list of smaller values.

Once elm-test finds a failing fuzz test, it tries to shrink the input using
the shrinker. We'll then try the smaller values as inputs to that test. If one
of the smaller values also fail, we continue shrinking from there instead.
Once the shrinker says that there are no smaller values, or no smaller values
fail the fuzz test, we stop shrinking.

It's helpful to think of Shrinkers as returning simpler values rather than
smaller values. For example, 1 is smaller/simpler than 47142, and -1 is
smaller/simpler than -47142.

Whether or not the shrunken value is actually smaller isn't that important,
as long as we aren't shrinking in a loop. The bool shrinker shrinks True to
False, but not vice versa. If it did, and your test failed no matter if this
variable was True or False, there would always be a smaller/simpler value, so
we'd never stop shrinking! We would just re-test the same values over and over
again, forever!


### How do I make my own Shrinkers?

Shrinkers are deterministic, since they do not have access to a random number
generator. It's the generator part of the fuzzer that's meant to find the rare
edge cases; it's the shrinkers job to make the failures as understandable as
possible.

Shrinkers have to return a LazyList, something that works a bit like a list.
That LazyList may or may not have another element each time we ask for one,
and doesn't necessarily have them all committed to memory. That allows it to
take less space (interesting since there may be quite a lot of elements).

That LazyList should also provide a finite number of shrunk values (if it
provided an infinite number of them, tests using it might continue indefinitely
at the shrinking phase).

Shrinkers must never shrink values in a circle, like:

    loopinBooleanShrinker True == [ False ]

    loopinBooleanShrinker False == [ True ]

Doing so will also result in tests looping indefinitely, testing and re-testing
the same values in a circle.

-}

import Array exposing (Array)
import Char
import Lazy exposing (Lazy, force, lazy)
import Lazy.List exposing (LazyList, append, cons, empty)
import List
import String


{-| The shrinker type.
A shrinker is a function that takes a value and returns a lazy list of values
that are in some sense "smaller" than the given value. If no such values exist,
then the shrinker should just return the empty list.
-}
type alias Shrinker a =
    a -> LazyList a


{-| Perform shrinking. Takes a predicate that returns `True` if you want
shrinking to continue (most likely the failing test for which we are attempting
to shrink the value). Also takes the shrinker and the value to shrink.

It returns the shrunken value, or the input value if no shrunken values that
satisfy the predicate are found.

-}
shrink : (a -> Bool) -> Shrinker a -> a -> a
shrink keepShrinking shrinker originalVal =
    let
        helper lazyList val =
            case force lazyList of
                Lazy.List.Nil ->
                    val

                Lazy.List.Cons head tail ->
                    if keepShrinking head then
                        helper (shrinker head) head

                    else
                        helper tail val
    in
    helper (shrinker originalVal) originalVal


{-| Perform no shrinking. Equivalent to the empty lazy list.
-}
noShrink : Shrinker a
noShrink _ =
    empty


{-| Shrink the empty tuple. Equivalent to `noShrink`.
-}
unit : Shrinker ()
unit =
    noShrink


{-| Shrinker of bools.
-}
bool : Shrinker Bool
bool b =
    case b of
        True ->
            cons False empty

        False ->
            empty


{-| Shrinker of `Order` values.
-}
order : Shrinker Order
order o =
    case o of
        GT ->
            cons EQ (cons LT empty)

        LT ->
            cons EQ empty

        EQ ->
            empty


{-| Shrinker of integers.
-}
int : Shrinker Int
int n =
    if n < 0 then
        cons -n (Lazy.List.map ((*) -1) (seriesInt 0 -n))

    else
        seriesInt 0 n


{-| Construct a shrinker of ints which considers the given int to
be most minimal.
-}
atLeastInt : Int -> Shrinker Int
atLeastInt min n =
    if n < 0 && n >= min then
        cons -n (Lazy.List.map ((*) -1) (seriesInt 0 -n))

    else
        seriesInt (max 0 min) n


{-| Shrinker of floats.
-}
float : Shrinker Float
float n =
    if n < 0 then
        cons -n (Lazy.List.map ((*) -1) (seriesFloat 0 -n))

    else
        seriesFloat 0 n


{-| Construct a shrinker of floats which considers the given float to
be most minimal.
-}
atLeastFloat : Float -> Shrinker Float
atLeastFloat min n =
    if n < 0 && n >= min then
        cons -n (Lazy.List.map ((*) -1) (seriesFloat 0 -n))

    else
        seriesFloat (max 0 min) n


{-| Shrinker of chars.
-}
char : Shrinker Char
char =
    convert Char.fromCode Char.toCode int


{-| Construct a shrinker of chars which considers the given char to
be most minimal.
-}
atLeastChar : Char -> Shrinker Char
atLeastChar ch =
    convert Char.fromCode Char.toCode (atLeastInt (Char.toCode ch))


{-| Shrinker of chars which considers the empty space as the most
minimal char and omits the control key codes.

Equivalent to:

    atLeastChar (Char.fromCode 32)

-}
character : Shrinker Char
character =
    atLeastChar (Char.fromCode 32)


{-| Shrinker of strings. Considers the empty string to be the most
minimal string and the space to be the most minimal char.

Equivalent to:

    convert String.fromList String.toList (list character)

-}
string : Shrinker String
string =
    convert String.fromList String.toList (list character)


{-| Maybe shrinker constructor.
Takes a shrinker of values and returns a shrinker of Maybes.
-}
maybe : Shrinker a -> Shrinker (Maybe a)
maybe shrinker m =
    case m of
        Just a ->
            cons Nothing (Lazy.List.map Just (shrinker a))

        Nothing ->
            empty


{-| Result shrinker constructor. Takes a shrinker of errors and a shrinker of
values and returns a shrinker of Results.
-}
result : Shrinker error -> Shrinker value -> Shrinker (Result error value)
result shrinkError shrinkValue r =
    case r of
        Ok value ->
            Lazy.List.map Ok (shrinkValue value)

        Err error ->
            Lazy.List.map Err (shrinkError error)


{-| Lazy List shrinker constructor. Takes a shrinker of values and returns a
shrinker of Lazy Lists. The lazy list being shrunk must be finite. (I mean
really, how do you shrink infinity?)
-}
lazylist : Shrinker a -> Shrinker (LazyList a)
lazylist shrinker l =
    lazy <|
        \() ->
            let
                n : Int
                n =
                    Lazy.List.length l

                shrinkOneHelp : LazyList a -> LazyList (LazyList a)
                shrinkOneHelp lst =
                    lazy <|
                        \() ->
                            case force lst of
                                Lazy.List.Nil ->
                                    force empty

                                Lazy.List.Cons x xs ->
                                    force
                                        (append (Lazy.List.map (\val -> cons val xs) (shrinker x))
                                            (Lazy.List.map (cons x) (shrinkOneHelp xs))
                                        )

                removes : Int -> Int -> Shrinker (LazyList a)
                removes k_ n_ l_ =
                    lazy <|
                        \() ->
                            if k_ > n_ then
                                force empty

                            else if Lazy.List.isEmpty l_ then
                                force (cons empty empty)

                            else
                                let
                                    first =
                                        Lazy.List.take k_ l_

                                    rest =
                                        Lazy.List.drop k_ l_
                                in
                                force <|
                                    cons rest (Lazy.List.map (append first) (removes k_ (n_ - k_) rest))
            in
            force <|
                append
                    (Lazy.List.andThen (\k -> removes k n l)
                        (Lazy.List.takeWhile (\x -> x > 0) (Lazy.List.iterate (\num -> num // 2) n))
                    )
                    (shrinkOneHelp l)


{-| List shrinker constructor.
Takes a shrinker of values and returns a shrinker of Lists.
-}
list : Shrinker a -> Shrinker (List a)
list shrinker =
    convert Lazy.List.toList Lazy.List.fromList (lazylist shrinker)


{-| Array shrinker constructor.
Takes a shrinker of values and returns a shrinker of Arrays.
-}
array : Shrinker a -> Shrinker (Array a)
array shrinker =
    convert Lazy.List.toArray Lazy.List.fromArray (lazylist shrinker)


{-| 2-Tuple shrinker constructor.
Takes a tuple of shrinkers and returns a shrinker of tuples.
-}
tuple : ( Shrinker a, Shrinker b ) -> Shrinker ( a, b )
tuple ( shrinkA, shrinkB ) ( a, b ) =
    append (Lazy.List.map (Tuple.pair a) (shrinkB b))
        (append (Lazy.List.map (\first -> ( first, b )) (shrinkA a))
            (Lazy.List.map2 Tuple.pair (shrinkA a) (shrinkB b))
        )


{-| 3-Tuple shrinker constructor.
Takes a tuple of shrinkers and returns a shrinker of tuples.
-}
tuple3 : ( Shrinker a, Shrinker b, Shrinker c ) -> Shrinker ( a, b, c )
tuple3 ( shrinkA, shrinkB, shrinkC ) ( a, b, c ) =
    append (Lazy.List.map (\c1 -> ( a, b, c1 )) (shrinkC c))
        (append (Lazy.List.map (\b2 -> ( a, b2, c )) (shrinkB b))
            (append (Lazy.List.map (\a2 -> ( a2, b, c )) (shrinkA a))
                (append (Lazy.List.map2 (\b2 c2 -> ( a, b2, c2 )) (shrinkB b) (shrinkC c))
                    (append (Lazy.List.map2 (\a2 c2 -> ( a2, b, c2 )) (shrinkA a) (shrinkC c))
                        (append (Lazy.List.map2 (\a2 b2 -> ( a2, b2, c )) (shrinkA a) (shrinkB b))
                            (Lazy.List.map3 (\a2 b2 c2 -> ( a2, b2, c2 )) (shrinkA a) (shrinkB b) (shrinkC c))
                        )
                    )
                )
            )
        )



----------------------
-- HELPER FUNCTIONS --
----------------------


{-| Convert a Shrinker of a's into a Shrinker of b's using two inverse functions.
)
If you use this function as follows:

    shrinkerB =
        convert f g shrinkerA

Make sure that:

    `f(g(x)) == x` for all x
    -- (putting something into g then feeding the output into f must give back
    -- just that original something, whatever it is)

Or else this process will generate garbage.

-}
convert : (a -> b) -> (b -> a) -> Shrinker a -> Shrinker b
convert f g shrinker b =
    Lazy.List.map f (shrinker (g b))


{-| Filter out the results of a shrinker. The resulting shrinker
will only produce shrinks which satisfy the given predicate.
-}
keepIf : (a -> Bool) -> Shrinker a -> Shrinker a
keepIf predicate shrinker a =
    Lazy.List.keepIf predicate (shrinker a)


{-| Filter out the results of a shrinker. The resulting shrinker
will only throw away shrinks which satisfy the given predicate.
-}
dropIf : (a -> Bool) -> Shrinker a -> Shrinker a
dropIf predicate =
    keepIf (not << predicate)


{-| Merge two shrinkers. Generates all the values in the first
shrinker, and then all the non-duplicated values in the second
shrinker.
-}
merge : Shrinker a -> Shrinker a -> Shrinker a
merge shrink1 shrink2 a =
    Lazy.List.unique (append (shrink1 a) (shrink2 a))


{-| Re-export of `Lazy.List.map`
This is useful in order to compose shrinkers, especially when used in
conjunction with `andMap`. For example:

    type alias Vector =
        { x : Float
        , y : Float
        , z : Float
        }

    vector : Shrinker Vector
    vector { x, y, z } =
        Vector
            `map` float x
            `andMap` float y
            `andMap` float z

-}
map : (a -> b) -> LazyList a -> LazyList b
map =
    Lazy.List.map


{-| Apply a lazy list of functions on a lazy list of values.

The argument order is so that it is easy to use in `|>` chains.

-}
andMap : LazyList a -> LazyList (a -> b) -> LazyList b
andMap =
    Lazy.List.andMap



-----------------------
-- PRIVATE FUNCTIONS --
-----------------------


seriesInt : Int -> Int -> LazyList Int
seriesInt low high =
    if low >= high then
        empty

    else if low == high - 1 then
        cons low empty

    else
        let
            low_ =
                low + ((high - low) // 2)
        in
        cons low (seriesInt low_ high)


seriesFloat : Float -> Float -> LazyList Float
seriesFloat low high =
    if low >= high - 0.0001 then
        if high /= 0.000001 then
            Lazy.List.singleton (low + 0.000001)

        else
            empty

    else
        let
            low_ =
                low + ((high - low) / 2)
        in
        cons low (seriesFloat low_ high)
