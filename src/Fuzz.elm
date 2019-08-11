module Fuzz exposing
    ( int, intRange, float, floatRange, percentage, string, bool, maybe, result, list, array
    , Fuzzer, oneOf, constant, map, map2, map3, map4, map5, andMap, frequency
    , pair, triple
    , custom, char, unit, order, invalid
    )

{-| This is a library of _fuzzers_ you can use to supply values to your fuzz
tests. You can typically pick out which ones you need according to their types.

A `Fuzzer a` knows how to create values of type `a` in two different ways. It
can create them randomly, so that your test's expectations are run against many
values. Fuzzers will often generate edge cases likely to find bugs. If the
fuzzer can make your test fail, it also knows how to "simplify" that failing input
into more minimal examples, some of which might also cause the tests to fail. In
this way, fuzzers can usually find the simplest input that
reproduces a bug.


## Common Fuzzers

@docs int, intRange, float, floatRange, percentage, string, bool, maybe, result, list, array


## Working with Fuzzers

@docs Fuzzer, oneOf, constant, map, map2, map3, map4, map5, andMap, frequency


## Tuple Fuzzers

Instead of using a tuple, consider using [`fuzz2`][fuzz2] or [`fuzz3`][fuzz3].

@docs pair, triple


## Uncommon Fuzzers

@docs custom, char, unit, order, invalid

[fuzz2]: /packages/elm-explorations/test/latest/Test#fuzz2
[fuzz3]: /packages/elm-explorations/test/latest/Test#fuzz3

-}

import Array exposing (Array)
import Char
import Fuzz.Internal as Internal
    exposing
        ( Fuzzer
        , Valid
        , ValidFuzzer
        , combineValid
        , invalidReason
        )
import Lazy
import Lazy.List exposing (LazyList, append)
import MicroRandomExtra as Random
import Random exposing (Generator)
import RoseTree exposing (RoseTree(..))
import Simplify exposing (Simplifier)
import Simplify.Internal


{-| The representation of fuzzers is opaque. Conceptually, a `Fuzzer a`
consists of a way to randomly generate values of type `a`, and a way to simplify
those values.
-}
type alias Fuzzer a =
    Internal.Fuzzer a


{-| Build a custom `Fuzzer a` by providing a `Generator a` and a `Simplifier a`. Generators are defined in
[`elm/random`](http://package.elm-lang.org/packages/elm/random/latest). Simplifiers are defined in the [`Simplify`
module](https://package.elm-lang.org/packages/elm-explorations/test/latest/Simplify). It is not possible to extract the
generator and simplifier from an existing fuzzer.

This function should be considered for advanced uses. It's often easier to use `map` and other functions in this
module to create a fuzzer.

Here is an example for a record:

    import Random
    import Simplify

    type alias Position =
        { x : Int, y : Int }

    position : Fuzzer Position
    position =
        Fuzz.custom
            (Random.map2 Position (Random.int -100 100) (Random.int -100 100))
            (\{ x, y } -> Simplify.map Position (Simplify.int x) |> Simplify.andMap (Simplify.int y))

Here is an example for a custom union type, assuming there is already a `genName : Generator String` defined:

    type Question
        = Name String
        | Age Int

    question =
        let
            generator =
                Random.bool
                    |> Random.andThen
                        (\b ->
                            if b then
                                Random.map Name genName

                            else
                                Random.map Age (Random.int 0 120)
                        )

            simplifier question =
                case question of
                    Name n ->
                        Simplify.string n |> Simplify.map Name

                    Age i ->
                        Simplify.int i |> Simplify.map Age
        in
        Fuzz.custom generator simplifier

-}
custom : Generator a -> Simplifier a -> Fuzzer a
custom generator simplifier =
    Ok <|
        Random.map (Simplify.Internal.simplifyTree simplifier) generator


{-| A fuzzer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.
-}
unit : Fuzzer ()
unit =
    RoseTree.singleton ()
        |> Random.constant
        |> Ok


{-| A fuzzer for boolean values. It's useful when building up fuzzers of complex
types that contain a boolean somewhere.

We recommend against writing tests fuzzing over booleans. Write a unit test for
the true and false cases explicitly.

-}
bool : Fuzzer Bool
bool =
    custom Random.bool Simplify.bool


{-| A fuzzer for order values.
-}
order : Fuzzer Order
order =
    let
        intToOrder i =
            if i == 0 then
                LT

            else if i == 1 then
                EQ

            else
                GT
    in
    custom (Random.map intToOrder (Random.int 0 2)) Simplify.order


{-| A fuzzer for int values. It will never produce `NaN`, `Infinity`, or `-Infinity`.

It's possible for this fuzzer to generate any 32-bit integer, signed or unsigned, but it favors
numbers between -50 and 50 and especially zero.

-}
int : Fuzzer Int
int =
    let
        generator =
            Random.frequency
                ( 3, Random.int -50 50 )
                [ ( 0.2, Random.constant 0 )
                , ( 1, Random.int 0 (Random.maxInt - Random.minInt) )
                , ( 1, Random.int (Random.minInt - Random.maxInt) 0 )
                ]
    in
    custom generator Simplify.int


{-| A fuzzer for int values between a given minimum and maximum value,
inclusive. The high and low boundaries will be tested often. Shrunken
values will also be within the range.

Remember that [Random.maxInt](http://package.elm-lang.org/packages/elm-lang/core/latest/Random#maxInt)
is the maximum possible int value, so you can do `intRange x Random.maxInt` to get all
the ints x or bigger.

-}
intRange : Int -> Int -> Fuzzer Int
intRange lo hi =
    if hi < lo then
        Err <| "Fuzz.intRange was given a lower bound of " ++ String.fromInt lo ++ " which is greater than the upper bound, " ++ String.fromInt hi ++ "."

    else
        custom
            (Random.frequency
                ( 8, Random.int lo hi )
                [ ( 1, Random.constant lo )
                , ( 1, Random.constant hi )
                ]
            )
            (Simplify.keepIf (\i -> i >= lo && i <= hi) Simplify.int)


{-| A fuzzer for float values. It will never produce `NaN`, `Infinity`, or `-Infinity`.

It's possible for this fuzzer to generate any other floating-point value, but it
favors numbers between -50 and 50, numbers between -1 and 1, and especially zero.

-}
float : Fuzzer Float
float =
    let
        generator =
            Random.frequency
                ( 3, Random.float -50 50 )
                [ ( 0.5, Random.constant 0 )
                , ( 1, Random.float -1 1 )
                , ( 1, Random.float 0 (toFloat <| Random.maxInt - Random.minInt) )
                , ( 1, Random.float (toFloat <| Random.minInt - Random.maxInt) 0 )
                ]
    in
    custom generator Simplify.float


{-| A fuzzer for float values within between a given minimum and maximum
value, inclusive. Values at the boundary will be tested often. Shrunken
values will also be within the range.
-}
floatRange : Float -> Float -> Fuzzer Float
floatRange lo hi =
    if hi < lo then
        Err <| "Fuzz.floatRange was given a lower bound of " ++ String.fromFloat lo ++ " which is greater than the upper bound, " ++ String.fromFloat hi ++ "."

    else
        custom
            (Random.frequency
                ( 8, Random.float lo hi )
                [ ( 1, Random.constant lo )
                , ( 1, Random.constant hi )
                ]
            )
            (Simplify.keepIf (\i -> i >= lo && i <= hi) Simplify.float)


{-| A fuzzer for percentage values. Generates random floats between `0.0` and
`1.0`. It will test zero and one about 10% of the time each.
-}
percentage : Fuzzer Float
percentage =
    let
        generator =
            Random.frequency
                ( 8, Random.float 0 1 )
                [ ( 1, Random.constant 0 )
                , ( 1, Random.constant 1 )
                ]
    in
    custom generator Simplify.float


{-| A fuzzer for char values. Generates random ascii chars disregarding the control
characters and the extended character set.
-}
char : Fuzzer Char
char =
    custom asciiCharGenerator Simplify.character


asciiCharGenerator : Generator Char
asciiCharGenerator =
    Random.map Char.fromCode (Random.int 32 126)


whitespaceCharGenerator : Generator Char
whitespaceCharGenerator =
    Random.sample [ ' ', '\t', '\n' ] |> Random.map (Maybe.withDefault ' ')


{-| Generates random printable ASCII strings of up to 1000 characters.

Shorter strings are more common, especially the empty string.

-}
string : Fuzzer String
string =
    let
        asciiGenerator : Generator String
        asciiGenerator =
            Random.frequency
                ( 3, Random.int 1 10 )
                [ ( 0.2, Random.constant 0 )
                , ( 1, Random.int 11 50 )
                , ( 1, Random.int 50 1000 )
                ]
                |> Random.andThen (Random.lengthString asciiCharGenerator)

        whitespaceGenerator : Generator String
        whitespaceGenerator =
            Random.int 1 10
                |> Random.andThen (Random.lengthString whitespaceCharGenerator)
    in
    custom
        (Random.frequency
            ( 9, asciiGenerator )
            [ ( 1, whitespaceGenerator )
            ]
        )
        Simplify.string


{-| Given a fuzzer of a type, create a fuzzer of a maybe for that type.
-}
maybe : Fuzzer a -> Fuzzer (Maybe a)
maybe fuzzer =
    let
        toMaybe : Bool -> RoseTree a -> RoseTree (Maybe a)
        toMaybe useNothing tree =
            if useNothing then
                RoseTree.singleton Nothing

            else
                RoseTree.map Just tree |> RoseTree.addChild (RoseTree.singleton Nothing)
    in
    (Result.map << Random.map2 toMaybe) (Random.oneIn 4) fuzzer


{-| Given fuzzers for an error type and a success type, create a fuzzer for
a result.
-}
result : Fuzzer error -> Fuzzer value -> Fuzzer (Result error value)
result fuzzerError fuzzerValue =
    let
        toResult : Bool -> RoseTree error -> RoseTree value -> RoseTree (Result error value)
        toResult useError errorTree valueTree =
            if useError then
                RoseTree.map Err errorTree

            else
                RoseTree.map Ok valueTree
    in
    (Result.map2 <| Random.map3 toResult (Random.oneIn 4)) fuzzerError fuzzerValue


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of varying length, favoring shorter lists.
-}
list : Fuzzer a -> Fuzzer (List a)
list fuzzer =
    let
        genLength =
            Random.frequency
                ( 1, Random.constant 0 )
                [ ( 1, Random.constant 1 )
                , ( 3, Random.int 2 10 )
                , ( 2, Random.int 10 100 )
                , ( 0.5, Random.int 100 400 )
                ]
    in
    fuzzer
        |> Result.map
            (\validFuzzer ->
                genLength
                    |> Random.andThen (\a -> Random.list a validFuzzer)
                    |> Random.map listSimplifyHelp
            )


listSimplifyHelp : List (RoseTree a) -> RoseTree (List a)
listSimplifyHelp listOfTrees =
    {- This extends listSimplifyRecurse algorithm with an attempt to simplify directly to the empty list. -}
    listSimplifyRecurse listOfTrees
        |> mapChildren (Lazy.List.cons <| RoseTree.singleton [])


mapChildren : (LazyList (RoseTree a) -> LazyList (RoseTree a)) -> RoseTree a -> RoseTree a
mapChildren fn (Rose root children) =
    Rose root (fn children)


listSimplifyRecurse : List (RoseTree a) -> RoseTree (List a)
listSimplifyRecurse listOfTrees =
    {- Simplifying a list of RoseTrees
       We need to do two things. First, simplify individual values. Second, shorten the list.
       To simplify individual values, we create every list copy of the input list where any
       one value is replaced by a simplified form.
       To shorten the length of the list, remove elements at various positions in the list.
       In all cases, recurse! The goal is to make a little forward progress and then recurse.
    -}
    let
        n =
            List.length listOfTrees

        root =
            List.map RoseTree.root listOfTrees

        dropFirstHalf : List (RoseTree a) -> RoseTree (List a)
        dropFirstHalf list_ =
            List.drop (List.length list_ // 2) list_
                |> listSimplifyRecurse

        dropSecondHalf : List (RoseTree a) -> RoseTree (List a)
        dropSecondHalf list_ =
            List.take (List.length list_ // 2) list_
                |> listSimplifyRecurse

        halved : LazyList (RoseTree (List a))
        halved =
            -- The list halving shortcut is useful only for large lists.
            -- For small lists attempting to remove elements one by one is good enough.
            if n >= 8 then
                Lazy.lazy <|
                    \_ ->
                        Lazy.List.fromList [ dropFirstHalf listOfTrees, dropSecondHalf listOfTrees ]
                            |> Lazy.force

            else
                Lazy.List.empty

        simplifyOne prefix aList =
            case aList of
                [] ->
                    Lazy.List.empty

                (Rose x simplifiedXs) :: more ->
                    Lazy.List.map (\childTree -> prefix ++ (childTree :: more) |> listSimplifyRecurse) simplifiedXs

        simplifiedVals =
            Lazy.lazy <|
                \_ ->
                    Lazy.List.numbers
                        |> Lazy.List.map (\i -> i - 1)
                        |> Lazy.List.take n
                        |> Lazy.List.andThen
                            (\i -> simplifyOne (List.take i listOfTrees) (List.drop i listOfTrees))
                        |> Lazy.force

        shortened =
            Lazy.lazy <|
                \_ ->
                    List.range 0 (n - 1)
                        |> Lazy.List.fromList
                        |> Lazy.List.map (\index -> removeOne index listOfTrees)
                        |> Lazy.List.map listSimplifyRecurse
                        |> Lazy.force

        removeOne index aList =
            List.append
                (List.take index aList)
                (List.drop (index + 1) aList)
    in
    Rose root (append halved (append shortened simplifiedVals))


{-| Given a fuzzer of a type, create a fuzzer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.
-}
array : Fuzzer a -> Fuzzer (Array a)
array fuzzer =
    map Array.fromList (list fuzzer)


{-| Turn a pair of fuzzers into a fuzzer of pairs.
-}
pair : ( Fuzzer a, Fuzzer b ) -> Fuzzer ( a, b )
pair ( fuzzerA, fuzzerB ) =
    map2 (\a b -> ( a, b )) fuzzerA fuzzerB


{-| Turn a triple of fuzzers into a fuzzer of triples.
-}
triple : ( Fuzzer a, Fuzzer b, Fuzzer c ) -> Fuzzer ( a, b, c )
triple ( fuzzerA, fuzzerB, fuzzerC ) =
    map3 (\a b c -> ( a, b, c )) fuzzerA fuzzerB fuzzerC


{-| Create a fuzzer that only and always returns the value provided, and performs no simplifying. This is hardly random,
and so this function is best used as a helper when creating more complicated fuzzers.
-}
constant : a -> Fuzzer a
constant x =
    Ok <| Random.constant (RoseTree.singleton x)


{-| Map a function over a fuzzer. This applies to both the generated and the simplified values.
-}
map : (a -> b) -> Fuzzer a -> Fuzzer b
map =
    Internal.map


{-| Map over two fuzzers.
-}
map2 : (a -> b -> c) -> Fuzzer a -> Fuzzer b -> Fuzzer c
map2 transform fuzzA fuzzB =
    (Result.map2 << Random.map2 << map2RoseTree) transform fuzzA fuzzB


{-| Map over three fuzzers.
-}
map3 : (a -> b -> c -> d) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d
map3 transform fuzzA fuzzB fuzzC =
    (Result.map3 << Random.map3 << map3RoseTree) transform fuzzA fuzzB fuzzC


{-| Map over four fuzzers.
-}
map4 : (a -> b -> c -> d -> e) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e
map4 transform fuzzA fuzzB fuzzC fuzzD =
    (Result.map4 << Random.map4 << map4RoseTree) transform fuzzA fuzzB fuzzC fuzzD


{-| Map over five fuzzers.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f
map5 transform fuzzA fuzzB fuzzC fuzzD fuzzE =
    (Result.map5 << Random.map5 << map5RoseTree) transform fuzzA fuzzB fuzzC fuzzD fuzzE


{-| Map over many fuzzers. This can act as `mapN` for `N > 5`.
The argument order is meant to accommodate chaining:

    map f aFuzzer
        |> andMap anotherFuzzer
        |> andMap aThirdFuzzer

Note that simplifying may be better using `mapN`.

-}
andMap : Fuzzer a -> Fuzzer (a -> b) -> Fuzzer b
andMap =
    map2 (|>)


{-| Create a new `Fuzzer` by providing a list of probabilistic weights to use
with other fuzzers.
For example, to create a `Fuzzer` that has a 1/4 chance of generating an int
between -1 and -100, and a 3/4 chance of generating one between 1 and 100,
you could do this:

    Fuzz.frequency
        [ ( 1, Fuzz.intRange -100 -1 )
        , ( 3, Fuzz.intRange 1 100 )
        ]

There are a few circumstances in which this function will return an invalid
fuzzer, which causes it to fail any test that uses it:

  - If you provide an empty list of frequencies
  - If any of the weights are less than 0
  - If the weights sum to 0

Be careful recursively using this fuzzer in its arguments. Often using `map`
is a better way to do what you want. If you are fuzzing a tree-like data
structure, you should include a depth limit so to avoid infinite recursion, like
so:

    type Tree
        = Leaf
        | Branch Tree Tree

    tree : Int -> Fuzzer Tree
    tree i =
        if i <= 0 then
            Fuzz.constant Leaf

        else
            Fuzz.frequency
                [ ( 1, Fuzz.constant Leaf )
                , ( 2, Fuzz.map2 Branch (tree (i - 1)) (tree (i - 1)) )
                ]

-}
frequency : List ( Float, Fuzzer a ) -> Fuzzer a
frequency aList =
    if List.isEmpty aList then
        invalid "You must provide at least one frequency pair."

    else if List.any (\( weight, _ ) -> weight < 0) aList then
        invalid "No frequency weights can be less than 0."

    else if List.sum (List.map Tuple.first aList) <= 0 then
        invalid "Frequency weights must sum to more than 0."

    else
        let
            toRandom validList =
                case validList of
                    [] ->
                        invalid "You must provide at least one frequency pair."

                    first :: rest ->
                        Ok (Random.frequency first rest)
        in
        aList
            |> List.map extractValid
            |> combineValid
            |> Result.andThen toRandom


extractValid : ( a, Valid b ) -> Valid ( a, b )
extractValid ( a, valid ) =
    Result.map (\b -> ( a, b )) valid


{-| Choose one of the given fuzzers at random. Each fuzzer has an equal chance
of being chosen; to customize the probabilities, use [`frequency`](#frequency).

    Fuzz.oneOf
        [ Fuzz.intRange 0 3
        , Fuzz.intRange 7 9
        ]

-}
oneOf : List (Fuzzer a) -> Fuzzer a
oneOf aList =
    if List.isEmpty aList then
        invalid "You must pass at least one Fuzzer to Fuzz.oneOf."

    else
        aList
            |> List.map (\fuzzer -> ( 1, fuzzer ))
            |> frequency


{-| A fuzzer that is invalid for the provided reason. Any fuzzers built with it
are also invalid. Any tests using an invalid fuzzer fail.
-}
invalid : String -> Fuzzer a
invalid reason =
    Err reason


map2RoseTree : (a -> b -> c) -> RoseTree a -> RoseTree b -> RoseTree c
map2RoseTree transform ((Rose root1 children1) as rose1) ((Rose root2 children2) as rose2) =
    {- Simplifying a pair of RoseTrees
       Recurse on all pairs created by substituting one element for any of its simplified values.
       A weakness of this algorithm is that it expects that values can be simplified independently.
       That is, to simplify from (a,b) to (a',b'), we must go through (a',b) or (a,b').
       "No pairs sum to zero" is a pathological predicate that cannot be simplified this way.
    -}
    let
        root =
            transform root1 root2

        simplify1 =
            Lazy.List.map (\subtree -> map2RoseTree transform subtree rose2) children1

        simplify2 =
            Lazy.List.map (\subtree -> map2RoseTree transform rose1 subtree) children2
    in
    Rose root <| append simplify1 simplify2



-- The RoseTree 'mapN, n > 2' functions below follow the same strategy as map2RoseTree.
-- They're implemented separately instead of in terms of `andMap` because this has significant perfomance benefits.


map3RoseTree : (a -> b -> c -> d) -> RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d
map3RoseTree transform ((Rose root1 children1) as rose1) ((Rose root2 children2) as rose2) ((Rose root3 children3) as rose3) =
    let
        root =
            transform root1 root2 root3

        simplify1 =
            Lazy.List.map (\childOf1 -> map3RoseTree transform childOf1 rose2 rose3) children1

        simplify2 =
            Lazy.List.map (\childOf2 -> map3RoseTree transform rose1 childOf2 rose3) children2

        simplify3 =
            Lazy.List.map (\childOf3 -> map3RoseTree transform rose1 rose2 childOf3) children3
    in
    Rose root <| append simplify1 (append simplify2 simplify3)


map4RoseTree : (a -> b -> c -> d -> e) -> RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d -> RoseTree e
map4RoseTree transform ((Rose root1 children1) as rose1) ((Rose root2 children2) as rose2) ((Rose root3 children3) as rose3) ((Rose root4 children4) as rose4) =
    let
        root =
            transform root1 root2 root3 root4

        simplify1 =
            Lazy.List.map (\childOf1 -> map4RoseTree transform childOf1 rose2 rose3 rose4) children1

        simplify2 =
            Lazy.List.map (\childOf2 -> map4RoseTree transform rose1 childOf2 rose3 rose4) children2

        simplify3 =
            Lazy.List.map (\childOf3 -> map4RoseTree transform rose1 rose2 childOf3 rose4) children3

        simplify4 =
            Lazy.List.map (\childOf4 -> map4RoseTree transform rose1 rose2 rose3 childOf4) children4
    in
    Rose root <| append simplify1 (append simplify2 (append simplify3 simplify4))


map5RoseTree : (a -> b -> c -> d -> e -> f) -> RoseTree a -> RoseTree b -> RoseTree c -> RoseTree d -> RoseTree e -> RoseTree f
map5RoseTree transform ((Rose root1 children1) as rose1) ((Rose root2 children2) as rose2) ((Rose root3 children3) as rose3) ((Rose root4 children4) as rose4) ((Rose root5 children5) as rose5) =
    let
        root =
            transform root1 root2 root3 root4 root5

        simplify1 =
            Lazy.List.map (\childOf1 -> map5RoseTree transform childOf1 rose2 rose3 rose4 rose5) children1

        simplify2 =
            Lazy.List.map (\childOf2 -> map5RoseTree transform rose1 childOf2 rose3 rose4 rose5) children2

        simplify3 =
            Lazy.List.map (\childOf3 -> map5RoseTree transform rose1 rose2 childOf3 rose4 rose5) children3

        simplify4 =
            Lazy.List.map (\childOf4 -> map5RoseTree transform rose1 rose2 rose3 childOf4 rose5) children4

        simplify5 =
            Lazy.List.map (\childOf5 -> map5RoseTree transform rose1 rose2 rose3 rose4 childOf5) children5
    in
    Rose root <| append simplify1 (append simplify2 (append simplify3 (append simplify4 simplify5)))
