module Fuzz exposing
    ( int, intRange, float, floatRange, percentage, string, bool, maybe, result, list, listOfLength, listOfLengthBetween, array
    , Fuzzer, oneOf, oneOfValues, constant, map, map2, map3, map4, map5, andMap, frequency, frequencyValues, andThen, filter
    , pair, triple
    , char, unit, order, invalid, weightedBool
    )

{-| This is a library of _fuzzers_ you can use to supply values to your fuzz
tests. You can typically pick out which ones you need according to their types.

A `Fuzzer a` knows how to create values of type `a`. It can create them randomly,
so that your test's expectations are run against many values. Fuzzers will often
generate edge cases likely to find bugs. If the fuzzer can make your test fail,
the test runner also knows how to "simplify" that failing input into more minimal
examples, some of which might also cause the tests to fail. In this way, fuzzers
can usually find the simplest input that reproduces a bug.


## Common Fuzzers

@docs int, intRange, float, floatRange, percentage, string, bool, maybe, result, list, listOfLength, listOfLengthBetween, array


## Working with Fuzzers

@docs Fuzzer, oneOf, oneOfValues, constant, map, map2, map3, map4, map5, andMap, frequency, frequencyValues, andThen, filter


## Tuple Fuzzers

Instead of using a tuple, consider using [`fuzz2`][fuzz2] or [`fuzz3`][fuzz3].

@docs pair, triple


## Uncommon Fuzzers

@docs char, unit, order, invalid, weightedBool

[fuzz2]: /packages/elm-explorations/test/latest/Test#fuzz2
[fuzz3]: /packages/elm-explorations/test/latest/Test#fuzz3

-}

import Array exposing (Array)
import Char
import Fuzz.Float
import Fuzz.Internal exposing (Fuzzer(..))
import GenResult exposing (GenResult(..))
import MicroListExtra as List
import PRNG exposing (PRNG(..))
import Random
import RandomRun
import Test.Internal


{-| The representation of fuzzers is opaque. Conceptually, a `Fuzzer a` consists
of a way to randomly generate values of type `a` in a way allowing the test runner
to simplify those values.
-}
type alias Fuzzer a =
    Fuzz.Internal.Fuzzer a


{-| A fuzzer for the unit value. Unit is a type with only one value, commonly
used as a placeholder.
-}
unit : Fuzzer ()
unit =
    constant ()


{-| A fuzzer for boolean values. It's useful when building up fuzzers of complex
types that contain a boolean somewhere.

We recommend against writing tests fuzzing over booleans. Write a unit test for
the true and false cases explicitly.

Simplifies in order `False < True`.

-}
bool : Fuzzer Bool
bool =
    oneOfValues [ False, True ]


{-| A fuzzer for order values.

Simplifies in order `LT < EQ < GT`.

-}
order : Fuzzer Order
order =
    oneOfValues [ LT, EQ, GT ]


{-| A fuzzer for int values. It will never produce `NaN`, `Infinity`, or
`-Infinity`.

This fuzzer will generate values in the range `Random.minInt .. Random.maxInt`,
but it will also try generating in the range `-(2^32) .. 2^32`. No guarantees
though!

Simplifies towards 0; prefers positive values over negative ones.

-}
int : Fuzzer Int
int =
    intRange (negate 0xFFFFFFFF) 0xFFFFFFFF


{-| A fuzzer for int values between a given minimum and maximum value,
inclusive. Shrunken values will also be within the range.

Remember that [Random.maxInt](http://package.elm-lang.org/packages/elm-lang/core/latest/Random#maxInt)
is the maximum possible int value, so you can do `intRange x Random.maxInt` to get all
the ints x or bigger.

-}
intRange : Int -> Int -> Fuzzer Int
intRange lo hi =
    if hi < lo then
        invalid <|
            "Fuzz.intRange was given a lower bound of "
                ++ String.fromInt lo
                ++ " which is greater than the upper bound, "
                ++ String.fromInt hi
                ++ "."

    else if lo == hi then
        constant lo

    else if lo >= 0 then
        -- both non-negative
        internalInt (hi - lo)
            {- intRange 2 5: internalInt 3: 0,1,2,3
               => (+) 2 => 2,3,4,5
               simplifying towards zero, not Inf
            -}
            |> map (\n -> n + lo)

    else if hi <= 0 then
        -- both negative
        internalInt (hi - lo)
            {- intRange -5 -2: internalInt 3: 0,1,2,3
               => negate => -0,-1,-2,-3
               => (+) -2 => -2,-3,-4,-5
               simplifying towards zero, not -Inf
            -}
            |> map (\n -> negate n + hi)

    else
        {- somewhere in the middle, divide it into negative and positive ranges,
           both of which will simplify towards zero. We prefer positive values.
        -}
        oneOf
            [ intRange 0 hi -- the conditions above guarantee hi >= 1
            , intRange lo -1 -- the conditions above guarantee lo <= -1
            ]


{-| A fuzzer for float values. It will never produce `NaN`, `Infinity`, or
`-Infinity`.
-}
float : Fuzzer Float
float =
    wellShrinkingFloat


{-| This float fuzzer will prefer non-fractional floats and (if it must) nice
fractions.
-}
wellShrinkingFloat : Fuzzer Float
wellShrinkingFloat =
    map3
        (\hi lo shouldNegate ->
            let
                f : Float
                f =
                    Fuzz.Float.wellShrinkingFloat ( hi, lo )
            in
            if shouldNegate then
                negate f

            else
                f
        )
        int32
        int32
        bool
        |> filter (\n -> not <| isInfinite n || isNaN n)


{-| A fuzzer for float values within between a given minimum and maximum
value, inclusive. Shrunken values will also be within the range.

TODO ~janiczek: these (m,n) range `floatRange`s will not shrink as nicely as
a (-Inf,Inf) `float` would. But we could make (-Inf,n) and (n,Inf) ones shrink
nicely. What about exposing functions for them?

-}
floatRange : Float -> Float -> Fuzzer Float
floatRange lo hi =
    if hi < lo then
        invalid <|
            "Fuzz.floatRange was given a lower bound of "
                ++ String.fromFloat lo
                ++ " which is greater than the upper bound, "
                ++ String.fromFloat hi
                ++ "."

    else if lo == hi then
        constant lo

    else if lo >= 0 then
        -- both non-negative
        scaledFloat lo hi

    else if hi <= 0 then
        -- both negative
        scaledFloat (negate hi) (negate lo)
            -- simplify towards zero
            |> map negate

    else
        {- somewhere in the middle, divide it into negative and positive ranges,
           both of which will simplify towards zero. We prefer positive values.
        -}
        oneOf
            [ scaledFloat 0 hi
            , scaledFloat 0 (negate lo)
                |> map negate
            ]


{-| This float fuzzer won't shrink nicely (to integers or nice fractions). For
that, use `wellShrinkingFloat`.
-}
scaledFloat : Float -> Float -> Fuzzer Float
scaledFloat lo hi =
    percentage
        |> map (\f -> f * (hi - lo) + lo)


{-| A fuzzer for percentage values. Generates random floats between `0.0` and
`1.0`.

Simplifies towards zero.

-}
percentage : Fuzzer Float
percentage =
    {- We can't use Random.Generators here as all fuzzed values must be
       representable as one or more ints. We generally use a pair of 32bit ints
       to represent a 64bit float.

       Here we know the top 12 bits of the high int wouldn't be used for the
       mantissa calculations so we don't bother generating those.
    -}
    pair (internalInt 0x000FFFFF) int32
        |> map Fuzz.Float.fractionalFloat


int32 : Fuzzer Int
int32 =
    internalInt 0xFFFFFFFF


{-| A fuzzer for char values. Generates random ASCII chars disregarding the
control characters and the extended character set.
-}
char : Fuzzer Char
char =
    intRange 32 126
        |> map Char.fromCode


{-| Generates random printable unicode strings of up to 100 characters.
-}
string : Fuzzer String
string =
    stringOfLengthBetween 0 100


stringOfLengthBetween : Int -> Int -> Fuzzer String
stringOfLengthBetween min max =
    listOfLengthBetween min max unicodeChar
        |> map String.fromList


unicodeChar : Fuzzer Char
unicodeChar =
    let
        whitespaceChar : Fuzzer Char
        whitespaceChar =
            oneOfValues
                [ ' '
                , '\t'
                , '\n'
                ]

        combiningDiacriticalMarkChar : Fuzzer Char
        combiningDiacriticalMarkChar =
            oneOfValues
                [ '̂'
                , '̃'
                , '̈'
                ]

        emojiChar : Fuzzer Char
        emojiChar =
            oneOfValues
                [ '🌈'
                , '❤'
                , '🔥'
                ]
    in
    smallIntFrequency
        [ ( 4, char )
        , ( 1, whitespaceChar )
        , ( 1, combiningDiacriticalMarkChar )
        , ( 1, emojiChar )
        ]


{-| Given a fuzzer of a type, create a fuzzer of a maybe for that type.
-}
maybe : Fuzzer a -> Fuzzer (Maybe a)
maybe fuzzer =
    smallIntFrequency
        [ ( 1, constant Nothing )
        , ( 3, map Just fuzzer )
        ]


{-| Given fuzzers for an error type and a success type, create a fuzzer for
a result.
-}
result : Fuzzer error -> Fuzzer value -> Fuzzer (Result error value)
result fuzzerError fuzzerValue =
    smallIntFrequency
        [ ( 1, map Err fuzzerError )
        , ( 3, map Ok fuzzerValue )
        ]


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of varying length, up to 100 elements.
-}
list : Fuzzer a -> Fuzzer (List a)
list fuzzer =
    listOfLengthBetween 0 100 fuzzer


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of exactly the specified length.
-}
listOfLength : Int -> Fuzzer a -> Fuzzer (List a)
listOfLength n fuzzer =
    listOfLengthBetween n n fuzzer


{-| Given a fuzzer of a type, create a fuzzer of a list of that type.
Generates random lists of length between the two given integers.
-}
listOfLengthBetween : Int -> Int -> Fuzzer a -> Fuzzer (List a)
listOfLengthBetween lo hi itemFuzzer =
    if lo > hi then
        -- the name allows for it, even if it's a little weird
        listOfLengthBetween hi lo itemFuzzer

    else if hi <= 0 then
        constant []

    else
        let
            average : Float
            average =
                toFloat lo + toFloat hi / 2

            continueProbability : Float
            continueProbability =
                {- Taken from Python Hypothesis library (ListStrategy).
                   It should supposedly be a geometric distribution, although I
                   don't see the connection from the below formula. ~janiczek
                -}
                1 - 1 / (1 + average)

            addItem : Int -> List a -> Fuzzer (List a)
            addItem length acc =
                itemFuzzer
                    |> andThen
                        (\item ->
                            go (length + 1) (item :: acc)
                        )

            end : List a -> Fuzzer (List a)
            end acc =
                constant (List.reverse acc)

            go : Int -> List a -> Fuzzer (List a)
            go length acc =
                if length < lo then
                    forcedChoice 1
                        |> andThen (\_ -> addItem length acc)

                else if length == hi then
                    forcedChoice 0
                        |> andThen (\_ -> end acc)

                else
                    weightedBool continueProbability
                        |> andThen
                            (\oneMorePlease ->
                                if oneMorePlease then
                                    addItem length acc

                                else
                                    end acc
                            )
        in
        go 0 []


{-| Given a fuzzer of a type, create a fuzzer of an array of that type.
Generates random arrays of varying length, favoring shorter arrays.
-}
array : Fuzzer a -> Fuzzer (Array a)
array fuzzer =
    map Array.fromList (list fuzzer)


{-| Create a fuzzer of pairs from two fuzzers.
-}
pair : Fuzzer a -> Fuzzer b -> Fuzzer ( a, b )
pair fuzzerA fuzzerB =
    map2 Tuple.pair fuzzerA fuzzerB


{-| Create a fuzzer of triples from three fuzzers.
-}
triple : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer ( a, b, c )
triple fuzzerA fuzzerB fuzzerC =
    map3 (\a b c -> ( a, b, c )) fuzzerA fuzzerB fuzzerC


{-| Create a fuzzer that only and always returns the value provided, and performs
no simplifying. This is hardly random, and so this function is best used as a
helper when creating more complicated fuzzers.
-}
constant : a -> Fuzzer a
constant x =
    Fuzzer <|
        \prng ->
            Generated
                { value = x
                , prng = prng
                }


{-| Map a function over a fuzzer.
-}
map : (a -> b) -> Fuzzer a -> Fuzzer b
map fn (Fuzzer fuzzer) =
    Fuzzer <|
        \prng ->
            case fuzzer prng of
                Generated g ->
                    Generated
                        { value = fn g.value
                        , prng = g.prng
                        }

                Rejected r ->
                    Rejected r


{-| Map over two fuzzers.
-}
map2 : (a -> b -> c) -> Fuzzer a -> Fuzzer b -> Fuzzer c
map2 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            Generated
                                { value = fn a.value b.value
                                , prng = b.prng
                                }

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over three fuzzers.
-}
map3 : (a -> b -> c -> d) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d
map3 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) (Fuzzer fuzzerC) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            case fuzzerC b.prng of
                                Generated c ->
                                    Generated
                                        { value = fn a.value b.value c.value
                                        , prng = c.prng
                                        }

                                Rejected r ->
                                    Rejected r

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over four fuzzers.
-}
map4 : (a -> b -> c -> d -> e) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e
map4 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) (Fuzzer fuzzerC) (Fuzzer fuzzerD) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            case fuzzerC b.prng of
                                Generated c ->
                                    case fuzzerD c.prng of
                                        Generated d ->
                                            Generated
                                                { value = fn a.value b.value c.value d.value
                                                , prng = d.prng
                                                }

                                        Rejected r ->
                                            Rejected r

                                Rejected r ->
                                    Rejected r

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over five fuzzers.
-}
map5 : (a -> b -> c -> d -> e -> f) -> Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer d -> Fuzzer e -> Fuzzer f
map5 fn (Fuzzer fuzzerA) (Fuzzer fuzzerB) (Fuzzer fuzzerC) (Fuzzer fuzzerD) (Fuzzer fuzzerE) =
    Fuzzer <|
        \prng ->
            case fuzzerA prng of
                Generated a ->
                    case fuzzerB a.prng of
                        Generated b ->
                            case fuzzerC b.prng of
                                Generated c ->
                                    case fuzzerD c.prng of
                                        Generated d ->
                                            case fuzzerE d.prng of
                                                Generated e ->
                                                    Generated
                                                        { value = fn a.value b.value c.value d.value e.value
                                                        , prng = e.prng
                                                        }

                                                Rejected r ->
                                                    Rejected r

                                        Rejected r ->
                                            Rejected r

                                Rejected r ->
                                    Rejected r

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


{-| Map over many fuzzers. This can act as `mapN` for `N > 5`.
The argument order is meant to accommodate chaining:

    map f aFuzzer
        |> andMap anotherFuzzer
        |> andMap aThirdFuzzer

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

This fuzzer will simplify towards the fuzzers earlier in the list (each of which
will also apply its own way to simplify the values).

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
frequency fuzzers =
    frequencyHelp "Fuzz.frequency" fuzzers


frequencyHelp : String -> List ( Float, Fuzzer a ) -> Fuzzer a
frequencyHelp functionName fuzzers =
    if List.any (\( w, _ ) -> w < 0) fuzzers then
        invalid <| functionName ++ ": No frequency weights can be less than 0."

    else
        let
            nonzeroFuzzers =
                List.filter (\( w, _ ) -> w > 0) fuzzers
        in
        if List.isEmpty nonzeroFuzzers then
            invalid <| functionName ++ ": You must provide at least one frequency pair with weight greater than 0."

        else
            let
                weightSum =
                    List.foldl (\( w, _ ) acc -> w + acc) 0 nonzeroFuzzers

                allWeightsAreInts =
                    List.all (\( w, _ ) -> w == toFloat (round w)) nonzeroFuzzers
            in
            if allWeightsAreInts && weightSum < 20 then
                smallIntFrequency (List.map (Tuple.mapFirst round) nonzeroFuzzers)

            else
                percentage
                    |> andThen
                        (\p ->
                            let
                                f : Float
                                f =
                                    p * weightSum

                                go : Float -> List ( Float, Fuzzer a ) -> Fuzzer a
                                go countdown acc =
                                    case acc of
                                        [] ->
                                            invalid <| "elm-test bug: " ++ functionName ++ " encountered empty list after checking for it."

                                        [ ( _, last ) ] ->
                                            last

                                        ( w, current ) :: rest ->
                                            if countdown <= w then
                                                current

                                            else
                                                go (countdown - w) rest
                            in
                            go f nonzeroFuzzers
                        )


{-| A restricted version of `frequency` that has a nicer RandomRun footprint.

`frequency` draws a `percentage` float = two 32bit integers.
This function is implemented in terms of `oneOf` and so only draws one small
integer (index into the list).

`frequency` will automatically switch to this function if used with integers
that sum to less than 20.

Note that the performance might hurt if used with eg. `(1000, fooFuzzer)`.
In that case it will be better to use `frequency`.

-}
smallIntFrequency : List ( Int, Fuzzer a ) -> Fuzzer a
smallIntFrequency fuzzers =
    fuzzers
        |> List.fastConcatMap (\( count, fuzzer ) -> List.repeat count fuzzer)
        |> oneOf


{-| Create a `Fuzzer` by providing a list of probabilistic weights to use with
values.
For example, to create a `Fuzzer` that has a 1/4 chance of generating a string
"foo", and a 3/4 chance of generating a string "bar", you could do this:

    Fuzz.frequency
        [ ( 1, "foo" )
        , ( 3, "bar" )
        ]

This fuzzer will simplify towards the values earlier in the list.

There are a few circumstances in which this function will return an invalid
fuzzer, which causes it to fail any test that uses it:

  - If you provide an empty list of frequencies
  - If any of the weights are less than 0
  - If the weights sum to 0

-}
frequencyValues : List ( Float, a ) -> Fuzzer a
frequencyValues values =
    frequencyHelp "Fuzz.frequencyValues"
        (List.map (Tuple.mapSecond constant) values)


{-| Choose one of the given fuzzers at random. Each fuzzer has an equal chance
of being chosen; to customize the probabilities, use [`frequency`](#frequency).

This fuzzer will simplify towards the fuzzers earlier in the list (each of which
will also apply its own way to simplify the values).

    Fuzz.oneOf
        [ Fuzz.intRange 0 3
        , Fuzz.intRange 7 9
        ]

-}
oneOf : List (Fuzzer a) -> Fuzzer a
oneOf fuzzers =
    oneOfHelp "Fuzz.oneOf" "fuzzer" fuzzers


oneOfHelp : String -> String -> List (Fuzzer a) -> Fuzzer a
oneOfHelp functionName itemName fuzzers =
    case List.length fuzzers of
        0 ->
            invalid <| functionName ++ ": You must provide at least one item."

        length ->
            internalInt (length - 1)
                |> andThen
                    (\i ->
                        case List.getAt i fuzzers of
                            Nothing ->
                                -- shouldn't happen
                                invalid <| "elm-test bug: " ++ functionName ++ " didn't find a " ++ itemName ++ " in the list."

                            Just fuzzer ->
                                fuzzer
                    )


{-| Choose one of the given values at random. Each value has an equal chance
of being chosen; to customize the probabilities, use [`frequencyValues`](#frequencyValues).

This fuzzer will simplify towards the values earlier in the list.

    Fuzz.oneOfValues
        [ 999
        , -42
        ]

-}
oneOfValues : List a -> Fuzzer a
oneOfValues values =
    oneOfHelp "Fuzz.oneOfValues" "value" (List.map constant values)


{-| A fuzzer that is invalid for the provided reason. Any fuzzers built with it
are also invalid. Any tests using an invalid fuzzer fail.
-}
invalid : String -> Fuzzer a
invalid reason =
    Fuzzer <|
        \prng ->
            Rejected
                { reason = reason
                , prng = prng
                }


{-| A fuzzer that only lets through values satisfying the given predicate
function.

Note that it's often better to get to your wanted values using [`map`](#map), as
you don't run the risk of rejecting too may values and slowing down your tests,
for example using `Fuzz.int 0 5 |> Fuzz.map (\x -> x * 2)` instead of
`Fuzz.int 0 9 |> Fuzz.filter (\x -> modBy 2 x == 0)`.

-}
filter : (a -> Bool) -> Fuzzer a -> Fuzzer a
filter predicate fuzzer =
    fuzzer
        |> andThen
            (\value ->
                if predicate value then
                    constant value

                else
                    -- TODO "too many values filtered out" failure reason
                    invalid <| "A value was filtered out: " ++ Test.Internal.toString value
            )


{-| Use a generated value to decide what fuzzer to use next.

For example, let's say you want to generate a list of given length.
One possible way to do that is first choosing how many elements will there be
(generating a number), `andThen` generating a list with that many items:

    Fuzz.int 1 10
        |> Fuzz.andThen
            (\length ->
                let
                    go : Int -> List a -> Fuzzer (List a)
                    go todo acc =
                        if todo <= 0 then
                            constant (List.reverse acc)

                        else
                            itemFuzzer
                                |> Fuzz.andThen (\item -> go (length - 1) (item :: acc))
                in
                go length []
            )

(By the way, it will probably be better to just use one of the [`list`](#list)
helpers in this module.)

Think of it as a generalization of [`map`](#map). Inside [`map`](#map) you don't
have the option to fuzz another value based on what you already have; inside
`andThen` you do.

-}
andThen : (a -> Fuzzer b) -> Fuzzer a -> Fuzzer b
andThen fn (Fuzzer fuzzer) =
    Fuzzer <|
        \prng ->
            case fuzzer prng of
                Generated g ->
                    let
                        (Fuzzer newFuzzer) =
                            fn g.value
                    in
                    newFuzzer g.prng

                Rejected r ->
                    Rejected r


{-| Will simplify towards 0.
-}
internalInt : Int -> Fuzzer Int
internalInt n =
    rollDice n (Random.int 0 n)


{-| A fuzzer for boolean values, generating True with the given probability
(0.0 = always False, 1.0 = always True).

Probabilities outside the `0..1` range will be clamped to `0..1`.

Simplifies towards False (if not prevented to do that by using probability >= 1).

-}
weightedBool : Float -> Fuzzer Bool
weightedBool p =
    (if p <= 0 then
        forcedChoice 0

     else if p >= 1 then
        forcedChoice 1

     else
        rollDice 1 (weightedBoolGenerator p)
    )
        |> map intToBool


{-| This is the only place that accepts Random.Generators.
And only Int ones at that!

This is because our underlying implementation is a sequence of Ints (RandomRun).
All other generated values (Floats, Bools, ...) have to be somehow mapped one or
more Ints.

Based on the PRNG value, this function:

  - either draws and remembers a random number (PRNG.Random)
  - or picks a number from the hardcoded list. (PRNG.Hardcoded)

-}
rollDice : Int -> Random.Generator Int -> Fuzzer Int
rollDice maxValue diceGenerator =
    Fuzzer <|
        \prng ->
            if RandomRun.isFull (PRNG.getRun prng) then
                Rejected
                    { reason = "Your fuzzers hit a recursion limit"
                    , prng = prng
                    }

            else
                case prng of
                    Random r ->
                        let
                            ( diceRoll, newSeed ) =
                                Random.step diceGenerator r.seed
                        in
                        if diceRoll < 0 then
                            Rejected
                                { reason = "elm-test bug: generated a choice < 0"
                                , prng = prng
                                }

                        else if diceRoll > maxValue then
                            Rejected
                                { reason = "elm-test bug: generated a choice > maxChoice"
                                , prng = prng
                                }

                        else
                            Generated
                                { value = diceRoll
                                , prng =
                                    Random
                                        { seed = newSeed
                                        , run = RandomRun.append diceRoll r.run
                                        }
                                }

                    Hardcoded h ->
                        case RandomRun.nextChoice h.unusedPart of
                            Nothing ->
                                -- This happens if we simplified too much / in an incompatible way
                                Rejected
                                    { reason = "elm-test internals: hardcoded PRNG run out of numbers"
                                    , prng = prng
                                    }

                            Just ( hardcodedChoice, restOfChoices ) ->
                                if hardcodedChoice < 0 then
                                    -- This happens eg. when decrementing after delete shrink
                                    Rejected
                                        { reason = "elm-test internals: generated a choice < 0"
                                        , prng = prng
                                        }

                                else if hardcodedChoice > maxValue then
                                    -- This happens eg. when redistributing choices
                                    Rejected
                                        { reason = "elm-test internals: generated a choice > maxChoice"
                                        , prng = prng
                                        }

                                else
                                    Generated
                                        { value = hardcodedChoice
                                        , prng = Hardcoded { h | unusedPart = restOfChoices }
                                        }


forcedChoice : Int -> Fuzzer Int
forcedChoice n =
    Fuzzer <|
        \prng ->
            if n < 0 then
                Rejected
                    { reason = "elm-test bug: forcedChoice: n < 0"
                    , prng = prng
                    }

            else if RandomRun.isFull (PRNG.getRun prng) then
                Rejected
                    { reason = "Your fuzzers hit a recursion limit"
                    , prng = prng
                    }

            else
                case prng of
                    Random r ->
                        Generated
                            { value = n
                            , prng = Random { r | run = RandomRun.append n r.run }
                            }

                    Hardcoded h ->
                        case RandomRun.nextChoice h.unusedPart of
                            Nothing ->
                                -- This happens if we simplified too much / in an incompatible way
                                Rejected
                                    { reason = "elm-test internals: hardcoded PRNG run out of numbers"
                                    , prng = prng
                                    }

                            Just ( hardcodedChoice, restOfChoices ) ->
                                if hardcodedChoice /= n then
                                    Rejected
                                        { reason = "elm-test internals: hardcoded value was not the same as the forced one"
                                        , prng = prng
                                        }

                                else
                                    Generated
                                        { value = n
                                        , prng = Hardcoded { h | unusedPart = restOfChoices }
                                        }


{-| We could golf this to ((/=) 0) but this is perhaps more readable.
-}
intToBool : Int -> Bool
intToBool n =
    if n == 0 then
        False

    else
        True


weightedBoolGenerator : Float -> Random.Generator Int
weightedBoolGenerator p =
    Random.float 0 1
        |> Random.map
            (\f ->
                if f <= p then
                    1

                else
                    0
            )
