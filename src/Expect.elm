module Expect exposing
    ( Expectation, equal, notEqual, not, all, oneOf
    , lessThan, atMost, greaterThan, atLeast
    , FloatingPointTolerance(..), within, notWithin
    , NaNBehavior(..), equalWithNumbers
    , ok, err, equalLists, equalDicts, equalSets
    , pass, fail, onFail
    , passesAll, passesOneOf
    )

{-| A library to create `Expectation`s, which describe a claim to be tested.


## Quick Reference

  - [`equal`](#equal) `(arg2 == arg1)`
  - [`notEqual`](#notEqual) `(arg2 /= arg1)`
  - [`not`](#not) (inverts any `Expectation`)
  - [`lessThan`](#lessThan) `(arg2 < arg1)`
  - [`atMost`](#atMost) `(arg2 <= arg1)`
  - [`greaterThan`](#greaterThan) `(arg2 > arg1)`
  - [`atLeast`](#atLeast) `(arg2 >= arg1)`
  - [Floating Point Comparisons](#floating-point-comparisons)


## Basic Expectations

@docs Expectation, equal, notEqual, not, all, oneOf


## Numeric Comparisons

@docs lessThan, atMost, greaterThan, atLeast


## Floating Point Comparisons

These functions allow you to compare `Float` values up to a specified rounding error, which may be relative, absolute,
or both. For an in-depth look, see our [Guide to Floating Point Comparison](#guide-to-floating-point-comparison).

@docs FloatingPointTolerance, within, notWithin

[`equalWithNumbers`](#equalWithNumbers) does the same comparison, but for
numbers _anywhere inside_ a data structure, so that it composes with the rest of
your data:

@docs NaNBehavior, equalWithNumbers


## Collections

@docs ok, err, equalLists, equalDicts, equalSets


## Customizing

These functions will let you build your own expectations.

@docs pass, fail, onFail
@docs passesAll, passesOneOf


## Guide to Floating Point Comparison

In general, if you are multiplying, you want relative tolerance, and if you're adding,
you want absolute tolerance. If you are doing both, you want both kinds of tolerance,
or to split the calculation into smaller parts for testing.


### Absolute Tolerance

Let's say we want to figure out if our estimation of pi is precise enough.

Is `3.14` within `0.01` of `pi`? Yes, because `3.13 < pi < 3.15`.

    test "3.14 approximates pi with absolute precision" <|
        \_ ->
            3.14 |> Expect.within (Absolute 0.01) pi


### Relative Tolerance

What if we also want to know if our circle circumference estimation is close enough?

Let's say our circle has a radius of `r` meters. The formula for circle circumference is `C=2*r*pi`.
To make the calculations a bit easier ([ahem](https://tauday.com/tau-manifesto)), we'll look at half the circumference; `C/2=r*pi`.
Is `r * 3.14` within `0.01` of `r * pi`?
That depends, what does `r` equal? If `r` is `0.01`mm, or `0.00001` meters, we're comparing
`0.00001 * 3.14 - 0.01 < r * pi < 0.00001 * 3.14 + 0.01` or `-0.0099686 < 0.0000314159 < 0.0100314`.
That's a huge tolerance! A circumference that is _a thousand times longer_ than we expected would pass that test!

On the other hand, if `r` is very large, we're going to need many more digits of pi.
For an absolute tolerance of `0.01` and a pi estimation of `3.14`, this expectation only passes if `r < 2*pi`.

If we use a relative tolerance of `0.01` instead, the circle area comparison becomes much better. Is `r * 3.14` within
`1%` of `r * pi`? Yes! In fact, three digits of pi approximation is always good enough for a 0.1% relative tolerance,
as long as `r` isn't [too close to zero](https://en.wikipedia.org/wiki/Denormal_number).

    fuzz
        "Circle half-circumference with relative tolerance"
        (floatRange 0.000001 100000)
        (\r -> r * 3.14 |> Expect.within (Relative 0.001) (r * pi))


### Trouble with Numbers Near Zero

If you are adding things near zero, you probably want absolute tolerance. If you're comparing values between `-1` and `1`, you should consider using absolute tolerance.

For example: Is `1 + 2 - 3` within `1%` of `0`? Well, if `1`, `2` and `3` have any amount of rounding error, you might not get exactly zero. What is `1%` above and below `0`? Zero. We just lost all tolerance. Even if we hard-code the numbers, we might not get exactly zero; `0.1 + 0.2` rounds to a value just above `0.3`, since computers, counting in binary, cannot write down any of those three numbers using a finite number of digits, just like we cannot write `0.333...` exactly in base 10.

Another example is comparing values that are on either side of zero. `0.0001` is more than `100%` away from `-0.0001`. In fact, `infinity` is closer to `0.0001` than `0.0001` is to `-0.0001`, if you are using a relative tolerance. Twice as close, actually. So even though both `0.0001` and `-0.0001` could be considered very close to zero, they are very far apart relative to each other. The same argument applies for any number of zeroes.

-}

import Dict exposing (Dict)
import Fuzz.Internal
import Set exposing (Set)
import Test.Distribution
import Test.Expectation
import Test.Internal as Internal
import Test.Internal.Equality as Equality
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| The result of a single test run: either a [`pass`](#pass) or a
[`fail`](#fail).
-}
type alias Expectation =
    Test.Expectation.Expectation


{-| Passes if the arguments are equal.

    Expect.equal 0 (List.length [])

    -- Passes because (0 == 0) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because the expected value didn't split the space in "Betty Botter"
    String.split " " "Betty Botter bought some butter"
        |> Expect.equal [ "Betty Botter", "bought", "some", "butter" ]

    {-

    [ "Betty", "Botter", "bought", "some", "butter" ]
    ╷
    │ Expect.equal
    ╵
    [ "Betty Botter", "bought", "some", "butter" ]

    -}

Lists, arrays, dicts and sets get a diff of their contents in the failure
message, so you don't need a special function for them:

    -- Fails
    Dict.fromList [ ( 1, "one" ), ( 2, "too" ) ]
        |> Expect.equal (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])

    {-

    Dict.fromList [(1,"one"),(2,"too")]
    ╷
    │ Expect.equal
    ╵
    Dict.fromList [(1,"one"),(2,"two")]

    These keys are extra: [ (2,"too") ]
    These keys are missing: [ (2,"two") ]

    -}

Do not equate `Float` values; use [`equalWithNumbers`](#equalWithNumbers) or
[`within`](#within) instead.

-}
equal : a -> a -> Expectation
equal expected actual =
    case floatAdvice "Expect.equal" expected actual of
        Just advice ->
            badUsage advice

        Nothing ->
            equalWith Equality.exact "Expect.equal" expected actual


{-| Passes if the arguments are not equal.

    -- Passes because (11 /= 100) is True
    90 + 10
        |> Expect.notEqual 11


    -- Fails because (100 /= 100) is False
    90 + 10
        |> Expect.notEqual 100

    {-

    100
    ╷
    │ Expect.notEqual
    ╵
    100

    -}

This is the same as `subject |> Expect.equal expected |> Expect.not`.

-}
notEqual : a -> a -> Expectation
notEqual expected actual =
    case floatAdvice "Expect.notEqual" expected actual of
        Just advice ->
            badUsage advice

        Nothing ->
            testWith Equality "Expect.notEqual" (/=) expected actual


{-| Passes if (and only if) the given expectation fails.

    -- Passes, because (100 == 11) is False
    90
        + 10
        |> Expect.equal 11
        |> Expect.not

    -- Passes, because 3.14 is not that close to pi
    3.14
        |> Expect.within (Absolute 0.0001) pi
        |> Expect.not

This lets you invert _any_ expectation, so expectations don't need to come in
`x` / `notX` pairs:

    Query.fromHtml html
        |> Query.has [ tag "ul" ]
        |> Expect.not

To use it as an argument to [`passesAll`](#passesAll) or
[`Query.each`](Test-Html-Query#each), compose with `>>`:

    Query.each (Query.has [ tag "ul" ] >> Expect.not)

The failure message is the message the inverted expectation would have shown if
it had failed:

    -- Fails, because (100 == 100) is True
    90 + 10
        |> Expect.equal 100
        |> Expect.not

    {-

    100
    ╵
    │ |> Expect.not (Expect.equal)
    ╷
    100

    -}

Expectations that are _invalid_ rather than failing - like `Expect.all []`, or a
negative tolerance - are not inverted: inverting a broken test would turn it into
a passing one.

-}
not : Expectation -> Expectation
not expectation =
    case expectation of
        Test.Expectation.Pass { distributionReport, ifInverted } ->
            let
                failure : Test.Expectation.InvertedFailure
                failure =
                    case ifInverted of
                        Just toFailure ->
                            toFailure ()

                        Nothing ->
                            { given = Nothing
                            , description = "Expect.not: the given expectation passed, but it was expected to fail."
                            , reason = Custom
                            }
            in
            Test.Expectation.Fail
                { given = failure.given
                , distributionReport = distributionReport
                , description = failure.description
                , reason = failure.reason
                }

        Test.Expectation.Fail failure ->
            case failure.reason of
                Invalid _ ->
                    expectation

                _ ->
                    Test.Expectation.Pass
                        { distributionReport = failure.distributionReport
                        , ifInverted =
                            Just
                                (\() ->
                                    { given = failure.given
                                    , description = failure.description
                                    , reason = failure.reason
                                    }
                                )
                        }


{-| Passes if the second argument is less than the first.

    Expect.lessThan 1 (List.length [])

    -- Passes because (0 < 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 < -1) is False
    List.length []
        |> Expect.lessThan -1


    {-

    0
    ╷
    │ Expect.lessThan
    ╵
    -1

    -}

Do not equate `Float` values; use [`notWithin`](#notWithin) instead.

-}
lessThan : comparable -> comparable -> Expectation
lessThan =
    compareWith "Expect.lessThan" (<)


{-| Passes if the second argument is less than or equal to the first.

    Expect.atMost 1 (List.length [])

    -- Passes because (0 <= 1) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 <= -3) is False
    List.length []
        |> Expect.atMost -3

    {-

    0
    ╷
    │ Expect.atMost
    ╵
    -3

    -}

-}
atMost : comparable -> comparable -> Expectation
atMost =
    compareWith "Expect.atMost" (<=)


{-| Passes if the second argument is greater than the first.

    Expect.greaterThan -2 List.length []

    -- Passes because (0 > -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 > 1) is False
    List.length []
        |> Expect.greaterThan 1

    {-

    0
    ╷
    │ Expect.greaterThan
    ╵
    1

    -}

-}
greaterThan : comparable -> comparable -> Expectation
greaterThan =
    compareWith "Expect.greaterThan" (>)


{-| Passes if the second argument is greater than or equal to the first.

    Expect.atLeast -2 (List.length [])

    -- Passes because (0 >= -2) is True

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because (0 >= 3) is False
    List.length []
        |> Expect.atLeast 3

    {-

    0
    ╷
    │ Expect.atLeast
    ╵
    3

    -}

-}
atLeast : comparable -> comparable -> Expectation
atLeast =
    compareWith "Expect.atLeast" (>=)


{-| A type to describe how close a floating point number must be to the expected value for the test to pass. This may be
specified as absolute or relative.

`AbsoluteOrRelative` tolerance uses a logical OR between the absolute (specified first) and relative tolerance. If you
want a logical AND, use [`Expect.all`](#all).

-}
type FloatingPointTolerance
    = Absolute Float
    | Relative Float
    | AbsoluteOrRelative Float Float


{-| Passes if the second and third arguments are equal within a tolerance
specified by the first argument. This is intended to avoid failing because of
minor inaccuracies introduced by floating point arithmetic.

    -- Fails because 0.1 + 0.2 == 0.30000000000000004 (0.1 is non-terminating in base 2)
    0.1 + 0.2 |> Expect.equal 0.3

    -- So instead write this test, which passes
    0.1 + 0.2 |> Expect.within (Absolute 0.000000001) 0.3

Failures resemble code written in pipeline style, so you can tell
which argument is which:

    -- Fails because 3.14 is not close enough to pi
    3.14 |> Expect.within (Absolute 0.0001) pi

    {-

    3.14
    ╷
    │ Expect.within Absolute 0.0001
    ╵
    3.141592653589793

    -}

-}
within : FloatingPointTolerance -> Float -> Float -> Expectation
within tolerance lower upper =
    nonNegativeToleranceError tolerance "within" <|
        compareWith ("Expect.within " ++ Internal.toString tolerance)
            (withinCompare tolerance)
            lower
            upper


{-| Passes if (and only if) a call to `within` with the same arguments would have failed.
-}
notWithin : FloatingPointTolerance -> Float -> Float -> Expectation
notWithin tolerance lower upper =
    nonNegativeToleranceError tolerance "notWithin" <|
        compareWith ("Expect.notWithin " ++ Internal.toString tolerance)
            (\a b -> Basics.not <| withinCompare tolerance a b)
            lower
            upper


{-| Whether two `NaN`s should count as equal to each other.

`NaN /= NaN` in both Elm and IEEE 754, which is usually what you want, but is
annoying when you're testing a computation that's _supposed_ to result in `NaN`.

-}
type NaNBehavior
    = NaNsAlwaysEqual
    | NaNsNeverEqual


{-| Like [`equal`](#equal), but compares numbers with the given tolerance (and
`NaN`s with the given behavior) instead of exactly - no matter how deep inside
the compared values those numbers are.

    -- Fails: Expect.within only works on two bare Floats
    { pi = 3.14 } |> Expect.within (Absolute 0.01) { pi = pi }

    -- Passes
    { pi = 3.14 }
        |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0.01) { pi = pi }

    -- Passes: this is what `Expect.within (Absolute 0.01) pi` does
    3.14 |> Expect.equalWithNumbers NaNsNeverEqual (Absolute 0.01) pi

    -- Passes: unlike `==`, this can be told to accept NaNs
    ( 0 / 0, 1 )
        |> Expect.equalWithNumbers NaNsAlwaysEqual (Absolute 0) ( 0 / 0, 1 )

Note that `Int` and `Float` are the same type at runtime, so the tolerance
applies to `Int`s inside the compared values as well. Use `Absolute 0` /
`Relative 0` if you only want the `NaN` behavior.

-}
equalWithNumbers : NaNBehavior -> FloatingPointTolerance -> a -> a -> Expectation
equalWithNumbers nanBehavior tolerance expected actual =
    nonNegativeToleranceError tolerance "equalWithNumbers" <|
        equalWith
            { nansAreEqual = nanBehavior == NaNsAlwaysEqual
            , absolute = absolute tolerance
            , relative = relative tolerance
            }
            ("Expect.equalWithNumbers "
                ++ Internal.toString nanBehavior
                ++ " ("
                ++ Internal.toString tolerance
                ++ ")"
            )
            expected
            actual


{-| Passes if the
[`Result`](https://package.elm-lang.org/packages/lang/core/latest/Result) is
an `Ok` rather than `Err`. This is useful for tests where you expect not to see
an error, but you don't care what the actual result is.

_(Tip: If your function returns a `Maybe` instead, consider `Expect.notEqual Nothing`.)_

    -- Passes
    String.toInt "20"
        |> Result.fromMaybe "not an int"
        |> Expect.ok

Test failures will be printed with the unexpected `Err` value contrasting with
any `Ok`.

    -- Fails
    String.toInt "not an int"
        |> Result.fromMaybe "not an int"
        |> Expect.ok

    {-

    Err "not an int"
    ╷
    │ Expect.ok
    ╵
    Ok _

    -}

-}
ok : Result a b -> Expectation
ok result =
    case result of
        Ok _ ->
            passWith
                (\() ->
                    { given = Nothing
                    , description = inverted "Expect.ok"
                    , reason = Comparison "Err _" (Internal.toString result)
                    }
                )

        Err _ ->
            Test.Expectation.Fail
                { given = Nothing
                , distributionReport = Fuzz.Internal.noDistribution
                , description = "Expect.ok"
                , reason = Comparison "Ok _" (Internal.toString result)
                }


{-| Passes if the
[`Result`](http://package.elm-lang.org/packages/elm-lang/core/latest/Result) is
an `Err` rather than `Ok`. This is useful for tests where you expect to get an
error but you don't care what the actual error is.

_(Tip: If your function returns a `Maybe` instead, consider `Expect.equal Nothing`.)_

    -- Passes
    String.toInt "not an int"
        |> Result.fromMaybe "not an int"
        |> Expect.err

Test failures will be printed with the unexpected `Ok` value contrasting with
any `Err`.

    -- Fails
    String.toInt "20"
        |> Result.fromMaybe "not an int"
        |> Expect.err

    {-

    Ok 20
    ╷
    │ Expect.err
    ╵
    Err _

    -}

-}
err : Result a b -> Expectation
err result =
    case result of
        Ok _ ->
            Test.Expectation.Fail
                { given = Nothing
                , distributionReport = Fuzz.Internal.noDistribution
                , description = "Expect.err"
                , reason = Comparison "Err _" (Internal.toString result)
                }

        Err _ ->
            passWith
                (\() ->
                    { given = Nothing
                    , description = inverted "Expect.err"
                    , reason = Comparison "Ok _" (Internal.toString result)
                    }
                )


{-| Passes if the arguments are equal lists.

    -- Passes
    [ 1, 2, 3 ]
        |> Expect.equalLists [ 1, 2, 3 ]

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which index the lists first
differed at or which list was longer:

    -- Fails
    [ 1, 2, 4, 6 ]
        |> Expect.equalLists [ 1, 2, 5 ]

    {-

    [1,2,4,6]
    first diff at index index 2: +`4`, -`5`
    ╷
    │ Expect.equalLists
    ╵
    first diff at index index 2: +`5`, -`4`
    [1,2,5]

    -}

[`equal`](#equal) now reports lists the same way, so you can use that instead.

-}
equalLists : List a -> List a -> Expectation
equalLists expected actual =
    equalWith Equality.exact "Expect.equalLists" expected actual


{-| Passes if the arguments are equal dicts.

    -- Passes
    Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ]
        |> Expect.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ) ])

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which keys were missing from
or added to each dict:

    -- Fails
    (Dict.fromList [ ( 1, "one" ), ( 2, "too" ) ])
        |> Expect.equalDicts (Dict.fromList [ ( 1, "one" ), ( 2, "two" ), ( 3, "three" ) ])

    {-

    Dict.fromList [(1,"one"),(2,"too")]
    diff: -[ (2,"two"), (3,"three") ] +[ (2,"too") ]
    ╷
    │ Expect.equalDicts
    ╵
    diff: +[ (2,"two"), (3,"three") ] -[ (2,"too") ]
    Dict.fromList [(1,"one"),(2,"two"),(3,"three")]

    -}

[`equal`](#equal) now reports dicts the same way, so you can use that instead.

-}
equalDicts : Dict comparable a -> Dict comparable a -> Expectation
equalDicts expected actual =
    equalWith Equality.exact "Expect.equalDicts" expected actual


{-| Passes if the arguments are equal sets.

    -- Passes
    Set.fromList [ 1, 2 ]
        |> Expect.equalSets (Set.fromList [ 1, 2 ])

Failures resemble code written in pipeline style, so you can tell
which argument is which, and reports which keys were missing from
or added to each set:

    -- Fails
    (Set.fromList [ 1, 2, 4, 6 ])
        |> Expect.equalSets (Set.fromList [ 1, 2, 5 ])

    {-

    Set.fromList [1,2,4,6]
    diff: -[ 5 ] +[ 4, 6 ]
    ╷
    │ Expect.equalSets
    ╵
    diff: +[ 5 ] -[ 4, 6 ]
    Set.fromList [1,2,5]

    -}

[`equal`](#equal) now reports sets the same way, so you can use that instead.

-}
equalSets : Set comparable -> Set comparable -> Expectation
equalSets expected actual =
    equalWith Equality.exact "Expect.equalSets" expected actual


{-| Always passes.

    import Expect
    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)


    test "Json.Decode.int can decode the number 42." <|
        \_ ->
            case decodeString int "42" of
                Ok _ ->
                    Expect.pass

                Err err ->
                    Expect.fail err

-}
pass : Expectation
pass =
    Test.Expectation.Pass
        { distributionReport = Test.Distribution.NoDistribution ()
        , ifInverted = Nothing
        }


{-| Fails with the given message.

    import Expect
    import Json.Decode exposing (decodeString, int)
    import Test exposing (test)


    test "Json.Decode.int can decode the number 42." <|
        \_ ->
            case decodeString int "42" of
                Ok _ ->
                    Expect.pass

                Err err ->
                    Expect.fail err

-}
fail : String -> Expectation
fail str =
    Test.Expectation.Fail
        { given = Nothing
        , distributionReport = Fuzz.Internal.noDistribution
        , description = str
        , reason = Custom
        }


{-| If the given expectation fails, replace its failure message with a custom one.

    "something"
        |> Expect.equal "something else"
        |> Expect.onFail "thought those two strings would be the same"

-}
onFail : String -> Expectation -> Expectation
onFail str expectation =
    case expectation of
        Test.Expectation.Pass _ ->
            expectation

        Test.Expectation.Fail failure ->
            Test.Expectation.Fail
                { given = failure.given
                , description = str
                , reason = Custom
                , distributionReport = failure.distributionReport
                }


{-| Passes if all given expectations pass.

    Expect.all
        [ user.name |> Expect.notEqual ""
        , user.age |> Expect.atLeast 0
        ]

`Expect.all []` is reported as a test failure.

-}
all : List Expectation -> Expectation
all list =
    if List.isEmpty list then
        Test.Expectation.Fail
            { given = Nothing
            , distributionReport = Fuzz.Internal.noDistribution
            , reason = Invalid EmptyList
            , description = "Expect.all was given an empty list. You must make at least one expectation to have a valid test!"
            }

    else
        allHelp list


{-| Passes if each of the given functions passes when applied to the subject.

See also [`all`](#all).

Useful as an argument to [`Query.each`](Test-Html-Query#each):

    Query.each
        (Expect.passesAll
            [ Query.has [ tag "ul" ]
            , Query.has [ classes [ "items", "active" ] ]
            ]
        )

`Expect.passesAll [] _` is reported as a test failure.

-}
passesAll : List (subject -> Expectation) -> subject -> Expectation
passesAll checks subject =
    all (List.map (\check -> check subject) checks)


allHelp : List Expectation -> Expectation
allHelp list =
    case list of
        [] ->
            passWith
                (\() ->
                    { given = Nothing
                    , description = inverted "Expect.all" ++ ": all of the expectations passed, but at least one of them was expected to fail."
                    , reason = Custom
                    }
                )

        check :: rest ->
            case check of
                Test.Expectation.Pass _ ->
                    allHelp rest

                outcome ->
                    outcome


{-| Passes if at least one of the given expectations passes.

    Expect.oneOf
        [ user.isPremiumMember |> Expect.equal True
        , user.cartTotal |> Expect.atLeast 50
        , user.coupon |> Expect.notEqual Nothing
        ]

If none of them pass, the failure lists all the inner failures.

`Expect.oneOf []` is reported as a test failure.

-}
oneOf : List Expectation -> Expectation
oneOf list =
    if List.isEmpty list then
        Test.Expectation.Fail
            { given = Nothing
            , distributionReport = Fuzz.Internal.noDistribution
            , reason = Invalid EmptyList
            , description = "Expect.oneOf was given an empty list. You must make at least one expectation to have a valid test!"
            }

    else
        oneOfHelp list []


{-| Passes if at least one of the given functions passes when applied to the subject.

See also [`oneOf`](#oneOf).

Useful as an argument to [`Query.each`](Test-Html-Query#each):

    Query.each
        (Expect.passesOneOf
            [ Query.has [ tag "ul" ]
            , Query.has [ tag "ol" ]
            ]
        )

`Expect.passesOneOf [] _` is reported as a test failure.

-}
passesOneOf : List (subject -> Expectation) -> subject -> Expectation
passesOneOf checks subject =
    oneOf (List.map (\check -> check subject) checks)


oneOfHelp :
    List Expectation
    -> List { given : Maybe String, description : String, reason : Reason }
    -> Expectation
oneOfHelp list failuresSoFar =
    case list of
        [] ->
            Test.Expectation.Fail
                { given = Nothing
                , distributionReport = Fuzz.Internal.noDistribution
                , reason = Multiple (List.reverse failuresSoFar)
                , description =
                    "Expect.oneOf: none of the "
                        ++ String.fromInt (List.length failuresSoFar)
                        ++ " expectations passed."
                }

        (Test.Expectation.Pass _) :: _ ->
            passWith
                (\() ->
                    { given = Nothing
                    , description = inverted "Expect.oneOf" ++ ": one of the expectations passed, but all of them were expected to fail."
                    , reason = Custom
                    }
                )

        (Test.Expectation.Fail failure) :: rest ->
            oneOfHelp rest
                ({ given = failure.given
                 , description = failure.description
                 , reason = failure.reason
                 }
                    :: failuresSoFar
                )



{---- Private helper functions ----}


{-| A pass that knows what to report if somebody inverts it with
[`not`](#not).
-}
passWith : (() -> Test.Expectation.InvertedFailure) -> Expectation
passWith ifInverted =
    Test.Expectation.Pass
        { distributionReport = Fuzz.Internal.noDistribution
        , ifInverted = Just ifInverted
        }


{-| The label to show for an expectation that [`not`](#not) inverted, e.g.
"Expect.not (Expect.equal)".
-}
inverted : String -> String
inverted label =
    "Expect.not (" ++ label ++ ")"


{-| Fails because the test itself is wrong, rather than because the thing being
tested is. [`not`](#not) refuses to invert these into passes.
-}
badUsage : String -> Expectation
badUsage description =
    Test.Expectation.Fail
        { given = Nothing
        , distributionReport = Fuzz.Internal.noDistribution
        , description = description
        , reason = Invalid BadUsage
        }


{-| Nudge people away from exact equality of `Float`s. String arg is the label,
e.g. "Expect.equal".
-}
floatAdvice : String -> a -> b -> Maybe String
floatAdvice label expected actual =
    let
        isJust : Maybe x -> Bool
        isJust x =
            case x of
                Just _ ->
                    True

                Nothing ->
                    False

        isFloat : String -> Bool
        isFloat x =
            isJust (String.toFloat x) && Basics.not (isJust (String.toInt x))

        usesFloats : Bool
        usesFloats =
            isFloat (Internal.toString actual) || isFloat (Internal.toString expected)
    in
    if usesFloats then
        if String.contains "not" label then
            Just "Do not use Expect.notEqual with floats. Use Expect.not (Expect.equalWithNumbers ...) or Expect.notWithin instead."

        else
            Just "Do not use Expect.equal with floats. Use Expect.equalWithNumbers or Expect.within instead."

    else
        Nothing


{-| Deep equality, with the nicest failure reason we can produce for whatever
the two values turn out to be at runtime.

String arg is the label, e.g. "Expect.equal".

-}
equalWith : Equality.Tolerance -> String -> a -> a -> Expectation
equalWith tolerance label expected actual =
    if Equality.deepEqual tolerance expected actual then
        passWith
            (\() ->
                { given = Nothing
                , description = inverted label

                {- The rich list/dict/set diffs only make sense for values that
                   actually differ, and these ones don't.
                -}
                , reason = Equality (Internal.toString expected) (Internal.toString actual)
                }
            )

    else
        Test.Expectation.Fail
            { given = Nothing
            , distributionReport = Fuzz.Internal.noDistribution
            , description = label
            , reason = inequalityReason tolerance expected actual
            }


inequalityReason : Equality.Tolerance -> a -> a -> Reason
inequalityReason tolerance expected actual =
    case ( Equality.structureOf expected, Equality.structureOf actual ) of
        ( Equality.AList, Equality.AList ) ->
            listDiff (Equality.listItems expected) (Equality.listItems actual)

        ( Equality.AnArray, Equality.AnArray ) ->
            listDiff (Equality.arrayItems expected) (Equality.arrayItems actual)

        ( Equality.ASet, Equality.ASet ) ->
            collectionDiff tolerance
                expected
                actual
                (Equality.setItems expected)
                (Equality.setItems actual)

        ( Equality.ADict, Equality.ADict ) ->
            collectionDiff tolerance
                expected
                actual
                (Equality.dictItems expected)
                (Equality.dictItems actual)

        _ ->
            Equality (Internal.toString expected) (Internal.toString actual)


listDiff : List item -> List item -> Reason
listDiff expectedItems actualItems =
    ListDiff
        (List.map Internal.toString expectedItems)
        (List.map Internal.toString actualItems)


collectionDiff : Equality.Tolerance -> a -> a -> List item -> List item -> Reason
collectionDiff tolerance expected actual expectedItems actualItems =
    let
        isMissingFrom : List item -> item -> Bool
        isMissingFrom items item =
            Basics.not (List.any (Equality.deepEqual tolerance item) items)
    in
    CollectionDiff
        { expected = Internal.toString expected
        , actual = Internal.toString actual
        , extra =
            actualItems
                |> List.filter (isMissingFrom expectedItems)
                |> List.map Internal.toString
        , missing =
            expectedItems
                |> List.filter (isMissingFrom actualItems)
                |> List.map Internal.toString
        }


compareWith : String -> (a -> b -> Bool) -> b -> a -> Expectation
compareWith =
    testWith Comparison


testWith : (String -> String -> Reason) -> String -> (a -> b -> Bool) -> b -> a -> Expectation
testWith makeReason label runTest expected actual =
    if runTest actual expected then
        passWith
            (\() ->
                { given = Nothing
                , description = inverted label
                , reason = makeReason (Internal.toString expected) (Internal.toString actual)
                }
            )

    else
        Test.Expectation.Fail
            { given = Nothing
            , distributionReport = Fuzz.Internal.noDistribution
            , description = label
            , reason = makeReason (Internal.toString expected) (Internal.toString actual)
            }



{---- Private *floating point* helper functions ----}


absolute : FloatingPointTolerance -> Float
absolute tolerance =
    case tolerance of
        Absolute val ->
            val

        AbsoluteOrRelative val _ ->
            val

        _ ->
            0


relative : FloatingPointTolerance -> Float
relative tolerance =
    case tolerance of
        Relative val ->
            val

        AbsoluteOrRelative _ val ->
            val

        _ ->
            0


nonNegativeToleranceError : FloatingPointTolerance -> String -> Expectation -> Expectation
nonNegativeToleranceError tolerance name result =
    if absolute tolerance < 0 && relative tolerance < 0 then
        Test.Expectation.Fail
            { given = Nothing
            , distributionReport = Fuzz.Internal.noDistribution
            , description = "Expect." ++ name ++ " was given negative absolute and relative tolerances"
            , reason = Invalid BadUsage
            }

    else if absolute tolerance < 0 then
        Test.Expectation.Fail
            { given = Nothing
            , distributionReport = Fuzz.Internal.noDistribution
            , description = "Expect." ++ name ++ " was given a negative absolute tolerance"
            , reason = Invalid BadUsage
            }

    else if relative tolerance < 0 then
        Test.Expectation.Fail
            { given = Nothing
            , distributionReport = Fuzz.Internal.noDistribution
            , description = "Expect." ++ name ++ " was given a negative relative tolerance"
            , reason = Invalid BadUsage
            }

    else
        result


withinCompare : FloatingPointTolerance -> Float -> Float -> Bool
withinCompare tolerance a b =
    (a == b)
        -- within absolute tolerance
        || (a - absolute tolerance <= b && b <= a + absolute tolerance)
        -- within relative tolerance
        || (a - abs (a * relative tolerance) <= b && b <= a + abs (a * relative tolerance))
        || (b - abs (b * relative tolerance) <= a && a <= b + abs (b * relative tolerance))
