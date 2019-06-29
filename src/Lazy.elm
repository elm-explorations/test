module Lazy exposing
    ( Lazy
    , force
    , lazy
    , map
    )

{-| This library lets you delay a computation until later.

This implementation is private to elm-test and has non-essential functions removed.
If you need a complete Lazy List implementation, one can be found on elm-package.

-}


{-| A wrapper around a value that will be lazily evaluated.
-}
type Lazy a
    = Lazy (() -> a)
    | Evaluated a


{-| Delay the evaluation of a value until later. For example, maybe we will
need to generate a very long list and find its sum, but we do not want to do
it unless it is absolutely necessary.

    lazySum : Lazy Int
    lazySum =
        lazy (\() -> sum <| List.range 1 1000000)

Now we only pay for `lazySum` if we actually need it.

-}
lazy : (() -> a) -> Lazy a
lazy thunk =
    Lazy thunk


{-| Force the evaluation of a lazy value. This means we only pay for the
computation when we need it. Here is a rather contrived example.

    lazySum : Lazy Int
    lazySum =
        lazy (\() -> List.sum <| List.range 1 1000000)

    sums : ( Int, Int, Int )
    sums =
        ( force lazySum, force lazySum, force lazySum )

-}
force : Lazy a -> a
force piece =
    case piece of
        Evaluated a ->
            a

        Lazy thunk ->
            thunk ()


{-| Lazily apply a function to a lazy value.

    lazySum : Lazy Int
    lazySum =
        map List.sum (lazy (\() -> <| List.range 1 1000000)

The resulting lazy value will create a big list and sum it up when it is
finally forced.

-}
map : (a -> b) -> Lazy a -> Lazy b
map f a =
    lazy (\() -> f (force a))
