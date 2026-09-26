module Test.Internal.Equality exposing
    ( Tolerance, exact, deepEqual
    , Structure(..), structureOf
    )

{-| Structural equality that knows about numbers, plus just enough runtime
introspection to be able to tell a `List` from a `Dict` from a record.

Both of these need kernel code: `Expect.equal : a -> a -> Expectation` throws
away all the type information we'd need to do either of them in Elm.

@docs Tolerance, exact, deepEqual
@docs Structure, structureOf

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Elm.Kernel.Test
import Set exposing (Set)


{-| How close two numbers need to be to count as equal, and whether two NaNs
count as equal.

`absolute` and `relative` are ORed together, like `Expect.AbsoluteOrRelative`
does; zero tolerance means "must be exactly equal".

-}
type alias Tolerance =
    { nansAreEqual : Bool
    , absolute : Float
    , relative : Float
    }


{-| The tolerance that makes [`deepEqual`](#deepEqual) behave exactly like `==`.
-}
exact : Tolerance
exact =
    { nansAreEqual = False
    , absolute = 0
    , relative = 0
    }


{-| Like `==`, except that numbers (at any depth inside the two values) are
compared with the given tolerance instead of exactly.

    deepEqual { exact | absolute = 0.01 } { pi = 3.14 } { pi = pi }
    --> True

Note `Int` and `Float` are the same thing at runtime, so the tolerance applies
to `Int`s as well.

-}
deepEqual : Tolerance -> a -> a -> Bool
deepEqual tolerance expected actual =
    kernelDeepEqual tolerance.nansAreEqual tolerance.absolute tolerance.relative expected actual


{-| What a value looks like at runtime. Anything we don't have a nicer diff for
(records, custom types, strings, numbers, ...) is `Opaque`.
-}
type Structure key item
    = AList (List item)
    | AnArray (Array item)
    | ASet (Set item)
    | ADict (Dict key item)
    | Opaque


{-| Guess the structure of a value by comparing its constructor tag with the
tags of values whose structure we _do_ know.

We ask the compiled JS for the tags instead of hardcoding them, since they're an
implementation detail of the compiler.

With `--optimize` this always answers `Opaque`: the tags are then small
per-type integers rather than names, so a `List` looks exactly like any other
two-field constructor. Failures then get the plain `Equality` reason instead
of a collection diff. (Test code can't be compiled with `--optimize` anyway -
this package needs `Debug.toString`.)

-}
structureOf : a -> Structure key item
structureOf value =
    let
        tag : String
        tag =
            kernelTag value
    in
    if tag == "" || Basics.not (kernelCanDetectStructure ()) then
        Opaque

    else if tag == tags.listNil || tag == tags.listCons then
        AList (kernelUnsafeCoerce value)

    else if tag == tags.array then
        AnArray (kernelUnsafeCoerce value)

    else if tag == tags.setEmpty || tag == tags.setNonEmpty then
        ASet (kernelUnsafeCoerce value)

    else if tag == tags.dictEmpty || tag == tags.dictNonEmpty then
        ADict (kernelUnsafeCoerce value)

    else
        Opaque



{---- Kernel ----}


kernelDeepEqual : Bool -> Float -> Float -> a -> a -> Bool
kernelDeepEqual =
    Elm.Kernel.Test.deepEqual


kernelTag : a -> String
kernelTag =
    Elm.Kernel.Test.tag


kernelUnsafeCoerce : a -> b
kernelUnsafeCoerce =
    Elm.Kernel.Test.unsafeCoerce


kernelCanDetectStructure : () -> Bool
kernelCanDetectStructure =
    Elm.Kernel.Test.canDetectStructure


tags :
    { listNil : String
    , listCons : String
    , array : String
    , setEmpty : String
    , setNonEmpty : String
    , dictEmpty : String
    , dictNonEmpty : String
    }
tags =
    let
        emptyList : List ()
        emptyList =
            []

        emptyArray : Array ()
        emptyArray =
            Array.empty

        emptySet : Set Int
        emptySet =
            Set.empty

        emptyDict : Dict Int ()
        emptyDict =
            Dict.empty
    in
    { listNil = kernelTag emptyList
    , listCons = kernelTag [ () ]
    , array = kernelTag emptyArray
    , setEmpty = kernelTag emptySet
    , setNonEmpty = kernelTag (Set.singleton 0)
    , dictEmpty = kernelTag emptyDict
    , dictNonEmpty = kernelTag (Dict.singleton 0 ())
    }
