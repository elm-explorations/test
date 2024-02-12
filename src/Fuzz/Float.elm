module Fuzz.Float ( fractionalFloat
    , getExponent
    , getMantissa
    , isFractional
    , maxFractionalFloat
    , setExponent
    , setMantissa
    , wellShrinkingFloat
    )
 where

import Array (Array)
import Array as Array
import Bitwise as Bitwise
import Bytes (Endianness(..))
import Bytes as Bytes
import Bytes.Decode as Bytes.Decode
import Bytes.Encode as Bytes.Encode
import MicroBitwiseExtra as Bitwise



{-| Input: two 32bit integers
Output: Float (in the full range of IEEE-754 doubles)
-}
fromBytes :: {a::Int, b::Int } -> Float
fromBytes {a:hi, b:lo } =
    Bytes.Encode.sequence
        [ Bytes.Encode.unsignedInt32 BE hi
        , Bytes.Encode.unsignedInt32 BE lo
        ]
        |> Bytes.Encode.encode
        |> Bytes.Decode.decode (Bytes.Decode.float64 BE)
        |> Maybe.withDefault (0 / 0)


{-| Converts the two 32bit integers to a float 0..1 (never reaching 1).
Discards 12 most significant bits of `hi`.
-}
fractionalFloat :: {a::Int, b::Int } -> Float
fractionalFloat {a:hi, b:lo } =
    {- Keep only the mantissa bits (0x000FFFFF 0xFFFFFFFF)
       and divide them by the maximal mantissa.
    -}
    fromBytes {a:Bitwise.and 0x000FFFFF hi, b:lo }
        / maxMantissa


{-| Mantissa is the fractional part of the Float.
-}
maxMantissa :: Float
maxMantissa =
    fromBytes {a:0x000FFFFF, b:0xFFFFFFFF }


{-| AKA, the float just below 1.
-}
maxFractionalFloat :: Float
maxFractionalFloat =
    1 - 2 ^ -52


{-| Generates float in the whole double range.

Does some rearranging of which bits mean what (compared to IEEE-754 arrangement
of sign, exponent and mantissa) so that lexicographically smaller `(hi, lo)`
pairs of 32bit integers give simpler values.

Elm doesn't have 64bit integers, so particularly for any mantissa operations we
will have to work on a pair of 32bit integers.

---

There's a lot of magic here. It comes originally from Hypothesis:
<https://github.com/HypothesisWorks/hypothesis/blob/d55849df92d01a25364aa21a1adb310ee0a3a390/hypothesis-python/src/hypothesis/internal/conjecture/floats.py>

Notable comment from Hypothesis docs:

> It doesn't make any attempt to get a good distribution, only to get a format
> that will shrink well.

For reference, here's the IEEE-754 double (binary64) layout:

     [ sign ] [ biased exponent ] [ mantissa (decimal digits) ]
       1 bit    11 bits             52 bits

And Hypothesis layout:

     [ isFractional flag ] [ rest ]
       1 bit                 63 bits

Where `rest` is interpreted in one of two ways based on the `isFractional` flag:

When isFractional = False:

     [ ignored ] [ integer ]
       7 bits      56 bits

(Note these 56 bits are more than float integers (53 bits) can represent so
multiple values will map to the same float.)

When isFractional = True:

     [ reordered biased exponent ] [ reordered mantissa ]
       11 bits                       52 bits

For how the reordering works, read the Hypothesis file, but essentially it will
prefer simpler fractions and smaller positive numbers, so eg. 0, 0.5, 0.25, ...

-}
wellShrinkingFloat :: {a::Int, b::Int } -> Float
wellShrinkingFloat {a:hi, b:lo } =
    if isFractional hi then
        let
            rawExponent :: Int
            rawExponent =
                getExponent {a:hi, b:lo }

            exponent :: Int
            exponent =
                reorderExponent rawExponent

            unbiasedExponent :: Int
            unbiasedExponent =
                exponent - exponentBias

            rawMantissaTuple :: {a::Int, b::Int }
            rawMantissaTuple =
                getMantissaTuple {a:hi, b:lo }

            {a:mantissaHi, b:mantissaLo } =
                reorderMantissa unbiasedExponent rawMantissaTuple

            newHi :: Int
            newHi =
                Bitwise.or
                    (Bitwise.shiftLeftBy 20 exponent)
                    mantissaHi

            newLo :: Int
            newLo =
                mantissaLo
        in
        fromBytes {a:newHi, b:newLo }

    else
        {a:hi, b:lo }
            |> Bitwise.int52FromTuple
            |> toFloat


exponentBias :: Int
exponentBias =
    1023


reorderExponent :: Int -> Int
reorderExponent e =
    Array.get e exponentMapping
        |> Maybe.withDefault 0


maxExponent :: Int
maxExponent =
    0x07FF


exponentMapping :: Array Int
exponentMapping =
    List.range 0 maxExponent
        |> List.sortBy exponentKey
        |> Array.fromList


exponentKey :: Int -> Int
exponentKey e =
    if e == maxExponent then
        -- sort this one last. `+Inf : Int`
        round (1 / 0)

    else
        let
            unbiased =
                e - exponentBias
        in
        if unbiased < 0 then
            10000 - unbiased

        else
            unbiased


reorderMantissa :: Int -> {a::Int, b::Int } -> {a::Int, b::Int }
reorderMantissa unbiasedExponent {a:hi, b:lo } =
    if unbiasedExponent <= 0 then
        Bitwise.reverse52Bits {a:hi, b:lo }

    else if unbiasedExponent <= 51 then
        let
            nFractionalBits =
                52 - unbiasedExponent

            fractionalPartHi =
                if nFractionalBits >= 32 then
                    Bitwise.keepBits (nFractionalBits - 32) hi

                else
                    0

            fractionalPartLo =
                if nFractionalBits < 32 then
                    Bitwise.keepBits nFractionalBits lo

                else
                    lo

            reversedHi =
                if nFractionalBits >= 32 then
                    Bitwise.reverseNBits (nFractionalBits - 32) fractionalPartHi

                else
                    0

            reversedLo =
                if nFractionalBits < 32 then
                    Bitwise.reverseNBits nFractionalBits lo

                else
                    Bitwise.reverseNBits 32 fractionalPartLo

            newHi =
                hi
                    |> Bitwise.xor fractionalPartHi
                    |> Bitwise.or reversedHi

            newLo =
                lo
                    |> Bitwise.xor fractionalPartLo
                    |> Bitwise.or reversedLo
        in
        {a:newHi, b:newLo }

    else
        {a:hi, b:lo }


getExponent :: {a::Int, b::Int } -> Int
getExponent ( hi, _ ) =
    hi
        |> Bitwise.shiftRightZfBy 20
        |> Bitwise.keepBits 11


getMantissa :: {a::Int, b::Int } -> Int
getMantissa {a:hi, b:lo } =
    {a:hi, b:lo }
        |> getMantissaTuple
        |> Bitwise.int52FromTuple


getMantissaTuple :: {a::Int, b::Int } -> {a::Int, b::Int }
getMantissaTuple {a:hi, b:lo } =
    {a:Bitwise.keepBits 20 hi
    , b:Bitwise.keepBits 32 lo
    }


setExponent :: Int -> {a::Int, b::Int } -> {a::Int, b::Int }
setExponent exponent {a:hi, b:lo } =
    ( hi
        |> Bitwise.and 0x800FFFFF
        |> Bitwise.or
            (exponent
                |> Bitwise.keepBits 11
                |> Bitwise.shiftLeftBy 20
            )
        |> Bitwise.signedToUnsigned
    , lo
    )


setMantissa :: Int -> {a::Int, b::Int } -> {a::Int, b::Int }
setMantissa mantissa ( hi, _ ) =
    let
        {a:mantissaHi, b:mantissaLo } =
            Bitwise.int52ToTuple mantissa
    in
    ( hi
        |> Bitwise.and 0xFFF00000
        |> Bitwise.or
            (mantissaHi
                |> Bitwise.keepBits 20
            )
        |> Bitwise.signedToUnsigned
    , mantissaLo
        |> Bitwise.signedToUnsigned
    )


isFractional :: Int -> Bool
isFractional hi =
    Bitwise.isBitSet 31 hi
