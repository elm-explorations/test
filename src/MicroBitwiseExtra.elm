module MicroBitwiseExtra exposing
    ( combineTo52BitInteger
    , isBitSet
    , keepBits
    , reverse52Bits
    , reverseNBits
    )

import Bitwise
import Elm.Kernel.BitwiseExtra


isBitSet : Int -> Int -> Bool
isBitSet index num =
    (num
        |> Bitwise.shiftRightBy index
        |> Bitwise.and 1
    )
        == 1


combineTo52BitInteger : ( Int, Int ) -> Int
combineTo52BitInteger ( highBits, lowBits ) =
    Bitwise.or
        (highBits |> keepBits 20 |> Bitwise.shiftLeftBy 32)
        (lowBits |> keepBits 32)


ones : Int -> Int
ones count =
    Bitwise.shiftLeftBy count 1 - 1


keepBits : Int -> Int -> Int
keepBits count num =
    Bitwise.and (ones count) num


reverseNBits : Int -> Int -> Int
reverseNBits n bits =
    Elm.Kernel.BitwiseExtra.reverseBits n bits


{-| A bit of bit twiddling between two "containers" to reverse a 52bit number.

Simplified view of the bits:

     hi20         lo32
     [ abcDEFGH ] [ IJKLMNOP ]
     ==>
     newHi20      newLo32
     [ abcPONML ] [ KJIHGFED ]

-}
reverse52Bits : ( Int, Int ) -> ( Int, Int )
reverse52Bits ( hi20, lo32 ) =
    let
        reversedLo32 =
            reverseNBits 32 lo32

        reversedHi20 =
            reverseNBits 20 hi20

        newHi20 =
            reversedLo32
                |> Bitwise.shiftRightBy 12
                |> keepBits 20

        newLo32 =
            reversedLo32
                |> keepBits 12
                |> Bitwise.shiftLeftBy 20
                |> Bitwise.or reversedHi20
    in
    ( newHi20, newLo32 )
