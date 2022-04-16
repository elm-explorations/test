module MicroBitwiseExtra exposing
    ( int52FromTuple
    , int52ToTuple
    , isBitSet
    , keepBits
    , reverse52Bits
    , reverseNBits
    , signedToUnsigned
    )

import Array exposing (Array)
import Bitwise


isBitSet : Int -> Int -> Bool
isBitSet index num =
    if index >= 32 then
        -- let's move this into the realm of 32bit numbers and try again
        isBitSet (index - 32) (num // 0xFFFFFFFF)

    else
        (num
            |> Bitwise.shiftRightZfBy index
            |> Bitwise.and 1
        )
            == 1


int52FromTuple : ( Int, Int ) -> Int
int52FromTuple ( highBits, lowBits ) =
    (+)
        (highBits
            |> keepBits 20
            |> signedToUnsigned
            -- Bitwise.shiftLeftBy 32 would be buggy, so we do:
            |> (*) 0x0000000100000000
        )
        (lowBits
            |> signedToUnsigned
            |> keepBits 32
        )


int52ToTuple : Int -> ( Int, Int )
int52ToTuple n =
    ( -- Bitwise.shiftRightZfBy 32 would be buggy, so we do:
      (n // 0x0000000100000000)
        |> keepBits 20
        |> signedToUnsigned
    , n
        |> keepBits 32
        |> signedToUnsigned
    )


ones : Int -> Int
ones count =
    if count == 32 then
        -- edge case...
        0xFFFFFFFF

    else
        Bitwise.shiftLeftBy count 1 - 1


keepBits : Int -> Int -> Int
keepBits count num =
    Bitwise.and (ones count) num


reverseByte : Int -> Int
reverseByte b_ =
    let
        go : Int -> Int -> Int -> Int
        go result i b =
            if i <= 0 then
                result

            else
                let
                    newResult =
                        result
                            |> Bitwise.shiftLeftBy 1
                            |> Bitwise.or (Bitwise.and 1 b)

                    newB =
                        Bitwise.shiftRightBy 1 b
                in
                go newResult (i - 1) newB
    in
    go 0 8 b_


reverseByteTable : Array Int
reverseByteTable =
    -- TODO PERF `Dict Int Int` or `IntDict Int` or `List` instead? Benchmark?
    List.range 0 255
        |> List.map reverseByte
        |> Array.fromList


memoizedReverseByte : Int -> Int
memoizedReverseByte b =
    Array.get b reverseByteTable
        -- shouldn't happen, we should only get values 0..255 here
        |> Maybe.withDefault 0


reverse32Bits : Int -> Int
reverse32Bits n =
    Bitwise.shiftLeftBy 24 (memoizedReverseByte (Bitwise.and 0xFF (Bitwise.shiftRightBy 0 n)))
        |> Bitwise.or (Bitwise.shiftLeftBy 16 (memoizedReverseByte (Bitwise.and 0xFF (Bitwise.shiftRightBy 8 n))))
        |> Bitwise.or (Bitwise.shiftLeftBy 8 (memoizedReverseByte (Bitwise.and 0xFF (Bitwise.shiftRightBy 16 n))))
        |> Bitwise.or (Bitwise.shiftLeftBy 0 (memoizedReverseByte (Bitwise.and 0xFF (Bitwise.shiftRightBy 24 n))))
        |> signedToUnsigned


signedToUnsigned : Int -> Int
signedToUnsigned =
    Bitwise.shiftRightZfBy 0


{-| Beware,

  - `value` must be at most a 32bit number
  - `count` must be lower than the number of bits in `value`.

-}
reverseNBits : Int -> Int -> Int
reverseNBits count value =
    value
        |> reverse32Bits
        |> Bitwise.shiftRightZfBy (32 - count)


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
            reverse32Bits lo32

        reversedHi20 =
            reverseNBits 20 hi20

        newHi20 =
            reversedLo32
                |> Bitwise.shiftRightBy 12
                |> keepBits 20
                |> signedToUnsigned

        newLo32 =
            reversedLo32
                |> keepBits 12
                |> Bitwise.shiftLeftBy 20
                |> Bitwise.or reversedHi20
                |> signedToUnsigned
    in
    ( newHi20, newLo32 )



{-
   -- LOGGING


   logInt11 : String -> Int -> Int
   logInt11 =
       logHexBits 11


   logInt32 : String -> Int -> Int
   logInt32 =
       logHexBits 32


   logBinaryInt32 : String -> Int -> Int
   logBinaryInt32 =
       logBinaryBits 32


   logInt52 : String -> Int -> Int
   logInt52 =
       logHexBits 52


   logBinaryInt52 : String -> Int -> Int
   logBinaryInt52 =
       logBinaryBits 52


   logTuple52 : String -> ( Int, Int ) -> ( Int, Int )
   logTuple52 label ( hi, lo ) =
       let
           hi_ =
               hexBits 20 hi

           lo_ =
               hexBits 32 lo

           _ =
               Debug.log label (hi_ ++ " " ++ lo_)
       in
       ( hi, lo )


   logTuple64 : String -> ( Int, Int ) -> ( Int, Int )
   logTuple64 label ( hi, lo ) =
       let
           hi_ =
               hexBits 32 hi

           lo_ =
               hexBits 32 lo

           _ =
               Debug.log label (hi_ ++ " " ++ lo_)
       in
       ( hi, lo )


   logBinaryTuple64 : String -> ( Int, Int ) -> ( Int, Int )
   logBinaryTuple64 label ( hi, lo ) =
       let
           hi_ =
               binaryBits 32 hi

           lo_ =
               binaryBits 32 lo

           _ =
               Debug.log label (hi_ ++ " " ++ lo_)
       in
       ( hi, lo )


   logHexBits : Int -> String -> Int -> Int
   logHexBits bits label n =
       let
           _ =
               Debug.log label (hexBits bits n)
       in
       n


   logBinaryBits : Int -> String -> Int -> Int
   logBinaryBits bits label n =
       let
           _ =
               Debug.log label (binaryBits bits n)
       in
       n


   hexBits : Int -> Int -> String
   hexBits bits n =
       let
           bits_ : List Int
           bits_ =
               List.range 0 (bits - 1)
                   |> List.map
                       (\i ->
                           if isBitSet i n then
                               1

                           else
                               0
                       )

           toHex : List Int -> Char
           toHex xs =
               case xs of
                   [ 0 ] ->
                       '0'

                   [ 1 ] ->
                       '1'

                   [ 0, 0 ] ->
                       '0'

                   [ 0, 1 ] ->
                       '1'

                   [ 1, 0 ] ->
                       '2'

                   [ 1, 1 ] ->
                       '3'

                   [ 0, 0, 0 ] ->
                       '0'

                   [ 0, 0, 1 ] ->
                       '1'

                   [ 0, 1, 0 ] ->
                       '2'

                   [ 0, 1, 1 ] ->
                       '3'

                   [ 1, 0, 0 ] ->
                       '4'

                   [ 1, 0, 1 ] ->
                       '5'

                   [ 1, 1, 0 ] ->
                       '6'

                   [ 1, 1, 1 ] ->
                       '7'

                   [ 0, 0, 0, 0 ] ->
                       '0'

                   [ 0, 0, 0, 1 ] ->
                       '1'

                   [ 0, 0, 1, 0 ] ->
                       '2'

                   [ 0, 0, 1, 1 ] ->
                       '3'

                   [ 0, 1, 0, 0 ] ->
                       '4'

                   [ 0, 1, 0, 1 ] ->
                       '5'

                   [ 0, 1, 1, 0 ] ->
                       '6'

                   [ 0, 1, 1, 1 ] ->
                       '7'

                   [ 1, 0, 0, 0 ] ->
                       '8'

                   [ 1, 0, 0, 1 ] ->
                       '9'

                   [ 1, 0, 1, 0 ] ->
                       'A'

                   [ 1, 0, 1, 1 ] ->
                       'B'

                   [ 1, 1, 0, 0 ] ->
                       'C'

                   [ 1, 1, 0, 1 ] ->
                       'D'

                   [ 1, 1, 1, 0 ] ->
                       'E'

                   [ 1, 1, 1, 1 ] ->
                       'F'

                   _ ->
                       '?'
       in
       bits_
           |> List.greedyGroupsOf 4
           |> List.map (List.reverse >> toHex)
           |> List.reverse
           |> String.fromList
           |> (\s -> "0x" ++ s)


   binaryBits : Int -> Int -> String
   binaryBits bits n =
       let
           bits_ : List Char
           bits_ =
               List.range 0 (bits - 1)
                   |> List.map
                       (\i ->
                           if isBitSet i n then
                               '1'

                           else
                               '0'
                       )
       in
       bits_
           |> List.greedyGroupsOf 8
           |> List.map (List.reverse >> String.fromList)
           |> List.reverse
           |> String.join "_"
-}
