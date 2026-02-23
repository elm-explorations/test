module EffectivenessTests exposing (all)

import EffectivenessSUT.Bst as Bst
import EffectivenessSUT.Bst1 as Bst1
import EffectivenessSUT.Bst2 as Bst2
import EffectivenessSUT.Bst3 as Bst3
import EffectivenessSUT.Bst4 as Bst4
import EffectivenessSUT.Bst5 as Bst5
import EffectivenessSUT.Bst6 as Bst6
import EffectivenessSUT.Bst7 as Bst7
import EffectivenessSUT.Bst8 as Bst8
import EffectivenessSUT.BstCommon exposing (BST(..))
import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


all : Test
all =
    Test.describe "Fuzzer effectiveness benchmark - Binary Search Tree"
        [ makeSuite "bug 1" Bst1.toList Bst1.insert Bst1.delete Bst1.union
        , makeSuite "bug 2" Bst2.toList Bst2.insert Bst2.delete Bst2.union
        , makeSuite "bug 3" Bst3.toList Bst3.insert Bst3.delete Bst3.union
        , makeSuite "bug 4" Bst4.toList Bst4.insert Bst4.delete Bst4.union
        , makeSuite "bug 5" Bst5.toList Bst5.insert Bst5.delete Bst5.union
        , makeSuite "bug 6" Bst6.toList Bst6.insert Bst6.delete Bst6.union
        , makeSuite "bug 7" Bst7.toList Bst7.insert Bst7.delete Bst7.union
        , makeSuite "bug 8" Bst8.toList Bst8.insert Bst8.delete Bst8.union
        ]


makeSuite :
    String
    -> (BST Int Int -> List ( Int, Int ))
    -> (Int -> Int -> BST Int Int -> BST Int Int)
    -> (Int -> BST Int Int -> BST Int Int)
    -> (BST Int Int -> BST Int Int -> BST Int Int)
    -> Test
makeSuite label toList insert delete union =
    --    -- Model-based testing against a List model
    --    Test.describe label
    --        [ Test.fuzz2 bstFuzzer pairFuzzer "insert" <|
    --            \t ( k, v ) ->
    --                toList (insert k v t)
    --                    |> Expect.equal (insertInOrder k v (deleteAll k (toList t)))
    --        , Test.fuzz2 bstFuzzer keyFuzzer "delete" <|
    --            \t k ->
    --                toList (delete k t)
    --                    |> Expect.equal (deleteFirst k (toList t))
    --        , Test.fuzz2 bstFuzzer bstFuzzer "union" <|
    --            \t1 t2 ->
    --                toList (union t1 t2)
    --                    |> Expect.equal (List.sort (unionL (toList t1) (toList t2)))
    --        ]
    -- Oracle test
    Test.describe label
        [ Test.fuzz2 bstFuzzer pairFuzzer "insert" <|
            \t ( k, v ) ->
                insert k v t
                    |> Expect.equal (Bst.insert k v t)
        , Test.fuzz2 bstFuzzer keyFuzzer "delete" <|
            \t k ->
                delete k t
                    |> Expect.equal (Bst.delete k t)
        , Test.fuzz2 bstFuzzer bstFuzzer "union" <|
            \t1 t2 ->
                union t1 t2
                    |> Expect.equal (Bst.union t1 t2)
        ]


insertInOrder : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
insertInOrder k v l =
    case l of
        [] ->
            [ ( k, v ) ]

        (( k1, _ ) as x) :: xs ->
            case compare k k1 of
                GT ->
                    x :: insertInOrder k v xs

                _ ->
                    ( k, v ) :: l


deleteAll : Int -> List ( Int, Int ) -> List ( Int, Int )
deleteAll k l =
    List.filter (\( k1, _ ) -> k1 /= k) l


deleteFirst : Int -> List ( Int, Int ) -> List ( Int, Int )
deleteFirst k l =
    case l of
        [] ->
            []

        (( k1, _ ) as x) :: xs ->
            if k == k1 then
                xs

            else
                x :: deleteFirst k xs


unionL : List ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
unionL l1 l2 =
    l1 ++ List.foldl (\( k, v ) acc -> deleteFirst k acc) (nub l2) l1


nub : List a -> List a
nub l =
    case l of
        [] ->
            []

        x :: xs ->
            x :: nub (List.filter (\y -> x /= y) xs)


bstFuzzer : Fuzzer (BST Int Int)
bstFuzzer =
    Fuzz.listOfLengthBetween 0 20 pairFuzzer
        |> Fuzz.map Bst.fromList


pairFuzzer : Fuzzer ( Int, Int )
pairFuzzer =
    Fuzz.pair keyFuzzer valueFuzzer


keyFuzzer : Fuzzer Int
keyFuzzer =
    Fuzz.intRange 0 10


valueFuzzer : Fuzzer Int
valueFuzzer =
    Fuzz.intRange 0 10
