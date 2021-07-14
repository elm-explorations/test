module ShrinkingChallengeTests exposing (shrinkingChallenges)

import Expect exposing (Expectation)
import Fuzz exposing (..)
import Helpers exposing (..)
import Random
import Set
import Test exposing (..)


{-| <https://github.com/jlink/shrinking-challenge>
-}
shrinkingChallenges : Test
shrinkingChallenges =
    describe "Shrinking challenges"
        [ reverse
        , largeUnionList
        , bound5
        , calculator
        , lengthList
        , difference1
        , difference2
        , difference3
        , binHeap
        , coupling
        , deletion
        , distinct
        , nestedLists
        ]


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/reverse.md>
-}
reverse : Test
reverse =
    simplifiesTowardsWith { runs = 1000 }
        "reverse"
        [ 0, 1 ]
        (Fuzz.list Fuzz.int)
        (\list -> list == List.reverse list)


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/large_union_list.md>
-}
largeUnionList : Test
largeUnionList =
    simplifiesTowardsMany
        "large union list"
        [ [ [ 0, 1, 2, 3, 4 ] ]
        , [ [ 0, 1, 2, 3, -1 ] ]
        , [ [ 1, 2, -2, 0, -1 ] ]
        , [ [ 2, -1, -2, 0, 1 ] ]
        ]
        (Fuzz.list (Fuzz.list Fuzz.int))
        (\lists -> Set.size (Set.fromList (List.concat lists)) <= 4)


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/bound5.md>

Elm doesn't have 16bit integers. We'll have to emulate that a little.

Here's an example:
<https://www.mathworks.com/matlabcentral/answers/355611-is-there-a-way-to-do-signed-16-bit-integer-math>

-}
bound5 : Test
bound5 =
    -- TODO filtering stops early when it shouldn't
    let
        bits =
            16

        max =
            2 ^ bits

        maxHalf =
            max // 2

        i16 : Int -> Int
        i16 n =
            ((n + maxHalf) |> modBy max) - maxHalf

        i16Add : Int -> Int -> Int
        i16Add a b =
            i16 (a + b)

        i16Sum : List Int -> Int
        i16Sum ns =
            List.foldl i16Add 0 ns

        boundedList : Fuzzer (List Int)
        boundedList =
            Fuzz.listOfLengthBetween 0 10 (Fuzz.intRange -32768 32767)
                |> Fuzz.filter (\list -> i16Sum list < 256)

        five : Fuzzer (List (List Int))
        five =
            Fuzz.listOfLength 5 boundedList
    in
    simplifiesTowards
        "Bound 5"
        [ [ -32768 ], [ -1 ], [], [], [] ]
        five
        (\lists -> i16Sum (List.concat lists) < 5 * 256)


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/calculator.md>
-}
calculator : Test
calculator =
    let
        exprFuzzer : Int -> Fuzzer CalcExpr
        exprFuzzer maxDepth =
            if maxDepth <= 0 then
                Fuzz.map Int Fuzz.int

            else
                let
                    subExprFuzzer =
                        exprFuzzer (maxDepth - 1)
                in
                Fuzz.oneOf
                    [ Fuzz.map Int Fuzz.int
                    , Fuzz.map2 Add subExprFuzzer subExprFuzzer
                    , Fuzz.map2 Div subExprFuzzer subExprFuzzer
                    ]

        noDivisionByLiteralZero : CalcExpr -> Bool
        noDivisionByLiteralZero expr =
            case expr of
                Div _ (Int 0) ->
                    False

                Int _ ->
                    True

                Add a b ->
                    noDivisionByLiteralZero a
                        && noDivisionByLiteralZero b

                Div a b ->
                    noDivisionByLiteralZero a
                        && noDivisionByLiteralZero b

        eval : CalcExpr -> Maybe Int
        eval expr =
            case expr of
                Int i ->
                    Just i

                Add a b ->
                    Maybe.map2 (+)
                        (eval a)
                        (eval b)

                Div a b ->
                    Maybe.map2
                        (\a_ b_ ->
                            if b_ == 0 then
                                Nothing

                            else
                                Just <| a_ // b_
                        )
                        (eval a)
                        (eval b)
                        |> Maybe.andThen identity
    in
    simplifiesTowardsMany
        "calculator"
        [ Div (Int 0) (Add (Int 0) (Int 0))
        , Div (Int 0) (Div (Int 0) (Int 1))
        ]
        (exprFuzzer 5 |> Fuzz.filter noDivisionByLiteralZero)
        (\expr -> eval expr /= Nothing)


type CalcExpr
    = Int Int
    | Add CalcExpr CalcExpr
    | Div CalcExpr CalcExpr


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/lengthlist.md>
-}
lengthList : Test
lengthList =
    simplifiesTowards
        "lengthList"
        [ 900 ]
        (Fuzz.intRange 1 100
            |> Fuzz.andThen
                (\len -> Fuzz.listOfLength len (Fuzz.intRange 0 1000))
        )
        (\list ->
            case List.maximum list of
                Nothing ->
                    Debug.todo "shouldn't have generated an empty list"

                Just max ->
                    max < 900
        )


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/difference.md>
-}
difference1 : Test
difference1 =
    -- TODO unlikely to hit
    simplifiesTowardsWith { runs = 3000 }
        "difference1"
        ( 10, 10 )
        (Fuzz.pair
            (Fuzz.intRange 0 Random.maxInt)
            (Fuzz.intRange 0 Random.maxInt)
        )
        (\( x, y ) -> x < 10 || x /= y)


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/difference.md>
-}
difference2 : Test
difference2 =
    -- TODO unlikely to hit
    simplifiesTowardsWith { runs = 3000 }
        "difference2"
        ( 10, 6 )
        (Fuzz.pair
            (Fuzz.intRange 0 Random.maxInt)
            (Fuzz.intRange 0 Random.maxInt)
        )
        (\( x, y ) ->
            let
                absDiff =
                    abs (x - y)
            in
            x < 10 || absDiff < 1 || absDiff > 4
        )


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/difference.md>
-}
difference3 : Test
difference3 =
    -- TODO unlikely to hit
    simplifiesTowardsWith { runs = 3000 }
        "difference3"
        ( 10, 9 )
        (Fuzz.pair
            (Fuzz.intRange 0 Random.maxInt)
            (Fuzz.intRange 0 Random.maxInt)
        )
        (\( x, y ) -> x < 10 || abs (x - y) /= 1)


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/binheap.md>
-}
binHeap : Test
binHeap =
    let
        heapFuzzer : Int -> Fuzzer Heap
        heapFuzzer depth =
            if depth <= 0 then
                Fuzz.map (\i -> Heap i Nothing Nothing) Fuzz.int

            else
                Fuzz.map3 Heap
                    Fuzz.int
                    (Fuzz.maybe (heapFuzzer (depth - 1)))
                    (Fuzz.maybe (heapFuzzer (depth - 1)))

        toList : Heap -> List Int
        toList heap =
            let
                go : List Int -> List Heap -> List Int
                go acc stack =
                    case stack of
                        [] ->
                            List.reverse acc

                        (Heap n left right) :: hs ->
                            go
                                (n :: acc)
                                (List.filterMap identity [ left, right ]
                                    ++ hs
                                )
            in
            go [] [ heap ]

        wrongToSortedList : Heap -> List Int
        wrongToSortedList (Heap n left right) =
            n
                :: (mergeHeaps left right
                        |> Maybe.map toList
                        |> Maybe.withDefault []
                   )

        mergeHeaps : Maybe Heap -> Maybe Heap -> Maybe Heap
        mergeHeaps left right =
            case ( left, right ) of
                ( Nothing, _ ) ->
                    right

                ( _, Nothing ) ->
                    left

                ( Just (Heap ln lleft lright), Just (Heap rn rleft rright) ) ->
                    Just <|
                        if ln <= rn then
                            Heap ln (mergeHeaps lright right) lleft

                        else
                            Heap rn (mergeHeaps rright left) rleft
    in
    simplifiesTowards
        "binHeap"
        (Heap 0 Nothing (Just (Heap -1 Nothing Nothing)))
        (heapFuzzer 4)
        (\heap ->
            let
                l1 =
                    toList heap

                l2 =
                    wrongToSortedList heap
            in
            (l2 == List.sort l2) && (List.sort l1 == l2)
        )


type Heap
    = Heap Int (Maybe Heap) (Maybe Heap)


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/coupling.md>
-}
coupling : Test
coupling =
    let
        getAt : Int -> List a -> Maybe a
        getAt index list =
            if index < 0 then
                Nothing

            else
                list
                    |> List.drop index
                    |> List.head
    in
    simplifiesTowards
        "coupling"
        [ 1, 0 ]
        (Fuzz.list (Fuzz.intRange 0 10)
            |> Fuzz.filter
                (\l ->
                    let
                        length =
                            List.length l
                    in
                    List.all (\i -> i < length) l
                )
        )
        (\list ->
            list
                |> List.indexedMap
                    (\i x ->
                        if i /= x then
                            getAt x list /= Just i

                        else
                            True
                    )
                |> List.all identity
        )


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/deletion.md>
-}
deletion : Test
deletion =
    let
        removeFirst : a -> List a -> List a
        removeFirst badX xs =
            go badX xs [] xs

        go : a -> List a -> List a -> List a -> List a
        go badX next prev orig =
            case next of
                [] ->
                    orig

                x :: rest ->
                    if x == badX then
                        List.reverse prev ++ rest

                    else
                        go badX rest (x :: prev) orig
    in
    -- TODO unlikely to hit if ints don't prefer smaller values
    simplifiesTowards
        "deletion"
        ( [ 0, 0 ], 0 )
        (Fuzz.listOfLengthBetween 1 100 Fuzz.int
            |> Fuzz.andThen
                (\list ->
                    Fuzz.pair
                        (Fuzz.constant list)
                        (Fuzz.oneOfValues list)
                )
        )
        (\( list, el ) -> not (List.member el (removeFirst el list)))


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/distinct.md>
-}
distinct : Test
distinct =
    -- ([1,0,0,  1,0,1,  1,0,2,  0],[0, 1, 2])
    -- ([1,0,0,  1,0,1,  1,1,0,  0],[0, 1,-1])
    -- ([1,0,0,  1,1,0,  1,0,1,  0],[0,-1, 1])
    simplifiesTowardsMany
        "distinct"
        [ [ 0, 1, 2 ]
        , [ 0, 1, -1 ]
        , [ 0, -1, 1 ]
        ]
        (Fuzz.list Fuzz.int)
        (\list -> Set.size (Set.fromList list) < 3)


{-| <https://github.com/jlink/shrinking-challenge/blob/836bafa664659a435ae186eed5b87e941228ae3d/challenges/nestedlists.md>
-}
nestedLists : Test
nestedLists =
    simplifiesTowards
        "nestedLists"
        [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ] ]
        (Fuzz.listOfLengthBetween 0 20 (Fuzz.listOfLengthBetween 0 20 Fuzz.int))
        (\lists -> List.sum (List.map List.length lists) <= 10)
