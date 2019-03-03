module Lazy.List exposing
    ( LazyList
    , LazyListView(..)
    , andMap
    , andThen
    , append
    , cons
    , drop
    , empty
    , fromArray
    , fromList
    , headAndTail
    , isEmpty
    , iterate
    , keepIf
    , length
    , map
    , map2
    , map3
    , numbers
    , reduce
    , singleton
    , take
    , takeWhile
    , toArray
    , toList
    , unique
    )

{-| Lazy list implementation in Elm.
-}

import Array exposing (Array)
import Lazy exposing (Lazy, force, lazy)
import List


{-| Analogous to `List` type. This is the actual implementation type for the
`LazyList` type. This type is exposed to the user if the user so wishes to
do pattern matching or understand how the list type works. It is not
recommended to work with this type directly. Try working solely with the
provided functions in the package.
-}
type LazyListView a
    = Nil
    | Cons a (LazyList a)


{-| Lazy List type.
-}
type alias LazyList a =
    Lazy (LazyListView a)


{-| Create an empty list.
-}
empty : LazyList a
empty =
    lazy <|
        \() -> Nil


{-| Create a singleton list.
-}
singleton : a -> LazyList a
singleton a =
    cons a empty


{-| Detect if a list is empty or not.
-}
isEmpty : LazyList a -> Bool
isEmpty list =
    case force list of
        Nil ->
            True

        _ ->
            False


{-| Add a value to the front of a list.
-}
cons : a -> LazyList a -> LazyList a
cons a list =
    lazy <|
        \() ->
            Cons a list


{-| Get the head and tail of a list.
-}
headAndTail : LazyList a -> Maybe ( a, LazyList a )
headAndTail list =
    case force list of
        Nil ->
            Nothing

        Cons first rest ->
            Just ( first, rest )


{-| Append a list to another list.
-}
append : LazyList a -> LazyList a -> LazyList a
append list1 list2 =
    lazy <|
        \() ->
            case force list1 of
                Nil ->
                    force list2

                Cons first rest ->
                    Cons first (append rest list2)


{-| Create an infinite list of applications of a function on some value.

Equivalent to:

    x ::: f x ::: f (f x) ::: f (f (f x)) ::: ... -- etc...

-}
iterate : (a -> a) -> a -> LazyList a
iterate f a =
    lazy <|
        \() ->
            Cons a (iterate f (f a))


{-| The infinite list of counting numbers.

i.e.:

    1 ::: 2 ::: 3 ::: 4 ::: 5 ::: ... -- etc...

-}
numbers : LazyList number
numbers =
    iterate ((+) 1) 1


{-| Take at most `n` many values from a list.
-}
take : Int -> LazyList a -> LazyList a
take n list =
    lazy <|
        \() ->
            if n <= 0 then
                Nil

            else
                case force list of
                    Nil ->
                        Nil

                    Cons first rest ->
                        Cons first (take (n - 1) rest)


{-| Take elements from a list as long as the predicate is satisfied.
-}
takeWhile : (a -> Bool) -> LazyList a -> LazyList a
takeWhile predicate list =
    lazy <|
        \() ->
            case force list of
                Nil ->
                    Nil

                Cons first rest ->
                    if predicate first then
                        Cons first (takeWhile predicate rest)

                    else
                        Nil


{-| Drop at most `n` many values from a list.
-}
drop : Int -> LazyList a -> LazyList a
drop n list =
    lazy <|
        \() ->
            if n <= 0 then
                force list

            else
                case force list of
                    Nil ->
                        Nil

                    Cons first rest ->
                        force (drop (n - 1) rest)


{-| Test if a value is a member of a list.
-}
member : a -> LazyList a -> Bool
member a list =
    case force list of
        Nil ->
            False

        Cons first rest ->
            first == a || member a rest


{-| Get the length of a lazy list.

Warning: This will not terminate if the list is infinite.

-}
length : LazyList a -> Int
length =
    reduce (\_ n -> n + 1) 0


{-| Remove all duplicates from a list and return a list of distinct elements.
-}
unique : LazyList a -> LazyList a
unique list =
    lazy <|
        \() ->
            case force list of
                Nil ->
                    Nil

                Cons first rest ->
                    if member first rest then
                        force (unique rest)

                    else
                        Cons first (unique rest)


{-| Keep all elements in a list that satisfy the given predicate.
-}
keepIf : (a -> Bool) -> LazyList a -> LazyList a
keepIf predicate list =
    lazy <|
        \() ->
            case force list of
                Nil ->
                    Nil

                Cons first rest ->
                    if predicate first then
                        Cons first (keepIf predicate rest)

                    else
                        force (keepIf predicate rest)


{-| Drop all elements in a list that satisfy the given predicate.
-}
dropIf : (a -> Bool) -> LazyList a -> LazyList a
dropIf predicate =
    keepIf (\n -> not (predicate n))


{-| Reduce a list with a given reducer and an initial value.

Example :
reduce (+) 0 (1 ::: 2 ::: 3 ::: 4 ::: empty) == 10

-}
reduce : (a -> b -> b) -> b -> LazyList a -> b
reduce reducer b list =
    case force list of
        Nil ->
            b

        Cons first rest ->
            reduce reducer (reducer first b) rest


{-| Flatten a list of lists into a single list by appending all the inner
lists into one big list.
-}
flatten : LazyList (LazyList a) -> LazyList a
flatten list =
    lazy <|
        \() ->
            case force list of
                Nil ->
                    Nil

                Cons first rest ->
                    force (append first (flatten rest))


{-| Chain list producing operations. Map then flatten.
-}
andThen : (a -> LazyList b) -> LazyList a -> LazyList b
andThen f list =
    map f list |> flatten


{-| Map a function to a list.
-}
map : (a -> b) -> LazyList a -> LazyList b
map f list =
    lazy <|
        \() ->
            case force list of
                Nil ->
                    Nil

                Cons first rest ->
                    Cons (f first) (map f rest)


{-| -}
map2 : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c
map2 f list1 list2 =
    lazy <|
        \() ->
            case force list1 of
                Nil ->
                    Nil

                Cons first1 rest1 ->
                    case force list2 of
                        Nil ->
                            Nil

                        Cons first2 rest2 ->
                            Cons (f first1 first2) (map2 f rest1 rest2)


{-| Known as `mapN` in some circles. Allows you to apply `map` in cases
where then number of arguments are greater than 5.

The argument order is such that it works well with `|>` chains.

-}
andMap : LazyList a -> LazyList (a -> b) -> LazyList b
andMap listVal listFuncs =
    map2 (<|) listFuncs listVal


{-| -}
map3 : (a -> b -> c -> d) -> LazyList a -> LazyList b -> LazyList c -> LazyList d
map3 f list1 list2 list3 =
    lazy <|
        \() ->
            case force list1 of
                Nil ->
                    Nil

                Cons first1 rest1 ->
                    case force list2 of
                        Nil ->
                            Nil

                        Cons first2 rest2 ->
                            case force list3 of
                                Nil ->
                                    Nil

                                Cons first3 rest3 ->
                                    Cons (f first1 first2 first3) (map3 f rest1 rest2 rest3)


{-| Convert a lazy list to a normal list.
-}
toList : LazyList a -> List a
toList list =
    case force list of
        Nil ->
            []

        Cons first rest ->
            first :: toList rest


{-| Convert a normal list to a lazy list.
-}
fromList : List a -> LazyList a
fromList =
    List.foldr cons empty


{-| Convert a lazy list to an array.
-}
toArray : LazyList a -> Array a
toArray list =
    case force list of
        Nil ->
            Array.empty

        Cons first rest ->
            Array.append (Array.push first Array.empty) (toArray rest)


{-| Convert an array to a lazy list.
-}
fromArray : Array a -> LazyList a
fromArray =
    Array.foldr cons empty
