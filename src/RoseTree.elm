module RoseTree exposing (RoseTree(..), addChild, map, root, singleton)

{-| RoseTree implementation in Elm using Lazy Lists.

This implementation is private to elm-test and has non-essential functions removed.
If you need a complete RoseTree implementation, one can be found on elm-package.

-}

import Lazy.List as LazyList exposing (LazyList, cons)


{-| RoseTree type.
A rosetree is a tree with a root whose children are themselves rosetrees.
-}
type RoseTree a
    = Rose a (LazyList (RoseTree a))


{-| Make a singleton rosetree.
-}
singleton : a -> RoseTree a
singleton a =
    Rose a LazyList.empty


{-| Get the root of a rosetree.
-}
root : RoseTree a -> a
root (Rose a _) =
    a


{-| Add a child to the rosetree.
-}
addChild : RoseTree a -> RoseTree a -> RoseTree a
addChild child (Rose a c) =
    Rose a (cons child c)


{-| Map a function over a rosetree.
-}
map : (a -> b) -> RoseTree a -> RoseTree b
map f (Rose a c) =
    Rose (f a) (LazyList.map (map f) c)
