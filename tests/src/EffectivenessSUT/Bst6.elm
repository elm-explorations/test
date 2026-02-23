module EffectivenessSUT.Bst6 exposing (delete, fromList, insert, keys, member, toList, union)

{-| Bug #6: union wrongly assumes that all the keys in the first argument
precede those in the second. It just appends t2 as the rightmost subtree of t1
without any merging.
-}

import EffectivenessSUT.BstCommon exposing (BST(..))


insert : comparable -> v -> BST comparable v -> BST comparable v
insert k v tree =
    case tree of
        Leaf ->
            Node Leaf k v Leaf

        Node left key val right ->
            if k < key then
                Node (insert k v left) key val right

            else if k > key then
                Node left key val (insert k v right)

            else
                Node left k v right


delete : comparable -> BST comparable v -> BST comparable v
delete k tree =
    case tree of
        Leaf ->
            Leaf

        Node left key val right ->
            if k < key then
                Node (delete k left) key val right

            else if k > key then
                Node left key val (delete k right)

            else
                case ( left, right ) of
                    ( Leaf, _ ) ->
                        right

                    ( _, Leaf ) ->
                        left

                    _ ->
                        case minNode right of
                            Nothing ->
                                tree

                            Just ( succKey, succVal ) ->
                                let
                                    newRight =
                                        deleteMin right
                                in
                                Node left succKey succVal newRight


minNode : BST k v -> Maybe ( k, v )
minNode tree =
    case tree of
        Leaf ->
            Nothing

        Node Leaf k v _ ->
            Just ( k, v )

        Node left _ _ _ ->
            minNode left


deleteMin : BST k v -> BST k v
deleteMin tree =
    case tree of
        Leaf ->
            Leaf

        Node Leaf _ _ right ->
            right

        Node left k v right ->
            Node (deleteMin left) k v right


{-| BUG: Assumes all keys in t1 are smaller than all keys in t2.
Simply grafts t2 onto the rightmost position of t1, which only
produces a valid BST if that assumption holds.
-}
union : BST k v -> BST k v -> BST k v
union t1 t2 =
    case t1 of
        Leaf ->
            t2

        Node left k v Leaf ->
            -- At the rightmost node, attach t2 directly
            Node left k v t2

        Node left k v right ->
            -- Keep descending to the right
            Node left k v (union right t2)


member : comparable -> BST comparable v -> Bool
member k tree =
    case tree of
        Leaf ->
            False

        Node left key _ right ->
            if k < key then
                member k left

            else if k > key then
                member k right

            else
                True


toList : BST k v -> List ( k, v )
toList tree =
    case tree of
        Leaf ->
            []

        Node left k v right ->
            toList left ++ [ ( k, v ) ] ++ toList right


fromList : List ( comparable, v ) -> BST comparable v
fromList pairs =
    List.foldl (\( k, v ) acc -> insert k v acc) Leaf pairs


keys : BST k v -> List k
keys tree =
    List.map Tuple.first (toList tree)
