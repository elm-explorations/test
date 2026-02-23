module EffectivenessSUT.Bst7 exposing (delete, fromList, insert, keys, member, toList, union)

{-| Bug #7: union wrongly assumes that if the key at the root of t1 is smaller
than the key at the root of t2, then ALL keys in t1 are smaller than the root
of t2. It therefore puts all of t1 to the left of t2's root without checking
individual keys.
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


{-| BUG: Compares only the roots of t1 and t2. If root(t1) < root(t2),
the entire t1 is placed as the left subtree of t2's root, and
t2's left subtree is discarded/unioned only with t1's right portion.
This is wrong when t1 has keys that are >= root(t2), or when t2's
left subtree has keys that should stay left of root(t2).
-}
union : BST comparable v -> BST comparable v -> BST comparable v
union t1 t2 =
    case ( t1, t2 ) of
        ( Leaf, _ ) ->
            t2

        ( _, Leaf ) ->
            t1

        ( Node l1 k1 v1 r1, Node l2 k2 v2 r2 ) ->
            if k1 == k2 then
                -- Left arg takes priority; recurse on subtrees
                Node (union l1 l2) k1 v1 (union r1 r2)

            else if k1 < k2 then
                -- BUG: assume ALL of t1 is less than k2, so put t1 entirely
                -- as the left child of t2's root, dropping l2 on the floor
                -- (or unioning it only with an empty tree).
                Node t1 k2 v2 r2

            else
                -- k1 > k2: assume ALL of t1 is greater than k2
                Node l2 k2 v2 t1


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
