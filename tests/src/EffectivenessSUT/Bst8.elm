module EffectivenessSUT.Bst8 exposing (delete, fromList, insert, keys, member, toList, union)

{-| Bug #8: union works correctly, except that when both trees contain the
same key, the left argument does not always take priority. Specifically, the
implementation inserts all entries from t1 into t2 (rather than t2 into t1), so
t2's values silently overwrite t1's values for shared keys during the fold —
meaning the RIGHT argument wins on duplicates instead of the left.
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


{-| BUG: We fold t1's entries into t2 as the accumulator. Because insert
does not overwrite an existing key (it keeps the existing value), and t2 is the
starting accumulator, t2's values win whenever a key appears in both trees. The
correct behaviour is for t1 to take priority.

Correct would be: fold t2's entries into t1 as the starting accumulator, so
that t1's values are already present and won't be overwritten.

-}
union : BST comparable v -> BST comparable v -> BST comparable v
union t1 t2 =
    -- BUG: inserting t1 entries INTO t2 means t2 values survive for shared keys
    List.foldl (\( k, v ) acc -> insert k v acc) t2 (toList t1)


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
