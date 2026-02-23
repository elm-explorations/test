module EffectivenessSUT.Bst exposing (delete, fromList, insert, keys, member, toList, union)

{-| A correct reference implementation of a Binary Search Tree.
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


union : BST comparable v -> BST comparable v -> BST comparable v
union t1 t2 =
    case t1 of
        Leaf ->
            t2

        Node left k v right ->
            let
                t2Without =
                    delete k t2

                newLeft =
                    union left (leftSubtreeOf k t2Without)

                newRight =
                    union right (rightSubtreeOf k t2Without)
            in
            Node newLeft k v newRight


leftSubtreeOf : comparable -> BST comparable v -> BST comparable v
leftSubtreeOf k tree =
    case tree of
        Leaf ->
            Leaf

        Node left key val right ->
            if k <= key then
                leftSubtreeOf k left

            else
                Node left key val (leftSubtreeOf k right)


rightSubtreeOf : comparable -> BST comparable v -> BST comparable v
rightSubtreeOf k tree =
    case tree of
        Leaf ->
            Leaf

        Node left key val right ->
            if k >= key then
                rightSubtreeOf k right

            else
                Node (rightSubtreeOf k left) key val right


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
