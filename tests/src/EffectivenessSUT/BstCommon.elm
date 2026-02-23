module EffectivenessSUT.BstCommon exposing (BST(..))


type BST k v
    = Leaf
    | Node (BST k v) k v (BST k v)
