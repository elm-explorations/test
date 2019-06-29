module Simplify.Internal exposing (Simplifier(..), simplifyTree)

import Lazy
import Lazy.List exposing (LazyList)
import RoseTree exposing (RoseTree(..))


type Simplifier a
    = Simp (a -> LazyList a)


simplifyTree : Simplifier a -> a -> RoseTree a
simplifyTree (Simp simplifier) root =
    let
        branches _ =
            Lazy.List.map (simplifyTree (Simp simplifier)) (simplifier root) |> Lazy.force
    in
    Rose root (Lazy.lazy branches)
