module Diff exposing (Change(..), diff)

{-| Temporary stand-in until jinjor/elm-diff is updated for Elm 0.19
-}


type Change a
    = NoChange a
    | Removed a
    | Added a


diff : List a -> List a -> List (Change a)
diff _ _ =
    Debug.todo "Upgrade jinjor/elm-diff for Elm 0.19"
