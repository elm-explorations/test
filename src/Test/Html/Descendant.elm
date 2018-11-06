module Test.Html.Descendant exposing (isDescendant)

{-| TODO: don't expose this module

@docs isDescendant

-}

import ElmHtml.InternalTypes exposing (ElmHtml(..))
import Html exposing (Html)
import Html.Inert exposing (fromHtml, toElmHtml)


{-| TODO: don't expose this module
-}
isDescendant : List (ElmHtml msg) -> ElmHtml msg -> Bool
isDescendant html potentialDescendant =
    case html of
        [] ->
            False

        current :: rest ->
            if current == potentialDescendant then
                True

            else
                isDescendant
                    (prependChildren current rest)
                    potentialDescendant


prependChildren : ElmHtml msg -> List (ElmHtml msg) -> List (ElmHtml msg)
prependChildren parentNode nodeList =
    case parentNode of
        NodeEntry { children } ->
            List.concat [ children, nodeList ]

        _ ->
            nodeList