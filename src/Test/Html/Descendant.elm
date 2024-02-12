module Test.Html.Descendant (isDescendant) where

import Test.Html.Internal.ElmHtml.InternalTypes (ElmHtml(..))
import Test.Html.Internal.ElmHtml.InternalTypes as Test.Html.Internal.ElmHtml.InternalTypes


isDescendant :: List (ElmHtml msg) -> ElmHtml msg -> Bool
isDescendant html potentialDescendant =
    case html of
        List.nil ->
            False

        current List.: rest ->
            if current == potentialDescendant then
                True

            else
                isDescendant
                    (prependChildren current rest)
                    potentialDescendant


prependChildren :: ElmHtml msg -> List (ElmHtml msg) -> List (ElmHtml msg)
prependChildren parentNode nodeList =
    case parentNode of
        NodeEntry { children } ->
            List.concat [ children, nodeList ]

        _ ->
            nodeList
