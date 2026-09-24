module Test.Html.Internal.ElmHtml.Query exposing
    ( findAll
    , getChildren
    , hasTag, hasClasses, hasAttribute, hasBoolAttribute, hasStyle
    , containsText
    , existsDescendant
    )

{-| Helpers for walking and inspecting an `ElmHtml` tree.

Lower-level than `Test.Html.Selector`.

@docs findAll
@docs getChildren
@docs hasTag, hasClasses, hasAttribute, hasBoolAttribute, hasStyle
@docs containsText

-}

import Dict
import Test.Html.Internal.ElmHtml.InternalTypes exposing (ElmHtml(..), Facts)


getChildren : ElmHtml msg -> List (ElmHtml msg)
getChildren elmHtml =
    case elmHtml of
        NodeEntry { children } ->
            children

        TextTag _ ->
            []

        CustomNode _ ->
            []

        MarkdownNode _ ->
            []


{-| Collect descendants (self included) that satisfy a predicate.
-}
findAll : (ElmHtml msg -> Bool) -> ElmHtml msg -> List (ElmHtml msg)
findAll predicate node =
    (if predicate node then
        [ node ]

     else
        []
    )
        ++ List.concatMap (findAll predicate) (getChildren node)


existsDescendant : (ElmHtml msg -> Bool) -> ElmHtml msg -> Bool
existsDescendant predicate node =
    predicate node || List.any (existsDescendant predicate) (getChildren node)


{-| Does this node's full text content (self and all descendants, concatenated
in document order) satisfy the predicate?
-}
containsText : (String -> Bool) -> ElmHtml msg -> Bool
containsText predicate node =
    predicate (textContent node)


{-| The full text content of a node: self and all descendants' text (and
Markdown source) concatenated in document order.

No separator is added between nodes (matches browser's `textContent`).

-}
textContent : ElmHtml msg -> String
textContent node =
    case node of
        TextTag text ->
            text

        MarkdownNode { model } ->
            model.markdown

        CustomNode _ ->
            ""

        NodeEntry { children } ->
            children
                |> List.map textContent
                |> String.concat


{-| Does this node have the given tag?
-}
hasTag : String -> ElmHtml msg -> Bool
hasTag tag node =
    case node of
        NodeEntry record ->
            record.tag == tag

        TextTag _ ->
            False

        MarkdownNode _ ->
            False

        CustomNode _ ->
            False


hasClasses : List String -> ElmHtml msg -> Bool
hasClasses classList node =
    case factsOf node of
        Just facts ->
            containsAll classList (classnames facts)

        Nothing ->
            False


hasAttribute : String -> String -> ElmHtml msg -> Bool
hasAttribute name value node =
    case factsOf node of
        Just facts ->
            Dict.get name facts.stringAttributes == Just value

        Nothing ->
            False


hasBoolAttribute : String -> Bool -> ElmHtml msg -> Bool
hasBoolAttribute name value node =
    case factsOf node of
        Just facts ->
            Dict.get name facts.boolAttributes == Just value

        Nothing ->
            False


hasStyle : { key : String, value : String } -> ElmHtml msg -> Bool
hasStyle style node =
    case factsOf node of
        Just facts ->
            Dict.get style.key facts.styles == Just style.value

        Nothing ->
            False


factsOf : ElmHtml msg -> Maybe (Facts msg)
factsOf node =
    case node of
        NodeEntry record ->
            Just record.facts

        MarkdownNode record ->
            Just record.facts

        TextTag _ ->
            Nothing

        CustomNode _ ->
            Nothing


classnames : Facts msg -> List String
classnames facts =
    (case
        ( Dict.get "class" facts.stringAttributes
        , Dict.get "className" facts.stringAttributes
        )
     of
        ( Just _, Just _ ) ->
            -- If you use both the `class` attribute and the `className` property at the same time,
            -- it’s undefined which classes you end up with. It depends on which order they are specified,
            -- which order elm/virtual-dom happens to apply them, and which of them changed most recently.
            -- Mixing both is not a good idea.
            --
            -- This code should be impossible to reach because of the validation in
            -- Test.Html.Internal.ElmHtml.InternalTypes.decodeOthers.
            --
            -- If we ever reach this code, silently claim that there are no classes (that no classes match
            -- the node).
            ""

        ( Just class, Nothing ) ->
            class

        ( Nothing, Just className ) ->
            className

        ( Nothing, Nothing ) ->
            ""
    )
        |> String.split " "


containsAll : List a -> List a -> Bool
containsAll a b =
    b
        |> List.foldl (\i acc -> List.filter ((/=) i) acc) a
        |> List.isEmpty
