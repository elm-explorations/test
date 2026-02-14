module Test.Html.Internal.ElmHtml.Query exposing
    ( Selector(..)
    , query, queryChildren
    , getChildren
    )

{-| Query things using ElmHtml

@docs Selector
@docs query, queryChildren
@docs getChildren

-}

import Dict
import Test.Html.Internal.ElmHtml.InternalTypes exposing (..)


{-| Selectors to query a Html element

  - Id, classname, classlist, tag are all what you'd expect
  - Attribute and bool attribute are attributes
  - ConainsText just searches inside for the given text

-}
type Selector
    = ClassList (List String)
    | Tag String
    | Attribute String String
    | BoolAttribute String Bool
    | Style { key : String, value : String }
    | ContainsText String
    | ContainsExactText String


{-| Query an ElmHtml node using a selector, considering both the node itself
as well as all of its descendants.
-}
query : Selector -> ElmHtml msg -> List (ElmHtml msg)
query selector =
    queryInNode Nothing selector


{-| Query an ElmHtml node using a selector, considering both the node itself
as well as all of its descendants.
-}
queryChildren : Selector -> ElmHtml msg -> List (ElmHtml msg)
queryChildren =
    queryInNode (Just 1)


{-| Returns just the immediate children of an ElmHtml node
-}
getChildren : ElmHtml msg -> List (ElmHtml msg)
getChildren elmHtml =
    case elmHtml of
        NodeEntry { children } ->
            children

        _ ->
            []


queryInNode : Maybe Int -> Selector -> ElmHtml msg -> List (ElmHtml msg)
queryInNode maxDescendantDepth selector node =
    case node of
        NodeEntry record ->
            let
                childEntries =
                    descendInQuery maxDescendantDepth selector record.children
            in
            if predicateFromSelector selector node then
                node :: childEntries

            else
                childEntries

        TextTag text ->
            case selector of
                ContainsText innerText ->
                    if String.contains innerText text then
                        [ node ]

                    else
                        []

                ContainsExactText innerText ->
                    if innerText == text then
                        [ node ]

                    else
                        []

                _ ->
                    []

        MarkdownNode _ ->
            if predicateFromSelector selector node then
                [ node ]

            else
                []

        _ ->
            []


descendInQuery : Maybe Int -> Selector -> List (ElmHtml msg) -> List (ElmHtml msg)
descendInQuery maxDescendantDepth selector children =
    case maxDescendantDepth of
        Nothing ->
            -- No maximum, so continue.
            List.concatMap
                (queryInNode Nothing selector)
                children

        Just depth ->
            if depth > 0 then
                -- Continue with maximum depth reduced by 1.
                List.concatMap
                    (queryInNode (Just (depth - 1)) selector)
                    children

            else
                []


predicateFromSelector : Selector -> ElmHtml msg -> Bool
predicateFromSelector selector html =
    case html of
        NodeEntry record ->
            record
                |> nodeRecordPredicate selector

        MarkdownNode markdownModel ->
            markdownModel
                |> markdownPredicate selector

        _ ->
            False


hasAttribute : String -> String -> Facts msg -> Bool
hasAttribute attribute queryString facts =
    case Dict.get attribute facts.stringAttributes of
        Just id ->
            id == queryString

        Nothing ->
            False


hasBoolAttribute : String -> Bool -> Facts msg -> Bool
hasBoolAttribute attribute value facts =
    case Dict.get attribute facts.boolAttributes of
        Just id ->
            id == value

        Nothing ->
            False


hasClasses : List String -> Facts msg -> Bool
hasClasses classList facts =
    containsAll classList (classnames facts)


hasStyle : { key : String, value : String } -> Facts msg -> Bool
hasStyle style facts =
    Dict.get style.key facts.styles == Just style.value


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


nodeRecordPredicate : Selector -> (NodeRecord msg -> Bool)
nodeRecordPredicate selector =
    case selector of
        ClassList classList ->
            .facts
                >> hasClasses classList

        Tag tag ->
            .tag
                >> (==) tag

        Attribute key value ->
            .facts
                >> hasAttribute key value

        BoolAttribute key value ->
            .facts
                >> hasBoolAttribute key value

        Style style ->
            .facts
                >> hasStyle style

        ContainsText _ ->
            always False

        ContainsExactText _ ->
            always False


markdownPredicate : Selector -> (MarkdownNodeRecord msg -> Bool)
markdownPredicate selector =
    case selector of
        ClassList classList ->
            .facts
                >> hasClasses classList

        Tag _ ->
            always False

        Attribute key value ->
            .facts
                >> hasAttribute key value

        BoolAttribute key value ->
            .facts
                >> hasBoolAttribute key value

        Style style ->
            .facts
                >> hasStyle style

        ContainsText text ->
            .model
                >> .markdown
                >> String.contains text

        ContainsExactText text ->
            .model
                >> .markdown
                >> (==) text
