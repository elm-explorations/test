module Test.Html.Internal.ElmHtml.Query exposing
    ( Selector(..)
    , querySelfOnly, querySelfAndDirectChildren, querySelfAndAllDescendants
    , queryById, queryByClassName, queryByClassList, queryByStyle, queryByTagName, queryByAttribute, queryByBoolAttribute
    , getChildren
    )

{-| Query things using ElmHtml

@docs Selector
@docs querySelfOnly, querySelfAndDirectChildren, querySelfAndAllDescendants
@docs queryById, queryByClassName, queryByClassList, queryByStyle, queryByTagName, queryByAttribute, queryByBoolAttribute
@docs getChildren

-}

import Dict
import String
import Test.Html.Internal.ElmHtml.InternalTypes exposing (..)


{-| Selectors to query a Html element

  - Id, classname, classlist, tag are all what you'd expect
  - Attribute and bool attribute are attributes
  - ConainsText just searches inside for the given text

-}
type Selector
    = Id String
    | ClassName String
    | ClassList (List String)
    | Tag String
    | Attribute String String
    | BoolAttribute String Bool
    | Style { key : String, value : String }
    | ContainsText String
    | ContainsExactText String
    | Multiple (List Selector)


type Depth
    = NoMoreLayers
    | SelfOnly
    | SelfAndDirectChildren
    | SelfAndAllDescendants


{-| Query for a node with a given tag in a Html element
-}
queryByTagName : String -> ElmHtml msg -> List (ElmHtml msg)
queryByTagName tagname =
    querySelfAndAllDescendants (Tag tagname)


{-| Query for a node with a given id in a Html element
-}
queryById : String -> ElmHtml msg -> List (ElmHtml msg)
queryById id =
    querySelfAndAllDescendants (Id id)


{-| Query for a node with a given classname in a Html element
-}
queryByClassName : String -> ElmHtml msg -> List (ElmHtml msg)
queryByClassName classname =
    querySelfAndAllDescendants (ClassName classname)


{-| Query for a node with all the given classnames in a Html element
-}
queryByClassList : List String -> ElmHtml msg -> List (ElmHtml msg)
queryByClassList classList =
    querySelfAndAllDescendants (ClassList classList)


{-| Query for a node with the given style in a Html element
-}
queryByStyle : { key : String, value : String } -> ElmHtml msg -> List (ElmHtml msg)
queryByStyle style =
    querySelfAndAllDescendants (Style style)


{-| Query for a node with a given attribute in a Html element
-}
queryByAttribute : String -> String -> ElmHtml msg -> List (ElmHtml msg)
queryByAttribute key value =
    querySelfAndAllDescendants (Attribute key value)


{-| Query for a node with a given attribute in a Html element
-}
queryByBoolAttribute : String -> Bool -> ElmHtml msg -> List (ElmHtml msg)
queryByBoolAttribute key value =
    querySelfAndAllDescendants (BoolAttribute key value)


{-| Query an ElmHtml node using a selector, considering both the node itself
as well as all of its descendants.
-}
querySelfAndAllDescendants : Selector -> ElmHtml msg -> List (ElmHtml msg)
querySelfAndAllDescendants =
    queryHelp SelfAndAllDescendants


{-| Query an ElmHtml node using a selector, considering both the node itself
as well as its direct children.
-}
querySelfAndDirectChildren : Selector -> ElmHtml msg -> List (ElmHtml msg)
querySelfAndDirectChildren =
    queryHelp SelfAndDirectChildren


{-| Query an ElmHtml node using a selector, considering only the node itself.
-}
querySelfOnly : Selector -> ElmHtml msg -> List (ElmHtml msg)
querySelfOnly =
    queryHelp SelfOnly


{-| Returns just the immediate children of an ElmHtml node
-}
getChildren : ElmHtml msg -> List (ElmHtml msg)
getChildren elmHtml =
    case elmHtml of
        NodeEntry { children } ->
            children

        _ ->
            []


queryHelp : Depth -> Selector -> ElmHtml msg -> List (ElmHtml msg)
queryHelp maxDescendantDepth selector node =
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

        TextTag { text } ->
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


descendInQuery : Depth -> Selector -> List (ElmHtml msg) -> List (ElmHtml msg)
descendInQuery maxDescendantDepth selector children =
    case maxDescendantDepth of
        SelfAndAllDescendants ->
            -- No maximum, so continue.
            List.concatMap
                (queryHelp SelfAndAllDescendants selector)
                children

        SelfAndDirectChildren ->
            -- Continue with maximum depth reduced by 1.
            List.concatMap
                (queryHelp SelfOnly selector)
                children

        SelfOnly ->
            -- The checking of self is done in queryHelp, not here.
            []

        NoMoreLayers ->
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


hasAllSelectors : List Selector -> ElmHtml msg -> Bool
hasAllSelectors selectors record =
    List.map predicateFromSelector selectors
        |> List.map (\selector -> selector record)
        |> List.all identity


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


hasClass : String -> Facts msg -> Bool
hasClass queryString facts =
    List.member queryString (classnames facts)


hasClasses : List String -> Facts msg -> Bool
hasClasses classList facts =
    containsAll classList (classnames facts)


hasStyle : { key : String, value : String } -> Facts msg -> Bool
hasStyle style facts =
    Dict.get style.key facts.styles == Just style.value


classnames : Facts msg -> List String
classnames facts =
    Dict.get "className" facts.stringAttributes
        |> Maybe.withDefault ""
        |> String.split " "


containsAll : List a -> List a -> Bool
containsAll a b =
    b
        |> List.foldl (\i acc -> List.filter ((/=) i) acc) a
        |> List.isEmpty


nodeRecordPredicate : Selector -> (NodeRecord msg -> Bool)
nodeRecordPredicate selector =
    case selector of
        Id id ->
            .facts
                >> hasAttribute "id" id

        ClassName classname ->
            .facts
                >> hasClass classname

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

        Multiple selectors ->
            NodeEntry
                >> hasAllSelectors selectors


markdownPredicate : Selector -> (MarkdownNodeRecord msg -> Bool)
markdownPredicate selector =
    case selector of
        Id id ->
            .facts
                >> hasAttribute "id" id

        ClassName classname ->
            .facts
                >> hasClass classname

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

        Multiple selectors ->
            MarkdownNode
                >> hasAllSelectors selectors
