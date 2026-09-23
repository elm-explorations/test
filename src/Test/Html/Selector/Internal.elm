module Test.Html.Selector.Internal exposing
    ( Selector(..)
    , findDescendants
    , hasAll
    , invalid
    , keepMatching
    , namedAttr
    , namedBoolAttr
    , selectorToString
    )

import Test.Html.Internal.ElmHtml.InternalTypes exposing (ElmHtml)
import Test.Html.Internal.ElmHtml.Query as ElmHtmlQuery


type Selector
    = All (List Selector)
    | Classes (List String)
    | Class String
    | Attribute { name : String, value : String }
    | BoolAttribute { name : String, value : Bool }
    | Style { key : String, value : String }
    | Tag String
    | Text String
    | ExactText String
    | Containing (List Selector)
    | Invalid ()


invalid : Selector
invalid =
    Invalid ()


selectorToString : Selector -> String
selectorToString criteria =
    case criteria of
        All list ->
            list
                |> List.map selectorToString
                |> String.join " "

        Classes list ->
            "classes " ++ quoteString (String.join " " list)

        Class class ->
            "class " ++ quoteString class

        Attribute { name, value } ->
            "attribute "
                ++ quoteString name
                ++ " "
                ++ quoteString value

        BoolAttribute { name, value } ->
            "attribute "
                ++ quoteString name
                ++ " "
                ++ boolToString value

        Style style ->
            "styles " ++ styleToString style

        Tag name ->
            "tag " ++ quoteString name

        Text text ->
            "text " ++ quoteString text

        ExactText text ->
            "exact text " ++ quoteString text

        Containing list ->
            let
                selectors =
                    list
                        |> List.map selectorToString
                        |> String.join ", "
            in
            "containing [ " ++ selectors ++ " ] "

        Invalid () ->
            "invalid"


quoteString : String -> String
quoteString s =
    "\"" ++ s ++ "\""


boolToString : Bool -> String
boolToString b =
    if b then
        "True"

    else
        "False"


styleToString : { key : String, value : String } -> String
styleToString { key, value } =
    key ++ ":" ++ value


matches : Selector -> ElmHtml msg -> Bool
matches selector node =
    case selector of
        All selectors ->
            List.all (\s -> matches s node) selectors

        Classes classes ->
            ElmHtmlQuery.hasClasses classes node

        Class class ->
            ElmHtmlQuery.hasClasses [ class ] node

        Attribute { name, value } ->
            ElmHtmlQuery.hasAttribute name value node

        BoolAttribute { name, value } ->
            ElmHtmlQuery.hasBoolAttribute name value node

        Style style ->
            ElmHtmlQuery.hasStyle style node

        Tag name ->
            ElmHtmlQuery.hasTag name node

        Text text ->
            hasDescendantText (String.contains text) node

        ExactText text ->
            hasDescendantText ((==) text) node

        Containing selectors ->
            ElmHtmlQuery.getChildren node
                |> List.concatMap (ElmHtmlQuery.findAll (matches (All selectors)))
                |> List.isEmpty
                |> not

        Invalid () ->
            False


hasDescendantText : (String -> Bool) -> ElmHtml msg -> Bool
hasDescendantText predicate node =
    node
        |> ElmHtmlQuery.findAll (ElmHtmlQuery.containsText predicate)
        |> List.isEmpty
        |> not


hasAll : List Selector -> List (ElmHtml msg) -> Bool
hasAll selectors elems =
    elems
        |> findDescendants selectors
        |> List.isEmpty
        |> not


{-| Search the whole subtree of each element for descendants (self included)
matching every selector in the list, all on the same element.
-}
findDescendants : List Selector -> List (ElmHtml msg) -> List (ElmHtml msg)
findDescendants selectors elems =
    case selectors of
        [] ->
            elems

        _ ->
            List.concatMap (ElmHtmlQuery.findAll (matches (All selectors))) elems


keepMatching : List Selector -> List (ElmHtml msg) -> List (ElmHtml msg)
keepMatching selectors elems =
    case selectors of
        [] ->
            elems

        _ ->
            List.filter (matches (All selectors)) elems


namedAttr : String -> String -> Selector
namedAttr name value =
    Attribute
        { name = name
        , value = value
        }


namedBoolAttr : String -> Bool -> Selector
namedBoolAttr name value =
    BoolAttribute
        { name = name
        , value = value
        }
