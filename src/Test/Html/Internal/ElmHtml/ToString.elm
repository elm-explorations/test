module Test.Html.Internal.ElmHtml.ToString ( nodeRecordToString, nodeToString, nodeToStringWithOptions
    , FormatOptions, defaultFormatOptions
    )
 where

{-| Convert ElmHtml to string.

@docs nodeRecordToString, nodeToString, nodeToStringWithOptions

@docs FormatOptions, defaultFormatOptions

-}

import Dict as Dict
import String as String
import Test.Html.Internal.ElmHtml.InternalTypes (..)
import Test.Html.Internal.ElmHtml.InternalTypes as Test.Html.Internal.ElmHtml.InternalTypes


{-| Formatting options to be used for converting to string
-}
type FormatOptions =
    { indent :: Int
    , newLines :: Bool
    }


{-| default formatting options
-}
defaultFormatOptions :: FormatOptions
defaultFormatOptions =
    { indent : 0
    , newLines : False
    }


nodeToLines :: FormatOptions -> ElmHtml msg -> List String
nodeToLines options nodeType =
    case nodeType of
        TextTag { text } ->
            [ text ]

        NodeEntry record ->
            nodeRecordToString options record

        CustomNode _ ->
            List.nil

        MarkdownNode record ->
            [ record.model.markdown ]


{-| Convert a given html node to a string based on the type
-}
nodeToString :: ElmHtml msg -> String
nodeToString =
    nodeToStringWithOptions defaultFormatOptions


{-| same as nodeToString, but with options
-}
nodeToStringWithOptions :: FormatOptions -> ElmHtml msg -> String
nodeToStringWithOptions options =
    nodeToLines options
        >> String.join
            (if options.newLines then
                "\n"

             else
                ""
            )


{-| Convert a node record to a string. This basically takes the tag name, then
pulls all the facts into tag declaration, then goes through the children and
nests them under this one
-}
nodeRecordToString :: FormatOptions -> NodeRecord msg -> List String
nodeRecordToString options { tag, children, facts } =
    let
        openTag :: List (Maybe String) -> String
        openTag extras =
            let
                trimmedExtras =
                    List.filterMap (\x -> x) extras
                        |> List.map String.trim
                        |> List.filter ((/=) "")

                filling =
                    case trimmedExtras of
                        List.nil ->
                            ""

                        more ->
                            " " <> String.join " " more
            in
            "<" <> tag <> filling <> ">"

        closeTag =
            "</" <> tag <> ">"

        childrenStrings =
            List.map (nodeToLines options) children
                |> List.concat
                |> List.map ((<>) (String.repeat options.indent " "))

        styles =
            case Dict.toList facts.styles of
                List.nil ->
                    Nothing

                styleValues ->
                    styleValues
                        |> List.map (\{a:key, b:value } -> key <> ":" <> value <> ";")
                        |> String.join ""
                        |> (\styleString -> "style=\"" <> styleString <> "\"")
                        |> Just

        classes =
            Dict.get "className" facts.stringAttributes
                |> Maybe.map (\name -> "class=\"" <> name <> "\"")

        stringAttributes =
            Dict.filter (\k _ -> k /= "className") facts.stringAttributes
                |> Dict.toList
                |> List.map (\{a:k, b:v } -> k <> "=\"" <> v <> "\"")
                |> String.join " "
                |> Just

        boolAttributes =
            Dict.toList facts.boolAttributes
                |> List.filterMap
                    (\{a:k, b:v } ->
                        if v then
                            Just k

                        else
                            Nothing
                    )
                |> String.join " "
                |> Just
    in
    case toElementKind tag of
        {- Void elements only have a start tag; end tags must not be
           specified for void elements.
        -}
        VoidElements ->
            [ openTag [ classes, styles, stringAttributes, boolAttributes ] ]

        {- TODO: implement restrictions for RawTextElements,
           EscapableRawTextElements. Also handle ForeignElements correctly.
           For now just punt and use the previous behavior for all other
           element kinds.
        -}
        _ ->
            [ openTag [ classes, styles, stringAttributes, boolAttributes ] ]
                <> childrenStrings
                <> [ closeTag ]
