module Test.Html.Internal.ElmHtml.ToString exposing
    ( nodeToStringWithOptions
    , FormatOptions
    )

{-| Convert ElmHtml to string.

@docs nodeToStringWithOptions

@docs FormatOptions

-}

import Dict
import Test.Html.Internal.ElmHtml.InternalTypes exposing (..)


{-| Formatting options to be used for converting to string
-}
type alias FormatOptions =
    { indent : Int
    , newLines : Bool
    }


{-| default formatting options
-}
defaultFormatOptions : FormatOptions
defaultFormatOptions =
    { indent = 0
    , newLines = False
    }


nodeToLines : FormatOptions -> ElmHtml msg -> List String
nodeToLines options nodeType =
    case nodeType of
        TextTag text ->
            [ text ]

        NodeEntry record ->
            nodeRecordToString options record

        CustomNode _ ->
            []

        MarkdownNode record ->
            [ record.model.markdown ]


{-| Convert a given html node to a string based on the type
-}
nodeToString : ElmHtml msg -> String
nodeToString =
    nodeToStringWithOptions defaultFormatOptions


{-| same as nodeToString, but with options
-}
nodeToStringWithOptions : FormatOptions -> ElmHtml msg -> String
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
nodeRecordToString : FormatOptions -> NodeRecord msg -> List String
nodeRecordToString options { tag, children, facts } =
    let
        openTag : List (Maybe String) -> String
        openTag extras =
            let
                trimmedExtras =
                    List.filterMap (Maybe.andThen (String.trim >> nothingIfEmpty)) extras

                filling =
                    case trimmedExtras of
                        [] ->
                            ""

                        more ->
                            " " ++ String.join " " more
            in
            "<" ++ tag ++ filling ++ ">"

        styles =
            if Dict.isEmpty facts.styles then
                Nothing

            else
                let
                    styleString : String
                    styleString =
                        Dict.foldl (\key value str -> str ++ key ++ ":" ++ value ++ ";") "" facts.styles
                in
                Just ("style=\"" ++ styleString ++ "\"")

        classes =
            Dict.get "className" facts.stringAttributes
                |> Maybe.map (\name -> "class=\"" ++ name ++ "\"")

        stringAttributes =
            Dict.foldl
                (\k v str ->
                    if k == "className" then
                        str

                    else
                        str ++ " " ++ k ++ "=\"" ++ v ++ "\""
                )
                ""
                facts.stringAttributes
                |> String.trimLeft
                |> Just

        boolAttributes =
            Dict.foldl
                (\k v str ->
                    if v then
                        str ++ " " ++ k

                    else
                        str
                )
                ""
                facts.boolAttributes
                |> String.trimLeft
                |> Just

        openTag_ : String
        openTag_ =
            openTag [ classes, styles, stringAttributes, boolAttributes ]
    in
    case toElementKind tag of
        {- Void elements only have a start tag; end tags must not be
           specified for void elements.
        -}
        VoidElements ->
            [ openTag_ ]

        {- TODO: implement restrictions for RawTextElements,
           EscapableRawTextElements. Also handle ForeignElements correctly.
           For now just punt and use the previous behavior for all other
           element kinds.
        -}
        _ ->
            let
                closeTag =
                    "</" ++ tag ++ ">"

                childrenStrings =
                    List.concatMap (nodeToLines options) children
                        |> List.map ((++) (String.repeat options.indent " "))
            in
            openTag_
                :: childrenStrings
                ++ [ closeTag ]


nothingIfEmpty : String -> Maybe String
nothingIfEmpty str =
    if str == "" then
        Nothing

    else
        Just str
