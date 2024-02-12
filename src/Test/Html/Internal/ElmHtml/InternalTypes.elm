module Test.Html.Internal.ElmHtml.InternalTypes ( ElmHtml(..), TextTagRecord, NodeRecord, CustomNodeRecord, MarkdownNodeRecord
    , Facts, Tagger, EventHandler, ElementKind(..)
    , Attribute(..), AttributeRecord, NamespacedAttributeRecord, PropertyRecord, EventRecord
    , decodeElmHtml, emptyFacts, toElementKind, decodeAttribute
    )
 where

{-| Internal types used to represent Elm Html in pure Elm

@docs ElmHtml, TextTagRecord, NodeRecord, CustomNodeRecord, MarkdownNodeRecord

@docs Facts, Tagger, EventHandler, ElementKind

@docs Attribute, AttributeRecord, NamespacedAttributeRecord, PropertyRecord, EventRecord

@docs decodeElmHtml, emptyFacts, toElementKind, decodeAttribute

-}

import Dict (Dict)
import Dict as Dict
import Json.Decode (field)
import Json.Decode as Json.Decode
import Test.Html.Internal.ElmHtml.Constants
import Test.Html.Internal.ElmHtml.Constants as Constants

import Test.Html.Internal.ElmHtml.Helpers (..)
import Test.Html.Internal.ElmHtml.Helpers as Test.Html.Internal.ElmHtml.Helpers
import Test.Html.Internal.ElmHtml.Markdown (..)
import Test.Html.Internal.ElmHtml.Markdown as Test.Html.Internal.ElmHtml.Markdown
import Test.Internal.KernelConstants (kernelConstants)
import Test.Internal.KernelConstants as Test.Internal.KernelConstants
import VirtualDom as VirtualDom


{-| Type tree for representing Elm's Html

  - TextTag is just a plain old bit of text.
  - NodeEntry is an actual HTML node, e.g a div
  - CustomNode are nodes defined to work with the renderer in some way, e.g webgl/markdown
  - MarkdownNode is just a wrapper for CustomNode designed just for markdown

-}
data ElmHtml msg
    = TextTag TextTagRecord
    | NodeEntry (NodeRecord msg)
    | CustomNode (CustomNodeRecord msg)
    | MarkdownNode (MarkdownNodeRecord msg)


{-| Text tags just contain text
-}
type TextTagRecord =
    { text :: String }


{-| A node contains the `tag` as a string, the children, the facts (e.g attributes) and descendantsCount
-}
type NodeRecord msg =
    { tag :: String
    , children :: List (ElmHtml msg)
    , facts ::
        Facts msg

    --, namespace : String
    , descendantsCount :: Int
    }


{-| A markdown node contains facts (e.g attributes) and the model used by markdown
-}
type MarkdownNodeRecord msg =
    { facts :: Facts msg
    , model :: MarkdownModel
    }


{-| Custom nodes contain facts (e.g attributes) and a json value for the model
-}
type CustomNodeRecord msg =
    { facts :: Facts msg
    , model :: Json.Decode.Value
    }


{-| Tagger holds the map function when Html.Map is used, the tagger
should then be applied to events comming from descendant nodes, it
is basically a javascript function.
-}
type Tagger =
    Json.Decode.Value


{-| EventHandler holds the function that is called when an event is
triggered, it is basically a javascript object like this:

{ decoder: [Function] }

-}
type EventHandler =
    Json.Decode.Value


{-| Facts contain various dictionaries and values for a node

  - styles are a mapping of rules
  - events may be a json object containing event handlers
  - attributes are pulled out into stringAttributes and boolAttributes - things with string values go into
    stringAttributes, things with bool values go into boolAttributes

-}
type Facts msg =
    { styles :: Dict String String
    , events :: Dict String (VirtualDom.Handler msg)
    , attributeNamespace :: Maybe Json.Decode.Value
    , stringAttributes :: Dict String String
    , boolAttributes :: Dict String Bool
    }


{-| Type for representing the five kinds of elements according to HTML 5
[spec](https://html.spec.whatwg.org/multipage/syntax.html#elements-2).
Used to handle different rendering behavior depending on the type of element.
-}
data ElementKind
    = VoidElements
    | RawTextElements
    | EscapableRawTextElements
    | ForeignElements
    | NormalElements


data HtmlContext msg
    = HtmlContext (List Tagger) (List Tagger -> EventHandler -> VirtualDom.Handler msg)


{-| Type for representing Elm's Attributes

  - Attribute is an HTML attribute, like `Html.Attributes.colspan`. These values
    are applied using `element.setAttribute(key, value)` during a patch.
  - NamespacedAttribute has an namespace, like `Svg.Attributes.xlinkHref`
  - Property assigns a value to a node like `Html.Attributes.class`, and can
    hold any encoded value. Unlike attributes, where `element.setAttribute()` is
    used during the patch, properties are applied directly as
    `element[key] = value`.
  - Styles hold a list of key value pairs to be applied to the node's style set
  - Event contains a decoder for a msg and the `Html.Event.Options` for the event

-}
data Attribute
    = Attribute AttributeRecord
    | NamespacedAttribute NamespacedAttributeRecord
    | Property PropertyRecord
    | Style { key :: String, value :: String }
    | Event EventRecord


{-| Attribute contains a string key and a string value
-}
type AttributeRecord =
    { key :: String
    , value :: String
    }


{-| NamespacedAttribute contains a string key, string namespace and string value
-}
type NamespacedAttributeRecord =
    { key :: String
    , value :: String
    , namespace :: String
    }


{-| Property contains a string key and a value with an arbitrary type
-}
type PropertyRecord =
    { key :: String
    , value :: Json.Decode.Value
    }


{-| Event contains a string key, a decoder for a msg and event options
-}
type EventRecord =
    { key :: String
    , decoder :: Json.Decode.Value
    , options :: EventOptions
    }


type EventOptions =
    { stopPropagation :: Bool
    , preventDefault :: Bool
    }


{-| decode a json object into ElmHtml, you have to pass a function that decodes
events from Html Nodes. If you don't want to decode event msgs, you can ignore it:

    decodeElmHtml (\_ _ -> VirtualDom.Normal (Json.Decode.succeed ())) jsonHtml

if you do want to decode them, you will probably need to write some native code
like elm-html-test does to extract the function inside those.

-}
decodeElmHtml :: (List Tagger -> EventHandler -> VirtualDom.Handler msg) -> Json.Decode.Decoder (ElmHtml msg)
decodeElmHtml eventDecoder =
    contextDecodeElmHtml (HtmlContext List.nil eventDecoder)


contextDecodeElmHtml :: HtmlContext msg -> Json.Decode.Decoder (ElmHtml msg)
contextDecodeElmHtml context =
    field kernelConstants.virtualDom.nodeType Json.Decode.int
        |> Json.Decode.andThen
            (\nodeType ->
                if nodeType == kernelConstants.virtualDom.nodeTypeText then
                    Json.Decode.map TextTag decodeTextTag

                else if nodeType == kernelConstants.virtualDom.nodeTypeKeyedNode then
                    Json.Decode.map NodeEntry (decodeKeyedNode context)

                else if nodeType == kernelConstants.virtualDom.nodeTypeNode then
                    Json.Decode.map NodeEntry (decodeNode context)

                else if nodeType == kernelConstants.virtualDom.nodeTypeCustom then
                    decodeCustomNode context

                else if nodeType == kernelConstants.virtualDom.nodeTypeTagger then
                    decodeTagger context

                else if nodeType == kernelConstants.virtualDom.nodeTypeThunk then
                    field kernelConstants.virtualDom.node (contextDecodeElmHtml context)

                else
                    Json.Decode.fail ("No such type as " <> String.fromInt nodeType)
            )


{-| decode text tag
-}
decodeTextTag :: Json.Decode.Decoder TextTagRecord
decodeTextTag =
    field kernelConstants.virtualDom.text
        (Json.Decode.andThen (\text -> Json.Decode.succeed { text : text }) Json.Decode.string)


{-| decode a tagger
-}
decodeTagger :: HtmlContext msg -> Json.Decode.Decoder (ElmHtml msg)
decodeTagger (HtmlContext taggers eventDecoder) =
    Json.Decode.field kernelConstants.virtualDom.tagger Json.Decode.value
        |> Json.Decode.andThen
            (\tagger ->
                let
                    nodeDecoder =
                        contextDecodeElmHtml (HtmlContext (taggers <> [ tagger ]) eventDecoder)
                in
                Json.Decode.at [ kernelConstants.virtualDom.node ] nodeDecoder
            )


decodeKeyedNode :: HtmlContext msg -> Json.Decode.Decoder (NodeRecord msg)
decodeKeyedNode context =
    let
        -- elm stores keyed nodes as tuples
        -- we only want to decode the html, in the second property
        decodeSecondNode =
            Json.Decode.field "b" (contextDecodeElmHtml context)
    in
    Json.Decode.map4 NodeRecord
        (Json.Decode.field kernelConstants.virtualDom.tag Json.Decode.string)
        (Json.Decode.field kernelConstants.virtualDom.kids (Json.Decode.list decodeSecondNode))
        (Json.Decode.field kernelConstants.virtualDom.facts (decodeFacts context))
        (Json.Decode.field kernelConstants.virtualDom.descendantsCount Json.Decode.int)


{-| decode a node record
-}
decodeNode :: HtmlContext msg -> Json.Decode.Decoder (NodeRecord msg)
decodeNode context =
    Json.Decode.map4 NodeRecord
        (field kernelConstants.virtualDom.tag Json.Decode.string)
        (field kernelConstants.virtualDom.kids (Json.Decode.list (contextDecodeElmHtml context)))
        (field kernelConstants.virtualDom.facts (decodeFacts context))
        (field kernelConstants.virtualDom.descendantsCount Json.Decode.int)


{-| decode custom node into either markdown or custom
-}
decodeCustomNode :: HtmlContext msg -> Json.Decode.Decoder (ElmHtml msg)
decodeCustomNode context =
    Json.Decode.oneOf
        [ Json.Decode.map MarkdownNode (decodeMarkdownNodeRecord context)
        , Json.Decode.map CustomNode (decodeCustomNodeRecord context)
        ]


{-| decode custom node record
-}
decodeCustomNodeRecord :: HtmlContext msg -> Json.Decode.Decoder (CustomNodeRecord msg)
decodeCustomNodeRecord context =
    Json.Decode.map2 CustomNodeRecord
        (field kernelConstants.virtualDom.facts (decodeFacts context))
        (field kernelConstants.virtualDom.model Json.Decode.value)


{-| decode markdown node record
-}
decodeMarkdownNodeRecord :: HtmlContext msg -> Json.Decode.Decoder (MarkdownNodeRecord msg)
decodeMarkdownNodeRecord context =
    Json.Decode.map2 MarkdownNodeRecord
        (field kernelConstants.virtualDom.facts (decodeFacts context))
        (field kernelConstants.virtualDom.model decodeMarkdownModel)


{-| decode the styles
-}
decodeStyles :: Json.Decode.Decoder (Dict String String)
decodeStyles =
    Json.Decode.oneOf
        [ field styleKey (Json.Decode.dict Json.Decode.string)
        , Json.Decode.succeed Dict.empty
        ]


{-| grab things from attributes via a decoder, then anything that isn't filtered on
the object
-}
decodeOthers :: Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeOthers otherDecoder =
    decodeAttributes otherDecoder
        |> Json.Decode.andThen
            (\attributes ->
                decodeDictFilterMap otherDecoder
                    |> Json.Decode.map (filterKnownKeys >> Dict.union attributes)
            )


{-| For a given decoder, keep the values from a dict that pass the decoder
-}
decodeDictFilterMap :: Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeDictFilterMap decoder =
    Json.Decode.dict Json.Decode.value
        |> Json.Decode.map
            (Dict.toList
                >> List.filterMap
                    (\{a:key, b:value } ->
                        case Json.Decode.decodeValue decoder value of
                            Err _ ->
                                Nothing

                            Ok v ->
                                Just {a:key, b:v }
                    )
                >> Dict.fromList
            )


decodeAttributes :: Json.Decode.Decoder a -> Json.Decode.Decoder (Dict String a)
decodeAttributes decoder =
    Json.Decode.oneOf
        [ Json.Decode.field attributeKey (decodeDictFilterMap decoder)
        , Json.Decode.succeed Dict.empty
        ]


decodeEvents :: (EventHandler -> VirtualDom.Handler msg) -> Json.Decode.Decoder (Dict String (VirtualDom.Handler msg))
decodeEvents taggedEventDecoder =
    Json.Decode.oneOf
        [ Json.Decode.field eventKey (Json.Decode.dict (Json.Decode.map taggedEventDecoder Json.Decode.value))
        , Json.Decode.succeed Dict.empty
        ]


{-| decode fact
-}
decodeFacts :: HtmlContext msg -> Json.Decode.Decoder (Facts msg)
decodeFacts (HtmlContext taggers eventDecoder) =
    Json.Decode.map5 Facts
        decodeStyles
        (decodeEvents (eventDecoder taggers))
        (Json.Decode.maybe (Json.Decode.field attributeNamespaceKey Json.Decode.value))
        (decodeOthers Json.Decode.string)
        (decodeOthers Json.Decode.bool)


{-| Just empty facts
-}
emptyFacts :: Facts msg
emptyFacts =
    { styles : Dict.empty
    , events : Dict.empty
    , attributeNamespace : Nothing
    , stringAttributes : Dict.empty
    , boolAttributes : Dict.empty
    }


{-| Decode a JSON object into an Attribute. You have to pass a function that
decodes events from event attributes. If you don't want to decode event msgs,
you can ignore it:

    decodeAttribute (\_ -> ()) jsonHtml

If you do want to decode them, you will probably need to write some native code
like elm-html-test does to extract the function inside those.

-}
decodeAttribute :: Json.Decode.Decoder Attribute
decodeAttribute =
    Json.Decode.field "$" Json.Decode.string
        |> Json.Decode.andThen
            (\tag ->
                if tag == Constants.attributeKey then
                    Json.Decode.map2 (\key val -> Attribute (AttributeRecord key val))
                        (Json.Decode.field "n" Json.Decode.string)
                        (Json.Decode.field "o" Json.Decode.string)

                else if tag == Constants.attributeNamespaceKey then
                    Json.Decode.map3 NamespacedAttributeRecord
                        (Json.Decode.field "n" Json.Decode.string)
                        (Json.Decode.at [ "o", "o" ] Json.Decode.string)
                        (Json.Decode.at [ "o", "f" ] Json.Decode.string)
                        |> Json.Decode.map NamespacedAttribute

                else if tag == Constants.styleKey then
                    Json.Decode.map2 (\key val -> Style { key : key, value : val })
                        (Json.Decode.field "n" Json.Decode.string)
                        (Json.Decode.field "o" Json.Decode.string)

                else if tag == Constants.propKey then
                    Json.Decode.map2 (\key val -> Property (PropertyRecord key val))
                        (Json.Decode.field "n" Json.Decode.string)
                        (Json.Decode.at [ "o", "a" ] Json.Decode.value)

                else
                    Json.Decode.fail ("Unexpected Html.Attribute tag: " <> tag)
            )


{-| A list of Void elements as defined by the HTML5 specification. These
elements must not have closing tags and most not be written as self closing
either
-}
voidElements :: List String
voidElements =
    [ "area"
    , "base"
    , "br"
    , "col"
    , "embed"
    , "hr"
    , "img"
    , "input"
    , "link"
    , "meta"
    , "param"
    , "source"
    , "track"
    , "wbr"
    ]


{-| A list of all Raw Text Elements as defined by the HTML5 specification. They
can contain only text and have restrictions on which characters can appear
within its innerHTML
-}
rawTextElements :: List String
rawTextElements =
    [ "script", "style" ]


{-| A list of all Escapable Raw Text Elements as defined by the HTML5
specification. They can have text and character references, but the text must
not contain an ambiguous ampersand along with addional restrictions:
<https://html.spec.whatwg.org/multipage/syntax.html#cdata-rcdata-restrictions>
-}
escapableRawTextElements :: List String
escapableRawTextElements =
    [ "textarea", "title" ]



{- Foreign elements are elements from the MathML namespace and the
   SVG namespace. TODO: detect these nodes and handle them correctly. Right
   now they will just be treated as Normal elements.
-}


{-| Identify the kind of element. Helper to convert an tag name into a type for
pattern matching.
-}
toElementKind :: String -> ElementKind
toElementKind element =
    if List.member element voidElements then
        VoidElements

    else if List.member element rawTextElements then
        RawTextElements

    else if List.member element escapableRawTextElements then
        EscapableRawTextElements

    else
        -- All other allowed HTML elements are normal elements
        NormalElements
