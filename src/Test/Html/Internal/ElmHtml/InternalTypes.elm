module Test.Html.Internal.ElmHtml.InternalTypes exposing
    ( ElmHtml(..), NodeRecord, MarkdownNodeRecord
    , Facts, Tagger, EventHandler, ElementKind(..)
    , Attribute(..), AttributeRecord, NamespacedAttributeRecord, PropertyRecord, EventRecord
    , toElementKind
    )

{-| Internal types used to represent Elm Html in pure Elm

@docs ElmHtml, NodeRecord, MarkdownNodeRecord

@docs Facts, Tagger, EventHandler, ElementKind

@docs Attribute, AttributeRecord, NamespacedAttributeRecord, PropertyRecord, EventRecord

@docs toElementKind

-}

import Dict exposing (Dict)
import Json.Decode exposing (field)
import Test.Html.Internal.ElmHtml.Constants as Constants exposing (..)
import Test.Html.Internal.ElmHtml.Helpers exposing (..)
import VirtualDom


{-| Type tree for representing Elm's Html

  - TextTag is just a plain old bit of text.
  - NodeEntry is an actual HTML node, e.g a div
  - CustomNode are nodes defined to work with the renderer in some way, e.g webgl/markdown
  - MarkdownNode is just a wrapper for CustomNode designed just for markdown

-}
type ElmHtml msg
    = TextTag String
    | NodeEntry (NodeRecord msg)
    | CustomNode (Facts msg)
    | MarkdownNode (MarkdownNodeRecord msg)


{-| A node contains the `tag` as a string, the children, the facts (e.g attributes) and namespace
-}
type alias NodeRecord msg =
    { tag : String
    , children : List (ElmHtml msg)
    , facts : Facts msg
    , namespace : Maybe String
    }


{-| A markdown node contains facts (e.g attributes) and the model used by markdown
-}
type alias MarkdownNodeRecord msg =
    { facts : Facts msg
    , markdown : String
    }


{-| Tagger holds the map function when Html.Map is used, the tagger
should then be applied to events comming from descendant nodes, it
is basically a javascript function.
-}
type alias Tagger =
    Json.Decode.Value


{-| EventHandler holds the function that is called when an event is
triggered, it is basically a javascript object like this:

{ decoder: [Function] }

-}
type alias EventHandler =
    Json.Decode.Value


{-| Facts contain various dictionaries and values for a node
-}
type alias Facts msg =
    { styles : Dict String String
    , events : Dict String (VirtualDom.Handler msg)
    , attributes : Dict String String
    , attributesNS : Dict String { namespace : String, value : String }
    , properties : Dict String Json.Decode.Value
    }


{-| Type for representing the five kinds of elements according to HTML 5
[spec](https://html.spec.whatwg.org/multipage/syntax.html#elements-2).
Used to handle different rendering behavior depending on the type of element.
-}
type ElementKind
    = VoidElements
    | RawTextElements
    | EscapableRawTextElements
    | ForeignElements
    | NormalElements


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
type Attribute
    = Attribute AttributeRecord
    | NamespacedAttribute NamespacedAttributeRecord
    | Property PropertyRecord
    | Style { key : String, value : String }
    | Event EventRecord


{-| Attribute contains a string key and a string value
-}
type alias AttributeRecord =
    { key : String
    , value : String
    }


{-| NamespacedAttribute contains a string key, string namespace and string value
-}
type alias NamespacedAttributeRecord =
    { key : String
    , value : String
    , namespace : String
    }


{-| Property contains a string key and a value with an arbitrary type
-}
type alias PropertyRecord =
    { key : String
    , value : Json.Decode.Value
    }


{-| Event contains a string event
-}
type alias EventRecord =
    { event : String
    }


{-| A list of Void elements as defined by the HTML5 specification. These
elements must not have closing tags and most not be written as self closing
either
-}
voidElements : List String
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
rawTextElements : List String
rawTextElements =
    [ "script", "style" ]


{-| A list of all Escapable Raw Text Elements as defined by the HTML5
specification. They can have text and character references, but the text must
not contain an ambiguous ampersand along with addional restrictions:
<https://html.spec.whatwg.org/multipage/syntax.html#cdata-rcdata-restrictions>
-}
escapableRawTextElements : List String
escapableRawTextElements =
    [ "textarea", "title" ]



{- Foreign elements are elements from the MathML namespace and the
   SVG namespace. TODO: detect these nodes and handle them correctly. Right
   now they will just be treated as Normal elements.
-}


{-| Identify the kind of element. Helper to convert an tag name into a type for
pattern matching.
-}
toElementKind : String -> ElementKind
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
