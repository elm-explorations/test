module Html.Inert exposing (Node, fromElmHtml, fromHtml, parseAttribute, toElmHtml)

{-| Inert Html - that is, can't do anything with events.
-}

import Elm.Kernel.HtmlAsJson
import ElmHtml.InternalTypes exposing (ElmHtml(..), EventHandler, Facts, Tagger, decodeAttribute, decodeElmHtml)
import Html exposing (Html)
import Json.Decode


type Node msg
    = Node (ElmHtml msg)


fromHtml : Html msg -> Node msg
fromHtml html =
    case Json.Decode.decodeValue (decodeElmHtml taggedEventDecoder) (toJson html) of
        Ok elmHtml ->
            Node elmHtml

        Err jsonError ->
            -- TODO: should this return a result instead?
            -- currently, the callers of this can't easily handle getting a (Result String Node) instead of just a Node
            Node
                (ElmHtml.InternalTypes.TextTag
                    { text =
                        "Error internally processing HTML for testing - please report this error message as a bug: "
                            ++ Json.Decode.errorToString jsonError
                    }
                )


fromElmHtml : ElmHtml msg -> Node msg
fromElmHtml =
    Node


{-| Convert a Html node to a Json string
-}
toJson : Html a -> Json.Decode.Value
toJson node =
    Elm.Kernel.HtmlAsJson.toJson node


toElmHtml : Node msg -> ElmHtml msg
toElmHtml (Node elmHtml) =
    elmHtml


impossibleMessage : String
impossibleMessage =
    "An Inert Node fired an event handler. This should never happen! Please report this bug."


attributeToJson : Html.Attribute a -> Json.Decode.Value
attributeToJson attribute =
    Elm.Kernel.HtmlAsJson.attributeToJson attribute


parseAttribute : Html.Attribute a -> Result String ElmHtml.InternalTypes.Attribute
parseAttribute attr =
    case Json.Decode.decodeValue decodeAttribute (attributeToJson attr) of
        Ok parsedAttribute ->
            Ok parsedAttribute

        Err jsonError ->
            Err
                ("Error internally processing Attribute for testing - please report this error message as a bug: "
                    ++ Json.Decode.errorToString jsonError
                )


{-| Gets the function out of a tagger
-}
taggerFunction : Tagger -> (a -> msg)
taggerFunction tagger =
    Elm.Kernel.HtmlAsJson.taggerFunction tagger


{-| Gets the decoder out of an EventHandler
-}
eventDecoder : EventHandler -> Json.Decode.Decoder msg
eventDecoder eventHandler =
    Elm.Kernel.HtmlAsJson.eventDecoder eventHandler


{-| Applies the taggers over the event handlers to have the complete event decoder
-}
taggedEventDecoder : List Tagger -> EventHandler -> Json.Decode.Decoder msg
taggedEventDecoder taggers eventHandler =
    case taggers of
        [] ->
            eventDecoder eventHandler

        [ tagger ] ->
            Json.Decode.map (taggerFunction tagger) (eventDecoder eventHandler)

        tagger :: rest ->
            Json.Decode.map (taggerFunction tagger) (taggedEventDecoder rest eventHandler)
