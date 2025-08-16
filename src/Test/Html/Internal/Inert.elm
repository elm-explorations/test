module Test.Html.Internal.Inert exposing (Node, fromElmHtml, fromHtml, parseAttribute, toElmHtml)

{-| Inert Html - that is, can't do anything with events.

@docs Node, fromElmHtml, fromHtml, parseAttribute, toElmHtml

-}

import Dict
import Elm.Kernel.Test
import Html exposing (Html)
import Json.Decode
import Test.Html.Internal.ElmHtml.InternalTypes as InternalTypes exposing (ElmHtml(..), EventHandler, Tagger)
import VirtualDom


type Node msg
    = Node (ElmHtml msg)


fromHtml : Html msg -> Node msg
fromHtml html =
    Node (Elm.Kernel.Test.virtualDomToTest html TextTag NodeEntry CustomNode MarkdownNode)


fromElmHtml : ElmHtml msg -> Node msg
fromElmHtml =
    Node


toElmHtml : Node msg -> ElmHtml msg
toElmHtml (Node elmHtml) =
    elmHtml


parseAttribute : Html.Attribute a -> Result String InternalTypes.Attribute
parseAttribute attr =
    case fromHtml (Html.div [ attr ] []) of
        Node (NodeEntry { facts }) ->
            case Dict.toList facts.attributes of
                [ ( key, value ) ] ->
                    Ok (InternalTypes.Attribute { key = key, value = value })

                _ ->
                    case Dict.toList facts.attributesNS of
                        [ ( key, { namespace, value } ) ] ->
                            Ok (InternalTypes.NamespacedAttribute { key = key, value = value, namespace = namespace })

                        _ ->
                            case Dict.toList facts.properties of
                                [ ( key, value ) ] ->
                                    Ok (InternalTypes.Property { key = key, value = value })

                                _ ->
                                    case Dict.toList facts.styles of
                                        [ ( key, value ) ] ->
                                            Ok (InternalTypes.Style { key = key, value = value })

                                        _ ->
                                            case Dict.toList facts.events of
                                                [ ( event, _ ) ] ->
                                                    Ok (InternalTypes.Event { event = event })

                                                _ ->
                                                    Err "Error internally processing Attribute for testing - please report this error message as a bug: Html.Attribute didn't end up as a fact in NodeEntry"

        _ ->
            Err "Error internally processing Attribute for testing - please report this error message as a bug: Html.div wasn't parsed as NodeEntry"
