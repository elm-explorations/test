module Test.Html.Event ( Event, simulate, expect, toResult
    , expectStopPropagation, expectNotStopPropagation, expectPreventDefault, expectNotPreventDefault
    , custom, click, doubleClick, mouseDown, mouseUp, mouseEnter, mouseLeave, mouseOver, mouseOut, input, check, submit, blur, focus
    )
 where

{-| This module lets you simulate events on `Html` values and expect that
they result in certain `Msg` values being sent to `update`.


## Simulating Events

@docs Event, simulate, expect, toResult


## Testing Event Effects

These functions allow you to test that your event handlers are (or are not) calling
[`stopPropagation()`](https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation)
and
[`preventDefault()`](https://developer.mozilla.org/en-US/docs/Web/API/Event/preventDefault).
In Elm, you do this by calling
[special functions](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#stopPropagationOn)
in `Html.Events`.

@docs expectStopPropagation, expectNotStopPropagation, expectPreventDefault, expectNotPreventDefault


## Event Builders

@docs custom, click, doubleClick, mouseDown, mouseUp, mouseEnter, mouseLeave, mouseOver, mouseOut, input, check, submit, blur, focus

-}

import Dict as Dict
import Expect (Expectation)
import Expect as Expect
import Json.Decode (Decoder)
import Json.Decode as Decode

import Json.Encode (Value)
import Json.Encode as Encode

import Test.Html.Internal.ElmHtml.InternalTypes (ElmHtml(..))
import Test.Html.Internal.ElmHtml.InternalTypes as Test.Html.Internal.ElmHtml.InternalTypes
import Test.Html.Query as Query

import Test.Html.Query.Internal as QueryInternal

import Test.Internal as Internal

import VirtualDom as VirtualDom


{-| A simulated event.

See [`simulate`](#simulate).

-}
data Event msg
    = Event {a::String, b::Value } (QueryInternal.Single msg)


{-| Simulate an event on a node.

    import Test.Html.Event as Event

    type Msg
        = Change String


    test "Input produces expected Msg" <|
        \() ->
            Html.input [ onInput Change ] [ ]
                |> Query.fromHtml
                |> Event.simulate (Event.input "cats")
                |> Event.expect (Change "cats")

-}
simulate :: {a::String, b::Value } -> Query.Single msg -> Event msg
simulate =
    Event


{-| Passes if the given message is triggered by the simulated event.

    import Test.Html.Event as Event

    type Msg
        = Change String


    test "Input produces expected Msg" <|
        \() ->
            Html.input [ onInput Change ] [ ]
                |> Query.fromHtml
                |> Event.simulate (Event.input "cats")
                |> Event.expect (Change "cats")

-}
expect :: msg -> Event msg -> Expectation
expect msg (Event event (QueryInternal.Single showTrace query)) =
    case toResult (Event event (QueryInternal.Single showTrace query)) of
        Err noEvent ->
            Expect.fail noEvent
                |> QueryInternal.failWithQuery showTrace "" query

        Ok foundMsg ->
            foundMsg
                |> Expect.equal msg
                |> QueryInternal.failWithQuery showTrace
                    ("Event.expectEvent: Expected the msg \u{001B}[32m"
                        <> Internal.toString msg
                        <> "\u{001B}[39m from the event \u{001B}[31m"
                        <> Internal.toString event
                        <> "\u{001B}[39m but could not find the event."
                    )
                    query


{-| Returns a Result with the Msg produced by the event simulated on a node.
Note that Event.expect gives nicer messages; this is generally more useful
when testing that an event handler is _not_ present.

    import Test.Html.Event as Event


    test "Input produces expected Msg" <|
        \() ->
            Html.input [ onInput Change ] [ ]
                |> Query.fromHtml
                |> Event.simulate (Event.input "cats")
                |> Event.toResult
                |> Expect.equal (Ok (Change "cats"))

-}
toResult :: Event msg -> Result String msg
toResult event =
    findHandler event
        |> Result.map (Decode.map .message)
        |> Result.andThen
            (\handler ->
                Decode.decodeValue handler (eventPayload event)
                    |> Result.mapError Decode.errorToString
            )



-- EFFECTS --


{-| Passes if the event handler stops propagation of the event.
-}
expectStopPropagation :: Event msg -> Expectation
expectStopPropagation event =
    case checkStopPropagation event of
        Err reason ->
            Expect.fail reason

        Ok False ->
            Expect.fail "I found a handler that could have stopped propagation of the event, but it didn't."

        Ok True ->
            Expect.pass


{-| Passes if the event handler doesn't stop propagation of the event.
-}
expectNotStopPropagation :: Event msg -> Expectation
expectNotStopPropagation event =
    case checkStopPropagation event of
        Err reason ->
            Expect.fail reason

        Ok False ->
            Expect.pass

        Ok True ->
            Expect.fail
                "I found a handler that should have not stopped propagation of the event, but it did."


{-| Passes if the event handler prevents default action of the event.
-}
expectPreventDefault :: Event msg -> Expectation
expectPreventDefault event =
    case checkPreventDefault event of
        Err reason ->
            Expect.fail reason

        Ok False ->
            Expect.fail "I found a handler that could have prevented default action of the event, but it didn't."

        Ok True ->
            Expect.pass


{-| Passes if the event handler doesn't prevent default action of the event.
-}
expectNotPreventDefault :: Event msg -> Expectation
expectNotPreventDefault event =
    case checkPreventDefault event of
        Err reason ->
            Expect.fail reason

        Ok False ->
            Expect.pass

        Ok True ->
            Expect.fail
                "I found a handler that should have not prevented the default action of the event, but it did."


{-| A [`click`](https://developer.mozilla.org/en-US/docs/Web/Events/click) event.
-}
click :: {a::String, b::Value }
click =
    ( "click", emptyObject )


{-| A [`dblclick`](https://developer.mozilla.org/en-US/docs/Web/Events/dblclick) event.
-}
doubleClick :: {a::String, b::Value }
doubleClick =
    ( "dblclick", emptyObject )


{-| A [`mousedown`](https://developer.mozilla.org/en-US/docs/Web/Events/mousedown) event.
-}
mouseDown :: {a::String, b::Value }
mouseDown =
    ( "mousedown", emptyObject )


{-| A [`mouseup`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseup) event.
-}
mouseUp :: {a::String, b::Value }
mouseUp =
    ( "mouseup", emptyObject )


{-| A [`mouseenter`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseenter) event.
-}
mouseEnter :: {a::String, b::Value }
mouseEnter =
    ( "mouseenter", emptyObject )


{-| A [`mouseleave`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseleave) event.
-}
mouseLeave :: {a::String, b::Value }
mouseLeave =
    ( "mouseleave", emptyObject )


{-| A [`mouseover`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseover) event.
-}
mouseOver :: {a::String, b::Value }
mouseOver =
    ( "mouseover", emptyObject )


{-| A [`mouseout`](https://developer.mozilla.org/en-US/docs/Web/Events/mouseout) event.
-}
mouseOut :: {a::String, b::Value }
mouseOut =
    ( "mouseout", emptyObject )


{-| An [`input`](https://developer.mozilla.org/en-US/docs/Web/Events/input) event.
-}
input :: String -> {a::String, b::Value }
input value =
    ( "input"
    , Encode.object
        [ ( "target"
          , Encode.object [ ( "value", Encode.string value ) ]
          )
        ]
    )


{-| A [`change`](https://developer.mozilla.org/en-US/docs/Web/Events/change) event
where `event.target.checked` is set to the given `Bool` value.
-}
check :: Bool -> {a::String, b::Value }
check checked =
    ( "change"
    , Encode.object
        [ ( "target"
          , Encode.object [ ( "checked", Encode.bool checked ) ]
          )
        ]
    )


{-| A [`submit`](https://developer.mozilla.org/en-US/docs/Web/Events/submit) event.
-}
submit :: {a::String, b::Value }
submit =
    ( "submit", emptyObject )


{-| A [`blur`](https://developer.mozilla.org/en-US/docs/Web/Events/blur) event.
-}
blur :: {a::String, b::Value }
blur =
    ( "blur", emptyObject )


{-| A [`focus`](https://developer.mozilla.org/en-US/docs/Web/Events/focus) event.
-}
focus :: {a::String, b::Value }
focus =
    ( "focus", emptyObject )


{-| Simulate a custom event. The `String` is the event name, and the `Value` is the event object
the browser would send to the event listener callback.

    import Test.Html.Event as Event
    import Json.Encode as Encode exposing (Value)


    type Msg
        = Change String


    test "Input produces expected Msg" <|
        \() ->
            let
                simulatedEventObject : Value
                simulatedEventObject =
                    Encode.object
                        [ ( "target"
                          , Encode.object [ ( "value", Encode.string "cats" ) ]
                          )
                        ]
            in
                Html.input [ onInput Change ] [ ]
                    |> Query.fromHtml
                    |> Event.simulate (Event.custom "input" simulatedEventObject)
                    |> Event.expect (Change "cats")

-}
custom :: String -> Value -> {a::String, b::Value }
custom =
    Tuple.pair



-- INTERNAL --


emptyObject :: Value
emptyObject =
    Encode.object List.nil


eventPayload :: Event msg -> Value
eventPayload (Event ( _, payload ) _) =
    payload


type Handling msg =
    { message :: msg, stopPropagation :: Bool, preventDefault :: Bool }


findHandler :: Event msg -> Result String (Decoder (Handling msg))
findHandler (Event ( eventName, _ ) (QueryInternal.Single _ query)) =
    QueryInternal.traverse query
        |> Result.andThen (QueryInternal.verifySingle eventName)
        |> Result.mapError QueryInternal.queryErrorToString
        |> Result.andThen (findEvent eventName)


findEvent :: String -> ElmHtml msg -> Result String (Decoder (Handling msg))
findEvent eventName element =
    let
        elementOutput =
            QueryInternal.prettyPrint element

        handlerToDecoder :: VirtualDom.Handler msg -> Decoder (Handling msg)
        handlerToDecoder handler =
            case handler of
                VirtualDom.Normal decoder ->
                    decoder |> Decode.map (\msg -> Handling msg False False)

                VirtualDom.MayStopPropagation decoder ->
                    decoder |> Decode.map (\{a:msg, b:sp } -> Handling msg sp False)

                VirtualDom.MayPreventDefault decoder ->
                    decoder |> Decode.map (\{a:msg, b:pd } -> Handling msg False pd)

                VirtualDom.Custom decoder ->
                    decoder

        eventDecoder node =
            node.facts.events
                |> Dict.get eventName
                |> Maybe.map handlerToDecoder
                |> Result.fromMaybe ("Event.expectEvent: I found a node, but it does not listen for \"" <> eventName <> "\" events like I expected it would.\n\n" <> elementOutput)
    in
    case element of
        TextTag _ ->
            Err ("I found a text node instead of an element. Text nodes do not receive events, so it would be impossible to simulate \"" <> eventName <> "\" events on it. The text in the node was: \"" <> elementOutput <> "\"")

        NodeEntry node ->
            eventDecoder node

        CustomNode node ->
            eventDecoder node

        MarkdownNode node ->
            eventDecoder node


checkStopPropagation :: Event msg -> Result String Bool
checkStopPropagation =
    checkEffect .stopPropagation


checkPreventDefault :: Event msg -> Result String Bool
checkPreventDefault =
    checkEffect .preventDefault


checkEffect :: (Handling msg -> Bool) -> Event msg -> Result String Bool
checkEffect extractor event =
    findHandler event
        |> Result.map (Decode.map extractor)
        |> Result.andThen
            (\handler ->
                Decode.decodeValue handler (eventPayload event)
                    |> Result.mapError Decode.errorToString
            )
