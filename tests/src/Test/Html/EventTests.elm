module Test.Html.EventTests exposing (all)

import Expect
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attr exposing (href)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy as Lazy
import Json.Decode exposing (Value, succeed)
import Json.Encode as Encode
import Test exposing (..)
import Test.Html.Event as Event exposing (Event)
import Test.Html.Query as Query exposing (Single)
import Test.Html.Selector exposing (tag)


all : Test
all =
    describe "trigerring events"
        [ test "returns msg for click on element" <|
            \() ->
                Query.fromHtml sampleHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect SampleMsg
        , test "return msg for stopPropagation event listener" <|
            \() ->
                div [ Attr.class "container" ]
                    [ button [ stopPropagationOn "click" (succeed ( SampleMsg, True )) ] [ text "click me" ]
                    ]
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect SampleMsg
        , test "return msg for preventDefault event listener" <|
            \() ->
                div [ Attr.class "container" ]
                    [ button [ preventDefaultOn "click" (succeed ( SampleMsg, True )) ] [ text "click me" ]
                    ]
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect SampleMsg
        , test "return msg for custom event listener" <|
            \() ->
                div [ Attr.class "container" ]
                    [ button [ Html.Events.custom "click" (succeed { message = SampleMsg, stopPropagation = True, preventDefault = True }) ] [ text "click me" ]
                    ]
                    |> Query.fromHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect SampleMsg
        , test "returns msg for click on lazy html" <|
            \() ->
                Query.fromHtml sampleLazyHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect SampleMsg
        , test "returns msg for click on mapped html" <|
            \() ->
                Query.fromHtml sampleMappedHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect MappedSampleMsg
        , test "returns msg for click on mapped lazy html" <|
            \() ->
                Query.fromHtml sampleMappedLazyHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect MappedSampleMsg
        , test "returns msg for click on mapped keyed html" <|
            \() ->
                Query.fromHtml sampleMappedKeyedHtml
                    |> Query.findAll [ tag "button" ]
                    |> Query.first
                    |> Event.simulate Event.click
                    |> Event.expect MappedSampleMsg
        , test "returns msg for click on deep mapped html" <|
            \() ->
                Query.fromHtml deepMappedHtml
                    |> Query.findAll [ tag "input" ]
                    |> Query.first
                    |> Event.simulate (Event.input "foo")
                    |> Event.expect (SampleInputMsg "foobar")
        , test "returns msg for input with transformation" <|
            \() ->
                input [ onInput (String.toUpper >> SampleInputMsg) ] []
                    |> Query.fromHtml
                    |> Event.simulate (Event.input "cats")
                    |> Event.expect (SampleInputMsg "CATS")
        , test "returns msg for check event" <|
            \() ->
                input [ onCheck SampleCheckedMsg ] []
                    |> Query.fromHtml
                    |> Event.simulate (Event.check True)
                    |> Event.expect (SampleCheckedMsg True)
        , test "returns msg for custom event" <|
            \() ->
                input [ on "keyup" (Json.Decode.map SampleKeyUpMsg keyCode) ] []
                    |> Query.fromHtml
                    |> Event.simulate ( "keyup", Encode.object [ ( "keyCode", Encode.int 5 ) ] )
                    |> Event.expect (SampleKeyUpMsg 5)
        , testEvent onDoubleClick Event.doubleClick
        , testEvent onMouseDown Event.mouseDown
        , testEvent onMouseUp Event.mouseUp
        , testEvent onMouseLeave Event.mouseLeave
        , testEvent onMouseOver Event.mouseOver
        , testEvent onMouseOut Event.mouseOut
        , testEvent onSubmit Event.submit
        , testEvent onBlur Event.blur
        , testEvent onFocus Event.focus
        , test "event result" <|
            \() ->
                Query.fromHtml sampleHtml
                    |> Query.find [ tag "button" ]
                    |> Event.simulate Event.click
                    |> Event.toResult
                    |> Expect.equal (Ok SampleMsg)
        , describe "stop propagation and prevent default"
            [ test "Html.Events.on" <|
                \() ->
                    Html.Events.on "click" (succeed SampleMsg)
                        |> effectHtml
                        |> Expect.all [ Event.expectNotStopPropagation, Event.expectNotPreventDefault ]
            , describe "Html.Events.stopPropagationOn"
                [ test "false case" <|
                    \() ->
                        Html.Events.stopPropagationOn "click" (succeed ( SampleMsg, False ))
                            |> effectHtml
                            |> Expect.all [ Event.expectNotStopPropagation, Event.expectNotPreventDefault ]
                , test "true case" <|
                    \() ->
                        Html.Events.stopPropagationOn "click" (succeed ( SampleMsg, True ))
                            |> effectHtml
                            |> Expect.all [ Event.expectStopPropagation, Event.expectNotPreventDefault ]
                ]
            , describe "Html.Events.preventDefaultOn"
                [ test "false case" <|
                    \() ->
                        Html.Events.preventDefaultOn "click" (succeed ( SampleMsg, False ))
                            |> effectHtml
                            |> Expect.all [ Event.expectNotStopPropagation, Event.expectNotPreventDefault ]
                , test "true case" <|
                    \() ->
                        Html.Events.preventDefaultOn "click" (succeed ( SampleMsg, True ))
                            |> effectHtml
                            |> Expect.all [ Event.expectNotStopPropagation, Event.expectPreventDefault ]
                ]
            , describe "Html.Events.custom"
                [ test "false case" <|
                    \() ->
                        Html.Events.custom "click"
                            (succeed
                                { message = SampleMsg
                                , stopPropagation = False
                                , preventDefault = False
                                }
                            )
                            |> effectHtml
                            |> Expect.all [ Event.expectNotStopPropagation, Event.expectNotPreventDefault ]
                , test "true case" <|
                    \() ->
                        Html.Events.custom "click"
                            (succeed
                                { message = SampleMsg
                                , stopPropagation = True
                                , preventDefault = True
                                }
                            )
                            |> effectHtml
                            |> Expect.all [ Event.expectStopPropagation, Event.expectPreventDefault ]
                ]
            ]
        ]


type Msg
    = SampleMsg
    | MappedSampleMsg
    | SampleInputMsg String
    | SampleCheckedMsg Bool
    | SampleKeyUpMsg Int


sampleHtml : Html Msg
sampleHtml =
    div [ Attr.class "container" ]
        [ button [ onClick SampleMsg ] [ text "click me" ]
        ]


sampleLazyHtml : Html Msg
sampleLazyHtml =
    div [ Attr.class "container" ]
        [ Lazy.lazy
            (\str -> button [ onClick SampleMsg ] [ text str ])
            "click me"
        ]


sampleMappedHtml : Html Msg
sampleMappedHtml =
    div [ Attr.class "container" ]
        [ Html.map (always MappedSampleMsg) (button [ onClick SampleMsg ] [ text "click me" ])
        ]


sampleMappedLazyHtml : Html Msg
sampleMappedLazyHtml =
    div [ Attr.class "container" ]
        [ Html.map (always MappedSampleMsg) <|
            Lazy.lazy
                (\str -> button [ onClick SampleMsg ] [ text str ])
                "click me"
        ]


sampleMappedKeyedHtml : Html Msg
sampleMappedKeyedHtml =
    div [ Attr.class "container" ]
        [ Html.map (always MappedSampleMsg) <|
            Keyed.node "button"
                [ onClick SampleMsg ]
                [ ( "key", text "click me" ) ]
        ]


deepMappedHtml : Html Msg
deepMappedHtml =
    div []
        [ Html.map SampleInputMsg
            (div []
                [ Html.map (\msg -> msg ++ "bar")
                    (div []
                        [ input [ onInput identity ] []
                        ]
                    )
                ]
            )
        ]


effectHtml : Html.Attribute Msg -> Event Msg
effectHtml attr =
    div [ Attr.class "container" ] [ button [ attr ] [ text "click me" ] ]
        |> Query.fromHtml
        |> Query.findAll [ tag "button" ]
        |> Query.first
        |> Event.simulate Event.click


testEvent : (Msg -> Html.Attribute Msg) -> ( String, Value ) -> Test
testEvent testOn (( eventName, eventValue ) as event) =
    test ("returns msg for " ++ eventName ++ "(" ++ Encode.encode 0 eventValue ++ ") event") <|
        \() ->
            input [ testOn SampleMsg ] []
                |> Query.fromHtml
                |> Event.simulate event
                |> Event.expect SampleMsg
