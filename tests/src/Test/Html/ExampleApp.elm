module Test.Html.ExampleApp exposing (Model, Msg, exampleModel, view)

import Html exposing (Html, a, div, footer, header, li, section, text)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy as Lazy


type alias Model =
    ()


exampleModel : Model
exampleModel =
    ()


type Msg
    = GoToHome
    | GoToExamples


view : Model -> Html Msg
view _ =
    div [ class "container" ]
        [ header [ class "funky themed", id "heading" ]
            [ a [ href "http://elm-lang.org", onClick GoToHome ] [ text "home" ]
            , a [ href "http://elm-lang.org/examples", onClick GoToExamples ] [ text "examples" ]
            , a [ href "http://elm-lang.org/docs" ] [ text "docs" ]
            ]
        , section [ class "funky themed", id "section" ]
            [ someList ]
        , footer [] [ text "this is the footer" ]
        ]


someList : Html Msg
someList =
    Keyed.ul [ class "some-list" ]
        [ ( "1"
          , Lazy.lazy (\_ -> li [ class "list-item themed" ] [ text "first item" ])
                Nothing
          )
        , ( "2"
          , Lazy.lazy (\_ -> li [ class "list-item themed" ] [ text "second item" ])
                Nothing
          )
        , ( "3"
          , Lazy.lazy (\_ -> li [ class "list-item themed selected" ] [ text "third item" ])
                Nothing
          )
        , ( "4"
          , Lazy.lazy (\_ -> li [ class "list-item themed" ] [ text "fourth item" ])
                Nothing
          )
        ]
