module EffectExample exposing (test)

import Dict
import Expect
import Expect.Effect as Effect
import Html exposing (Html)
import Http
import Test exposing (Test)
import Test.Effect


type Msg
    = SubmittedForm


type alias Model =
    { token : String }


initialModel : Model
initialModel =
    { token = "blah" }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html msg
view model =
    Html.text "hi!"


test : Test
test =
    Test.Effect.test "if we get back a User, render their username" <|
        \key ->
            let
                fakeResponse =
                    Http.GoodStatus_
                        { url = "stuff.com"
                        , statusCode = 200
                        , statusText = "Looks good!"
                        , headers = Dict.empty
                        }
                        """
                        {"username": "foo", "email": "bar"}
                        """
            in
            update SubmittedForm initialModel
                |> Tuple.second
                |> Effect.fromCmd key
                |> Effect.httpRequestString
                    (\_ httpCallback ->
                        httpCallback fakeResponse
                            |> Effect.completed
                                (\msg ->
                                    update msg initialModel
                                        |> Tuple.first
                                        |> view
                                        |> (\_ -> Expect.pass)
                                )
                    )
