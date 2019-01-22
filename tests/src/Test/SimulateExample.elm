module Example exposing (Model, Msg(..), example, example2, example3, update)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Html exposing (Html)
import Http
import Test.Simulate exposing (ExpectCmd(..), HttpExpect(..), fromCmd, test)


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


example =
    test "submitting the form sends an X-CSRF-Token header. " <|
        \key ->
            case
                update SubmittedForm initialModel
                    |> Tuple.second
                    |> fromCmd key
            of
                HttpRequest req _ ->
                    Expect.equal req.headers [ ( "X-CSRF-Token", initialModel.token ) ]

                _ ->
                    Expect.fail "Form did not send a HTTP request as expected."


examplePostBody =
    test "submitting a POST request with a particular JSON body" <|
        \key ->
            let
                expectedJson =
                    Json.Encode.object
                        [ ( "magicNumber", Json.Encode.int 72383 ) ]
            in
            case
                update SubmittedForm initialModel
                    |> Tuple.second
                    |> fromCmd key
            of
                HttpRequest req _ ->
                    case req.body of
                        StringBody json ->
                            json
                                |> Expect.equal (Json.Encode.encode 0 expectedJson)

                        _ ->
                            Expect.fail "Form did not send a String body in the HTTP request"

                _ ->
                    Expect.fail "Form did not send a HTTP request as expected."


example2 =
    test "if it times out, render an appropriate error" <|
        \key ->
            case
                update SubmittedForm initialModel
                    |> Tuple.second
                    |> fromCmd key
            of
                HttpRequest req (ExpectString { simulateResponse }) ->
                    update (simulateResponse Http.Timeout_) initialModel
                        |> Tuple.first
                        |> view
                        |> Debug.todo "check that we rendered a timeout error"

                _ ->
                    Expect.fail "Form did not send a HTTP request as expected."


example3 =
    test "if we get back a User, render their username" <|
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
            case
                update SubmittedForm initialModel
                    |> Tuple.second
                    |> fromCmd key
            of
                HttpRequest req (ExpectString { simulateResponse }) ->
                    update (simulateResponse fakeResponse) initialModel
                        |> Tuple.first
                        |> view
                        |> Debug.todo "check that we rendered the username"

                _ ->
                    Expect.fail "Form did not send a HTTP request as expected."


example4 =
    test "a request with multipart file body was made with particular file contents" <|
        \key ->
            case
                update SubmittedForm initialModel
                    |> Tuple.second
                    |> fromCmd key
            of
                HttpRequest req _ ->
                    Expect.equal req.body [ ( "X-CSRF-Token", initialModel.token ) ]

                _ ->
                    Expect.fail "Form did not send a HTTP request as expected."
