module Example exposing (Model, Msg(..), example, example2, example3, update)

import Dict exposing (Dict)
import Expect exposing (Expectation)
import Html exposing (Html)
import Http
import Test.Cmd exposing (ExpectCmd(..), HttpExpect(..), fromCmd, test)


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


exampleOnExpectation =
    test "submitting the form sends an X-CSRF-Token header. " <|
        \key ->
            update SubmittedForm initialModel
                |> Tuple.second
                |> fromCmd key
                |> Expect.Cmd.httpRequest key
                    (\req _ ->
                        Expect.equal req.headers [ ( "X-CSRF-Token", initialModel.token ) ]
                    )


examplePostBodyOnExpectation =
    test "submitting a POST request with a particular JSON body" <|
        \key ->
            let
                expectedJson =
                    Json.Encode.object
                        [ ( "magicNumber", Json.Encode.int 72383 ) ]
            in
            update SubmittedForm initialModel
                |> Tuple.second
                |> Expect.Cmd.httpRequest key
                    (\req _ ->
                        case req.body of
                            StringBody json ->
                                json
                                    |> Expect.equal (Json.Encode.encode 0 expectedJson)

                            _ ->
                                Expect.fail "Form did not send a String body in the HTTP request"
                    )



-- |> Expect.Cmd.httpRequest key callback
-- |> Expect.Cmd.unbatch key
-- |> Expect.Cmd.task key callback
--
-- works great for making only MINOR changes when new commands come out.
--
-- Expect.Cmd.httpRequest passes if either the Cmd is a matching httpRequest, OR if it's a batch and any Cmd in the batch passes.
--
-- Still to solve:
--
-- 1. How do tasks work?
-- 2. How can we detect "this Cmd contains a batch of 3 HTTP requests going to different URLs."
--    Something like:
--        1. Run each of these N checks (in a List) on each of the Cmds in the batch.
--        2. If any checks fail for *all* Cmds, fail the test.
--        3. Return any Cmds for which no checks passed on that Cmd.
--
--    So then you get back either Cmd.none (if all Cmds in the batch passed 1+ checks),
--    or the remaining ones that you didn't care about.
--
--    The reason
--
--    Maybe it's not useful to return the extra Cmds, but it's at least useful
--    to tell if it
--
-- Expect.Cmd.unbatch : Cmd msg -> List (Cmd msg)


example2OnExpectation =
    test "if it times out, render an appropriate error" <|
        \key ->
            update SubmittedForm initialModel
                |> Tuple.second
                |> Expect.Cmd.httpStringRequest key
                    (\req { simulateResponse } ->
                        update (simulateResponse Http.Timeout_) initialModel
                            |> Tuple.first
                            |> view
                            |> Debug.todo "check that we rendered a timeout error"
                    )


exampleOn =
    test "submitting the form sends an X-CSRF-Token header. " <|
        \key ->
            update SubmittedForm initialModel
                |> Tuple.second
                |> fromCmd key
                |> expectHttpRequest (Expect.fail "Form did not send a HTTP request as expected.")
                    (\req _ ->
                        Expect.equal req.headers [ ( "X-CSRF-Token", initialModel.token ) ]
                    )


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
    test "subscribes to download progress for the made request" <|
        \key ->
            let
                ( model, cmd ) =
                    update SubmittedForm initialModel

                cmdTracker =
                    case fromCmd cmd of
                        HttpRequest req _ ->
                            req.tracker

                        _ ->
                            -- TODO: this would be better to return Result String instead of Maybe
                            Nothing

                subTracker =
                    case fromSub key (subscriptions model) of
                        HttpTrack info ->
                            Just info.tracker

                        _ ->
                            -- TODO: this would be better to return Result String instead of Maybe
                            Nothing
            in
            Maybe.map2 Expect.equal cmdTracker subTracker
                |> Maybe.withDefault (Expect.fail "TODO: better message")


example5 =
    test "tracking download progress" <|
        \key ->
            case
                subscriptions initialModel
                    |> fromSub key
            of
                HttpTrack info ->
                    info.tracker
                        |> Expect.equal "trackingKey1"

                sub ->
                    Expect.fail ("Expected subscription to Http.track, but it was a different subscription: " ++ Debug.toString sub)



-- example5 =
--     test "a request with multipart file body was made with particular file contents" <|
--         \key ->
--             case
--                 update SubmittedForm initialModel
--                     |> Tuple.second
--                     |> fromCmd key
--             of
--                 HttpRequest req _ ->
--                     Expect.equal req.body [ ( "X-CSRF-Token", initialModel.token ) ]
--                 _ ->
--                     Expect.fail "Form did not send a HTTP request as expected."
