port module Main exposing (main)

{-| The measurement half of the F-metric harness.

This is a dumb, one-measurement-per-message worker: `runner.mjs` decides what to
measure and does all the statistics, this module only runs a single fuzz test and
reports what happened. Keeping it this way means

  - re-analysing results never requires recompiling Elm,
  - each measurement lands in its own JS tick, so we don't build a deep chain of
    `Task`s or grow the stack over a long run,
  - the JS side can stream results to disk and show progress.

Everything here goes through the public `Test.RunnerV2` API, so this survives
changes to elm-test internals.

-}

import Array exposing (Array)
import Corpus
import Corpus.Case as Case exposing (Case, Role(..))
import Json.Decode as Decode
import Json.Encode as Encode
import Platform
import Random
import Task
import Test.RunnerV2 as Runner exposing (FuzzTestExpectation(..))


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( Array.fromList (List.map prepare Corpus.all), Cmd.none )
        , update = update
        , subscriptions = \_ -> fromJs FromJs
        }



-- MODEL


type alias Model =
    Array Entry


type alias Entry =
    { name : String
    , category : String
    , role : Role
    , minima : List String
    , budget : Maybe Int

    {- A corpus case is supposed to be exactly one fuzz test. If someone writes
       one that isn't, we want to say so rather than measure nonsense.
    -}
    , fuzzTest : Result String Runner.FuzzTest
    }


prepare : Case -> Entry
prepare case_ =
    let
        fuzzTests : Array Runner.FuzzTest
        fuzzTests =
            Runner.toTests case_.test
                |> Runner.getFuzzTests
    in
    { name = case_.name
    , category = case_.category
    , role = case_.role
    , minima = case_.minima
    , budget = case_.budget
    , fuzzTest =
        case Array.toList fuzzTests of
            [ fuzzTest ] ->
                Ok fuzzTest

            other ->
                Err
                    ("expected the case to be exactly one fuzz test, got "
                        ++ String.fromInt (List.length other)
                    )
    }



-- UPDATE


type Msg
    = FromJs Decode.Value
    | Measured Entry MeasureParams ( FuzzTestExpectation, Float, String )


type Request
    = Manifest
    | Measure MeasureParams


type alias MeasureParams =
    { index : Int
    , seed : Int
    , budget : Int
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromJs raw ->
            case Decode.decodeValue requestDecoder raw of
                Err error ->
                    ( model, toJs (encodeProtocolError (Decode.errorToString error)) )

                Ok Manifest ->
                    ( model, toJs (encodeManifest model) )

                Ok (Measure params) ->
                    case Array.get params.index model of
                        Nothing ->
                            ( model
                            , toJs
                                (encodeProtocolError
                                    ("no corpus case at index " ++ String.fromInt params.index)
                                )
                            )

                        Just entry ->
                            case entry.fuzzTest of
                                Err error ->
                                    ( model, toJs (encodeBroken entry params error) )

                                Ok fuzzTest ->
                                    ( model
                                    , Runner.runFuzzTest fuzzTest
                                        (Random.initialSeed params.seed)
                                        params.budget
                                        {- No "fuzzer ints" to replay: we want to
                                           measure finding the defect from scratch.
                                        -}
                                        []
                                        |> Task.perform (Measured entry params)
                                    )

        Measured entry params result ->
            ( model, toJs (encodeMeasurement entry params result) )



-- PROTOCOL: DECODING


requestDecoder : Decode.Decoder Request
requestDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "manifest" ->
                        Decode.succeed Manifest

                    "measure" ->
                        Decode.map3 MeasureParams
                            (Decode.field "index" Decode.int)
                            (Decode.field "seed" Decode.int)
                            (Decode.field "budget" Decode.int)
                            |> Decode.map Measure

                    _ ->
                        Decode.fail ("unknown request tag: " ++ tag)
            )



-- PROTOCOL: ENCODING


encodeManifest : Model -> Encode.Value
encodeManifest model =
    Encode.object
        [ ( "tag", Encode.string "manifest" )
        , ( "cases"
          , model
                |> Array.toList
                |> List.indexedMap
                    (\index entry ->
                        Encode.object
                            [ ( "index", Encode.int index )
                            , ( "name", Encode.string entry.name )
                            , ( "category", Encode.string entry.category )
                            , ( "role"
                              , Encode.string
                                    (case entry.role of
                                        Search ->
                                            "search"

                                        ShrinkOnly ->
                                            "shrinkOnly"
                                    )
                              )
                            , ( "minima", Encode.list Encode.string entry.minima )
                            , ( "budget", encodeMaybe Encode.int entry.budget )
                            , ( "error"
                              , case entry.fuzzTest of
                                    Ok _ ->
                                        Encode.null

                                    Err error ->
                                        Encode.string error
                              )
                            ]
                    )
                |> Encode.list identity
          )
        ]


encodeMeasurement : Entry -> MeasureParams -> ( FuzzTestExpectation, Float, String ) -> Encode.Value
encodeMeasurement entry params ( expectation, durationMs, _ ) =
    let
        details : List ( String, Encode.Value )
        details =
            case expectation of
                FuzzTestPass _ ->
                    [ ( "status", Encode.string "notDetected" ) ]

                FuzzTestFail failData ->
                    let
                        description : String
                        description =
                            Runner.getFuzzTestFailDescription failData

                        given : Maybe String
                        given =
                            Runner.getFuzzTestFailGiven failData

                        fuzzerInts : List Int
                        fuzzerInts =
                            Runner.getFuzzTestFailFuzzerInts failData
                    in
                    [ {- The count of values the fuzzer generated before it found
                         the defect: the F-metric in its original sense. Unlike
                         the duration it's an exact integer with no timer noise,
                         so it stays meaningful for cases that finish in
                         microseconds.
                      -}
                      ( "cases", Encode.int (Runner.getFuzzTestFailRunsElapsed failData) )
                    , ( "status"
                      , Encode.string
                            (if String.contains Case.violation description then
                                "detected"

                             else
                                {- The test failed for a reason other than the defect
                                   we injected: an invalid fuzzer, too many filtered
                                   values, a runtime exception. Not a detection.
                                -}
                                "error"
                            )
                      )
                    , ( "description", Encode.string description )
                    , ( "given", encodeMaybe Encode.string given )
                    , ( "fuzzerInts", Encode.list Encode.int fuzzerInts )

                    {- The length and sum of the fuzzer ints are the two components
                       of the shortlex order that simplifying minimises, so they give
                       us a numeric counterexample-quality score even for cases whose
                       optimum we haven't written down.
                    -}
                    , ( "runLength", Encode.int (List.length fuzzerInts) )
                    , ( "runSum", Encode.int (List.sum fuzzerInts) )
                    , ( "optimal"
                      , if List.isEmpty entry.minima then
                            Encode.null

                        else
                            case given of
                                Nothing ->
                                    Encode.bool False

                                Just g ->
                                    Encode.bool (List.member g entry.minima)
                      )
                    ]
    in
    Encode.object
        (( "tag", Encode.string "measurement" )
            :: ( "index", Encode.int params.index )
            :: ( "name", Encode.string entry.name )
            :: ( "category", Encode.string entry.category )
            :: ( "seed", Encode.int params.seed )
            :: ( "budget", Encode.int params.budget )
            :: ( "durationMs", Encode.float durationMs )
            :: details
        )


encodeBroken : Entry -> MeasureParams -> String -> Encode.Value
encodeBroken entry params error =
    Encode.object
        [ ( "tag", Encode.string "measurement" )
        , ( "index", Encode.int params.index )
        , ( "name", Encode.string entry.name )
        , ( "category", Encode.string entry.category )
        , ( "seed", Encode.int params.seed )
        , ( "budget", Encode.int params.budget )
        , ( "durationMs", Encode.float 0 )
        , ( "status", Encode.string "error" )
        , ( "description", Encode.string error )
        ]


encodeProtocolError : String -> Encode.Value
encodeProtocolError error =
    Encode.object
        [ ( "tag", Encode.string "protocolError" )
        , ( "error", Encode.string error )
        ]


encodeMaybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
encodeMaybe encode maybe =
    case maybe of
        Just value ->
            encode value

        Nothing ->
            Encode.null
