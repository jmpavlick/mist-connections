module Main exposing (main, update)

import Browser
import Forecast exposing (..)
import Http exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Task
import Time exposing (Posix, Zone)
import View exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Flags =
    { latitude : Float
    , longitude : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model =
            Location flags.latitude flags.longitude
                |> initialModel
    in
    ( model
    , Cmd.batch
        [ Task.map Here Time.here |> Task.perform identity
        , getForecastSummary model.location
        ]
    )



-- TYPES


initialModel : Location -> Model
initialModel location =
    { location = location
    , forecastSummary = Nothing
    , errorData = []
    , zone = Time.utc
    , applicationView = CurrentForecast
    }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        addErrorData : Http.Error -> Model
        addErrorData err =
            let
                _ =
                    Debug.log "error" err
            in
            { model | errorData = err :: model.errorData }
    in
    case msg of
        GotForecastSummary result ->
            case result of
                Err e ->
                    ( addErrorData e, Cmd.none )

                Ok summary ->
                    ( { model | forecastSummary = Just summary }, Cmd.none )

        Here here ->
            ( { model | zone = here }, Cmd.none )

        ChangeApplicationView v ->
            ( { model | applicationView = v }, Cmd.none )



-- REST


baseUrl : String
baseUrl =
    "http://localhost:5000/"


getForecastSummary : Location -> Cmd Msg
getForecastSummary location =
    let
        url =
            baseUrl
                ++ "forecast?latitude="
                ++ String.fromFloat location.latitude
                ++ "&longitude="
                ++ String.fromFloat location.longitude
    in
    Http.get
        { url = url
        , expect = expectJson GotForecastSummary forecastSummaryDecoder
        }
