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


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.batch
        [ Task.map Here Time.here |> Task.perform identity
        , getForecastSummary initialModel.location
        ]
    )



-- TYPES


initialModel : Model
initialModel =
    { location =
        { latitude = 42
        , longitude = -83
        }
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
