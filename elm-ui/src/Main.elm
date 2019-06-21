module Main exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Forecast exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Http exposing (..)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, getForecastSummary initialModel.location )



-- TYPES


initialModel : Model
initialModel =
    { location =
        { latitude = 37
        , longitude = -122
        }
    , forecastSummary = Nothing
    , errorData = []
    , demoModelField = 0
    }


type alias Model =
    { location : Location
    , forecastSummary : Maybe ForecastSummary
    , errorData : List Http.Error
    , demoModelField : Int
    }



-- MSG, UPDATE


type Msg
    = Increment
    | Decrement
    | GotForecastSummary (Result Http.Error ForecastSummary)


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
        Increment ->
            ( { model | demoModelField = model.demoModelField + 1 }, Cmd.none )

        Decrement ->
            ( { model | demoModelField = model.demoModelField - 1 }, Cmd.none )

        GotForecastSummary result ->
            case result of
                Err e ->
                    ( addErrorData e, Cmd.none )

                Ok summary ->
                    ( { model | forecastSummary = Just summary }, Cmd.none )


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.demoModelField) ]
        , button [ onClick Increment ] [ text "+" ]
        ]



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
