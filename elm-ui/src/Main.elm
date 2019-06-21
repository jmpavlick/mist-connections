module Main exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Forecast exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import Round


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
    }


type alias Model =
    { location : Location
    , forecastSummary : Maybe ForecastSummary
    , errorData : List Http.Error
    }



-- MSG, UPDATE


type Msg
    = GotForecastSummary (Result Http.Error ForecastSummary)


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



-- VIEW


weatherIconView : WeatherIcon -> Html Msg
weatherIconView icon =
    i [ weatherIconAsClass icon |> class ] []


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ] <|
        case model.forecastSummary of
            Nothing ->
                [ div [ class "row" ] [ div [ class "col" ] [ h1 [] [ text "Mist Opportunities" ] ] ]
                , div [ class "row" ] [ div [ class "col" ] [ h2 [] [ text "Loading..." ] ] ]
                ]

            Just summary ->
                [ div [ class "row" ] [ div [ class "col" ] [ h1 [] [ text "Mist Opportunities" ] ] ]
                , div [ class "row" ]
                    [ div [ class "col" ]
                        [ h2 []
                            [ text "Currently: "
                            , weatherIconView summary.currentForecastSummary.icon
                            ]
                        ]
                    ]
                , div [ class "row" ] [ div [ class "col" ] [ currentForecastSummaryView summary.currentForecastSummary ] ]
                ]


currentForecastSummaryView : CurrentForecastSummary -> Html Msg
currentForecastSummaryView summary =
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ h3 []
                    [ summary.summary
                        ++ ". It's "
                        ++ Round.round 0 summary.temperature
                        ++ "º F outside."
                        |> text
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ h4 []
                    [ let
                        precipProbabilityFragment =
                            case Round.round 0 summary.precipProbability of
                                "0" ->
                                    "Right now, "

                                p ->
                                    "Right now, there's a " ++ p ++ "% chance of precipitation, and "
                      in
                      precipProbabilityFragment
                        ++ "the wind is blowing at "
                        ++ Round.round 0 summary.windSpeed
                        ++ " MPH."
                        |> text
                    ]
                ]
            ]
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
