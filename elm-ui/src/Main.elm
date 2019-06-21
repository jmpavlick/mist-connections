module Main exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Forecast exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http exposing (..)
import HumanDates exposing (..)
import Round
import Task
import Time exposing (Posix, Zone)


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
    }


type alias Model =
    { location : Location
    , forecastSummary : Maybe ForecastSummary
    , errorData : List Http.Error
    , zone : Zone
    }



-- MSG, UPDATE


type Msg
    = GotForecastSummary (Result Http.Error ForecastSummary)
    | Here Zone


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



-- VIEW


weatherIconView : WeatherIcon -> Int -> Html Msg
weatherIconView icon padding =
    div [ style "display" "inline-block", "px-" ++ String.fromInt padding |> class ] [ i [ weatherIconAsClass icon |> class ] [] ]


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
                            , weatherIconView summary.currentForecastSummary.icon 0
                            ]
                        ]
                    ]
                , div [ class "row" ] [ div [ class "col" ] [ currentForecastSummaryView summary.currentForecastSummary ] ]
                , div [ class "row" ] [ div [ class "col" ] [ hourlyForecastSummaryView summary.hourlyForecastSummary model.zone ] ]
                , div [ class "row" ] [ div [ class "col" ] [ dailyForecastSummaryView summary.dailyForecastSummary model.zone ] ]
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


hourlyForecastSummaryView : HourlyForecastSummary -> Zone -> Html Msg
hourlyForecastSummaryView summary zone =
    let
        next5hours =
            List.take 5 summary.data
    in
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ h4 [] [ text "Hourly:" ]
                , h4 [] <|
                    List.map (\x -> weatherIconView x.icon 1) next5hours
                ]
            ]
        ]


dailyForecastSummaryView : DailyForecastSummary -> Zone -> Html Msg
dailyForecastSummaryView summary zone =
    let
        next5days =
            List.take 5 summary.data

        topRow =
            div [ class "row" ]
                [ div [ class "col" ]
                    [ h4 [] [ text "Daily:" ]
                    , h4 [] <|
                        List.map (\x -> weatherIconView x.icon 1) next5days
                    ]
                ]
    in
    div [ class "container-fluid" ] <|
        topRow
            :: List.map (\x -> dailyForecastDetailSummaryView x zone) next5days


hourlyForecastDetailSummaryView : HourlyForecastDetail -> Zone -> Html Msg
hourlyForecastDetailSummaryView detail zone =
    div [] []


dailyForecastDetailSummaryView : DailyForecastDetail -> Zone -> Html Msg
dailyForecastDetailSummaryView detail zone =
    div [ class "row" ]
        [ div [ class "col" ]
            [ h5 []
                [ HumanDates.prettyDay zone detail.time ++ ": " |> text
                , weatherIconView detail.icon 0
                ]
            , ul [ class "list-unstyled" ]
                [ li [] [ "High: " ++ Round.round 0 detail.temperatureHigh ++ "º F at " ++ prettyHourMinute zone detail.temperatureHighTime |> text ]
                , li [] [ "Low: " ++ Round.round 0 detail.temperatureLow ++ "º F at " ++ prettyHourMinute zone detail.temperatureLowTime |> text ]
                ]
            ]
        ]


hourlyForecastDetailDetailView : HourlyForecastDetail -> Zone -> Html Msg
hourlyForecastDetailDetailView detail zone =
    div [] []


dailyForecastDetailDetailView : DailyForecastDetail -> Zone -> Html Msg
dailyForecastDetailDetailView detail zone =
    div [] []



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
