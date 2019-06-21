module View exposing (currentForecastSummaryView, dailyForecastDetailDetailView, dailyForecastDetailSummaryView, dailyForecastSummaryView, hourlyForecastDetailDetailView, hourlyForecastDetailSummaryView, hourlyForecastSummaryView, view, weatherIconView)

import Forecast exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import HumanDates exposing (..)
import Model exposing (..)
import Round
import Time exposing (Posix, Zone)



-- VIEW


weatherIconView : WeatherIcon -> Int -> Html msg
weatherIconView icon padding =
    div [ "d-inline-block px-" ++ String.fromInt padding |> class ] [ i [ weatherIconAsClass icon |> class ] [] ]


view : Model -> Html msg
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


currentForecastSummaryView : CurrentForecastSummary -> Html msg
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


hourlyForecastSummaryView : HourlyForecastSummary -> Zone -> Html msg
hourlyForecastSummaryView summary zone =
    let
        next5hours =
            List.take 5 summary.data

        topRow =
            div [ class "row" ]
                [ div [ class "col" ]
                    [ h4 [] [ text "Hourly:" ]
                    , h4 [] <|
                        List.map (\x -> weatherIconView x.icon 1) next5hours
                    ]
                ]
    in
    div [ class "container-fluid" ] <|
        topRow
            :: List.map (\x -> hourlyForecastDetailSummaryView x zone) next5hours


dailyForecastSummaryView : DailyForecastSummary -> Zone -> Html msg
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


hourlyForecastDetailSummaryView : HourlyForecastDetail -> Zone -> Html msg
hourlyForecastDetailSummaryView detail zone =
    div [ class "row" ]
        [ div [ class "col" ]
            [ h5 []
                [ HumanDates.prettyHourMinute zone detail.time
                    ++ ": "
                    ++ Round.round 0 detail.temperature
                    ++ "º F, "
                    |> text
                , weatherIconView detail.icon 0
                ]
            , p [] [ detail.summary ++ "." |> text ]
            ]
        ]


dailyForecastDetailSummaryView : DailyForecastDetail -> Zone -> Html msg
dailyForecastDetailSummaryView detail zone =
    div [ class "row" ]
        [ div [ class "col" ]
            [ h5 []
                [ HumanDates.prettyDay zone detail.time ++ ": " |> text
                , weatherIconView detail.icon 0
                ]
            , p [] [ text detail.summary ]
            , ul [ class "list-unstyled" ]
                [ li [] [ "High: " ++ Round.round 0 detail.temperatureHigh ++ "º F at " ++ prettyHourMinute zone detail.temperatureHighTime |> text ]
                , li [] [ "Low: " ++ Round.round 0 detail.temperatureLow ++ "º F at " ++ prettyHourMinute zone detail.temperatureLowTime |> text ]
                ]
            , ul [ class "list-unstyled" ]
                [ li []
                    [ case detail.precipProbability > 0 of
                        False ->
                            text "No precipitation today."

                        True ->
                            let
                                accumulationClause =
                                    case Round.round 3 detail.precipIntensity of
                                        "0.000" ->
                                            ""

                                        somethingElse ->
                                            ", with " ++ somethingElse ++ " inches of accumulation per hour"
                            in
                            Round.round 0 detail.precipProbability
                                ++ "% chance of "
                                ++ detail.precipType
                                ++ accumulationClause
                                ++ "."
                                |> text
                    ]
                ]
            , ul [ class "list-unstyled" ]
                [ li []
                    [ case detail.windSpeed > 0 of
                        False ->
                            text "No wind today."

                        True ->
                            let
                                gustsClause =
                                    case detail.windGust > 0 of
                                        False ->
                                            ""

                                        True ->
                                            ", with gusts of up to "
                                                ++ Round.round 0 detail.windGust
                                                ++ " MPH"
                            in
                            Round.round 0 detail.windSpeed
                                ++ " MPH wind, coming from the "
                                ++ String.toLower (bearingDirectionToString detail.windBearing)
                                ++ gustsClause
                                ++ "."
                                |> text
                    ]
                ]
            ]
        ]


hourlyForecastDetailDetailView : HourlyForecastDetail -> Zone -> Html msg
hourlyForecastDetailDetailView detail zone =
    div [] []


dailyForecastDetailDetailView : DailyForecastDetail -> Zone -> Html msg
dailyForecastDetailDetailView detail zone =
    div [] []
