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
        let
            titleDiv =
                div [ class "row" ] [ div [ class "col" ] [ h1 [] [ text "Mist Opportunities" ] ] ]
        in
        case model.forecastSummary of
            Nothing ->
                [ titleDiv
                , div [ class "row" ] [ div [ class "col" ] [ h2 [] [ text "Loading..." ] ] ]
                ]

            Just summary ->
                [ titleDiv
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
                        ++ "º outside."
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
        next24hours =
            List.take 24 summary.data

        next8hours =
            List.take 8 next24hours

        topRow =
            div [ class "row" ]
                [ div [ class "col" ]
                    [ h4 [] [ text "Hourly:" ]
                    , h4 [] <|
                        List.map (\x -> weatherIconView x.icon 1) next8hours
                    ]
                ]
    in
    div [ class "container-fluid" ] <|
        topRow
            :: List.map (\x -> hourlyForecastDetailSummaryView x zone) next24hours


dailyForecastSummaryView : DailyForecastSummary -> Zone -> Html msg
dailyForecastSummaryView summary zone =
    let
        next8days =
            List.take 8 summary.data

        topRow =
            div [ class "row" ]
                [ div [ class "col" ]
                    [ h4 [] [ text "Daily:" ]
                    , h4 [] <|
                        List.map (\x -> weatherIconView x.icon 1) next8days
                    ]
                ]
    in
    div [ class "container-fluid" ] <|
        topRow
            :: List.map (\x -> dailyForecastDetailSummaryView x zone) next8days


hourlyForecastDetailSummaryView : HourlyForecastDetail -> Zone -> Html msg
hourlyForecastDetailSummaryView detail zone =
    div [ class "row" ]
        [ div [ class "col" ]
            [ h5 []
                [ HumanDates.prettyHourMinute zone detail.time
                    ++ ": "
                    ++ Round.round 0 detail.temperature
                    ++ "º "
                    |> text
                , weatherIconView detail.icon 0
                ]
            , div
                []
                [ hourlyForecastDetailDetailView detail zone ]
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
            , div [] [ dailyForecastDetailDetailView detail zone ]
            ]
        ]


hourlyForecastDetailDetailView : HourlyForecastDetail -> Zone -> Html msg
hourlyForecastDetailDetailView detail zone =
    div []
        [ p [] [ detail.summary ++ "." |> text ]
        , ul [ class "list-unstyled" ]
            [ li []
                [ precipitationText detail.precipProbability detail.precipIntensity detail.precipType |> text
                ]
            ]
        , ul [ class "list-unstyled" ]
            [ li []
                [ windSpeedText detail.windSpeed detail.windGust detail.windBearing |> text
                ]
            ]
        ]


dailyForecastDetailDetailView : DailyForecastDetail -> Zone -> Html msg
dailyForecastDetailDetailView detail zone =
    div []
        [ p [] [ text detail.summary ]
        , ul [ class "list-unstyled" ]
            [ li [] [ "High: " ++ Round.round 0 detail.temperatureHigh ++ "º at " ++ prettyHourMinute zone detail.temperatureHighTime |> text ]
            , li [] [ "Low: " ++ Round.round 0 detail.temperatureLow ++ "º at " ++ prettyHourMinute zone detail.temperatureLowTime |> text ]
            ]
        , ul [ class "list-unstyled" ]
            [ li []
                [ precipitationText detail.precipProbability detail.precipIntensity detail.precipType |> text
                ]
            ]
        , ul [ class "list-unstyled" ]
            [ li []
                [ windSpeedText detail.windSpeed detail.windGust detail.windBearing |> text
                ]
            ]
        ]


windSpeedText : Float -> Float -> BearingDirection -> String
windSpeedText windSpeed windGust bearingDirection =
    case windSpeed > 0 of
        False ->
            "No wind today."

        True ->
            let
                gustsClause =
                    case windGust > 0 of
                        False ->
                            ""

                        True ->
                            ", with gusts of up to "
                                ++ Round.round 0 windGust
                                ++ " MPH"
            in
            Round.round 0 windSpeed
                ++ " MPH wind, coming from the "
                ++ String.toLower (bearingDirectionToString bearingDirection)
                ++ gustsClause
                ++ "."


precipitationText : Float -> Float -> String -> String
precipitationText precipProbability precipIntensity precipType =
    case precipProbability > 0 of
        False ->
            "No precipitation today."

        True ->
            let
                accumulationClause =
                    case Round.round 3 precipIntensity of
                        "0.000" ->
                            ""

                        somethingElse ->
                            ", with " ++ somethingElse ++ " inches of accumulation per hour"
            in
            Round.round 0 precipProbability
                ++ "% chance of "
                ++ precipType
                ++ accumulationClause
                ++ "."
