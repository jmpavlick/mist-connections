module View exposing (view)

import Forecast exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import HumanDates exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Round
import Time exposing (Posix, Zone)



-- VIEW


weatherIconView : WeatherIcon -> Int -> Html Msg
weatherIconView icon padding =
    div [ "d-inline-block px-" ++ String.fromInt padding |> class ] [ i [ weatherIconAsClass icon |> class ] [] ]


view : Model -> Html Msg
view model =
    div [ class "container" ] <|
        let
            titleDiv =
                div [ class "row", ChangeApplicationView CurrentForecast |> onClick ] [ div [ class "col" ] [ h1 [] [ text "Mist Opportunities" ] ] ]
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
                        [ case model.applicationView of
                            CurrentForecast ->
                                currentForecastView summary model.zone

                            HourlyForecasts ->
                                hourlyForecastView summary.hourlyForecastSummary model.zone

                            DailyForecasts ->
                                dailyForecastView summary.dailyForecastSummary model.zone
                        ]
                    ]
                ]


currentForecastView : ForecastSummary -> Zone -> Html Msg
currentForecastView forecastSummary zone =
    let
        current =
            forecastSummary.currentForecastSummary

        getIcons forecasts =
            List.take 8 forecasts.data
                |> List.map (\x -> x.icon)
                |> List.map (\x -> weatherIconView x 1)
                |> h4 []

        dailyIcons =
            getIcons forecastSummary.dailyForecastSummary

        hourlyIcons =
            getIcons forecastSummary.hourlyForecastSummary

        iconCard : String -> Html Msg -> ApplicationView -> Html Msg
        iconCard label icons appView =
            div [ class "row" ]
                [ div [ class "col" ]
                    [ div
                        [ class "card my-2"
                        , style "width" "24rem"
                        , style "cursor" "pointer"
                        , ChangeApplicationView appView |> onClick
                        ]
                        [ div [ class "card-body" ]
                            [ h4 [] [ label ++ ":" |> text ]
                            , icons
                            ]
                        ]
                    ]
                ]
    in
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "col" ]
                [ h2 []
                    [ text "Currently: "
                    , weatherIconView current.icon 0
                    ]
                ]
            ]
        , div [ class "row" ]
            [ div [ class "col" ]
                [ h3 []
                    [ current.summary
                        ++ ". It's "
                        ++ Round.round 0 current.temperature
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
                            case Round.round 0 current.precipProbability of
                                "0" ->
                                    "Right now, "

                                p ->
                                    "Right now, there's a " ++ p ++ "% chance of precipitation, and "
                      in
                      precipProbabilityFragment
                        ++ "the wind is blowing at "
                        ++ Round.round 0 current.windSpeed
                        ++ " MPH."
                        |> text
                    ]
                ]
            ]
        , iconCard "Hourly" hourlyIcons HourlyForecasts
        , iconCard "Daily" dailyIcons DailyForecasts
        ]


hourlyForecastView : HourlyForecastSummary -> Zone -> Html Msg
hourlyForecastView summary zone =
    let
        next24hours =
            List.take 24 summary.data

        topRow =
            div [ class "row" ]
                [ div [ class "col" ]
                    [ h4 [] [ text "Hourly" ]
                    ]
                ]
    in
    div [ class "container-fluid" ] <|
        topRow
            :: List.map (\x -> hourlyForecastDetailSummaryView x zone) next24hours


dailyForecastView : DailyForecastSummary -> Zone -> Html Msg
dailyForecastView summary zone =
    let
        next8days =
            List.take 8 summary.data

        topRow =
            div [ class "row" ]
                [ div [ class "col" ]
                    [ h4 [] [ text "Daily" ]
                    ]
                ]
    in
    div [ class "container-fluid" ] <|
        topRow
            :: List.map (\x -> dailyForecastDetailSummaryView x zone) next8days


hourlyForecastDetailSummaryView : HourlyForecastDetail -> Zone -> Html Msg
hourlyForecastDetailSummaryView detail zone =
    div [ class "row" ]
        [ div
            [ class "col"
            , SelectHourlyForecastDetail detail |> onClick
            , style "cursor" "pointer"
            ]
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


dailyForecastDetailSummaryView : DailyForecastDetail -> Zone -> Html Msg
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


hourlyForecastDetailDetailView : HourlyForecastDetail -> Zone -> Html Msg
hourlyForecastDetailDetailView detail zone =
    div
        [ class <|
            case detail.detailSelected of
                True ->
                    ""

                False ->
                    "d-none"
        ]
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


dailyForecastDetailDetailView : DailyForecastDetail -> Zone -> Html Msg
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
