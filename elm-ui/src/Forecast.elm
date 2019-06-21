module Forecast exposing (BearingDirection(..), DailyForecastDetail, DailyForecastSummary, ForecastSummary, HourlyForecastDetail, HourlyForecastSummary, Location, bearingDirectionDecoder, dailyForecastDetailDecoder, dailyForecastSummaryDecoder, forecastSummaryDecoder, hourlyForecastDetailDecoder, hourlyForecastSummaryDecoder, unixTimeDecoder)

import Json.Decode as Decode exposing (Decoder, andThen, float, int, list, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Time exposing (Posix)


type alias Location =
    { latitude : Float
    , longitude : Float
    }


type alias ForecastSummary =
    { latitude : Float
    , longitude : Float
    , timezone : String
    , hourlyForecastSummary : HourlyForecastSummary
    , dailyForecastSummary : DailyForecastSummary
    }


type alias HourlyForecastSummary =
    { summary : String
    , icon : String
    , data : List HourlyForecastDetail
    }


type alias DailyForecastSummary =
    { summary : String
    , icon : String
    , data : List DailyForecastDetail
    }


type BearingDirection
    = North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest


type alias HourlyForecastDetail =
    { time : Posix
    , summary : String
    , icon : String
    , precipIntensity : Float
    , precipProbability : Float
    , temperature : Float
    , windSpeed : Float
    , windGust : Float
    , windBearing : BearingDirection
    }


type alias DailyForecastDetail =
    { time : Posix
    , summary : String
    , icon : String
    , sunriseTime : Posix
    , sunsetTime : Posix
    , precipIntensity : Float
    , precipProbability : Float
    , precipType : String
    , temperatureHigh : Float
    , temperatureHighTime : Posix
    , temperatureLow : Float
    , temperatureLowTime : Posix
    , windSpeed : Float
    , windGust : Float
    , windBearing : BearingDirection
    }



-- DECODERS


forecastSummaryDecoder : Decoder ForecastSummary
forecastSummaryDecoder =
    Decode.succeed ForecastSummary
        |> required "latitude" float
        |> required "longitude" float
        |> required "timezone" string
        |> required "hourly" hourlyForecastSummaryDecoder
        |> required "daily" dailyForecastSummaryDecoder


unixTimeDecoder : Decoder Posix
unixTimeDecoder =
    int
        |> andThen
            (\x ->
                x * 1000 |> Time.millisToPosix |> Decode.succeed
            )


bearingDirectionDecoder : Decoder BearingDirection
bearingDirectionDecoder =
    let
        bearingDirectionFromInt : Int -> Maybe BearingDirection
        bearingDirectionFromInt degrees =
            let
                bearingDirectionMap : BearingDirection -> List Int -> List ( BearingDirection, Int )
                bearingDirectionMap bearingDirection range =
                    List.map (\x -> ( bearingDirection, x )) range

                north =
                    (List.range 339 359 ++ List.range 0 23) |> bearingDirectionMap North

                northEast =
                    List.range 24 68 |> bearingDirectionMap NorthEast

                east =
                    List.range 69 113 |> bearingDirectionMap East

                southEast =
                    List.range 114 158 |> bearingDirectionMap SouthEast

                south =
                    List.range 159 203 |> bearingDirectionMap South

                southWest =
                    List.range 204 248 |> bearingDirectionMap SouthWest

                west =
                    List.range 249 293 |> bearingDirectionMap West

                northWest =
                    List.range 294 338 |> bearingDirectionMap NorthWest

                allDirections =
                    north
                        ++ northEast
                        ++ east
                        ++ southEast
                        ++ south
                        ++ southWest
                        ++ west
                        ++ northWest
            in
            List.filter (\( dir, deg ) -> deg == degrees) allDirections
                |> List.map (\( dir, deg ) -> dir)
                |> List.head
    in
    int
        |> andThen
            (\x ->
                case bearingDirectionFromInt x of
                    Just bearingDirection ->
                        Decode.succeed bearingDirection

                    Nothing ->
                        "Degrees for bearing direction out of range (must be between 0 and 359): "
                            ++ String.fromInt x
                            |> Decode.fail
            )


dailyForecastSummaryDecoder : Decoder DailyForecastSummary
dailyForecastSummaryDecoder =
    Decode.succeed DailyForecastSummary
        |> required "summary" string
        |> required "icon" string
        |> required "data" (list dailyForecastDetailDecoder)


dailyForecastDetailDecoder : Decoder DailyForecastDetail
dailyForecastDetailDecoder =
    Decode.succeed DailyForecastDetail
        |> required "time" unixTimeDecoder
        |> required "summary" string
        |> required "icon" string
        |> required "sunriseTime" unixTimeDecoder
        |> required "sunsetTime" unixTimeDecoder
        |> required "precipIntensity" float
        |> optional "precipProbability" float 0
        |> optional "precipType" string ""
        |> required "temperatureHigh" float
        |> required "temperatureHighTime" unixTimeDecoder
        |> required "temperatureLow" float
        |> required "temperatureLowTime" unixTimeDecoder
        |> required "windSpeed" float
        |> required "windGust" float
        |> required "windBearing" bearingDirectionDecoder


hourlyForecastDetailDecoder : Decoder HourlyForecastDetail
hourlyForecastDetailDecoder =
    Decode.succeed HourlyForecastDetail
        |> required "time" unixTimeDecoder
        |> required "summary" string
        |> required "icon" string
        |> required "precipIntensity" float
        |> required "precipProbability" float
        |> required "temperature" float
        |> required "windSpeed" float
        |> required "windGust" float
        |> required "windBearing" bearingDirectionDecoder


hourlyForecastSummaryDecoder : Decoder HourlyForecastSummary
hourlyForecastSummaryDecoder =
    Decode.succeed HourlyForecastSummary
        |> required "summary" string
        |> required "icon" string
        |> required "data" (list hourlyForecastDetailDecoder)
