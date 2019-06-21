module Forecast exposing (BearingDirection(..), CurrentForecastSummary, DailyForecastDetail, DailyForecastSummary, ForecastSummary, HourlyForecastDetail, HourlyForecastSummary, Location, WeatherIcon(..), bearingDirectionDecoder, bearingDirectionToString, currentForecastSummaryDecoder, dailyForecastDetailDecoder, dailyForecastSummaryDecoder, forecastSummaryDecoder, hourlyForecastDetailDecoder, hourlyForecastSummaryDecoder, unixTimeDecoder, weatherIconAsClass, weatherIconDecoder)

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
    , currentForecastSummary : CurrentForecastSummary
    , hourlyForecastSummary : HourlyForecastSummary
    , dailyForecastSummary : DailyForecastSummary
    }


type alias CurrentForecastSummary =
    { time : Posix
    , summary : String
    , icon : WeatherIcon
    , precipProbability : Float
    , temperature : Float
    , windSpeed : Float
    }


type alias HourlyForecastSummary =
    { summary : String
    , icon : WeatherIcon
    , data : List HourlyForecastDetail
    }


type alias DailyForecastSummary =
    { summary : String
    , icon : WeatherIcon
    , data : List DailyForecastDetail
    }


type BearingDirection
    = North
    | Northeast
    | East
    | Southeast
    | South
    | Southwest
    | West
    | Northwest


bearingDirectionToString : BearingDirection -> String
bearingDirectionToString b =
    case b of
        North ->
            "North"

        Northeast ->
            "Northeast"

        East ->
            "East"

        Southeast ->
            "Southeast"

        South ->
            "South"

        Southwest ->
            "Southwest"

        West ->
            "West"

        Northwest ->
            "Northwest"


type WeatherIcon
    = ClearDay
    | ClearNight
    | Rain
    | Snow
    | Sleet
    | Wind
    | Fog
    | Cloudy
    | PartlyCloudyDay
    | PartlyCloudyNight


weatherIconAsClass : WeatherIcon -> String
weatherIconAsClass icon =
    "wi wi-"
        ++ (case icon of
                ClearDay ->
                    "day-sunny"

                ClearNight ->
                    "night-clear"

                Rain ->
                    "rain"

                Snow ->
                    "snow"

                Sleet ->
                    "sleet"

                Wind ->
                    "strong-wind"

                Fog ->
                    "fog"

                Cloudy ->
                    "cloudy"

                PartlyCloudyDay ->
                    "day-cloudy"

                PartlyCloudyNight ->
                    "night-alt-cloudy"
           )


type alias HourlyForecastDetail =
    { time : Posix
    , summary : String
    , icon : WeatherIcon
    , precipIntensity : Float
    , precipProbability : Float
    , precipType : String
    , temperature : Float
    , windSpeed : Float
    , windGust : Float
    , windBearing : BearingDirection
    }


type alias DailyForecastDetail =
    { time : Posix
    , summary : String
    , icon : WeatherIcon
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
        |> required "currently" currentForecastSummaryDecoder
        |> required "hourly" hourlyForecastSummaryDecoder
        |> required "daily" dailyForecastSummaryDecoder


currentForecastSummaryDecoder : Decoder CurrentForecastSummary
currentForecastSummaryDecoder =
    Decode.succeed CurrentForecastSummary
        |> required "time" unixTimeDecoder
        |> required "summary" string
        |> required "icon" weatherIconDecoder
        |> optional "precipProbability" float 0
        |> required "temperature" float
        |> required "windSpeed" float


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
                    List.range 24 68 |> bearingDirectionMap Northeast

                east =
                    List.range 69 113 |> bearingDirectionMap East

                southEast =
                    List.range 114 158 |> bearingDirectionMap Southeast

                south =
                    List.range 159 203 |> bearingDirectionMap South

                southWest =
                    List.range 204 248 |> bearingDirectionMap Southwest

                west =
                    List.range 249 293 |> bearingDirectionMap West

                northWest =
                    List.range 294 338 |> bearingDirectionMap Northwest

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
        |> required "icon" weatherIconDecoder
        |> required "data" (list dailyForecastDetailDecoder)


dailyForecastDetailDecoder : Decoder DailyForecastDetail
dailyForecastDetailDecoder =
    Decode.succeed DailyForecastDetail
        |> required "time" unixTimeDecoder
        |> required "summary" string
        |> required "icon" weatherIconDecoder
        |> required "sunriseTime" unixTimeDecoder
        |> required "sunsetTime" unixTimeDecoder
        |> required "precipIntensity" millimetersAsInchesDecoder
        |> optional "precipProbability" floatAsPercentDecoder 0
        |> optional "precipType" string "precipitation"
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
        |> required "icon" weatherIconDecoder
        |> required "precipIntensity" millimetersAsInchesDecoder
        |> required "precipProbability" floatAsPercentDecoder
        |> optional "precipType" string "precipitation"
        |> required "temperature" float
        |> required "windSpeed" float
        |> required "windGust" float
        |> required "windBearing" bearingDirectionDecoder


hourlyForecastSummaryDecoder : Decoder HourlyForecastSummary
hourlyForecastSummaryDecoder =
    Decode.succeed HourlyForecastSummary
        |> required "summary" string
        |> required "icon" weatherIconDecoder
        |> required "data" (list hourlyForecastDetailDecoder)


weatherIconDecoder : Decoder WeatherIcon
weatherIconDecoder =
    string
        |> andThen
            (\x ->
                Decode.succeed <|
                    case x of
                        "clear-day" ->
                            ClearDay

                        "clear-night" ->
                            ClearNight

                        "rain" ->
                            Rain

                        "snow" ->
                            Snow

                        "sleet" ->
                            Sleet

                        "wind" ->
                            Wind

                        "fog" ->
                            Fog

                        "cloudy" ->
                            Cloudy

                        "partly-cloudy-day" ->
                            PartlyCloudyDay

                        "partly-cloudy-night" ->
                            PartlyCloudyNight

                        _ ->
                            Cloudy
            )


floatAsPercentDecoder : Decoder Float
floatAsPercentDecoder =
    float
        |> andThen (\x -> x * 100 |> Decode.succeed)


millimetersAsInchesDecoder : Decoder Float
millimetersAsInchesDecoder =
    float
        |> andThen (\x -> x * 0.0393701 |> Decode.succeed)
