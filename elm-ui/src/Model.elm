module Model exposing (ApplicationView(..), Model)

import Forecast exposing (ForecastSummary, Location)
import Http
import Time exposing (Zone)


type alias Model =
    { location : Location
    , forecastSummary : Maybe ForecastSummary
    , errorData : List Http.Error
    , zone : Zone
    , applicationView : ApplicationView
    }


type ApplicationView
    = CurrentForecast
    | HourlyForecasts
    | DailyForecasts
