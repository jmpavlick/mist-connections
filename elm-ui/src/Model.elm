module Model exposing (Model)

import Forecast exposing (ForecastSummary, Location)
import Http
import Time exposing (Zone)


type alias Model =
    { location : Location
    , forecastSummary : Maybe ForecastSummary
    , errorData : List Http.Error
    , zone : Zone
    }
