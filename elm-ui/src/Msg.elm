module Msg exposing (Msg(..))

import Forecast exposing (..)
import Http
import Model exposing (..)
import Time exposing (Zone)


type Msg
    = GotForecastSummary (Result Http.Error ForecastSummary)
    | Here Zone
    | ChangeApplicationView ApplicationView
    | SelectHourlyForecastDetail HourlyForecastDetail
