module Msg exposing (Msg(..))

import Forecast exposing (ForecastSummary)
import Http
import Model exposing (..)
import Time exposing (Zone)


type Msg
    = GotForecastSummary (Result Http.Error ForecastSummary)
    | Here Zone
    | ChangeApplicationView ApplicationView
