module Main exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Forecast exposing (..)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = initialModel, update = update, view = view }



-- TYPES


initialModel : Model
initialModel =
    Model (Location 37 -122) 0


type alias Model =
    { location : Location
    , demoModelField : Int
    }



-- MSG, UPDATE


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            { model | demoModelField = model.demoModelField + 1 }

        Decrement ->
            { model | demoModelField = model.demoModelField - 1 }


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.demoModelField) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
