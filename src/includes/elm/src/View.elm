module View exposing (view)

import Html exposing (..)
import State exposing (..)

view : Model -> Html Model
view model =
    div []
        [ div [] [ text model.message ]
        ]
