import Html exposing (..)
import State exposing (..)
import View exposing (view)
import Update exposing (update)

main = Html.beginnerProgram { model = model, view = view, update = update }
