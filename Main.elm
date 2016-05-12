module Main exposing (main)

import Html.App as Html
import Card exposing (model, view, update)


main =
    Html.beginnerProgram { model = model, view = view, update = update }
