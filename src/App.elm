module App exposing (main)

import Html.App
import Board


main : Program Never
main =
    Html.App.beginnerProgram { model = Board.model, view = Board.view, update = Board.update }
