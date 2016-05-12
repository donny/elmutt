module Main exposing (main)

import Html.App
import CardList


main =
    Html.App.beginnerProgram { model = CardList.model, view = CardList.view, update = CardList.update }
