module App exposing (main)

import Html.App
import CardList


main : Program Never
main =
    Html.App.beginnerProgram { model = CardList.model, view = CardList.view, update = CardList.update }
