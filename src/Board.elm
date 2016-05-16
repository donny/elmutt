module Board exposing (model, view, update)

import CardList
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.App exposing (map)


type alias Model =
    { topBoard : CardList.Model, bottomBoard : CardList.Model }


model : Model
model =
    { topBoard = CardList.model, bottomBoard = CardList.model }


type Msg
    = Top CardList.Msg
    | Bottom CardList.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Top m ->
            { model | topBoard = CardList.update m model.topBoard }

        Bottom m ->
            { model | bottomBoard = CardList.update m model.bottomBoard }


view : Model -> Html Msg
view model =
    div [ class "mdl-grid" ]
        [ div [ class "mdl-cell mdl-cell--6-col" ]
            [ map Top (CardList.view model.topBoard)
            ]
        , div [ class "mdl-cell mdl-cell--6-col" ]
            [ map Bottom (CardList.view model.bottomBoard)
            ]
        ]
