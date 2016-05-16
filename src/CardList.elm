module CardList exposing (model, view, update)

import Card
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Html.App exposing (map)


type alias Model =
    { topCard : Card.Model, bottomCard : Card.Model }


model : Model
model =
    { topCard = Card.model, bottomCard = Card.model }


type Msg
    = Top Card.Msg
    | Bottom Card.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Top m ->
            { model | topCard = Card.update m model.topCard }

        Bottom m ->
            { model | bottomCard = Card.update m model.bottomCard }


view : Model -> Html Msg
view model =
    div [ class "mdl-grid" ]
        [ div [ class "mdl-cell mdl-cell--4-col" ]
            [ map Top (Card.view model.topCard)
            ]
        , div [ class "mdl-cell mdl-cell--4-col" ]
            [ map Bottom (Card.view model.bottomCard)
            ]
        ]
