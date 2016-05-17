module CardList exposing (Model, Msg, model, view, update)

import Card
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (map)
import Html.Events exposing (onClick)


type alias ID =
    Int


type alias Model =
    { cards : List ( ID, Card.Model ), nextID : ID }


model : Model
model =
    { cards = [], nextID = 0 }


type Msg
    = Insert
    | Modify ID Card.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Insert ->
            let
                newCard =
                    ( model.nextID, Card.model )

                newCards =
                    model.cards ++ [ newCard ]
            in
                { model | cards = newCards, nextID = model.nextID + 1 }

        Modify id cardMsg ->
            let
                updateCard ( cardID, cardModel ) =
                    if cardID == id then
                        ( cardID, Card.update cardMsg cardModel )
                    else
                        ( cardID, cardModel )
            in
                { model | cards = List.map updateCard model.cards }


view : Model -> Html Msg
view model =
    let
        cardView ( cardID, cardModel ) =
            li [ class "list-group-item" ] [ map (Modify cardID) (Card.view cardModel) ]

        cardViews =
            List.map cardView model.cards
    in
        div [ class "col-md-6" ]
            [ div [ class "panel panel-default" ]
                [ div [ class "panel-heading" ]
                    [ div [ class "input-group" ]
                        [ input [ type' "text", class "form-control", value "List", disabled True ] []
                        , div [ class "input-group-btn" ]
                            [ button [ class "btn btn-default", onClick Insert ]
                                [ i [ class "fa fa-plus" ] [] ]
                            ]
                        ]
                    ]
                , ul [ class "list-group" ] cardViews
                ]
            ]
