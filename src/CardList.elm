module CardList exposing (Model, Msg, Dispatch(Rename), model, view, update)

import Card
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (map)
import Html.Events exposing (onInput, onClick)
import Debug


type Dispatch
    = Rename String


type alias ID =
    Int


type alias Model =
    { text : String, isEditingText : Bool, cards : List ( ID, Card.Model ), nextID : ID }


model : Model
model =
    { text = "Untitled", isEditingText = False, cards = [], nextID = 0 }


type Msg
    = StartEditingText
    | FinishEditingText
    | TextChanged String
    | Insert
    | Modify ID Card.Msg


update : Msg -> Model -> ( Model, Maybe Dispatch )
update msg model =
    case Debug.log "CardList Update " msg of
        StartEditingText ->
            ( { model | isEditingText = True }, Nothing )

        TextChanged newText ->
            ( { model | text = newText }, Nothing )

        FinishEditingText ->
            ( { model | isEditingText = False }, Just (Rename model.text) )

        Insert ->
            let
                newCard =
                    ( model.nextID, Card.model )

                newCards =
                    model.cards ++ [ newCard ]
            in
                ( { model | cards = newCards, nextID = model.nextID + 1 }, Nothing )

        Modify id cardMsg ->
            let
                updateCard ( cardID, cardModel ) =
                    if cardID == id then
                        ( cardID, Card.update cardMsg cardModel )
                    else
                        ( cardID, cardModel )
            in
                ( { model | cards = List.map updateCard model.cards }, Nothing )


view : Model -> Html Msg
view model =
    let
        cardView ( cardID, cardModel ) =
            map (Modify cardID) (Card.view cardModel)

        cardViews =
            div [ class "card-columns" ] (List.map cardView model.cards)
    in
        div [ class "col-md-6" ]
            [ nav [ class "navbar navbar-light bg-faded" ]
                [ if model.isEditingText then
                    input [ type' "text", class "form-control", onInput TextChanged, value model.text ] []
                  else
                    h1 [ class "navbar-brand" ] [ text model.text ]
                , div [ class "btn-group pull-right" ]
                    [ if model.isEditingText then
                        button [ class "btn btn-secondary", onClick FinishEditingText ]
                            [ i [ class "fa fa-check-square-o" ] [] ]
                      else
                        button [ class "btn btn-secondary", onClick StartEditingText ]
                            [ i [ class "fa fa-pencil-square-o" ] [] ]
                    , button [ class "btn btn-secondary", onClick Insert ] [ i [ class "fa fa-plus" ] [] ]
                    ]
                ]
            , br [] []
            , cardViews
            ]
