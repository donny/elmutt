module CardList exposing (Model, Msg(Insert), Dispatch(Rename, NewCard), model, view, update)

import Card
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (map)
import Html.Events exposing (onInput, onClick)
import Debug


type Dispatch
    = Rename String
    | NewCard


type alias ID =
    String


type alias Model =
    { text : String, isEditingText : Bool, cards : List ( ID, Card.Model ) }


model : Model
model =
    { text = "Untitled", isEditingText = False, cards = [] }


type Msg
    = StartEditingText
    | FinishEditingText
    | TextChanged String
    | Insert String String
    | InsertNewCard
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

        InsertNewCard ->
            ( model, Just (NewCard))

        Insert identifier text ->
            let
                newModel =
                    Card.model

                newCard =
                    ( identifier, { newModel | text = text } )

                newCards =
                    model.cards ++ [ newCard ]
            in
                ( { model | cards = newCards }, Nothing )

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
                    , button [ class "btn btn-secondary", onClick InsertNewCard ] [ i [ class "fa fa-plus" ] [] ]
                    ]
                ]
            , br [] []
            , cardViews
            ]
