module CardList exposing (Model, Msg(InsertingCard, RenamingCard, UpvotingCard), Dispatch(RequestRename, RequestNewCard, RequestRenameCard, RequestUpvoteCard), model, view, update)

import Card
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (map)
import Html.Events exposing (onInput, onClick)


type Dispatch
    = RequestRename String
    | RequestNewCard
    | RequestRenameCard String String
    | RequestUpvoteCard String


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
    | AskNewCard
    | InsertingCard String String
    | RenamingCard String String
    | UpvotingCard String Int
    | Modify ID Card.Msg


update : Msg -> Model -> ( Model, Maybe Dispatch )
update msg model =
    case msg of
        StartEditingText ->
            ( { model | isEditingText = True }, Nothing )

        FinishEditingText ->
            ( { model | isEditingText = False }, Just (RequestRename model.text) )

        TextChanged newText ->
            ( { model | text = newText }, Nothing )

        AskNewCard ->
            ( model, Just (RequestNewCard) )

        InsertingCard identifier text ->
            let
                newModel =
                    Card.model

                newCard =
                    ( identifier, { newModel | text = text } )

                newCards =
                    model.cards ++ [ newCard ]
            in
                ( { model | cards = newCards }, Nothing )

        RenamingCard identifier text ->
            let
                updateCard ( cardID, cardModel ) =
                    if cardID == identifier then
                        ( cardID, { cardModel | text = text } )
                    else
                        ( cardID, cardModel )
            in
                ( { model | cards = List.map updateCard model.cards }, Nothing )

        UpvotingCard identifier counter ->
            let
                updateCard ( cardID, cardModel ) =
                    if cardID == identifier then
                        ( cardID, { cardModel | counter = counter } )
                    else
                        ( cardID, cardModel )
            in
                ( { model | cards = List.map updateCard model.cards }, Nothing )

        Modify id cardMsg ->
            let
                updateCard ( cardID, cardModel ) =
                    if cardID == id then
                        let
                            ( newCardModel, dispatch ) =
                                Card.update cardMsg cardModel
                        in
                            ( ( cardID, newCardModel ), dispatch )
                    else
                        ( ( cardID, cardModel ), Nothing )

                ( cards, dispatches ) =
                    List.unzip (List.map updateCard model.cards)

                newModel =
                    { model | cards = cards }
            in
                case (List.head (List.filterMap identity dispatches)) of
                    Nothing ->
                        ( newModel, Nothing )

                    Just (Card.RequestRename newName) ->
                        ( newModel, Just (RequestRenameCard id newName) )

                    Just (Card.RequestUpvote) ->
                        ( newModel, Just (RequestUpvoteCard id) )


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
                    , button [ class "btn btn-secondary", onClick AskNewCard ] [ i [ class "fa fa-plus" ] [] ]
                    ]
                ]
            , br [] []
            , cardViews
            ]
