module Card exposing (Model, Msg, Dispatch(..), model, view, update)

import Html exposing (..)
import Html.Attributes exposing (class, value, type', disabled, readonly, attribute)
import Html.Events exposing (onInput, onClick)


type Dispatch
    = RequestRename String
    | RequestUpvote
    | RequestHide


type alias Model =
    { counter : Int, text : String, isHidden : Bool, isEditingText : Bool }


model : Model
model =
    Model 0 "Untitled" False False


type Msg
    = StartEditingText
    | FinishEditingText
    | TextChanged String
    | Upvote
    | Hide


update : Msg -> Model -> ( Model, Maybe Dispatch )
update msg model =
    case msg of
        StartEditingText ->
            ( { model | isEditingText = True }, Nothing )

        FinishEditingText ->
            ( { model | isEditingText = False }, Just (RequestRename model.text) )

        TextChanged newText ->
            ( { model | text = newText }, Nothing )

        Upvote ->
            ( model, Just (RequestUpvote) )

        Hide ->
            ( model, Just (RequestHide) )


view : Model -> Html Msg
view model =
    if model.isHidden then
        div [] []
    else
        div [ class "card" ]
            [ div [ class "card-block" ]
                [ if model.isEditingText then
                    input [ type' "text", class "form-control", onInput TextChanged, value model.text ] []
                  else
                    h5 [ class "card-title" ] [ text model.text ]
                , br [] []
                , div [ class "text-xs-center" ]
                    [ div [ class "btn-group dropup btn-group-sm" ]
                        [ button [ class "btn btn-secondary", onClick Upvote ]
                            [ text (toString model.counter ++ "  "), i [ class "fa fa-thumbs-o-up" ] [] ]
                        , if model.isEditingText then
                            button [ class "btn btn-secondary", onClick FinishEditingText ]
                                [ i [ class "fa fa-check-square-o" ] [] ]
                          else
                            button [ class "btn btn-secondary", onClick StartEditingText ]
                                [ i [ class "fa fa-pencil-square-o" ] [] ]
                        , button [ class "btn btn-secondary dropdown-toggle", attribute "data-toggle" "dropdown" ]
                            [ i [ class "fa fa-trash-o" ] [] ]
                        , div [ class "dropdown-menu" ]
                            [ a [ class "dropdown-item", onClick Hide ] [ text "Delete" ] ]
                        ]
                    ]
                ]
            ]
