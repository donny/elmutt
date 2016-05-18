module Card exposing (Model, Msg, model, view, update)

import Html exposing (..)
import Html.Attributes exposing (class, value, type', disabled, readonly, attribute)
import Html.Events exposing (onInput, onClick)


type alias Model =
    { counter : Int, text : String, isEditingText : Bool }


model : Model
model =
    Model 0 "Text" False


type Msg
    = Increment
    | StartEditingText
    | FinishEditingText
    | TextChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + 1 }

        StartEditingText ->
            { model | isEditingText = True }

        TextChanged newText ->
            { model | text = newText }

        FinishEditingText ->
            { model | isEditingText = False }


view : Model -> Html Msg
view model =
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ div [ class "btn-group btn-group-sm" ]
                [ button [ class "btn btn-secondary", onClick Increment ]
                    [ text (toString model.counter ++ "\x00A0\x00A0"), i [ class "fa fa-thumbs-o-up" ] [] ]
                , if model.isEditingText then
                    button [ class "btn btn-secondary", onClick FinishEditingText ]
                        [ i [ class "fa fa-check-square-o" ] [] ]
                  else
                    button [ class "btn btn-secondary", onClick StartEditingText ]
                        [ i [ class "fa fa-pencil-square-o" ] [] ]
                , button [ class "btn btn-secondary dropdown-toggle", attribute "data-toggle" "dropdown" ]
                    [ i [ class "fa fa-trash-o" ] [] ]
                , div [ class "dropdown-menu" ]
                    [ a [ class "dropdown-item" ] [ text "Delete" ]
                    ]
                ]
            , br [] []
            , br [] []
            , if model.isEditingText then
                input [ type' "text", class "form-control", onInput TextChanged, value model.text ] []
              else
                h5 [ class "card-title" ] [ text model.text ]
            ]
        ]
