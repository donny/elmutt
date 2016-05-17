module Card exposing (Model, Msg, model, view, update)

import Html exposing (Html, div, input, button, h3, h6, text, p, i, h4, span, ul, li, a)
import Html.Attributes exposing (class, value, type', disabled, readonly, attribute)
import Html.Events exposing (onInput, onClick)


type alias Model =
    { counter : Int, text : String, isEditingText : Bool }


model : Model
model =
    Model 0 "Text" False


type Msg
    = Increment
    | Decrement
    | StartEditingText
    | FinishEditingText
    | TextChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + 1 }

        Decrement ->
            { model | counter = model.counter - 1 }

        StartEditingText ->
            { model | isEditingText = True }

        TextChanged newText ->
            { model | text = newText }

        FinishEditingText ->
            { model | isEditingText = False }


view : Model -> Html Msg
view model =
    div [ class "mdl-cell mdl-cell--4-col mdl-card mdl-shadow--2dp" ]
        [ div [ class "mdl-card__title mdl-card--expand" ]
            [ div [ class "input-group" ]
                [ div [ class "input-group-btn" ]
                    [ button [ class "btn btn-default dropdown-toggle", attribute "data-toggle" "dropdown" ] [ text (toString model.counter ++ "  "), span [ class "caret" ] [] ]
                    , ul [ class "dropdown-menu" ]
                        [ li []
                            [ a [ onClick Increment ] [ text " Thumbs Up" ]
                            ]
                        , li [ class "divider" ] []
                        , li []
                            [ a [ onClick Decrement ] [ text " Thumbs Down" ]
                            ]
                        ]
                    ]
                , if model.isEditingText then
                    input [ type' "text", class "form-control", onInput TextChanged, value model.text ] []
                  else
                    input [ type' "text", class "form-control", onClick StartEditingText, value model.text, disabled True ] []
                , div [ class "input-group-btn" ]
                    [ if model.isEditingText then
                        button [ class "btn btn-default", onClick FinishEditingText ]
                            [ i [ class "fa fa-check-square-o" ] [] ]
                      else
                        button [ class "btn btn-default", onClick StartEditingText ]
                            [ i [ class "fa fa-pencil-square-o" ] [] ]
                    , button [ class "btn btn-default dropdown-toggle", attribute "data-toggle" "dropdown" ] [ i [ class "fa fa-trash-o" ] [] ]
                    , ul [ class "dropdown-menu" ]
                        [ li [] [ a [] [ text "Confirm Delete" ] ]
                        ]
                    ]
                ]
            ]
        ]
