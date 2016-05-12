module Card exposing (model, view, update)

import Html exposing (Html, div, input, button, h1, text)
import Html.Attributes exposing (value, type')
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
    div []
        [ div []
            [ if model.isEditingText then
                div []
                    [ input [ type' "text", onInput TextChanged, value model.text ] []
                    , button [ onClick FinishEditingText ] [ text "Save" ]
                    ]
              else
                h1 [ onClick StartEditingText ] [ text model.text ]
            ]
        , div [] [ h1 [] [ text (toString model.counter) ] ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick Decrement ] [ text "-" ]
        ]
