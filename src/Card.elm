module Card exposing (Model, Msg, Dispatch(Rename, UpVote), model, view, update)

import Html exposing (..)
import Html.Attributes exposing (class, value, type', disabled, readonly, attribute)
import Html.Events exposing (onInput, onClick)


type Dispatch
    = Rename String
    | UpVote


type alias Model =
    { counter : Int, text : String, isEditingText : Bool }


model : Model
model =
    Model 0 "Untitled" False


type Msg
    = Increment
    | StartEditingText
    | FinishEditingText
    | TextChanged String


update : Msg -> Model -> ( Model, Maybe Dispatch )
update msg model =
    case msg of
        Increment ->
            ( model, Just (UpVote) )

        StartEditingText ->
            ( { model | isEditingText = True }, Nothing )

        TextChanged newText ->
            ( { model | text = newText }, Nothing )

        FinishEditingText ->
            ( { model | isEditingText = False }, Just (Rename model.text) )


view : Model -> Html Msg
view model =
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ if model.isEditingText then
                input [ type' "text", class "form-control", onInput TextChanged, value model.text ] []
              else
                h5 [ class "card-title" ] [ text model.text ]
            , br [] []
            , div [ class "btn-group dropup btn-group-sm" ]
                [ button [ class "btn btn-secondary", onClick Increment ]
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
                    [ a [ class "dropdown-item" ] [ text "Delete" ]
                    ]
                ]
            ]
        ]
