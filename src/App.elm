module App exposing (main)

import CardList
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.App exposing (beginnerProgram, map)
import Html.Events exposing (onClick)


type alias ID =
    Int


type alias Model =
    { lists : List ( ID, CardList.Model ), nextID : ID }


model : Model
model =
    { lists = [], nextID = 0 }


type Msg
    = Insert
    | Modify ID CardList.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        Insert ->
            let
                newList =
                    ( model.nextID, CardList.model )

                newLists =
                    model.lists ++ [ newList ]
            in
                { model | lists = newLists, nextID = model.nextID + 1 }

        Modify id listMsg ->
            let
                updateList ( listID, listModel ) =
                    if listID == id then
                        ( listID, CardList.update listMsg listModel )
                    else
                        ( listID, listModel )
            in
                { model | lists = List.map updateList model.lists }


view : Model -> Html Msg
view model =
    let
        cardListView ( listID, listModel ) =
            map (\model -> Modify listID model) (CardList.view listModel)

        cardListViews =
            div [ class "row" ] (List.map cardListView model.lists)
    in
        div []
            [ nav [ class "navbar navbar-light" ]
                [ h1 [ class "navbar-brand" ] [ text "IdeaBoard" ]
                , div [ class "pull-right" ]
                    [ button [ class "btn btn-secondary", onClick Insert ] [ text "New List" ]
                    ]
                ]
            , br [] []
            , cardListViews
            ]


main : Program Never
main =
    beginnerProgram { model = model, view = view, update = update }
