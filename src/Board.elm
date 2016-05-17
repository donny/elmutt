module Board exposing (model, view, update)

import CardList
import Html exposing (Html, div, text, h3, button)
import Html.Attributes exposing (class)
import Html.App exposing (map)
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
            div [ class "col-md-6" ]
                [ div [ class "panel panel-default" ]
                    [ div [ class "panel-heading" ]
                        [ h3 [ class "panel-title" ] [ text "hello" ]
                        ]
                    , map (\model -> Modify listID model) (CardList.view listModel)
                    ]
                ]

        cardListViews =
            List.map cardListView model.lists
    in
        div [ class "page-content" ]
            [ div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ div [ class "panel panel-default" ]
                        [ div [ class "panel-body" ]
                            [ div [ class "btn-group" ]
                                [ button [ class "btn btn-default", onClick Insert ]
                                    [ text "New List" ]
                                ]
                            ]
                        ]
                    ]
                ]
            , div [ class "row" ]
                cardListViews
            ]
