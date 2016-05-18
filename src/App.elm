module App exposing (main)

import CardList
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.App exposing (program, map)
import Html.Events exposing (onClick)
import Debug
import WebSocket


type alias ID =
    Int


type alias Model =
    { lists : List ( ID, CardList.Model ), nextID : ID }


init : ( Model, Cmd Msg )
init =
    ( { lists = [], nextID = 0 }, Cmd.none )


type Msg
    = Insert
    | Modify ID CardList.Msg
    | Refresh
    | NewMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Insert ->
            let
                newList =
                    ( model.nextID, CardList.model )

                newLists =
                    model.lists ++ [ newList ]
            in
                ( { model | lists = newLists, nextID = model.nextID + 1 }, Cmd.none )

        Modify id listMsg ->
            let
                updateList ( listID, listModel ) =
                    if listID == id then
                        ( listID, CardList.update listMsg listModel )
                    else
                        ( listID, listModel )
            in
                ( { model | lists = List.map updateList model.lists }, Cmd.none )

        Refresh ->
            ( model, WebSocket.send "wss://ideaboard-websocket.herokuapp.com/submit" "HELLO" )

        NewMessage str ->
            Debug.log str ( model, Cmd.none )


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
                    [ button [ class "btn btn-secondary", onClick Refresh ] [ text "Refresh" ]
                    , button [ class "btn btn-secondary", onClick Insert ] [ text "New List" ]
                    ]
                ]
            , br [] []
            , cardListViews
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "wss://ideaboard-websocket.herokuapp.com/receive" NewMessage


main : Program Never
main =
    program { init = init, update = update, subscriptions = subscriptions, view = view }
