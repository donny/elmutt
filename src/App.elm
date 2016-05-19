module App exposing (main)

import CardList
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.App
import Html.Events exposing (onClick)
import Debug
import WebSocket
import Json.Decode exposing (..)


-- APP


main : Program Never
main =
    Html.App.program { init = init, update = update, subscriptions = subscriptions, view = view }



-- INIT


type alias ID =
    String


type alias Model =
    { lists : List ( ID, CardList.Model ), isProcessing : Bool, isConnected : Bool }


init : ( Model, Cmd Msg )
init =
    ( { lists = [], isProcessing = False, isConnected = False }, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Insert String String
    | Rename String String
    | Modify ID CardList.Msg
    | AddNewCard String String String
    | SendNetworkRequest NetworkRequest
    | NetworkResponseDidReceive String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "App Update " msg of
        NoOp ->
            ( model, Cmd.none )

        Insert identifier text ->
            let
                newModel =
                    CardList.model

                newList =
                    ( identifier, { newModel | text = text } )

                newLists =
                    model.lists ++ [ newList ]
            in
                ( { model | lists = newLists }, Cmd.none )

        AddNewCard identifier text listidentifier ->
            let
                updateCardList ( listID, listModel ) =
                    if listID == listidentifier then
                        ( listID, fst (CardList.update (CardList.Insert identifier text) listModel))
                    else
                        ( listID, listModel )
            in
                ( { model | lists = List.map updateCardList model.lists }, Cmd.none )

        Rename identifier text ->
            let
                updateCardList ( listID, listModel ) =
                    if listID == identifier then
                        ( listID, { listModel | text = text } )
                    else
                        ( listID, listModel )
            in
                ( { model | lists = List.map updateCardList model.lists }, Cmd.none )

        Modify id listMsg ->
            let
                updateCardList ( listID, listModel ) =
                    case ( listID == id, CardList.update listMsg listModel ) of
                        ( False, _ ) ->
                            ( ( listID, listModel ), Nothing )

                        ( True, ( newListModel, Nothing ) ) ->
                            ( ( listID, newListModel ), Nothing )

                        ( True, ( newListModel, dispatch ) ) ->
                            ( ( listID, newListModel ), dispatch )

                ( lists, dispatches ) =
                    List.unzip (List.map updateCardList model.lists)

                newModel =
                    { model | lists = lists }
            in
                case (List.head (List.filterMap identity dispatches)) of
                    Nothing ->
                        ( newModel, Cmd.none )

                    Just (CardList.Rename newName) ->
                        networkRequestHandler (REQ_RENAMELIST id newName) newModel

                    Just (CardList.NewCard) ->
                        networkRequestHandler (REQ_NEWCARD id) newModel

        SendNetworkRequest req ->
            networkRequestHandler req model

        NetworkResponseDidReceive str ->
            networkResponseHandler (decodeNetworkResponse str) model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://0.0.0.0:5000/receive" NetworkResponseDidReceive



-- VIEW


view : Model -> Html Msg
view model =
    let
        cardListView ( listID, listModel ) =
            Html.App.map (\model -> Modify listID model) (CardList.view listModel)

        cardListViews =
            div [ class "row" ] (List.map cardListView model.lists)
    in
        if model.isConnected then
            div []
                [ nav [ class "navbar navbar-light" ]
                    [ button [ class "btn btn-secondary", onClick NoOp ] [ text "Refresh" ]
                    , span [] [ text " " ]
                    , button [ class "btn btn-secondary", onClick (SendNetworkRequest REQ_NEWLIST) ] [ text "New List" ]
                    , div [ class "pull-right" ]
                        [ if model.isProcessing then
                            span [ class "label label-pill label-warning" ] [ text "Processing..." ]
                          else
                            span [ class "label label-pill label-default" ] [ text "Connected" ]
                        ]
                    ]
                , br [] []
                , cardListViews
                ]
        else
            div []
                [ nav [ class "navbar navbar-light" ]
                    [ button [ class "btn btn-secondary", onClick (SendNetworkRequest REQ_CONNECT) ] [ text "Connect" ]
                    ]
                ]



-- NETWORK


type NetworkRequest
    = REQ_CONNECT
    | REQ_REFRESH
    | REQ_NEWLIST
    | REQ_RENAMELIST String String
    | REQ_NEWCARD String


type NetworkResponse
    = RESP_ERROR
    | RESP_NEWLIST String String
    | RESP_RENAMELIST String String
    | RESP_NEWCARD String String String


networkRequestHandler : NetworkRequest -> Model -> ( Model, Cmd Msg )
networkRequestHandler req model =
    case Debug.log "DONNY RESP" req of
        REQ_CONNECT ->
            ( { model | isConnected = True }, WebSocket.send "ws://0.0.0.0:5000/submit" """{"REQ":"NOOP"}""" )

        REQ_REFRESH ->
            ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" """{"REQ":"NEWLIST"}""" )

        REQ_NEWLIST ->
            ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" """{"REQ":"NEWLIST"}""" )

        REQ_RENAMELIST identifier text ->
            let
                requestString =
                    "{\"REQ\":\"RENAMELIST\", \"IDENTIFIER\":\"" ++ identifier ++ "\", \"TEXT\":\"" ++ text ++ "\"}"
            in
                ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" requestString )

        REQ_NEWCARD identifier  ->
            let
                requestString =
                    "{\"REQ\":\"NEWCARD\", \"LISTIDENTIFIER\":\"" ++ identifier ++ "\"}"
            in
                ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" requestString )


networkResponseHandler : NetworkResponse -> Model -> ( Model, Cmd Msg )
networkResponseHandler resp model =
    case Debug.log "DONNY RESP" resp of
        RESP_NEWLIST identifier text ->
            update (Insert identifier text) { model | isProcessing = False }

        RESP_RENAMELIST identifier text ->
            update (Rename identifier text) { model | isProcessing = False }

        RESP_NEWCARD identifier text listidentifier ->
            update (AddNewCard identifier text listidentifier) { model | isProcessing = False }

        RESP_ERROR ->
            update NoOp model


decodeNetworkResponse : String -> NetworkResponse
decodeNetworkResponse message =
    case Debug.log "decodeNetworkResponse: " (decodeString ("RESP" := string) message) of
        Err error ->
            RESP_ERROR

        Ok value ->
            case value of
                "NEWLIST" ->
                    let
                        parsedCardList =
                            object2 (,) ("IDENTIFIER" := string) ("TEXT" := string)
                    in
                        case (decodeString parsedCardList message) of
                            Err error ->
                                RESP_ERROR

                            Ok ( identifier, text ) ->
                                RESP_NEWLIST identifier text

                "RENAMELIST" ->
                    let
                        parsedCardList =
                            object2 (,) ("IDENTIFIER" := string) ("TEXT" := string)
                    in
                        case (decodeString parsedCardList message) of
                            Err error ->
                                RESP_ERROR

                            Ok ( identifier, text ) ->
                                RESP_RENAMELIST identifier text

                "NEWCARD" ->
                    let
                        parsedCardList =
                            object3 (,,) ("IDENTIFIER" := string) ("TEXT" := string) ("LISTIDENTIFIER" := string)
                    in
                        case (decodeString parsedCardList message) of
                            Err error ->
                                RESP_ERROR

                            Ok ( identifier, text, listidentifier ) ->
                                RESP_NEWCARD identifier text listidentifier

                _ ->
                    RESP_ERROR
