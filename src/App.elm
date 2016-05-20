module App exposing (main)

import Card
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
    | Refresh (List ( String, String, List ( String, String, Int ) ))
    | Insert String String
    | Rename String String
    | Modify ID CardList.Msg
    | AddNewCard String String String
    | RenameCard String String String
    | UpVoteCard String String Int
    | SendNetworkRequest NetworkRequest
    | NetworkResponseDidReceive String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "App Update " msg of
        NoOp ->
            ( model, Cmd.none )

        Refresh newlist ->
            let
                updateCard ( cardID, cardText, cardCounter ) =
                    let
                        newCard =
                            Card.model
                    in
                        ( cardID, { newCard | text = cardText, counter = cardCounter } )

                updateCardList ( listID, listText, listCards ) =
                    let
                        newCardList =
                            CardList.model
                    in
                        ( listID, { newCardList | text = listText, cards = (List.map updateCard listCards) } )

                lists =
                    List.map updateCardList newlist
            in
                ( { model | lists = lists }, Cmd.none )

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

        AddNewCard listidentifier identifier text ->
            let
                updateCardList ( listID, listModel ) =
                    if listID == listidentifier then
                        ( listID, fst (CardList.update (CardList.InsertingCard identifier text) listModel) )
                    else
                        ( listID, listModel )
            in
                ( { model | lists = List.map updateCardList model.lists }, Cmd.none )

        RenameCard listidentifier identifier text ->
            let
                updateCardList ( listID, listModel ) =
                    if listID == listidentifier then
                        ( listID, fst (CardList.update (CardList.RenamingCard identifier text) listModel) )
                    else
                        ( listID, listModel )
            in
                ( { model | lists = List.map updateCardList model.lists }, Cmd.none )

        UpVoteCard listidentifier identifier counter ->
            let
                updateCardList ( listID, listModel ) =
                    if listID == listidentifier then
                        ( listID, fst (CardList.update (CardList.UpvotingCard identifier counter) listModel) )
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
                    if listID == id then
                        let
                            ( newListModel, dispatch ) =
                                CardList.update listMsg listModel
                        in
                            ( ( listID, newListModel ), dispatch )
                    else
                        ( ( listID, listModel ), Nothing )

                ( lists, dispatches ) =
                    List.unzip (List.map updateCardList model.lists)

                newModel =
                    { model | lists = lists }
            in
                case (List.head (List.filterMap identity dispatches)) of
                    Nothing ->
                        ( newModel, Cmd.none )

                    Just (CardList.RequestRename newName) ->
                        networkRequestHandler (REQ_RENAMELIST id newName) newModel

                    Just (CardList.RequestNewCard) ->
                        networkRequestHandler (REQ_NEWCARD id) newModel

                    Just (CardList.RequestRenameCard cardIdentifier newName) ->
                        networkRequestHandler (REQ_RENAMECARD id cardIdentifier newName) newModel

                    Just (CardList.RequestUpvoteCard cardIdentifier) ->
                        networkRequestHandler (REQ_UPVOTECARD id cardIdentifier) newModel

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
                    [ button [ class "btn btn-secondary", onClick (SendNetworkRequest REQ_REFRESH) ] [ text "Refresh" ]
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
    | REQ_RENAMECARD String String String
    | REQ_UPVOTECARD String String


type NetworkResponse
    = RESP_ERROR
    | RESP_REFRESH (List ( String, String, List ( String, String, Int ) ))
    | RESP_NEWLIST String String
    | RESP_RENAMELIST String String
    | RESP_NEWCARD String String String
    | RESP_RENAMECARD String String String
    | RESP_UPVOTECARD String String Int


networkRequestHandler : NetworkRequest -> Model -> ( Model, Cmd Msg )
networkRequestHandler req model =
    case Debug.log "DONNY RESP" req of
        REQ_CONNECT ->
            ( { model | isConnected = True }, WebSocket.send "ws://0.0.0.0:5000/submit" """{"REQ":"NOOP"}""" )

        REQ_REFRESH ->
            ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" """{"REQ":"REFRESH"}""" )

        REQ_NEWLIST ->
            ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" """{"REQ":"NEWLIST"}""" )

        REQ_RENAMELIST identifier text ->
            let
                requestString =
                    "{\"REQ\":\"RENAMELIST\", \"IDENTIFIER\":\"" ++ identifier ++ "\", \"TEXT\":\"" ++ text ++ "\"}"
            in
                ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" requestString )

        REQ_NEWCARD identifier ->
            let
                requestString =
                    "{\"REQ\":\"NEWCARD\", \"LISTIDENTIFIER\":\"" ++ identifier ++ "\"}"
            in
                ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" requestString )

        REQ_RENAMECARD listidentifier cardIdentifier text ->
            let
                requestString =
                    "{\"REQ\":\"RENAMECARD\", \"IDENTIFIER\":\"" ++ cardIdentifier ++ "\", \"LISTIDENTIFIER\":\"" ++ listidentifier ++ "\", \"TEXT\":\"" ++ text ++ "\"}"
            in
                ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" requestString )

        REQ_UPVOTECARD listidentifier cardIdentifier ->
            let
                requestString =
                    "{\"REQ\":\"UPVOTECARD\", \"IDENTIFIER\":\"" ++ cardIdentifier ++ "\", \"LISTIDENTIFIER\":\"" ++ listidentifier ++ "\"}"
            in
                ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" requestString )


networkResponseHandler : NetworkResponse -> Model -> ( Model, Cmd Msg )
networkResponseHandler resp model =
    case Debug.log "DONNY RESP" resp of
        RESP_ERROR ->
            update NoOp model

        RESP_REFRESH newList ->
            update (Refresh newList) { model | isProcessing = False }

        RESP_NEWLIST identifier text ->
            update (Insert identifier text) { model | isProcessing = False }

        RESP_RENAMELIST identifier text ->
            update (Rename identifier text) { model | isProcessing = False }

        RESP_NEWCARD listidentifier identifier text ->
            update (AddNewCard listidentifier identifier text) { model | isProcessing = False }

        RESP_RENAMECARD listidentifier identifier text ->
            update (RenameCard listidentifier identifier text) { model | isProcessing = False }

        RESP_UPVOTECARD listidentifier identifier counter ->
            update (UpVoteCard listidentifier identifier counter) { model | isProcessing = False }


decodeNetworkResponse : String -> NetworkResponse
decodeNetworkResponse message =
    case Debug.log "decodeNetworkResponse: " (decodeString ("RESP" := string) (Debug.log "rawMessage: " message)) of
        Err error ->
            RESP_ERROR

        Ok value ->
            case value of
                "RESP_REFRESH" ->
                    let
                        parsedCard =
                            object3 (,,) ("identifier" := string) ("text" := string) ("counter" := int)

                        parsedCardList =
                            object3 (,,) ("identifier" := string) ("text" := string) ("cards" := (list parsedCard))
                    in
                        case (decodeString ("DATA" := (list parsedCardList)) message) of
                            Err error ->
                                RESP_ERROR

                            Ok newList ->
                                RESP_REFRESH (Debug.log "--->" newList)

                "RESP_NEWLIST" ->
                    let
                        parsedCardList =
                            object2 (,) ("IDENTIFIER" := string) ("TEXT" := string)
                    in
                        case (decodeString parsedCardList message) of
                            Err error ->
                                RESP_ERROR

                            Ok ( identifier, text ) ->
                                RESP_NEWLIST identifier text

                "RESP_RENAMELIST" ->
                    let
                        parsedCardList =
                            object2 (,) ("IDENTIFIER" := string) ("TEXT" := string)
                    in
                        case (decodeString parsedCardList message) of
                            Err error ->
                                RESP_ERROR

                            Ok ( identifier, text ) ->
                                RESP_RENAMELIST identifier text

                "RESP_NEWCARD" ->
                    let
                        parsedCard =
                            object3 (,,) ("LISTIDENTIFIER" := string) ("IDENTIFIER" := string) ("TEXT" := string)
                    in
                        case (decodeString parsedCard message) of
                            Err error ->
                                RESP_ERROR

                            Ok ( listidentifier, identifier, text ) ->
                                RESP_NEWCARD listidentifier identifier text

                "RESP_RENAMECARD" ->
                    let
                        parsedCard =
                            object3 (,,) ("LISTIDENTIFIER" := string) ("IDENTIFIER" := string) ("TEXT" := string)
                    in
                        case (decodeString parsedCard message) of
                            Err error ->
                                RESP_ERROR

                            Ok ( listidentifier, identifier, text ) ->
                                RESP_RENAMECARD listidentifier identifier text

                "RESP_UPVOTECARD" ->
                    let
                        parsedCard =
                            object3 (,,) ("LISTIDENTIFIER" := string) ("IDENTIFIER" := string) ("COUNTER" := int)
                    in
                        case (decodeString parsedCard message) of
                            Err error ->
                                RESP_ERROR

                            Ok ( listidentifier, identifier, counter ) ->
                                RESP_UPVOTECARD listidentifier identifier counter

                _ ->
                    RESP_ERROR
