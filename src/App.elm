module App exposing (main)

import Card
import CardList
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.App
import Html.Events exposing (onClick)
import WebSocket
import Json.Decode exposing (..)


-- APP


main : Program (String)
main =
    Html.App.programWithFlags { init = init, update = update, subscriptions = subscriptions, view = view }



-- INIT


type alias ID =
    String


type alias Model =
    { backend : String, lists : List ( ID, CardList.Model ), isProcessing : Bool, isConnected : Bool }


init : String -> ( Model, Cmd Msg )
init backend =
    ( { backend = backend, lists = [], isProcessing = False, isConnected = False }, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Refresh (List ( String, String, List ( String, String, Int ) ))
    | InsertList String String
    | RenameList String String
    | InsertCard String String String
    | RenameCard String String String
    | UpvoteCard String String Int
    | Modify ID CardList.Msg
    | SendNetworkRequest NetworkRequest
    | NetworkResponseDidReceive String


updateItem : ID -> List ( ID, a ) -> (a -> a) -> List ( ID, a )
updateItem identifier list update =
    List.map
        (\( id, item ) ->
            if identifier == id then
                ( id, update item )
            else
                ( id, item )
        )
        list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        InsertList identifier text ->
            let
                newModel =
                    CardList.model

                newList =
                    ( identifier, { newModel | text = text } )

                newLists =
                    model.lists ++ [ newList ]
            in
                ( { model | lists = newLists }, Cmd.none )

        RenameList identifier text ->
            let
                newLists =
                    updateItem identifier model.lists (\item -> { item | text = text })
            in
                ( { model | lists = newLists }, Cmd.none )

        InsertCard listidentifier identifier text ->
            let
                newLists =
                    updateItem listidentifier model.lists (\item -> fst (CardList.update (CardList.InsertingCard identifier text) item))
            in
                ( { model | lists = newLists }, Cmd.none )

        RenameCard listidentifier identifier text ->
            let
                newLists =
                    updateItem listidentifier model.lists (\item -> fst (CardList.update (CardList.RenamingCard identifier text) item))
            in
                ( { model | lists = newLists }, Cmd.none )

        UpvoteCard listidentifier identifier counter ->
            let
                newLists =
                    updateItem listidentifier model.lists (\item -> fst (CardList.update (CardList.UpvotingCard identifier counter) item))
            in
                ( { model | lists = newLists }, Cmd.none )

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
    WebSocket.listen (model.backend ++ "/receive") NetworkResponseDidReceive



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
    let
        handleNetRequest model requestString =
            ( { model | isProcessing = True }, WebSocket.send (model.backend ++ "/submit") requestString )
    in
        case req of
            REQ_CONNECT ->
                ( { model | isConnected = True }, WebSocket.send (model.backend ++ "/submit") """{"REQ":"NOOP"}""" )

            REQ_REFRESH ->
                handleNetRequest model """{"REQ":"REFRESH"}"""

            REQ_NEWLIST ->
                handleNetRequest model """{"REQ":"NEWLIST"}"""

            REQ_RENAMELIST identifier text ->
                handleNetRequest model ("{\"REQ\":\"RENAMELIST\", \"IDENTIFIER\":\"" ++ identifier ++ "\", \"TEXT\":\"" ++ text ++ "\"}")

            REQ_NEWCARD identifier ->
                handleNetRequest model ("{\"REQ\":\"NEWCARD\", \"LISTIDENTIFIER\":\"" ++ identifier ++ "\"}")

            REQ_RENAMECARD listidentifier cardIdentifier text ->
                handleNetRequest model ("{\"REQ\":\"RENAMECARD\", \"IDENTIFIER\":\"" ++ cardIdentifier ++ "\", \"LISTIDENTIFIER\":\"" ++ listidentifier ++ "\", \"TEXT\":\"" ++ text ++ "\"}")

            REQ_UPVOTECARD listidentifier cardIdentifier ->
                handleNetRequest model ("{\"REQ\":\"UPVOTECARD\", \"IDENTIFIER\":\"" ++ cardIdentifier ++ "\", \"LISTIDENTIFIER\":\"" ++ listidentifier ++ "\"}")


networkResponseHandler : NetworkResponse -> Model -> ( Model, Cmd Msg )
networkResponseHandler resp model =
    let
        handleNetResponse msg model =
            update msg { model | isProcessing = False }
    in
        case resp of
            RESP_ERROR ->
                update NoOp model

            RESP_REFRESH newList ->
                handleNetResponse (Refresh newList) model

            RESP_NEWLIST identifier text ->
                handleNetResponse (InsertList identifier text) model

            RESP_RENAMELIST identifier text ->
                handleNetResponse (RenameList identifier text) model

            RESP_NEWCARD listidentifier identifier text ->
                handleNetResponse (InsertCard listidentifier identifier text) model

            RESP_RENAMECARD listidentifier identifier text ->
                handleNetResponse (RenameCard listidentifier identifier text) model

            RESP_UPVOTECARD listidentifier identifier counter ->
                handleNetResponse (UpvoteCard listidentifier identifier counter) model


decodeNetworkResponse : String -> NetworkResponse
decodeNetworkResponse message =
    case (decodeString ("RESP" := string) message) of
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
                                RESP_REFRESH newList

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
