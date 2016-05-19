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
    | Modify ID CardList.Msg
    | SendNetworkRequest NetworkRequest
    | NetworkResponseDidReceive String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        Modify id listMsg ->
            let
                updateList ( listID, listModel ) =
                    if listID == id then
                        ( listID, CardList.update listMsg listModel )
                    else
                        ( listID, listModel )
            in
                ( { model | lists = List.map updateList model.lists }, Cmd.none )

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


type NetworkResponse
    = RESP_ERROR
    | RESP_NEWLIST String String


networkRequestHandler : NetworkRequest -> Model -> ( Model, Cmd Msg )
networkRequestHandler req model =
    case req of
        REQ_CONNECT ->
            ( { model | isConnected = True }, WebSocket.send "ws://0.0.0.0:5000/submit" """{"REQ":"NOOP"}""" )

        REQ_REFRESH ->
            ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" """{"REQ":"NEWLIST"}""" )

        REQ_NEWLIST ->
            ( { model | isProcessing = True }, WebSocket.send "ws://0.0.0.0:5000/submit" """{"REQ":"NEWLIST"}""" )


networkResponseHandler : NetworkResponse -> Model -> ( Model, Cmd Msg )
networkResponseHandler resp model =
    case resp of
        RESP_NEWLIST identifier text ->
            update (Insert identifier text) { model | isProcessing = False }

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
                            object2 (,) ("identifier" := string) ("text" := string)
                    in
                        case (decodeString parsedCardList message) of
                            Err error ->
                                RESP_ERROR

                            Ok ( identifier, text ) ->
                                RESP_NEWLIST identifier text

                _ ->
                    RESP_ERROR
