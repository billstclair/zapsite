----------------------------------------------------------------------
--
-- Notification.elm
-- Elm interface to JavaScript's Notification facility.
-- Copyright (c) 2021 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module PortFunnel.Notification exposing
    ( Error(..)
    , Message
    , Notification
    , Permission(..)
    , Response(..)
    , State
    , commander
    , dismissNotification
    , displayNotification
    , getPermission
    , initialState
    , isAvailable
    , lookupNotification
    , moduleDesc
    , moduleName
    , notificationId
    , notificationOptions
    , requestPermission
    , send
    )

{-| PortFunnel interface to the Notification DOM.
See <https://developer.mozilla.org/en-US/docs/Web/API/notification>
-}

import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import PortFunnel exposing (GenericMessage, ModuleDesc)


{-| This funnel has no state.
-}
type State
    = State ()


initialState : State
initialState =
    State ()


{-| Responsible for sending a `CmdResponse` back through the port.

This funnel doesn't initiate any sends, so this function always returns `Cmd.none`.

-}
commander : (GenericMessage -> Cmd msg) -> Response -> Cmd msg
commander _ _ =
    Cmd.none


type Permission
    = PermissionGranted
    | PermissionDenied
    | PermissionDefault


type alias Options =
    -- TODO
    ()


type Notification
    = Notification { id : Int, title : String, options : Maybe Options }


type Error
    = NotAvailableError
    | NoPermissionError
    | RandomError String


type Response
    = NoResponse
    | AvailableResponse Bool
    | PermissionResponse Permission
    | NotificationResponse Notification
    | ClickResponse Int
    | ErrorResponse String


type Message
    = IsAvailableReq
    | GetPermissionReq
    | RequestPermissionReq
    | SendNotificationReq String (Maybe Options)
    | DismissNotificationReq Int
    | LookupNotificationReq Int
      -- Messages from JS
    | IsAvailableAnswer Bool
    | GetPermissionAnswer Permission
    | NotificationAnswer { id : Int, title : String, options : Maybe Options }
    | NotificationClicked Int
    | ErrorAnswer String


isAvailable : Message
isAvailable =
    IsAvailableReq


getPermission : Message
getPermission =
    GetPermissionReq


requestPermission : Message
requestPermission =
    RequestPermissionReq


dismissNotification : Int -> Message
dismissNotification id =
    DismissNotificationReq id


displayNotification : String -> Message
displayNotification title =
    SendNotificationReq title Nothing


lookupNotification : Int -> Message
lookupNotification id =
    LookupNotificationReq id


notificationId : Notification -> Int
notificationId (Notification notification) =
    notification.id


notificationTitle : Notification -> String
notificationTitle (Notification notification) =
    notification.title


notificationOptions : Notification -> Maybe Options
notificationOptions (Notification notification) =
    notification.options


send : (Value -> Cmd msg) -> Message -> Cmd msg
send outport message =
    PortFunnel.sendMessage moduleDesc outport message


moduleName : String
moduleName =
    "Notification"


moduleDesc : ModuleDesc Message State Response
moduleDesc =
    PortFunnel.makeModuleDesc moduleName encode decode process


tags =
    { isAvailable = "isAvailable"
    , getPermission = "getPermission"
    , requestPermission = "requestPermission"
    , sendNotification = "sendNotification"
    , dismissNotification = "dismissNotification"
    , lookupNotification = "lookupNotification"
    , wasAvailable = "wasAvailable"
    , gotPermission = "gotPermission"
    , notification = "notification"
    , onClick = "onClick"
    , error = "error"
    }


encode : Message -> GenericMessage
encode message =
    case message of
        IsAvailableReq ->
            GenericMessage moduleName tags.isAvailable JE.null

        GetPermissionReq ->
            GenericMessage moduleName tags.getPermission JE.null

        RequestPermissionReq ->
            GenericMessage moduleName tags.requestPermission JE.null

        SendNotificationReq title options ->
            GenericMessage moduleName tags.sendNotification <| JE.string title

        DismissNotificationReq id ->
            GenericMessage moduleName tags.dismissNotification <| JE.int id

        LookupNotificationReq id ->
            GenericMessage moduleName tags.lookupNotification <| JE.int id

        IsAvailableAnswer bool ->
            GenericMessage moduleName tags.wasAvailable <| JE.bool bool

        GetPermissionAnswer permission ->
            let
                v =
                    JE.string <|
                        case permission of
                            PermissionGranted ->
                                "granted"

                            PermissionDenied ->
                                "denied"

                            PermissionDefault ->
                                "default"
            in
            GenericMessage moduleName tags.gotPermission v

        NotificationAnswer { id, title } ->
            let
                v =
                    JE.object
                        [ ( "id", JE.int id )
                        , ( "title", JE.string title )
                        ]
            in
            GenericMessage moduleName tags.notification v

        NotificationClicked id ->
            GenericMessage moduleName tags.onClick <| JE.int id

        ErrorAnswer s ->
            GenericMessage moduleName tags.error <| JE.string s


decode : GenericMessage -> Result String Message
decode { tag, args } =
    if tag == tags.isAvailable then
        Ok IsAvailableReq

    else if tag == tags.getPermission then
        Ok GetPermissionReq

    else if tag == tags.requestPermission then
        Ok RequestPermissionReq

    else if tag == tags.sendNotification then
        case JD.decodeValue JD.string args of
            Ok title ->
                Ok <| SendNotificationReq title Nothing

            Err _ ->
                Err "Bad title from JS code. Shouldn't happen."

    else if tag == tags.dismissNotification then
        case JD.decodeValue JD.int args of
            Ok id ->
                Ok <| DismissNotificationReq id

            Err _ ->
                Err "Bad id from JS code. Shouldn't happen."

    else if tag == tags.lookupNotification then
        case JD.decodeValue JD.int args of
            Ok id ->
                Ok <| LookupNotificationReq id

            Err _ ->
                Err "Bad id from JS code. Shouldn't happen."

    else if tag == tags.wasAvailable then
        case JD.decodeValue JD.bool args of
            Ok bool ->
                Ok <| IsAvailableAnswer bool

            Err _ ->
                Err "Bad available bool from JS code. Shouldn't happen."

    else if tag == tags.gotPermission then
        case JD.decodeValue JD.string args of
            Ok s ->
                case s of
                    "granted" ->
                        Ok <| GetPermissionAnswer PermissionGranted

                    "denied" ->
                        Ok <| GetPermissionAnswer PermissionDenied

                    "default" ->
                        Ok <| GetPermissionAnswer PermissionDefault

                    _ ->
                        Err <| "Unknown permission: " ++ s

            Err _ ->
                Err "Non-string from JS for permission. Shouldn't happen."

    else if tag == tags.notification then
        case JD.decodeValue notificationDecoder args of
            Ok { id, title } ->
                Ok <|
                    NotificationAnswer
                        { id = id, title = title, options = Nothing }

            Err _ ->
                Err "Bad title string from JS code. Shouldn't happen."

    else if tag == tags.onClick then
        case JD.decodeValue JD.int args of
            Ok id ->
                Ok <| NotificationClicked id

            Err _ ->
                Err "Bad notification ID from JS code. Shouldn't happen."

    else if tag == tags.error then
        case JD.decodeValue JD.string args of
            Ok s ->
                Ok <| ErrorAnswer s

            Err _ ->
                Err "Bad error string from JS code. Shouldn't happen."

    else
        Err <| "Unknown tag: " ++ tag


notificationDecoder : Decoder { id : Int, title : String }
notificationDecoder =
    JD.map2
        (\id title ->
            { id = id, title = title }
        )
        (JD.field "id" JD.int)
        (JD.field "title" JD.string)


process : Message -> State -> ( State, Response )
process message state =
    case message of
        IsAvailableAnswer bool ->
            ( state, AvailableResponse bool )

        GetPermissionAnswer permission ->
            ( state, PermissionResponse permission )

        NotificationAnswer { id, title, options } ->
            ( state
            , NotificationResponse <|
                Notification { id = id, title = title, options = options }
            )

        NotificationClicked id ->
            ( state
            , ClickResponse id
            )

        ErrorAnswer string ->
            ( state
            , ErrorResponse string
            )

        _ ->
            ( state
            , ErrorResponse <| "Unexpected message from JS code. Shouldn't happen."
            )
