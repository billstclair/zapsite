---------------------------------------------------------------------
--
-- Main.elm
-- Zapsite, a simple web site editor, with user suggestion and merge support.
-- Copyright (C) 2024 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Dom as Dom exposing (Viewport)
import Browser.Events as Events
import Browser.Navigation as Navigation exposing (Key)
import Cmd.Extra exposing (addCmd, withCmd, withCmds, withNoCmd)
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , col
        , div
        , figcaption
        , figure
        , h1
        , h2
        , h3
        , img
        , input
        , label
        , option
        , p
        , pre
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        , video
        )
import Html.Attributes
    exposing
        ( alt
        , autocomplete
        , autofocus
        , autoplay
        , checked
        , class
        , cols
        , colspan
        , controls
        , disabled
        , draggable
        , height
        , hidden
        , href
        , id
        , name
        , placeholder
        , readonly
        , rows
        , selected
        , size
        , src
        , style
        , target
        , title
        , type_
        , value
        , width
        )
import Html.Events exposing (keyCode, on, onCheck, onClick, onInput, onMouseDown)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)
import List.Extra as LE
import Markdown
import Markdown.Block as Markdown
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown
import Markdown.Scaffolded as Scaffolded
import PortFunnel.LocalStorage as LocalStorage exposing (Label)
import PortFunnel.Notification as Notification exposing (Permission(..))
import PortFunnel.WebSocket as WebSocket exposing (Response(..))
import PortFunnels exposing (FunnelDict, Handler(..))
import Result as Result
import Result.Extra as Result
import Time exposing (Posix, Zone)
import Url exposing (Url)
import ZapSite.EncodeDecode as ED
import ZapSite.Template as Template
import ZapSite.Types as Types exposing (Page(..), SavedModel, Variables)


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | UpdateInput String
    | AddPair String String
    | DeletePair String
    | UpdateVariableValue String String
    | UpdateNewVar String
    | UpdateNewVal String
    | DeleteVariable String
    | AddNewVariable
    | Process Value
    | SetPage Page


zeroTick : Posix
zeroTick =
    Time.millisToPosix 0


type alias Model =
    { started : Bool
    , tick : Posix
    , input : String
    , parsed : Result String (List Markdown.Block)
    , variables : Variables
    , page : Page
    , newvar : String
    , newval : String
    , funnelState : PortFunnels.State
    }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { input = model.input
    , variables = model.variables
    , page = model.page
    , newvar = model.newvar
    , newval = model.newval
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel sm model =
    { model
        | input = sm.input
        , variables = sm.variables
        , page = sm.page
        , newvar = sm.newvar
        , newval = sm.newval
    }


initialMarkdown : String
initialMarkdown =
    """# {title}
## Making web sites, that invite additions, with templates.

The quick brown fox {verb} over the {adjective} dog.

_italic_ **bold** **_bold italic_**

[{link-name}]({link})

{col} 1 | {col} 2
------- | -------
{rn}, column 1 | {rn}, column 2
    """


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    let
        string =
            case JD.decodeValue JD.string value of
                Err e ->
                    "Error: " ++ Debug.toString e

                Ok s ->
                    s
    in
    { started = False
    , tick = zeroTick
    , input = initialMarkdown
    , parsed = parseMarkdown initialMarkdown
    , variables =
        Dict.fromList
            [ ( "title", "Zapsite" )
            , ( "link", "https://google.com" )
            , ( "link-name", "google.com" )
            , ( "col", "Column" )
            , ( "rn1", "Row 1" )
            , ( "rn2", "Row 2" )
            , ( "verb", "jumped" )
            , ( "adjective", "lazy" )
            ]
    , page = TemplatePage
    , newvar = ""
    , newval = ""
    , funnelState = initialFunnelState
    }
        |> withNoCmd


parseMarkdown : String -> Result String (List Markdown.Block)
parseMarkdown markdown =
    Markdown.parse markdown
        |> Result.mapError
            (List.map Markdown.deadEndToString >> String.join "#\n")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput input ->
            { model
                | input = input
                , parsed = parseMarkdown input
            }
                |> withNoCmd

        AddPair key value ->
            model |> withNoCmd

        DeletePair key ->
            model |> withNoCmd

        UpdateVariableValue var val ->
            { model | variables = Dict.insert var val model.variables }
                |> withNoCmd

        UpdateNewVar var ->
            { model | newvar = var } |> withNoCmd

        UpdateNewVal val ->
            { model | newval = val } |> withNoCmd

        DeleteVariable k ->
            { model
                | variables = Dict.remove k model.variables
            }
                |> withNoCmd

        AddNewVariable ->
            { model
                | variables =
                    Dict.insert model.newvar model.newval model.variables
                , newvar = ""
                , newval = ""
            }
                |> withNoCmd

        Process value ->
            case
                PortFunnels.processValue funnelDict
                    value
                    model.funnelState
                    model
            of
                Err error ->
                    -- Maybe we should display an error here,
                    -- but I don't think it will ever happen.
                    model |> withNoCmd

                Ok res ->
                    res

        SetPage page ->
            { model | page = page }
                |> withNoCmd

        _ ->
            model |> withNoCmd


br : Html msg
br =
    Html.br [] []


view : Model -> Document Msg
view model =
    { title = "Zapsite"
    , body =
        [ div [ style "left-margin" "20px" ] <|
            case model.page of
                TemplatePage ->
                    viewTemplatePage model

                _ ->
                    viewMainPage model
        , div [ style "text-align" "center" ] <|
            case model.page of
                TemplatePage ->
                    [ text "template "
                    , a
                        [ href "#"
                        , onClick <| SetPage MainPage
                        ]
                        [ text "main" ]
                    ]

                _ ->
                    [ a
                        [ href "#"
                        , onClick <| SetPage TemplatePage
                        ]
                        [ text "template" ]
                    , text " main"
                    ]
        ]
    }


viewMainPage : Model -> List (Html Msg)
viewMainPage model =
    [ text "Main Page under construction." ]


viewTemplatePage : Model -> List (Html Msg)
viewTemplatePage model =
    [ h1 [] [ text "Zapsite" ]
    , h2 [] [ text "Current Playground" ]
    , p []
        [ text "Type in the textarea below, see rendering and data structure below." ]
    , p []
        [ textarea
            [ rows 18
            , cols 80
            , value model.input
            , onInput UpdateInput
            ]
            []
        ]
    , p [] <| Template.render model.input model.variables
    , viewVariables model
    , p []
        [ h2 [] [ text "Parsed (Result String (List Markdown.Block)" ]
        , model.parsed
            |> Debug.toString
            |> text
        ]
    , p []
        [ a [ href "https://github.com/billstclair/zapsite" ]
            [ text "GitHub" ]
        ]
    ]


viewVariables : Model -> Html Msg
viewVariables model =
    let
        viewRow ( k, v ) =
            tr []
                [ td [] [ text k ]
                , td []
                    [ textarea
                        [ rows 1
                        , cols 50
                        , value v
                        , onInput (UpdateVariableValue k)
                        ]
                        []
                    ]
                , td []
                    [ button "Delete" <| DeleteVariable k
                    ]
                ]
    in
    p []
        [ h2 [] [ text "Variables:" ]
        , br
        , table [] <|
            List.concat
                [ [ tr []
                        [ th [] [ text "Var" ]
                        , th [] [ text "Val" ]
                        , th [] [ text "Action" ]
                        ]
                  ]
                , model.variables
                    |> Dict.toList
                    |> List.map viewRow
                , [ tr []
                        [ td []
                            [ textarea
                                [ rows 1
                                , cols 10
                                , value model.newvar
                                , onInput UpdateNewVar
                                ]
                                []
                            ]
                        , td []
                            [ textarea
                                [ rows 1
                                , cols 50
                                , value model.newval
                                , onInput UpdateNewVal
                                ]
                                []
                            ]
                        , td []
                            [ button "Add" AddNewVariable ]
                        ]
                  ]
                ]
        ]


button : String -> Msg -> Html Msg
button label msg =
    Html.button [ onClick msg ]
        [ text label ]


viewMarkdown : String -> List (Html Msg)
viewMarkdown markdown =
    [ Html.h2 [] [ text "Prettyprinted:" ]
    , Html.hr [] []
    , Html.pre [ style "white-space" "pre-wrap" ] [ text markdown ]
    , Html.h2 [] [ text "toHTML:" ]
    , p []
        [ toHtml markdown ]
    ]


toHtml : String -> Html Msg
toHtml markdown =
    Markdown.toHtml
        [ style "overflow" "auto"
        , style "width" "100%"
        ]
        markdown


viewError : String -> List (Html Msg)
viewError errorMessage =
    [ Html.pre [ style "white-space" "pre-wrap" ]
        [ Html.text errorMessage ]
    ]


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


get : String -> Cmd Msg
get key =
    localStorageSend <| Debug.log "LocalStorage" (LocalStorage.get key)


getLabeled : String -> String -> Cmd Msg
getLabeled label key =
    localStorageSend
        (Debug.log "LocalStorage" <|
            LocalStorage.getLabeled label key
        )


listKeys : String -> Cmd Msg
listKeys prefix =
    localStorageSend (LocalStorage.listKeys prefix)


listKeysLabeled : String -> String -> Cmd Msg
listKeysLabeled label prefix =
    localStorageSend (LocalStorage.listKeysLabeled label prefix)


clearStorage : Cmd Msg
clearStorage =
    localStorageSend (LocalStorage.clear "")


localStoragePrefix : String
localStoragePrefix =
    "SayUncle"


initialFunnelState : PortFunnels.State
initialFunnelState =
    PortFunnels.initialState localStoragePrefix


localStorageSend : LocalStorage.Message -> Cmd Msg
localStorageSend message =
    LocalStorage.send (getCmdPort LocalStorage.moduleName ())
        message
        initialFunnelState.storage


webSocketSend : WebSocket.Message -> Cmd Msg
webSocketSend message =
    WebSocket.send (getCmdPort WebSocket.moduleName ()) <|
        Debug.log "webSocketSend" message


{-| The `model` parameter is necessary here for `PortFunnels.makeFunnelDict`.
-}
getCmdPort : String -> model -> (Value -> Cmd Msg)
getCmdPort moduleName _ =
    PortFunnels.getCmdPort Process moduleName False


funnelDict : FunnelDict Model Msg
funnelDict =
    PortFunnels.makeFunnelDict
        [ LocalStorageHandler storageHandler
        , WebSocketHandler socketHandler
        , NotificationHandler notificationHandler
        ]
        getCmdPort


notificationHandler : Notification.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
notificationHandler response state mdl =
    mdl |> withNoCmd


socketHandler : Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
socketHandler response state mdl =
    mdl |> withNoCmd


storageHandler : LocalStorage.Response -> PortFunnels.State -> Model -> ( Model, Cmd Msg )
storageHandler response state model =
    let
        mdl =
            { model
                | started =
                    if LocalStorage.isLoaded state.storage then
                        True

                    else
                        model.started
            }

        cmd =
            if mdl.started && not model.started && model.tick /= zeroTick then
                get pk.model

            else
                Cmd.none
    in
    case response of
        LocalStorage.ListKeysResponse { label, prefix, keys } ->
            handleListKeysResponse label prefix keys mdl

        LocalStorage.GetResponse { label, key, value } ->
            case value of
                Nothing ->
                    mdl |> withNoCmd

                Just v ->
                    handleGetResponse label key v model

        _ ->
            mdl |> withCmd cmd


handleListKeysResponse : Label -> String -> List String -> Model -> ( Model, Cmd Msg )
handleListKeysResponse label prefix keys model =
    case label of
        Nothing ->
            model |> withNoCmd

        Just lab ->
            model |> withNoCmd


handleGetResponse : Label -> String -> Value -> Model -> ( Model, Cmd Msg )
handleGetResponse label key value model =
    case label of
        Just lab ->
            model |> withNoCmd

        Nothing ->
            if key == pk.model then
                case ED.decodeSavedModel value of
                    Err e ->
                        model |> withNoCmd

                    Ok savedModel ->
                        savedModelToModel savedModel model
                            |> withNoCmd

            else
                model |> withNoCmd


{-| Persistent storage keys
-}
pk =
    { model = "model"
    }
