--------------------------------------------------------------------
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
import Task
import Time exposing (Posix, Zone)
import Url exposing (Url)
import ZapSite.EncodeDecode as ED
import ZapSite.Persistence as Persistence
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
    Sub.batch
        [ PortFunnels.subscriptions Process model
        , Time.every 900 Tick
        ]


type Msg
    = SetZone Zone
    | Tick Posix
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | SetUrlInput String
    | SetUrl
    | RevertUrl
    | SetTemplateNameInput String
    | SetTemplateName
    | RevertTemplateName
    | LookupTemplateName
    | UpdateTemplateInput String
    | SetTemplate
    | RevertTemplate
    | UpdateVariableValue String String
    | UpdateNewVar String
    | UpdateNewVal String
    | DeleteVariable String
    | AddNewVariable
    | SetVariables
    | RevertVariables
    | ClearVariables
    | Process Value
    | SetPage Page


zeroTick : Posix
zeroTick =
    Time.millisToPosix 0


type alias Model =
    { started : Bool
    , tick : Posix
    , here : Zone
    , storage : Persistence.Config Msg
    , error : Maybe String
    , editing : Bool
    , urlInput : String
    , url : String
    , templateNameInput : String
    , templateName : String
    , templateInput : String
    , template : String
    , parsed : Result String (List Markdown.Block)
    , variablesInput : Variables
    , variables : Variables
    , page : Page
    , newvar : String
    , newval : String
    , funnelState : PortFunnels.State
    }


modelToSavedModel : Model -> SavedModel
modelToSavedModel model =
    { editing = model.editing
    , urlInput = model.urlInput
    , url = model.url
    , templateNameInput = model.templateNameInput
    , templateName = model.templateName
    , templateInput = model.templateInput
    , template = model.template
    , variablesInput = model.variablesInput
    , variables = model.variables
    , page = MainPage -- model.page
    , newvar = model.newvar
    , newval = model.newval
    }


savedModelToModel : SavedModel -> Model -> Model
savedModelToModel sm model =
    { model
        | editing = sm.editing
        , urlInput = sm.urlInput
        , url = sm.url
        , templateNameInput = sm.templateNameInput
        , templateName = sm.templateName
        , templateInput = sm.templateInput
        , template = sm.template
        , variablesInput = sm.variablesInput
        , variables = sm.variables

        -- , page = sm.page
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


initialVariables : Variables
initialVariables =
    Dict.fromList
        [ ( "title", "Zapsite" )
        , ( "link", "https://google.com" )
        , ( "link-name", "google.com" )
        , ( "col", "Column" )
        , ( "rn1", "Row 1" )
        , ( "rn2", "Row 2" )
        , ( "verb", "jumps" )
        , ( "adjective", "lazy" )
        ]


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
    , here = Time.utc
    , storage = Persistence.localConfig storageGet storagePut
    , error = Nothing
    , editing = True
    , urlInput = ""
    , url = ""
    , templateNameInput = ""
    , templateName = ""
    , templateInput = initialMarkdown
    , template = initialMarkdown
    , parsed = parseMarkdown initialMarkdown
    , variablesInput = initialVariables
    , variables = initialVariables
    , page = MainPage
    , newvar = ""
    , newval = ""
    , funnelState = initialFunnelState
    }
        |> withCmds
            [ Task.perform Tick Time.now
            , Task.perform SetZone Time.here
            ]


parseMarkdown : String -> Result String (List Markdown.Block)
parseMarkdown markdown =
    Markdown.parse markdown
        |> Result.mapError
            (List.map Markdown.deadEndToString >> String.join "#\n")


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        doStore =
            case msg of
                Process _ ->
                    False

                Tick _ ->
                    False

                LookupTemplateName ->
                    False

                _ ->
                    True

        ( mdl, cmd ) =
            updateInternal msg model

        cmd2 =
            if doStore then
                Cmd.batch [ cmd, putModel mdl ]

            else
                cmd
    in
    mdl |> withCmd cmd2


updateInternal : Msg -> Model -> ( Model, Cmd Msg )
updateInternal msg model =
    case msg of
        OnUrlRequest url ->
            -- TODO
            model |> withNoCmd

        OnUrlChange url ->
            -- TODO
            model |> withNoCmd

        SetZone zone ->
            { model | here = zone }
                |> withNoCmd

        Tick posix ->
            if model.tick == zeroTick then
                { model
                    | tick = posix
                }
                    |> withCmd
                        (if model.started then
                            getModel

                         else
                            Cmd.none
                        )

            else
                model |> withNoCmd

        SetUrlInput input ->
            { model | urlInput = input }
                |> withNoCmd

        SetUrl ->
            -- TODO: initiate lookup of URL template name
            { model | url = model.urlInput }
                |> withCmd (Persistence.getUrlBindings model.storage model.urlInput)

        RevertUrl ->
            { model | urlInput = model.url }
                |> withNoCmd

        SetTemplateNameInput input ->
            { model | templateNameInput = input }
                |> withNoCmd

        SetTemplateName ->
            let
                name =
                    model.templateNameInput
            in
            if name /= "" then
                model
                    |> withCmd (Persistence.getTemplate model.storage name)

            else
                { model
                    | templateName = ""
                    , templateInput = ""
                    , variables = Dict.empty
                }
                    |> withNoCmd

        RevertTemplateName ->
            { model | templateNameInput = model.templateName }
                |> withNoCmd

        LookupTemplateName ->
            -- TODO
            model |> withNoCmd

        UpdateTemplateInput template ->
            { model
                | templateInput = template
                , parsed = parseMarkdown template
            }
                |> withNoCmd

        SetTemplate ->
            let
                name =
                    model.templateName
            in
            if name /= model.templateNameInput then
                { model
                    | error = Just "Save template name to set new template."
                }
                    |> withNoCmd

            else if name == "" then
                { model
                    | error = Just "Can't save template with blank name."
                }
                    |> withNoCmd

            else
                { model
                    | template = model.templateInput
                    , error = Nothing
                }
                    |> withCmd
                        (Persistence.putTemplate model.storage
                            name
                            model.template
                        )

        RevertTemplate ->
            { model | templateInput = model.template }
                |> withNoCmd

        UpdateVariableValue var val ->
            { model | variablesInput = Dict.insert var val model.variablesInput }
                |> withNoCmd

        UpdateNewVar var ->
            { model | newvar = var } |> withNoCmd

        UpdateNewVal val ->
            { model | newval = val } |> withNoCmd

        DeleteVariable k ->
            { model
                | variablesInput = Dict.remove k model.variablesInput
            }
                |> withNoCmd

        AddNewVariable ->
            { model
                | variablesInput =
                    Dict.insert model.newvar model.newval model.variablesInput
                , newvar = ""
                , newval = ""
            }
                |> withNoCmd

        SetVariables ->
            let
                url =
                    model.url

                templateName =
                    model.templateName
            in
            if url /= model.urlInput || templateName /= model.templateNameInput then
                { model
                    | error = Just "Save url & template name to save variables."
                }
                    |> withNoCmd

            else
                { model
                    | variables = model.variablesInput
                    , error = Nothing
                }
                    |> withCmd
                        (Persistence.putUrlBindings model.storage
                            url
                            templateName
                            model.variablesInput
                        )

        RevertVariables ->
            { model | variablesInput = model.variables }
                |> withNoCmd

        ClearVariables ->
            { model | variablesInput = Dict.empty }
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


view : Model -> Document Msg
view model =
    let
        pageDiv =
            div [ style "text-align" "center" ] <|
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
    in
    { title = "Zapsite"
    , body =
        [ text "" -- pageDiv
        , div [ style "left-margin" "20px" ] <|
            case model.page of
                TemplatePage ->
                    viewTemplatePage model

                _ ->
                    viewMainPage model
        , text "" -- pageDiv
        ]
    }


b : String -> Html msg
b string =
    Html.b [] [ text string ]


br : Html msg
br =
    Html.br [] []


labeledInput : String -> String -> Int -> (String -> Msg) -> List (Html Msg)
labeledInput label v w tagger =
    [ b label
    , b ": "
    , input
        [ type_ "text"
        , width w
        , value v
        , onInput tagger
        ]
        []
    ]


viewMainPage : Model -> List (Html Msg)
viewMainPage model =
    [ h1 [] [ text "Zapsite" ]
    , if model.editing then
        div [] <|
            List.concat
                [ labeledInput "url" model.urlInput 20 SetUrlInput
                , if model.url == model.urlInput then
                    []

                  else
                    [ text " "
                    , button "set" SetUrl
                    , text " "
                    , button "revert" RevertUrl
                    ]
                , [ br ]
                , labeledInput "template name" model.templateNameInput 20 SetTemplateNameInput
                , if model.templateName == model.templateNameInput then
                    [ text " "
                    , button "lookup" LookupTemplateName
                    ]

                  else
                    [ text " "
                    , button "set" SetTemplateName
                    , text " "
                    , button "revert" RevertTemplateName
                    , text " "
                    , button "lookup" LookupTemplateName
                    ]
                , [ br ]
                , [ let
                        disabled =
                            model.template == model.templateInput
                    in
                    p []
                        [ b "template:"
                        , br
                        , disabledButton disabled "save" SetTemplate
                        , text " "
                        , disabledButton disabled "revert" RevertTemplate
                        ]
                  , p []
                        [ textarea
                            [ rows 18
                            , cols 80
                            , value model.templateInput
                            , onInput UpdateTemplateInput
                            ]
                            []
                        ]
                  , viewVariables model
                  , p [] [ Html.hr [] [] ]
                  , p [] <| Template.render model.templateInput model.variablesInput
                  , p [] [ Html.hr [] [] ]
                  , p []
                        [ a [ href "https://github.com/billstclair/zapsite" ]
                            [ text "GitHub" ]
                        ]
                  ]
                ]

      else
        p [] <| Template.render model.templateInput model.variablesInput
    ]


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
            , value model.templateInput
            , onInput UpdateTemplateInput
            ]
            []
        ]
    , p [] <| Template.render model.templateInput model.variablesInput
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
        [ let
            disabled =
                model.variables == model.variablesInput
          in
          p []
            [ b "variables:"
            , br
            , disabledButton disabled "save" SetVariables
            , text " "
            , disabledButton disabled "revert" RevertVariables
            , text " "
            , disabledButton (model.variablesInput == Dict.empty)
                "clear"
                ClearVariables
            ]
        , table [] <|
            List.concat
                [ [ tr []
                        [ th [] [ text "Var" ]
                        , th [] [ text "Val" ]
                        , th [] [ text "Action" ]
                        ]
                  ]
                , model.variablesInput
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


disabledButton : Bool -> String -> Msg -> Html Msg
disabledButton isDisabled label msg =
    Html.button
        [ onClick msg
        , disabled isDisabled
        ]
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


putModel : Model -> Cmd Msg
putModel model =
    put pk.model
        (modelToSavedModel model |> ED.encodeSavedModel |> Just)


getModel : Cmd Msg
getModel =
    get pk.model


put : String -> Maybe Value -> Cmd Msg
put key value =
    localStorageSend (LocalStorage.put (Debug.log "put" key) value)


storagePrefix : String
storagePrefix =
    "=storage=/"


storagePrefixLength : Int
storagePrefixLength =
    String.length storagePrefix


storagePut : String -> Maybe Value -> Cmd Msg
storagePut key =
    put (storagePrefix ++ key)


storageGet : String -> Cmd Msg
storageGet key =
    getLabeled "storage" <| storagePrefix ++ key


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
    "zapsite"


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
        rsp =
            Debug.log "storageHandler response" response

        stat =
            (\_ -> Debug.log "  state" state) rsp

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
                getModel

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


handleGetStorageResponse : String -> Value -> Model -> ( Model, Cmd Msg )
handleGetStorageResponse key value model =
    if String.left storagePrefixLength key /= storagePrefix then
        let
            e2 =
                Debug.log "handleGetStorageResponse key" key
        in
        model |> withNoCmd

    else
        let
            subkey =
                String.dropLeft storagePrefixLength key
        in
        case Persistence.maybeDecodeTemplate subkey value of
            Nothing ->
                case Persistence.maybeDecodeUrlBindings subkey value of
                    Nothing ->
                        -- TODO
                        model |> withNoCmd

                    Just result ->
                        case result of
                            Err e ->
                                let
                                    e2 =
                                        Debug.log "handleGetStorageResponse error" e
                                in
                                model |> withNoCmd

                            Ok urlBindings ->
                                -- TODO
                                model |> withNoCmd

            Just result ->
                case result of
                    Err e ->
                        let
                            e2 =
                                Debug.log "handleGetStorageResponse error" e
                        in
                        model |> withNoCmd

                    Ok template ->
                        -- TODO
                        { model
                            | template = template
                            , parsed = parseMarkdown template
                        }
                            |> withNoCmd


handleGetResponse : Label -> String -> Value -> Model -> ( Model, Cmd Msg )
handleGetResponse label key value model =
    case label of
        Just lab ->
            if lab == "storage" then
                handleGetStorageResponse key value model

            else
                model |> withNoCmd

        Nothing ->
            if Debug.log "handleGetResponse" key == pk.model then
                case ED.decodeSavedModel value of
                    Err e ->
                        let
                            m =
                                JE.encode 0 value
                                    |> Debug.log "  "
                        in
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
