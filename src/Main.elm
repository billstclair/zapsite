----------------------------------------------------------------------
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
import Result as Result
import Result.Extra as Result
import Url exposing (Url)
import ZapSite.Template as Template exposing (Variables)


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


type Page
    = MarkdownPage
    | TemplatePage


type alias Model =
    { input : String
    , parsed : Result String (List Markdown.Block)
    , variables : Variables
    , page : Page
    , newvar : String
    , newval : String
    }


initialMarkdown : String
initialMarkdown =
    """# Zapsite
## Making web sites, that invite additions, with templates.

The quick brown fox jumped over the lazy dog.

_italic_ **bold** **_bold italic_**

[google.com](https://google.com)

col1 | col2
---- | ----
r1c2 | r1c2
row 2 column 1 | row 2 column 2
r3c1 | row 3 column 2
row 4 column 1 | r4c2
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
    { input = initialMarkdown
    , parsed = parseMarkdown initialMarkdown
    , variables = Template.emptyVariables
    , page = TemplatePage
    , newvar = ""
    , newval = ""
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

        _ ->
            model |> withNoCmd


br : Html msg
br =
    Html.br [] []


view : Model -> Document Msg
view model =
    { title = "Zapsite"
    , body =
        [ div [ style "left-margin" "20px" ]
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
            , p []
                (case model.parsed of
                    Ok blocks ->
                        case Markdown.render Markdown.defaultHtmlRenderer blocks of
                            Ok htmls ->
                                htmls

                            Err errmsg ->
                                [ text <| "Error: " ++ errmsg ]

                    Err errmsg ->
                        [ text <| "Error: " ++ errmsg ]
                )
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
        ]
    }


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
                ]
    in
    p []
        [ h2 [] [ text "Pairs:" ]
        , br
        , table [] <|
            List.concat
                [ [ tr []
                        [ th [] [ text "Var" ]
                        , th [] [ text "Val" ]
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
                        ]
                  ]
                ]
        ]


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
