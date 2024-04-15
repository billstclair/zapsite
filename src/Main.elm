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
import Markdown
import Markdown.Block as Markdown
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.PrettyTables as PrettyTables
    exposing
        ( ColumnInfo
        , TableInfo
        , TableStyle
        , finishReduction
        , reducePrettyTable
        )
import Markdown.Renderer as Markdown
import Markdown.Scaffolded as Scaffolded
import Result as Result
import Result.Extra as Result
import Url exposing (Url)


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


type alias Model =
    { input : String
    , parsed : Result String (List Markdown.Block)
    }


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
    { input = ""
    , parsed = Err "Not yet initialized."
    }
        |> withNoCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput input ->
            { model
                | input = input
                , parsed =
                    input
                        |> Markdown.parse
                        |> Result.mapError
                            (List.map Markdown.deadEndToString >> String.join "\n")
            }
                |> withNoCmd

        _ ->
            model |> withNoCmd


columnInfo : ColumnInfo
columnInfo =
    { size = 50
    , alignment = Nothing
    }


headerTableInfo : TableInfo String
headerTableInfo =
    { render = \_ -> "" -- \_ -> columnInfo --Dict Int ColumnInfo -> ColumnInfo
    , info = Dict.empty --Dict Int ColumnInfo
    }


view : Model -> Document Msg
view model =
    { title = "Zapsite"
    , body =
        [ h2 [] [ text "Zapsite" ]
        , p []
            [ text "Current Playground" ]
        , p []
            [ textarea
                [ rows 8
                , cols 80
                , value model.input
                , onInput UpdateInput
                ]
                []
            ]
        , p []
            (model.parsed
                |> Result.andThen (Markdown.render customHtmlRenderer)
                |> Result.map PrettyTables.finishReduction
                |> Result.unpack viewError viewMarkdown
            )
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
    }


customHtmlRenderer : Markdown.Renderer (Int -> PrettyTables.TableInfo String)
customHtmlRenderer =
    Scaffolded.toRenderer
        { renderHtml = Markdown.Html.oneOf []
        , renderMarkdown = Debug.log "reducePrettyTable" (PrettyTables.reducePrettyTable PrettyTables.defaultStyle)
        }


viewMarkdown : String -> List (Html Msg)
viewMarkdown markdown =
    [ Html.h2 [] [ text "Prettyprinted:" ]
    , Html.hr [] []
    , Html.pre [ style "white-space" "pre-wrap" ] [ text markdown ]
    , Html.h2 [] [ text "toHTML:" ]
    , p []
        [ Markdown.toHtml
            [ style "overflow" "auto"
            , style "width" "100%"
            ]
            markdown
        ]
    ]


viewError : String -> List (Html Msg)
viewError errorMessage =
    [ Html.pre [ style "white-space" "pre-wrap" ]
        [ Html.text errorMessage ]
    ]
