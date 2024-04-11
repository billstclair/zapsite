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
import Markdown.PrettyTables exposing (ColumnInfo, TableInfo, TableStyle, finishReduction, reducePrettyTable)
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
    }


init : Value -> Url -> Key -> ( Model, Cmd Msg )
init value url key =
    { input = "" } |> withNoCmd


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput input ->
            { model | input = input } |> withNoCmd

        _ ->
            model |> withNoCmd


view : Model -> Document Msg
view model =
    { title = "Zapsite"
    , body =
        [ h2 [] [ text "Zapsite" ]
        , p []
            [ textarea
                [ rows 8
                , cols 80
                , value model.input
                , onInput UpdateInput
                ]
                []
            ]
        , p [] [ text <| model.input ]
        ]
    }
