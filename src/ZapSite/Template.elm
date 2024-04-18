----------------------------------------------------------------------
--
-- ZapSite/Template.elm
-- Support filling in templates from key/value stores.
-- Copyright (C) 2024 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module ZapSite.Template exposing (Variables, render)

{-| Templates can be filled in from variable bindings.
-}

import Dict exposing (Dict)
import Html exposing (Html, text)
import Markdown
import Markdown.Block as Markdown
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown
import Result as Result


{-| A key/value store
-}
type alias Variables =
    Dict String String


{-| Render a markdown string with template variables.
-}
render : String -> Variables -> List (Html msg)
render markdown variables =
    case parseMarkdown markdown of
        Err errmsg ->
            [ text <| "Markdown parsing error: " ++ errmsg ]

        Ok blocks ->
            case Markdown.render Markdown.defaultHtmlRenderer blocks of
                Ok htmls ->
                    htmls

                Err errmsg ->
                    [ text <| "Error: " ++ errmsg ]


parseMarkdown : String -> Result String (List Markdown.Block)
parseMarkdown markdown =
    Markdown.parse markdown
        |> Result.mapError
            (List.map Markdown.deadEndToString >> String.join "#\n")
