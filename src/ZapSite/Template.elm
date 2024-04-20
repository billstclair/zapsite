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


module ZapSite.Template exposing (Variables, emptyVariables, render)

{-| Templates can be filled in from variable bindings.
-}

import Dict exposing (Dict)
import Html exposing (Html, text)
import Markdown
import Markdown.Block as Markdown exposing (Block(..), Inline(..))
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown
import Result as Result


{-| A key/value store
-}
type alias Variables =
    Dict String String


{-| The empty key/value store
-}
emptyVariables : Variables
emptyVariables =
    Dict.empty


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


isTemplate : String -> Maybe String
isTemplate string =
    if
        (String.left 1 string == "{")
            && (String.right 1 string == "}")
    then
        Just <| String.slice 1 -1 string

    else
        Nothing


walkTemplate : Variables -> (Block -> Block) -> List Block -> List Block
walkTemplate variables f blocks =
    let
        walk : List Block -> List Block -> List Block
        walk blocksTail res =
            case blocksTail of
                [] ->
                    List.reverse res

                head :: tail ->
                    walk tail (Markdown.walk f head :: res)
    in
    walk blocks []


replaceVariables : Variables -> List Block -> List Block
replaceVariables variables blocks =
    let
        replaceVariable : Inline -> Inline
        replaceVariable inline =
            case inline of
                Text s ->
                    case isTemplate s of
                        Just v ->
                            case Dict.get v variables of
                                Just value ->
                                    Text value

                                Nothing ->
                                    inline

                        Nothing ->
                            inline

                _ ->
                    inline

        walkOne : Block -> Block
        walkOne block =
            case block of
                Heading level inlines ->
                    Heading level (List.map replaceVariable inlines)

                Paragraph inlines ->
                    Paragraph (List.map replaceVariable inlines)

                _ ->
                    block

        walker block =
            Markdown.walk walkOne block
    in
    walkTemplate variables walker blocks
