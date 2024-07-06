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


module ZapSite.Template exposing (render)

{-| Templates can be filled in from variable bindings.
-}

import Dict exposing (Dict)
import Html exposing (Html, text)
import Markdown
import Markdown.Block as Markdown exposing (Block(..), Inline(..))
import Markdown.Html
import Markdown.Parser as Markdown
import Markdown.Renderer as Markdown
import Regex exposing (Regex)
import Result as Result
import Url
import ZapSite.Types as Types exposing (Variables)


{-| Render a markdown string with template variables.
-}
render : String -> Variables -> List (Html msg)
render markdown variables =
    case parseMarkdown markdown of
        Err errmsg ->
            [ text <| "Markdown parsing error: " ++ errmsg ]

        Ok blocks ->
            let
                finalBlocks =
                    replaceVariables variables blocks
            in
            case Markdown.render Markdown.defaultHtmlRenderer finalBlocks of
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


lookupWithSuffix : String -> String -> Variables -> String
lookupWithSuffix string suffix variables =
    let
        ( res, _ ) =
            replaceTemplatesWithSuffix variables suffix string
    in
    res


lookup : String -> Variables -> String
lookup string variables =
    replaceTemplates variables string


lookupUrl : String -> Variables -> String
lookupUrl string variables =
    replaceTemplates variables (percentDecode string)
        |> percentEncode


percentDecode : String -> String
percentDecode string =
    String.replace "%7D" "}" <|
        String.replace "%7B" "{" string


percentEncode : String -> String
percentEncode string =
    String.replace "}" "%7D" <|
        String.replace "{" "%7B" string


{-| TODO: Loop on text replace and parse until no change.
This allows variable values to have other variables, or more markdown.
-}
replaceVariables : Variables -> List Block -> List Block
replaceVariables variables blocks =
    let
        replaceVariable : Inline -> Inline
        replaceVariable inline =
            case inline of
                HtmlInline block ->
                    case block of
                        Markdown.HtmlElement tag attributes children ->
                            HtmlInline <|
                                Markdown.HtmlElement tag attributes <|
                                    List.map walkOne children

                        _ ->
                            inline

                Link url maybe inlines ->
                    Link (lookupUrl url variables) maybe <|
                        List.map replaceVariable inlines

                Image url maybe inlines ->
                    Image (lookup url variables) maybe <|
                        List.map replaceVariable inlines

                Emphasis inlines ->
                    Emphasis <| List.map replaceVariable inlines

                Strong inlines ->
                    Strong <| List.map replaceVariable inlines

                Strikethrough inlines ->
                    Strikethrough <| List.map replaceVariable inlines

                CodeSpan s ->
                    CodeSpan <| lookup s variables

                Text s ->
                    Text <| replaceTemplates variables s

                _ ->
                    inline

        replaceHeader { label, alignment } =
            { label = List.map replaceVariable label
            , alignment = alignment
            }

        replaceRows : List (List (List Inline)) -> List (List (List Inline))
        replaceRows rows =
            case rows of
                [ row ] ->
                    duplicateRow row

                _ ->
                    replaceAllRows rows []

        replaceAllVariables : List (List Inline) -> List (List Inline)
        replaceAllVariables cols =
            let
                replaceOneCol col =
                    List.map replaceVariable col
            in
            List.map replaceOneCol cols

        replaceAllRows : List (List (List Inline)) -> List (List (List Inline)) -> List (List (List Inline))
        replaceAllRows rows res =
            case rows of
                [] ->
                    List.reverse res

                cols :: tail ->
                    replaceAllRows tail
                        (replaceAllVariables cols :: res)

        replaceAllRowVariables : String -> List (List Inline) -> ( List (List Inline), Bool )
        replaceAllRowVariables suffix cols =
            let
                colLoop : List Inline -> List Inline -> Bool -> ( List Inline, Bool )
                colLoop colsTail colsRes isNewRow =
                    case colsTail of
                        [] ->
                            ( List.reverse colsRes, isNewRow )

                        col :: colsRest ->
                            let
                                ( newcol, isnew ) =
                                    replaceRowVariable suffix col
                            in
                            colLoop colsRest
                                (newcol :: colsRes)
                                (isNewRow || isnew)

                loop : List (List Inline) -> List (List Inline) -> Bool -> ( List (List Inline), Bool )
                loop colsTail res isNewRow =
                    case colsTail of
                        [] ->
                            ( List.reverse res, isNewRow )

                        col :: colsRest ->
                            let
                                ( newcol, isnew ) =
                                    colLoop col [] isNewRow
                            in
                            loop colsRest (newcol :: res) isnew
            in
            loop cols [] False

        replaceRowVariable : String -> Inline -> ( Inline, Bool )
        replaceRowVariable suffix inline =
            case inline of
                Text s ->
                    let
                        ( news, isNewRow ) =
                            replaceTemplatesWithSuffix variables suffix s
                    in
                    ( Text news, isNewRow )

                _ ->
                    ( inline, False )

        duplicateRow : List (List Inline) -> List (List (List Inline))
        duplicateRow cols =
            let
                tryN n res =
                    let
                        nstr =
                            String.fromInt n

                        ( replacedCols, isNewRow ) =
                            replaceAllRowVariables nstr cols
                    in
                    if not isNewRow then
                        List.reverse res

                    else
                        tryN (n + 1) (replacedCols :: res)
            in
            tryN 1 []

        walkOne : Block -> Block
        walkOne block =
            case block of
                Heading level inlines ->
                    Heading level (List.map replaceVariable inlines)

                Paragraph inlines ->
                    Paragraph (List.map replaceVariable inlines)

                Table headers rows ->
                    Table (List.map replaceHeader headers) (replaceRows rows)

                _ ->
                    block

        walker block =
            Markdown.walk walkOne block
    in
    walkTemplate variables walker blocks


templateRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "{[^}]*}"


replaceTemplates : Variables -> String -> String
replaceTemplates variables string =
    let
        replace matches res =
            case matches of
                [] ->
                    res

                { index, match } :: rest ->
                    let
                        len =
                            String.length match

                        var =
                            String.slice (index + 1) (index + len - 1) res
                    in
                    let
                        newres =
                            case Dict.get var variables of
                                Just val ->
                                    String.left index res
                                        ++ val
                                        ++ String.dropLeft (index + len) res

                                Nothing ->
                                    res
                    in
                    replace rest newres
    in
    replace (List.reverse <| Regex.find templateRegex string) string


replaceTemplatesWithSuffix : Variables -> String -> String -> ( String, Bool )
replaceTemplatesWithSuffix variables suffix string =
    let
        replace matches res isRow =
            case matches of
                [] ->
                    ( res, isRow )

                { index, match } :: rest ->
                    let
                        len =
                            String.length match

                        var =
                            String.slice (index + 1) (index + len - 1) res
                    in
                    let
                        ( newres, newIsRow ) =
                            case Dict.get var variables of
                                Just val ->
                                    ( String.left index res
                                        ++ val
                                        ++ String.dropLeft (index + len) res
                                    , isRow
                                    )

                                Nothing ->
                                    case Dict.get (var ++ suffix) variables of
                                        Just val ->
                                            ( String.left index res
                                                ++ val
                                                ++ String.dropLeft (index + len) res
                                            , True
                                            )

                                        Nothing ->
                                            ( res, isRow )
                    in
                    replace rest newres newIsRow
    in
    replace (List.reverse <| Regex.find templateRegex string) string False
