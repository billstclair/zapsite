---------------------------------------------------------------------
--
-- EncodeDecode.elm
-- Zapsite JSON encoders and decoders
-- Copyright (c) 2024 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module ZapSite.EncodeDecode exposing (decodeSavedModel, encodeSavedModel)

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import ZapSite.Types as Types exposing (Page(..), SavedModel)


encodePage : Page -> Value
encodePage page =
    case page of
        MainPage ->
            JE.string "MainPage"

        TemplatePage ->
            JE.string "TemplatePage"


pageDecoder : Decoder Page
pageDecoder =
    JD.string
        |> JD.andThen
            (\s ->
                if s == "TemplatePage" then
                    JD.succeed TemplatePage

                else
                    JD.succeed MainPage
            )


encodeSavedModel : SavedModel -> Value
encodeSavedModel model =
    JE.object
        [ ( "editing", JE.bool model.editing )
        , ( "urlInput", JE.string model.urlInput )
        , ( "url", JE.string model.url )
        , ( "templateNameInput", JE.string model.templateNameInput )
        , ( "templateName", JE.string model.templateName )
        , ( "templateInput", JE.string model.templateInput )
        , ( "template", JE.string model.template )
        , ( "variablesInput", JE.dict identity JE.string model.variablesInput )
        , ( "variables", JE.dict identity JE.string model.variables )
        , ( "page", encodePage model.page )
        , ( "newvar", JE.string model.newvar )
        , ( "newval", JE.string model.newval )
        ]


decodeSavedModel : Value -> Result JD.Error SavedModel
decodeSavedModel value =
    JD.decodeValue savedModelDecoder value


savedModelDecoder : Decoder SavedModel
savedModelDecoder =
    JD.succeed SavedModel
        |> optional "editing" JD.bool True
        |> optional "urlInput" JD.string ""
        |> optional "url" JD.string ""
        |> optional "templateNameInput" JD.string ""
        |> optional "templateName" JD.string ""
        |> optional "templateInput" JD.string ""
        |> optional "template" JD.string ""
        |> optional "variablesInput" (JD.dict JD.string) Dict.empty
        |> optional "variables" (JD.dict JD.string) Dict.empty
        |> optional "page" pageDecoder MainPage
        |> optional "newvar" JD.string ""
        |> optional "newval" JD.string ""
