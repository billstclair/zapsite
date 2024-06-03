----------------------------------------------------------------------
--
-- ZapSite/Persistence.elm
-- Choices for persisting content.
-- Copyright (C) 2024 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module ZapSite.Persistence exposing
    ( Config
    , UrlBindings
    , decodeTemplate
    , decodeUrlBindings
    , fsConfig
    , get
    , getTemplate
    , getUrlBindings
    , localConfig
    , put
    , putTemplate
    , putUrlBindings
    , s3Config
    , unprefixTemplateKey
    , unprefixUrlBindingsKey
    )

{-| Persistence controls how your site data is stored.
-}

import Dict exposing (Dict)
import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as DP exposing (custom, hardcoded, optional, required)
import Json.Encode as JE exposing (Value)
import Task exposing (Task)
import ZapSite.Types as Types exposing (Variables)


{-| Config instances are created with `s3Config`, `fsConfig`, and `localConfig`.
-}
type Config msg
    = S3Config S3ConfigRec
    | FSConfig FSConfigRec
    | LocalConfig (LocalConfigRec msg)


type alias S3ConfigRec =
    { name : String }


type alias FSConfigRec =
    { name : String
    , prefix : String
    }


type alias LocalConfigRec msg =
    { name : String
    , prefix : String
    , getter : String -> Cmd msg
    , putter : String -> Maybe Value -> Cmd msg
    }


s3Config : Config msg
s3Config =
    S3Config { name = "Amazon S3" }


fsConfig : Config msg
fsConfig =
    FSConfig
        { name = "File System"
        , prefix = "ZapSite/"
        }


localConfig : (String -> Cmd msg) -> (String -> Maybe Value -> Cmd msg) -> Config msg
localConfig getter putter =
    LocalConfig
        { name = "LocalConfig"
        , prefix = "ZapSite/"
        , getter = getter
        , putter = putter
        }


get : Config msg -> String -> Cmd msg
get config key =
    case config of
        S3Config rec ->
            s3Get rec key

        FSConfig rec ->
            fsGet rec key

        LocalConfig rec ->
            localGet rec key


s3Get : S3ConfigRec -> String -> Cmd msg
s3Get rec key =
    Cmd.none


fsGet : FSConfigRec -> String -> Cmd msg
fsGet rec key =
    Cmd.none


localGet : LocalConfigRec msg -> String -> Cmd msg
localGet { prefix, getter } key =
    getter (prefix ++ key)


put : Config msg -> String -> Maybe Value -> Cmd msg
put config key val =
    case config of
        S3Config conf ->
            s3Put conf key val

        FSConfig conf ->
            fsPut conf key val

        LocalConfig conf ->
            localPut conf key val


s3Put : S3ConfigRec -> String -> Maybe Value -> Cmd msg
s3Put config key val =
    Cmd.none


fsPut : FSConfigRec -> String -> Maybe Value -> Cmd msg
fsPut config key val =
    Cmd.none


localPut : LocalConfigRec msg -> String -> Maybe Value -> Cmd msg
localPut { prefix, putter } key val =
    putter (prefix ++ key) val



----------------------------------------------------------------------
---
--- The "database"
---
----------------------------------------------------------------------


templatePrefix : String
templatePrefix =
    "=template=/"


templatePrefixLength : Int
templatePrefixLength =
    String.length templatePrefix


getTemplate : Config msg -> String -> Cmd msg
getTemplate config templateName =
    get config <| templatePrefix ++ templateName


putTemplate : Config msg -> String -> String -> Cmd msg
putTemplate config templateName value =
    let
        v =
            if value == "" then
                Nothing

            else
                Just <| JE.string value
    in
    put config (templatePrefix ++ templateName) v


unprefixTemplateKey : String -> Maybe String
unprefixTemplateKey key =
    if String.left templatePrefixLength key == templatePrefix then
        Just <| String.dropLeft templatePrefixLength key

    else
        Nothing


decodeTemplate : Value -> Result JD.Error String
decodeTemplate value =
    JD.decodeValue JD.string value


type alias UrlBindings =
    { templateName : String
    , variables : Variables
    }


encodeUrlBindings : String -> Variables -> Value
encodeUrlBindings templateName variables =
    JE.object
        [ ( "templateName", JE.string templateName )
        , ( "variables", JE.dict identity JE.string variables )
        ]


urlBindingsDecoder : Decoder UrlBindings
urlBindingsDecoder =
    JD.succeed UrlBindings
        |> required "templateName" JD.string
        |> required "variables" (JD.dict JD.string)


urlBindingsPrefix : String
urlBindingsPrefix =
    "=urlBindings=/"


urlBindingsPrefixLength : Int
urlBindingsPrefixLength =
    String.length urlBindingsPrefix


getUrlBindings : Config msg -> String -> Cmd msg
getUrlBindings config url =
    get config <| urlBindingsPrefix ++ url


putUrlBindings : Config msg -> String -> String -> Variables -> Cmd msg
putUrlBindings config url templateName variables =
    let
        v =
            encodeUrlBindings templateName variables
                |> Just
    in
    put config (urlBindingsPrefix ++ url) v


unprefixUrlBindingsKey : String -> Maybe String
unprefixUrlBindingsKey key =
    if String.left urlBindingsPrefixLength key == urlBindingsPrefix then
        Just <| String.dropLeft urlBindingsPrefixLength key

    else
        Nothing


decodeUrlBindings : Value -> Result JD.Error UrlBindings
decodeUrlBindings value =
    JD.decodeValue urlBindingsDecoder value
