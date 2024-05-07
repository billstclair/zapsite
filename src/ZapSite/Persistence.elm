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


module ZapSite.Persistence exposing (Config, fsConfig, get, localConfig, put, s3Config)

{-| Persistence controls how your site data is stored.
-}

import Dict exposing (Dict)
import Json.Encode as JE exposing (Value)
import Task exposing (Task)


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


put : Config msg -> String -> String -> Cmd msg
put config key val =
    case config of
        S3Config conf ->
            s3Put conf key val

        FSConfig conf ->
            fsPut conf key val

        LocalConfig conf ->
            localPut conf key val


s3Put : S3ConfigRec -> String -> String -> Cmd msg
s3Put config key val =
    Cmd.none


fsPut : FSConfigRec -> String -> String -> Cmd msg
fsPut config key val =
    Cmd.none


localPut : LocalConfigRec msg -> String -> String -> Cmd msg
localPut { prefix, putter } key val =
    let
        v =
            if val == "" then
                Nothing

            else
                Just <| JE.string val
    in
    putter (prefix ++ key) v
