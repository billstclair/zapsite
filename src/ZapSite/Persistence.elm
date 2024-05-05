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


module ZapSite.Persistence exposing (Config, fsConfig, get, put, s3Config)

{-| Persistence controls how your site data is stored.
-}

import Dict exposing (Dict)
import Task exposing (Task)


type Config
    = S3Config S3ConfigRec
    | FSConfig FSConfigRec


type alias S3ConfigRec =
    { name : String }


type alias FSConfigRec =
    { name : String
    , prefix : String
    }


s3Config : Config
s3Config =
    S3Config { name = "Amazon S3" }


fsConfig : Config
fsConfig =
    FSConfig
        { name = "File System"
        , prefix = "site/"
        }


get : Config -> (Result String String -> msg) -> String -> Cmd msg
get config wrapper key =
    case config of
        S3Config _ ->
            s3Get wrapper key

        FSConfig _ ->
            fsGet wrapper key


s3Get : (Result String String -> msg) -> String -> Cmd msg
s3Get wrapper key =
    Task.perform wrapper <| Task.succeed (Err "s3Get not implemented")


fsGet : (Result String String -> msg) -> String -> Cmd msg
fsGet wrapper key =
    Task.perform wrapper <| Task.succeed (Err "fsGet not implemented")


put : Config -> (Result String String -> msg) -> String -> String -> Cmd msg
put config wrapper key val =
    case config of
        S3Config conf ->
            s3Put conf wrapper key val

        FSConfig conf ->
            fsPut conf wrapper key val


s3Put : S3ConfigRec -> (Result String String -> msg) -> String -> String -> Cmd msg
s3Put config wrapper key val =
    Task.perform wrapper <| Task.succeed (Err "s3Put not implemented.")


fsPut : FSConfigRec -> (Result String String -> msg) -> String -> String -> Cmd msg
fsPut config wrapper key val =
    Task.perform wrapper <| Task.succeed (Err "fsPut not implemented.")
