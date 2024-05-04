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


type Config
    = S3Config S3Config
    | FSConfig FSCOnfig


type alias S3Config =
    { name : String }


type alias FSConfig =
    { name : String }


s3Config : Config
s3Config =
    S3Config { name = "Amazon S3" }


fsConfig : Config
fsConfig =
    FSCOnfig { name = "File System" }


get : Config -> String -> Cmd msg
get config key =
    case config of
        S3Config _ ->
            s3Get key

        FSConfig _ ->
            fsGet key


s3Get : String -> Cmd msg
s3Get key =
    Task.succeed key


fsGet : String -> Cmd msg
fsGet key =
    Task.succeed key
