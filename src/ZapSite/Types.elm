---------------------------------------------------------------------
--
-- Types.elm
-- Zapsite types
-- Copyright (c) 2024 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE
--
----------------------------------------------------------------------


module ZapSite.Types exposing (Page(..), SavedModel, Variables, emptyVariables)

{-| Types for ZapSite
-}

import Dict exposing (Dict)


{-| A key/value store
-}
type alias Variables =
    Dict String String


{-| The empty key/value store
-}
emptyVariables : Variables
emptyVariables =
    Dict.empty


type Page
    = MainPage
    | TemplatePage


type alias SavedModel =
    { input : String
    , variables : Variables
    , page : Page
    , newvar : String
    , newval : String
    }
