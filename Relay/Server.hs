{-# LANGUAGE OverloadedStrings #-}

module Relay.Server where

import Relay.Server.Types
import Relay.Server.Auth.HMAC (auth)

import Snap
import Control.Exception.Lifted
import Data.Aeson
import Data.Maybe

import Debug.Trace

site :: Snap ()
site = auth $ writeText "SUCCESSFUL REQUEST!!"

main :: IO ()
main = quickHttpServe site


