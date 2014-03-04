{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Relay.Server where

-------------------------------------------------------------------------------
--Explicit Imports
-------------------------------------------------------------------------------


import           Snap
import           Data.ByteString
import           Control.Lens

import           Relay.Server.JSON
import           Relay.Server.Auth.HMAC (auth)

-------------------------------------------------------------------------------
--Storage Code
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--Application State
-------------------------------------------------------------------------------
data App = App {
               }

-------------------------------------------------------------------------------
--Snap Code
-------------------------------------------------------------------------------
site :: Snap ()
site = auth $ writeText "SUCCESSFUL REQUEST!!"

main :: IO ()
main = quickHttpServe site


