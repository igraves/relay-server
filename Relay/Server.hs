{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
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
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
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
                 _pg :: Snaplet Postgres
               }
makeLenses ''App

instance HasPostgres (Handler b App) where
  getPostgresState = with pg get

-------------------------------------------------------------------------------
--Snap Code
-------------------------------------------------------------------------------
site :: Snap ()
site = auth $ writeText "SUCCESSFUL REQUEST!!"

main :: IO ()
main = quickHttpServe site


