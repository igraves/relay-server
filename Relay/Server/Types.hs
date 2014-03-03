{-# LANGUAGE OverloadedStrings #-}
module Relay.Server.Types where

import Data.Aeson
import Control.Applicative 
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

type Latitude  = Double
type Longitude = Double

data LocRequest = LocRequest Latitude Longitude deriving Show


instance ToJSON LocRequest where
  toJSON (LocRequest lat long) = object [
                                          "latitude"  .= lat,
                                          "longitude" .= long
                                        ]

instance FromJSON LocRequest where
  parseJSON (Object v) = LocRequest <$>
                            v .: "latitude" <*>
                            v .: "longitude"


type Identity  = BS.ByteString
type Nonce     = BS.ByteString
type Signature = BS.ByteString
type Secret    = BS.ByteString
type Message   = LBS.ByteString
