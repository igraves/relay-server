{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson
import Control.Applicative 

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

