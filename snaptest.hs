{-# LANGUAGE OverloadedStrings #-}

import Snap
import Control.Exception.Lifted
import Data.Aeson
import Data.Maybe

site :: Snap ()
site = writeText "Heyo!"

main :: IO ()
main = quickHttpServe site


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

