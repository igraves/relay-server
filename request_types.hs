{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Request where


import Yesod
import Data.Aeson
import Data.Conduit (runResourceT)
import Data.Functor ((<$>))
import Control.Applicative 
import Data.Digest.Pure.SHA

import Data.Conduit (ResourceT, ($$))
import Data.Conduit.Attoparsec (sinkParser)

import Yesod.Core.Handler
import Control.Exception (SomeException)
import Control.Exception.Lifted (handle)
import Network.Wai (Response, responseLBS)
import Network.HTTP.Types (status400) 
import Data.Text



type Latitude  = Double
type Longitude = Double

data LocUpdate = LocUpdate Latitude Longitude

instance FromJSON LocUpdate where
  parseJSON (Object v) = LocUpdate <$>
                           v .: "latitude" <*>
                           v .: "longitude" 



handler :: (HandlerT a IO) Value
handler = do
                param <- lookupPostParam "Foo"
                return $ toJSON ("error"::Text, "foo"::Text)
               



invalidRequest :: SomeException -> (HandlerT a IO) Value
invalidRequest ex = return $ toJSON ("error"::Text,show ex)

