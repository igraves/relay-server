{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad.IO.Class  (liftIO)
import           Data.Aeson              (Value (Object, String))
import           Data.Aeson              (encode, object, (.=))
import           Data.Aeson.Parser       (json)
import           Data.Conduit            (($$+-))
import           Data.Conduit.Attoparsec (sinkParser)
import           Network.HTTP.Conduit    (RequestBody (RequestBodyLBS),
                                          Response (..), http, method, parseUrl, Request(..), httpLbs,
                                          requestBody, withManager)
import Network.HTTP.Types
import Types
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Digest.Pure.SHA
import Data.CaseInsensitive (CI, mk)
import qualified Data.Binary as B

main :: IO ()
main = withManager $ \manager -> do
    valueBS <- liftIO makeValue
    -- We need to know the size of the request body, so we convert to a
    -- ByteString
    req' <- liftIO $ parseUrl "http://localhost:8000/"
    let req = req' { method = "POST", requestBody = RequestBodyLBS valueBS }
        req'' = sign_request req ("nonce","identity","secret",valueBS)
    res <- withManager $ httpLbs req'' -- manager
    let resValue = responseBody res -- $$+- sinkParser json
    liftIO $ print resValue

-- Application-specific function to make the request value
makeValue = return $ encode $ LocRequest 5.0 6.0

-- Application-specific function to handle the response from the server
handleResponse :: Value -> IO ()
handleResponse = print

type Nonce    = LBS.ByteString
type Identity = LBS.ByteString
type Secret   = LBS.ByteString
type Message  = LBS.ByteString
type SigComponents = (Nonce,Identity,Secret,Message)

signature_header :: CI BS.ByteString
signature_header = mk "relay-signature"

identity_header :: CI BS.ByteString
identity_header = mk "relay-identity"

nonce_header :: CI BS.ByteString
nonce_header = mk "relay-nonce"

sign_request :: Request -> SigComponents -> Request
sign_request req (n,i,s,m) = let m' = LBS.append n m
                                 signature = B.encode $ hmacSha256 s m'
                                 headers = [
                                             (signature_header, signature),
                                             (identity_header, i),
                                             (nonce_header, n)
                                           ]
                                 headers' = map (\(x,y) -> (x, LBS.toStrict y)) headers
                                 oldheaders = requestHeaders req
                              in req {requestHeaders = (oldheaders ++ headers')}
                              
