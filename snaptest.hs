{-# LANGUAGE OverloadedStrings #-}

import Snap
import Types
import Control.Exception.Lifted
import Data.Aeson
import Data.Maybe
import Snap.Extras.JSON
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.CaseInsensitive (CI, mk)
import Data.Maybe (isJust)
import Data.Digest.Pure.SHA
import Data.Binary (encode)

import Debug.Trace

site :: Snap ()
--site = do
--         json <- reqJSON :: Snap LocRequest
--         liftIO $ putStrLn $ show json
--         writeJSON $ LocRequest 5.0 5.0 

site = auth $ do
                writeText "SUCCESSFUL REQUEST!!"

main :: IO ()
main = quickHttpServe site

signature_header :: CI BS.ByteString
signature_header = mk "relay-signature"

identity_header :: CI BS.ByteString
identity_header = mk "relay-identity"

nonce_header :: CI BS.ByteString
nonce_header = mk "relay-nonce"

unauth_req_response :: Snap ()
unauth_req_response = do
                            rsp <- getResponse 
                            putResponse $ setResponseCode 401 rsp
                            writeJSON [("error"::BS.ByteString,"Request Unauthorized"::BS.ByteString)]


lookup_secret :: Identity -> Maybe Secret 
lookup_secret x = Just "secret"

type Identity  = BS.ByteString
type Nonce     = BS.ByteString
type Signature = BS.ByteString
type Secret    = BS.ByteString
type Message   = LBS.ByteString

lbsappend a b = LBS.append a b 

authorized :: Signature -> Nonce -> Secret -> Message -> Bool
authorized sig nonce secret message = let digest  = hmacSha256 (LBS.fromChunks [secret]) $ (LBS.fromChunks [nonce]) `lbsappend` message
                                          edigest = Data.Binary.encode digest
                                       in (edigest) == (LBS.fromChunks [sig])

auth :: Snap () -> Snap ()
auth sm = do
            req <- getRequest
            let ident = getHeader identity_header req
                nonce = getHeader nonce_header req 
                sig   = getHeader signature_header req
            case (all isJust [ident,nonce,sig]) of
                      False -> unauth_req_response
                      True  -> do
                                  let sec = lookup_secret $ fromJust ident
                                  body <- readRequestBody 262144 -- 256kB
                                  case (isJust sec && authorized (fromJust sig) (fromJust nonce) (fromJust sec) body) of
                                            True  -> sm
                                            False -> unauth_req_response
                                  
                                  
      
