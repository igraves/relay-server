{-#LANGUAGE OverloadedStrings #-}
module Relay.Server.Auth.HMAC where

import Relay.Server.Types
import Relay.Server.HTTPComponents

import Snap
import Snap.Extras.JSON (writeJSON)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Maybe (isJust,fromJust)
import Data.Digest.Pure.SHA
import Data.Binary (encode)


lookup_secret :: MonadSnap m => Identity -> m (Maybe Secret)
lookup_secret x = return $ Just "secret"


lbsappend a b = LBS.append a b 

unauth_req_response :: Snap ()
unauth_req_response = do
                            rsp <- getResponse 
                            putResponse $ setResponseCode 401 rsp
                            writeJSON [("error"::BS.ByteString,"Request Unauthorized"::BS.ByteString)]

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
                                  sec <- lookup_secret $ fromJust ident
                                  body <- readRequestBody 262144 -- 256kB
                                  case (isJust sec && authorized (fromJust sig) (fromJust nonce) (fromJust sec) body) of
                                            True  -> sm
                                            False -> unauth_req_response
