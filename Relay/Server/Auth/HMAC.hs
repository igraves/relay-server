{-#LANGUAGE OverloadedStrings #-}
module Relay.Server.Auth.HMAC where

import Relay.Server.Types
import Relay.Server.HTTPComponents

import Snap
import Snap.Extras.JSON (writeJSON)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (readInteger)
import Data.Maybe (isJust,fromJust)
import Data.Digest.Pure.SHA
import Data.Binary (encode)
import Control.Monad (liftM)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (lift)
import Data.Word


lookup_secret :: MonadSnap m => Maybe Identity -> m (Maybe Secret)
lookup_secret x = return (x >>= \_ -> Just "secret")

validate_nonce :: MonadSnap m => Nonce -> m Bool
validate_nonce bs = do
                      nonce <- get_nonce
                      let mb = do
                                 (int,_) <- readInteger bs
                                 let nonce' = fromIntegral nonce
                                 return (int >= nonce') -- if the nonce is less than or equal, we're good, hasn't been used yet
                      return $ (isJust mb && fromJust mb) 

get_nonce :: MonadSnap m => m Word64
get_nonce = error "Nonce state undefined"

put_nonce :: MonadSnap m => Nonce -> m ()
put_nonce = error "Nonce put not defined"

lbsappend a b = LBS.append a b 

unauth_req_response :: Snap ()
unauth_req_response = do
                            rsp <- getResponse 
                            putResponse $ setResponseCode 401 rsp
                            writeJSON [("error"::BS.ByteString,"Request Unauthorized"::BS.ByteString)]

authorized :: Signature -> Nonce -> Secret -> Message -> Bool
authorized sig nonce secret message = do 
                let digest  = hmacSha256 (LBS.fromChunks [secret]) $ (LBS.fromChunks [nonce]) `lbsappend` message
                    edigest = Data.Binary.encode digest
                                       in (edigest) == (LBS.fromChunks [sig])

auth_headers :: MonadSnap m => m (Maybe (Identity, Nonce, Signature))
auth_headers = do
            req <- getRequest
            let res = do 
                        ident <- getHeader identityHeader req
                        nonce <- getHeader nonceHeader req 
                        sig   <- getHeader signatureHeader req
                        return (ident,nonce,sig)
            return res 

auth :: Snap () -> Snap ()
auth sm = do
            headers <- auth_headers
            secret <- lookup_secret $ liftM (\(i,_,_) -> i) headers
            case (valid (headers,secret)) of
                      False -> unauth_req_response
                      True  -> do
                                  let (ident,nonce,sig) = fromJust headers
                                      sec = fromJust secret
                                  body <- readRequestBody 262144 -- 256kB
                                  case (authorized sig nonce sec body) of
                                            True  -> sm
                                            False -> unauth_req_response
    where
      valid (a,b) = isJust a && isJust b
