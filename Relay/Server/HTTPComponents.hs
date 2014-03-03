{-# LANGUAGE OverloadedStrings #-}
module Relay.Server.HTTPComponents where

import Relay.Server.Types
import Data.CaseInsensitive (CI, mk)
import qualified Data.ByteString as BS


signature_header :: CI BS.ByteString
signature_header = mk "relay-signature"

identity_header :: CI BS.ByteString
identity_header  = mk "relay-identity"

nonce_header :: CI BS.ByteString
nonce_header     = mk "relay-nonce"
