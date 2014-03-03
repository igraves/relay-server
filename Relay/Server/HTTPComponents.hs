{-# LANGUAGE OverloadedStrings #-}
module Relay.Server.HTTPComponents where

import Relay.Server.Types
import Data.CaseInsensitive (CI, mk)
import qualified Data.ByteString as BS


signatureHeader :: CI BS.ByteString
signatureHeader = mk "relay-signature"

identityHeader :: CI BS.ByteString
identityHeader  = mk "relay-identity"

nonceHeader :: CI BS.ByteString
nonceHeader     = mk "relay-nonce"
