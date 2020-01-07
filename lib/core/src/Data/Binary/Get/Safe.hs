-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Extra helpers for safe decoding of binary data.

module Data.Binary.Get.Safe
    ( eitherRunGet
    ) where

import Prelude

import Data.Binary.Get
    ( Get, runGetOrFail )

import qualified Data.ByteString.Lazy as BL


-- | A safe version of 'runGet' which doesn't throw on error.
eitherRunGet
    :: Get a
    -> BL.ByteString
    -> Either String a
eitherRunGet decoder bytes = case runGetOrFail decoder bytes of
    Right (_, _, a) -> Right a
    Left  (_, _, e) -> Left  e
