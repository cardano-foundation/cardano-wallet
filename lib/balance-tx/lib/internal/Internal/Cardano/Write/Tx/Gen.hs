{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
module Internal.Cardano.Write.Tx.Gen
    ( genDatumHash
    )
    where

import Prelude

import Data.Maybe
    ( fromMaybe
    )
import Internal.Cardano.Write.Tx
    ( DatumHash
    , datumHashFromBytes
    )
import Test.QuickCheck
    ( Gen
    , arbitrary
    , vectorOf
    )

import qualified Data.ByteString as BS

genDatumHash :: Gen DatumHash
genDatumHash =
    fromMaybe (error "genDatumHash should always generate valid hashes")
    . datumHashFromBytes
    . BS.pack <$> vectorOf 32 arbitrary
