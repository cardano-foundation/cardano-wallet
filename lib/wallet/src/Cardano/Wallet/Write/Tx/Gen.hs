{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
module Cardano.Wallet.Write.Tx.Gen
    ( genDatumHash
    , genTxOut
    )
    where

import Prelude

import Cardano.Wallet.Write.Tx
    ( DatumHash
    , RecentEra
    , ShelleyLedgerEra
    , TxOut
    , cardanoEraFromRecentEra
    , datumHashFromBytes
    , shelleyBasedEraFromRecentEra
    )
import Data.Maybe
    ( fromMaybe )
import Test.QuickCheck
    ( Gen, arbitrary, vectorOf )

import qualified Cardano.Api.Gen as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Data.ByteString as BS

genDatumHash :: Gen DatumHash
genDatumHash =
    fromMaybe (error "genDatumHash should always generate valid hashes")
    . datumHashFromBytes
    . BS.pack <$> vectorOf 32 arbitrary

genTxOut :: RecentEra era -> Gen (TxOut (ShelleyLedgerEra era))
genTxOut era = Cardano.toShelleyTxOut (shelleyBasedEraFromRecentEra era)
    <$> Cardano.genTxOut (cardanoEraFromRecentEra era)
