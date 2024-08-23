{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Read.Tx.TxIdSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Read.Eras
    ( IsEra
    )
import Cardano.Wallet.Read.Hash
    ( hashFromBytesAsHex
    )
import Cardano.Wallet.Read.Tx
    ( Tx
    )
import Cardano.Wallet.Read.Tx.TxId
    ( TxId
    , getTxId
    , txIdFromHash
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Property
    , (===)
    )

import qualified Test.Unit.Cardano.Read.Ledger.Tx as Txs

spec :: Spec
spec =
    describe "TxId golden tests" $ do
        it "byron tx" $ do
            prop_matches_TxId Txs.byronTx byronTxId
        it "shelley tx" $
            prop_matches_TxId Txs.shelleyTx shelleyTxId
        it "allegra tx" $
            prop_matches_TxId Txs.allegraTx allegraTxId
        it "mary tx" $
            prop_matches_TxId Txs.maryTx maryTxId
        it "mary tx, long output" $
            prop_matches_TxId Txs.maryTxLongOutput maryTxLongOutputId
        it "alonzo tx" $
            prop_matches_TxId Txs.alonzoTx alonzoTxId
        it "babbage tx" $
            prop_matches_TxId Txs.babbageTx babbageTxId
        it "conway tx" $
            prop_matches_TxId Txs.conwayTx conwayTxId

prop_matches_TxId :: forall era. IsEra era => Tx era -> TxId -> Property
prop_matches_TxId tx txId = getTxId tx === txId

byronTxId :: TxId
byronTxId = unsafeTxIdFromHex
    "376293e16cf87c377dce58ea6efd256276f86454fc13d390f673db789dcd7104"

shelleyTxId :: TxId
shelleyTxId = unsafeTxIdFromHex
    "ca011f22d07b97ee17f6f2e2ef568b9521791608169425e92993c8c6e5541d79"

allegraTxId :: TxId
allegraTxId = unsafeTxIdFromHex
    "ca011f22d07b97ee17f6f2e2ef568b9521791608169425e92993c8c6e5541d79"

maryTxId :: TxId
maryTxId = unsafeTxIdFromHex
    "ca011f22d07b97ee17f6f2e2ef568b9521791608169425e92993c8c6e5541d79"

maryTxLongOutputId :: TxId
maryTxLongOutputId = unsafeTxIdFromHex
    "369826b2f60b443d27966d222fa5bedc6b900326b77a09f15473d77f5ef2871c"

alonzoTxId :: TxId
alonzoTxId = unsafeTxIdFromHex
    "ca011f22d07b97ee17f6f2e2ef568b9521791608169425e92993c8c6e5541d79"

babbageTxId :: TxId
babbageTxId = unsafeTxIdFromHex
    "1b81e33957d8b96e17a142ae06004213d25dc6abed0fc3949a16f3a96eb2a093"

conwayTxId :: TxId
conwayTxId = unsafeTxIdFromHex
    "4d4bb2d0c9a63de35e6b44f18f1c5a0b83c495ea2220b9923d854d1e10b35335"

-- | Parse a hex-encoded 'TxId'
unsafeTxIdFromHex :: ByteString -> TxId
unsafeTxIdFromHex =
    txIdFromHash
    . fromMaybe (error "unsafeTxIdFromHex: invalid hex length")
    . hashFromBytesAsHex
