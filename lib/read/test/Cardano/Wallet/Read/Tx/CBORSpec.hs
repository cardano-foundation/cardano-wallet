{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Read.Tx.CBORSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Read.Eras
    ( EraValue (..)
    , IsEra
    )
import Cardano.Wallet.Read.Tx
    ( Tx
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( parseTxFromCBOR
    , renderTxToCBOR
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Property
    , property
    , (===)
    )

import qualified Test.Unit.Cardano.Read.Ledger.Tx as Txs

spec :: Spec
spec = describe "Cardano.Read.Ledger.Tx.CBOR" $ do
    describe "TxCBOR roundtrips" $ do
        it "byron tx" $ do
            property $ prop_roundtrip Txs.byronTx
        it "shelley tx" $ do
            property $ prop_roundtrip Txs.shelleyTx
        it "allegra tx" $ do
            property $ prop_roundtrip Txs.allegraTx
        it "mary tx" $ do
            property $ prop_roundtrip Txs.maryTx
        it "alonzo tx" $ do
            property $ prop_roundtrip Txs.alonzoTx
        it "babbage tx" $ do
            property $ prop_roundtrip Txs.babbageTx
        it "conway tx" $ do
            property $ prop_roundtrip Txs.conwayTx

prop_roundtrip :: forall era. IsEra era => Tx era -> Property
prop_roundtrip tx =
    parseTxFromCBOR (renderTxToCBOR (EraValue tx))
        === Right (EraValue tx)
