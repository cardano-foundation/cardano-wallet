{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.DB.Store.UTxOHistory.TxOutCBORSpec
    ( spec
    )
where

import Prelude

import Cardano.Wallet.DB.Arbitrary
    (
    )
import Cardano.Wallet.DB.Store.UTxOHistory.TxOutCBOR
    ( deserializeTxOut
    , serializeTxOut
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Testable (property)
    , (===)
    )

spec :: Spec
spec = do
    describe "TxOutBinary" $ do
        it "can serialize and deserialize TxOut"
            $ property
            $ \txOut ->
                deserializeTxOut (serializeTxOut txOut) === Right txOut
