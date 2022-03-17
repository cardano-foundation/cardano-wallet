{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelectionSpec
    where

import Prelude

import Cardano.Wallet.CoinSelection
    ( toExternalUTxO, toExternalUTxOMap, toInternalUTxO, toInternalUTxOMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxIn, genTxOut, shrinkTxIn, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, genUTxOLarge, shrinkUTxO )
import Data.Function
    ( (&) )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..), Property, oneof, property, (===) )

spec :: Spec
spec = describe "Cardano.Wallet.CoinSelectionSpec" $ do

    parallel $ describe
        "Conversion between external (wallet) and internal UTxOs" $ do

        it "prop_toInternalUTxO_toExternalUTxO" $
            prop_toInternalUTxO_toExternalUTxO & property

        it "prop_toInternalUTxOMap_toExternalUTxOMap" $
            prop_toInternalUTxOMap_toExternalUTxOMap & property

--------------------------------------------------------------------------------
-- Conversion between external (wallet) and internal UTxOs
--------------------------------------------------------------------------------

prop_toInternalUTxO_toExternalUTxO :: TxIn -> TxOut -> Property
prop_toInternalUTxO_toExternalUTxO i o =
    (toExternalUTxO . toInternalUTxO) (i, o) === (i, o)

prop_toInternalUTxOMap_toExternalUTxOMap :: UTxO -> Property
prop_toInternalUTxOMap_toExternalUTxOMap u =
    (toExternalUTxOMap . toInternalUTxOMap) u === u

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary TxIn where
    arbitrary = genTxIn
    shrink = shrinkTxIn

instance Arbitrary TxOut where
    arbitrary = genTxOut
    shrink = shrinkTxOut

instance Arbitrary UTxO where
    arbitrary = oneof
        [ genUTxO
        , genUTxOLarge
        ]
    shrink = shrinkUTxO
