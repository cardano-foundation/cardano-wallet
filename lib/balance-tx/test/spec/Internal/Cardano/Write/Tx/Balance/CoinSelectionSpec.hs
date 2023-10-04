{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.Cardano.Write.Tx.Balance.CoinSelectionSpec
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, shrinkCoin )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundle, shrinkTokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn, shrinkTxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOut, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO, genUTxOLarge, shrinkUTxO )
import Data.Function
    ( (&) )
import Generics.SOP
    ( NP (..) )
import Internal.Cardano.Write.Tx.Balance.CoinSelection
    ( Selection
    , SelectionOf (..)
    , toExternalSelection
    , toExternalUTxO
    , toExternalUTxOMap
    , toInternalSelection
    , toInternalUTxO
    , toInternalUTxOMap
    )
import Test.Hspec
    ( Spec, describe, it )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , liftShrink2
    , listOf
    , oneof
    , property
    , shrinkList
    , (===)
    )
import Test.QuickCheck.Extra
    ( genNonEmpty, genericRoundRobinShrink, shrinkNonEmpty, (<:>), (<@>) )
import Test.Utils.Pretty
    ( (====) )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle

spec :: Spec
spec = describe "Cardano.Wallet.CoinSelectionSpec" $ do

    parallel $ describe
        "Conversion between external (wallet) and internal UTxOs" $ do

        it "prop_toInternalUTxO_toExternalUTxO" $
            prop_toInternalUTxO_toExternalUTxO & property

        it "prop_toInternalUTxOMap_toExternalUTxOMap" $
            prop_toInternalUTxOMap_toExternalUTxOMap & property

    parallel $ describe
        "Conversion between external (wallet) and internal selections" $ do

        it "prop_toInternalSelection_toExternalSelection" $
            prop_toInternalSelection_toExternalSelection & property

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
-- Conversion between external (wallet) and internal selections
--------------------------------------------------------------------------------

prop_toInternalSelection_toExternalSelection :: Selection -> Property
prop_toInternalSelection_toExternalSelection s =
    (toExternalSelection . toInternalSelection id) s ==== s

--------------------------------------------------------------------------------
-- External (wallet) selections
--------------------------------------------------------------------------------

genSelection :: Gen Selection
genSelection = Selection
    <$> genInputs
    <*> genCollateral
    <*> genOutputs
    <*> genChange
    <*> genAssetsToMint
    <*> genAssetsToBurn
    <*> genExtraCoinSource
    <*> genExtraCoinSink
  where
    genInputs = genNonEmpty ((,) <$> genTxIn <*> genTxOut)
    genCollateral = listOf ((,) <$> genTxIn <*> genTxOutCoin)
    genOutputs = listOf genTxOut
    genChange = listOf genTokenBundle
    genAssetsToMint = genTokenMap
    genAssetsToBurn = genTokenMap
    genExtraCoinSource = genCoin
    genExtraCoinSink = genCoin
    genTxOutCoin = TxOut <$> genAddress <*> (TokenBundle.fromCoin <$> genCoin)

shrinkSelection :: Selection -> [Selection]
shrinkSelection = genericRoundRobinShrink
    <@> shrinkInputs
    <:> shrinkCollateral
    <:> shrinkOutputs
    <:> shrinkChange
    <:> shrinkAssetsToMint
    <:> shrinkAssetsToBurn
    <:> shrinkExtraCoinSource
    <:> shrinkExtraCoinSink
    <:> Nil
  where
    shrinkInputs = shrinkNonEmpty (liftShrink2 shrinkTxIn shrinkTxOut)
    shrinkCollateral = shrinkList (liftShrink2 shrinkTxIn shrinkTxOut)
    shrinkOutputs = shrinkList shrinkTxOut
    shrinkChange = shrinkList shrinkTokenBundle
    shrinkAssetsToMint = shrinkTokenMap
    shrinkAssetsToBurn = shrinkTokenMap
    shrinkExtraCoinSource = shrinkCoin
    shrinkExtraCoinSink = shrinkCoin

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Selection where
    arbitrary = genSelection
    shrink = shrinkSelection

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
