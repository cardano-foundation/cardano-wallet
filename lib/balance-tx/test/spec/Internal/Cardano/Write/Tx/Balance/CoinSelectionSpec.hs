{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.Cardano.Write.Tx.Balance.CoinSelectionSpec
    where

import Prelude

import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO
    )
import Cardano.Wallet.Primitive.Types.UTxO.Gen
    ( genUTxO
    , genUTxOLarge
    , shrinkUTxO
    )
import Data.Function
    ( (&)
    )
import Generics.SOP
    ( NP (..)
    )
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
    ( Spec
    , describe
    , it
    )
import Test.Hspec.Extra
    ( parallel
    )
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
    ( genNonEmpty
    , genericRoundRobinShrink
    , shrinkNonEmpty
    , (<:>)
    , (<@>)
    )
import Test.Utils.Pretty
    ( (====)
    )

import qualified Cardano.Wallet.Primitive.Types.Address.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Coin.Gen as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W.TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenBundle.Gen as W
import qualified Cardano.Wallet.Primitive.Types.TokenMap.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen as W

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

prop_toInternalUTxO_toExternalUTxO :: W.TxIn -> W.TxOut -> Property
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
    genInputs = genNonEmpty ((,) <$> W.genTxIn <*> W.genTxOut)
    genCollateral = listOf ((,) <$> W.genTxIn <*> genTxOutCoin)
    genOutputs = listOf W.genTxOut
    genChange = listOf W.genTokenBundle
    genAssetsToMint = W.genTokenMap
    genAssetsToBurn = W.genTokenMap
    genExtraCoinSource = W.genCoin
    genExtraCoinSink = W.genCoin
    genTxOutCoin =
        W.TxOut <$> W.genAddress <*> (W.TokenBundle.fromCoin <$> W.genCoin)

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
    shrinkInputs = shrinkNonEmpty (liftShrink2 W.shrinkTxIn W.shrinkTxOut)
    shrinkCollateral = shrinkList (liftShrink2 W.shrinkTxIn W.shrinkTxOut)
    shrinkOutputs = shrinkList W.shrinkTxOut
    shrinkChange = shrinkList W.shrinkTokenBundle
    shrinkAssetsToMint = W.shrinkTokenMap
    shrinkAssetsToBurn = W.shrinkTokenMap
    shrinkExtraCoinSource = W.shrinkCoin
    shrinkExtraCoinSink = W.shrinkCoin

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary Selection where
    arbitrary = genSelection
    shrink = shrinkSelection

instance Arbitrary W.TxIn where
    arbitrary = W.genTxIn
    shrink = W.shrinkTxIn

instance Arbitrary W.TxOut where
    arbitrary = W.genTxOut
    shrink = W.shrinkTxOut

instance Arbitrary UTxO where
    arbitrary = oneof
        [ genUTxO
        , genUTxOLarge
        ]
    shrink = shrinkUTxO
