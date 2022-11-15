{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx
    , genTxOut
    , genTxOutCoin
    , genTxOutTokenBundle
    , genTxScriptValidity
    , shrinkTx
    , shrinkTxOut
    , shrinkTxOutCoin
    , shrinkTxScriptValidity
    , TxWithoutId (..)
    , txWithoutIdToTx
    )
    where

import Prelude

import Cardano.Wallet.Gen
    ( genNestedTxMetadata, shrinkTxMetadata )
import Cardano.Wallet.Primitive.Types.Address.Gen
    ( genAddress, shrinkAddress )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive, shrinkCoinPositive )
import Cardano.Wallet.Primitive.Types.Hash
    ( mockHash )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount.Gen
    ( genRewardAccount, shrinkRewardAccount )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange, shrinkTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genAssetIdLargeRange )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxIn (..), TxMetadata (..), TxOut (..), TxScriptValidity (..) )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( coinIsValidForTxOut
    , txOutMaxCoin
    , txOutMaxTokenQuantity
    , txOutMinCoin
    , txOutMinTokenQuantity
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn, shrinkTxIn )
import Control.Monad
    ( replicateM )
import Data.Map.Strict
    ( Map )
import Generics.SOP
    ( NP (..) )
import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( Gen
    , choose
    , frequency
    , liftArbitrary
    , liftArbitrary2
    , liftShrink
    , liftShrink2
    , listOf
    , listOf1
    , oneof
    , shrinkList
    , shrinkMapBy
    , suchThat
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Extra
    ( chooseNatural
    , genMapWith
    , genericRoundRobinShrink
    , shrinkInterleaved
    , shrinkMapWith
    , shrinkNatural
    , (<:>)
    , (<@>)
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Data.List as L

--------------------------------------------------------------------------------
-- Transactions generated according to the size parameter
--------------------------------------------------------------------------------

genTx :: Gen Tx
genTx = txWithoutIdToTx <$> genTxWithoutId

shrinkTx :: Tx -> [Tx]
shrinkTx = shrinkMapBy txWithoutIdToTx txToTxWithoutId shrinkTxWithoutId

data TxWithoutId = TxWithoutId
    { fee :: !(Maybe Coin)
    , resolvedInputs :: ![(TxIn, Coin)]
    , resolvedCollateralInputs :: ![(TxIn, Coin)]
    , outputs :: ![TxOut]
    , collateralOutput :: !(Maybe TxOut)
    , metadata :: !(Maybe TxMetadata)
    , withdrawals :: !(Map RewardAccount Coin)
    , scriptValidity :: !(Maybe TxScriptValidity)
    }
    deriving (Eq, Generic, Ord, Show)

genTxWithoutId :: Gen TxWithoutId
genTxWithoutId = TxWithoutId
    <$> liftArbitrary genCoinPositive
    <*> listOf1 (liftArbitrary2 genTxIn genCoinPositive)
    <*> listOf1 (liftArbitrary2 genTxIn genCoinPositive)
    <*> listOf genTxOut
    <*> liftArbitrary genTxOut
    <*> liftArbitrary genNestedTxMetadata
    <*> genMapWith genRewardAccount genCoinPositive
    <*> liftArbitrary genTxScriptValidity

shrinkTxWithoutId :: TxWithoutId -> [TxWithoutId]
shrinkTxWithoutId = genericRoundRobinShrink
    <@> liftShrink shrinkCoinPositive
    <:> shrinkList (liftShrink2 shrinkTxIn shrinkCoinPositive)
    <:> shrinkList (liftShrink2 shrinkTxIn shrinkCoinPositive)
    <:> shrinkList shrinkTxOut
    <:> liftShrink shrinkTxOut
    <:> liftShrink shrinkTxMetadata
    <:> shrinkMapWith shrinkRewardAccount shrinkCoinPositive
    <:> liftShrink shrinkTxScriptValidity
    <:> Nil

txWithoutIdToTx :: TxWithoutId -> Tx
txWithoutIdToTx tx@TxWithoutId {..}
    = Tx {txId = mockHash tx, txCBOR = Nothing, ..}

txToTxWithoutId :: Tx -> TxWithoutId
txToTxWithoutId Tx {..} = TxWithoutId {..}

genTxScriptValidity :: Gen TxScriptValidity
genTxScriptValidity = genericArbitrary

shrinkTxScriptValidity :: TxScriptValidity -> [TxScriptValidity]
shrinkTxScriptValidity = genericShrink

--------------------------------------------------------------------------------
-- Transaction outputs generated according to the size parameter
--------------------------------------------------------------------------------

genTxOut :: Gen TxOut
genTxOut = TxOut
    <$> genAddress
    <*> genTokenBundleSmallRange `suchThat` tokenBundleHasNonZeroCoin

shrinkTxOut :: TxOut -> [TxOut]
shrinkTxOut (TxOut a b) = uncurry TxOut <$> shrinkInterleaved
    (a, shrinkAddress)
    (b, filter tokenBundleHasNonZeroCoin . shrinkTokenBundleSmallRange)

tokenBundleHasNonZeroCoin :: TokenBundle -> Bool
tokenBundleHasNonZeroCoin b = TokenBundle.getCoin b /= Coin 0

--------------------------------------------------------------------------------
-- Coins chosen from the full range allowed in a transaction output
--------------------------------------------------------------------------------

-- | Generates coins across the full range allowed in a transaction output.
--
-- This generator has a slight bias towards the limits of the range, but
-- otherwise generates values uniformly across the whole range.
--
-- This can be useful when testing roundtrip conversions between different
-- types.
--
genTxOutCoin :: Gen Coin
genTxOutCoin = frequency
    [ (1, pure txOutMinCoin)
    , (1, pure txOutMaxCoin)
    , (8, Coin.fromNatural <$> chooseNatural
        ( Coin.toNatural txOutMinCoin + 1
        , Coin.toNatural txOutMaxCoin - 1
        )
      )
    ]

shrinkTxOutCoin :: Coin -> [Coin]
shrinkTxOutCoin
    = L.filter coinIsValidForTxOut
    . shrinkMapBy Coin.fromNatural Coin.toNatural shrinkNatural

--------------------------------------------------------------------------------
-- Token bundles with fixed numbers of assets.
--
-- Values are chosen from across the full range of values permitted within
-- transaction outputs.
--
-- Policy identifiers, asset names, token quantities are all allowed to vary.
--------------------------------------------------------------------------------

genTxOutTokenBundle :: Int -> Gen TokenBundle
genTxOutTokenBundle fixedAssetCount
    = TokenBundle.fromFlatList
        <$> genTxOutCoin
        <*> replicateM fixedAssetCount genAssetQuantity
  where
    genAssetQuantity = (,)
        <$> genAssetIdLargeRange
        <*> genTokenQuantity
    genTokenQuantity = integerToTokenQuantity <$> oneof
        [ pure $ tokenQuantityToInteger txOutMinTokenQuantity
        , pure $ tokenQuantityToInteger txOutMaxTokenQuantity
        , choose
            ( tokenQuantityToInteger txOutMinTokenQuantity + 1
            , tokenQuantityToInteger txOutMaxTokenQuantity - 1
            )
        ]
      where
        tokenQuantityToInteger :: TokenQuantity -> Integer
        tokenQuantityToInteger = fromIntegral . unTokenQuantity

        integerToTokenQuantity :: Integer -> TokenQuantity
        integerToTokenQuantity = TokenQuantity . fromIntegral
