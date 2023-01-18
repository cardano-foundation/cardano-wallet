{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTx
    , genTxScriptValidity
    , shrinkTx
    , shrinkTxScriptValidity
    , TxWithoutId (..)
    , txWithoutIdToTx
    )
    where

import Prelude

import Cardano.Wallet.Gen
    ( genNestedTxMetadata, shrinkTxMetadata )
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
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxMetadata (..), TxScriptValidity (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn.Gen
    ( genTxIn, shrinkTxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOut, shrinkTxOut )
import Data.Map.Strict
    ( Map )
import Generics.SOP
    ( NP (..) )
import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( Gen
    , liftArbitrary
    , liftArbitrary2
    , liftShrink
    , liftShrink2
    , listOf
    , listOf1
    , shrinkList
    , shrinkMapBy
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Extra
    ( genMapWith, genericRoundRobinShrink, shrinkMapWith, (<:>), (<@>) )

--------------------------------------------------------------------------------
-- Transactions generated according to the size parameter
--------------------------------------------------------------------------------

genTx :: Gen Tx
genTx = txWithoutIdToTx <$> genTxWithoutId

shrinkTx :: Tx -> [Tx]
shrinkTx = shrinkMapBy txWithoutIdToTx txToTxWithoutId shrinkTxWithoutId

data TxWithoutId = TxWithoutId
    { fee :: !(Maybe Coin)
    , resolvedInputs :: ![(TxIn, Maybe TxOut)]
    , resolvedCollateralInputs :: ![(TxIn, Maybe TxOut)]
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
    <*> listOf1 (liftArbitrary2 genTxIn (pure Nothing))
    <*> listOf1 (liftArbitrary2 genTxIn (pure Nothing))
    <*> listOf genTxOut
    <*> liftArbitrary genTxOut
    <*> liftArbitrary genNestedTxMetadata
    <*> genMapWith genRewardAccount genCoinPositive
    <*> liftArbitrary genTxScriptValidity

shrinkTxWithoutId :: TxWithoutId -> [TxWithoutId]
shrinkTxWithoutId = genericRoundRobinShrink
    <@> liftShrink shrinkCoinPositive
    <:> shrinkList (liftShrink2 shrinkTxIn (liftShrink shrinkTxOut))
    <:> shrinkList (liftShrink2 shrinkTxIn (liftShrink shrinkTxOut))
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
