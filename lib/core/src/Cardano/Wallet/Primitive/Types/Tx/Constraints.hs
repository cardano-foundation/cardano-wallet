{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides `TxConstraints` data type.
--
module Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..)
    , txOutputCoinCost
    , txOutputCoinSize
    , txOutputHasValidSize
    , txOutputHasValidTokenQuantities
    , TxSize (..)
    , txSizeDistance
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Control.DeepSeq
    ( NFData (..) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Quiet
    ( Quiet (..) )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap

--------------------------------------------------------------------------------
-- Constraints
--------------------------------------------------------------------------------

-- | Provides an abstract cost and size model for transactions.
--
-- This allows parts of a transaction to be costed (or sized) individually,
-- without having to compute the cost (or size) of an entire transaction.
--
-- Note that the following functions assume one witness is required per input:
--
-- - 'txInputCost'
-- - 'txInputSize'
--
-- This will lead to slight overestimation in the case of UTxOs that share the
-- same payment key.
--
data TxConstraints = TxConstraints
    { txBaseCost :: Coin
      -- ^ The constant cost of an empty transaction.
    , txBaseSize :: TxSize
      -- ^ The constant size of an empty transaction.
    , txInputCost :: Coin
      -- ^ The constant cost of a transaction input, assuming one witness is
      -- required per input.
    , txInputSize :: TxSize
      -- ^ The constant size of a transaction input, assuming one witness is
      -- required per input.
    , txOutputCost :: TokenBundle -> Coin
      -- ^ The variable cost of a transaction output.
    , txOutputSize :: TokenBundle -> TxSize
      -- ^ The variable size of a transaction output.
    , txOutputMaximumSize :: TxSize
      -- ^ The maximum size of a transaction output.
    , txOutputMaximumTokenQuantity :: TokenQuantity
      -- ^ The maximum token quantity that can appear in a transaction output.
    , txOutputMinimumAdaQuantity :: Address -> TokenMap -> Coin
      -- ^ The variable minimum ada quantity of a transaction output.
    , txOutputBelowMinimumAdaQuantity :: Address -> TokenBundle -> Bool
      -- ^ Returns 'True' if the given 'TokenBundle' has a 'Coin' value that is
      -- below the minimum required.
    , txRewardWithdrawalCost :: Coin -> Coin
      -- ^ The variable cost of a reward withdrawal.
    , txRewardWithdrawalSize :: Coin -> TxSize
      -- ^ The variable size of a reward withdrawal.
    , txMaximumSize :: TxSize
      -- ^ The maximum size of a transaction.
    }
    deriving Generic

txOutputCoinCost :: TxConstraints -> Coin -> Coin
txOutputCoinCost constraints = txOutputCost constraints . TokenBundle.fromCoin

txOutputCoinSize :: TxConstraints -> Coin -> TxSize
txOutputCoinSize constraints = txOutputSize constraints . TokenBundle.fromCoin

txOutputHasValidSize :: TxConstraints -> TokenBundle -> Bool
txOutputHasValidSize constraints b =
    txOutputSize constraints b <= txOutputMaximumSize constraints

txOutputHasValidTokenQuantities :: TxConstraints -> TokenMap -> Bool
txOutputHasValidTokenQuantities constraints m =
    TokenMap.maximumQuantity m <= txOutputMaximumTokenQuantity constraints

-- | The size of a transaction, or part of a transaction, in bytes.
--
newtype TxSize = TxSize { unTxSize :: Natural }
    deriving stock (Eq, Ord, Generic)
    deriving Show via (Quiet TxSize)

instance NFData TxSize

instance Semigroup TxSize where
    TxSize a <> TxSize b = TxSize (a + b)

instance Monoid TxSize where
    mempty = TxSize 0

-- | Computes the absolute distance between two transaction size quantities.
--
txSizeDistance :: TxSize -> TxSize -> TxSize
txSizeDistance (TxSize a) (TxSize b)
    | a >= b    = TxSize (a - b)
    | otherwise = TxSize (b - a)
