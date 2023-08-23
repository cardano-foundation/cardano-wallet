{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
--
-- This module provides types and functions that relate to constraints on the
-- sizes and costs of transactions and their constituent components.
--
module Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxConstraints (..)
    , txOutputCoinCost
    , txOutputCoinSize
    , txOutputHasValidSize
    , txOutputHasValidTokenQuantities
    , TxSize (..)
    , txSizeDistance
    , txOutMinCoin
    , txOutMaxCoin
    , txOutMinTokenQuantity
    , txOutMaxTokenQuantity
    , txMintBurnMaxTokenQuantity
    , coinIsValidForTxOut
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
import Data.Int
    ( Int64 )
import Data.Monoid
    ( Sum (..) )
import Data.Word
    ( Word64 )
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
    deriving Num via Natural
    deriving Semigroup via Sum Natural

instance NFData TxSize

instance Monoid TxSize where
    mempty = TxSize 0

-- | Computes the absolute distance between two transaction size quantities.
--
txSizeDistance :: TxSize -> TxSize -> TxSize
txSizeDistance (TxSize a) (TxSize b)
    | a >= b    = TxSize (a - b)
    | otherwise = TxSize (b - a)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | The smallest quantity of lovelace that can appear in a transaction output's
--   token bundle.
--
txOutMinCoin :: Coin
txOutMinCoin = Coin 0

-- | The greatest quantity of lovelace that can appear in a transaction output's
--   token bundle.
--
txOutMaxCoin :: Coin
txOutMaxCoin = Coin 45_000_000_000_000_000

-- | The smallest token quantity that can appear in a transaction output's
--   token bundle.
--
txOutMinTokenQuantity :: TokenQuantity
txOutMinTokenQuantity = TokenQuantity 1

-- | The greatest token quantity that can appear in a transaction output's
--   token bundle.
--
-- Although the ledger specification allows token quantities of unlimited
-- sizes, in practice we'll only see transaction outputs where the token
-- quantities are bounded by the size of a 'Word64'.
--
txOutMaxTokenQuantity :: TokenQuantity
txOutMaxTokenQuantity = TokenQuantity $ fromIntegral $ maxBound @Word64

-- | The greatest quantity of any given token that can be minted or burned in a
--   transaction.
--
txMintBurnMaxTokenQuantity :: TokenQuantity
txMintBurnMaxTokenQuantity = TokenQuantity $ fromIntegral $ maxBound @Int64

--------------------------------------------------------------------------------
-- Checks
--------------------------------------------------------------------------------

coinIsValidForTxOut :: Coin -> Bool
coinIsValidForTxOut c = (&&)
    (c >= txOutMinCoin)
    (c <= txOutMaxCoin)
