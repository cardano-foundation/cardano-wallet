{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the API of Coin Selection algorithm and Fee related functionality.
-- For more information refer to:
-- https://iohk.io/blog/self-organisation-in-coin-selection/


module Cardano.Wallet.CoinSelection where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Coin, Tx, TxIn, TxOut, UTxO )
import Control.Monad.Random
    ( MonadRandom )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.ByteString
    ( ByteString )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

data CoinSelectionOptions = CoinSelectionOptions
    { estimateFee
        :: Int
        -> NonEmpty Coin
        -> Coin
        -- ^ A function to estimate the fees.
    , dustThreshold
        :: Coin
        -- ^ Change addresses below the given threshold will be evicted
        -- from the created transaction. If you only want to remove change
        -- outputs equal to 0, set 'csoDustThreshold' to 0.
    , maximumNumberOfInputs
        :: Word64
    } deriving (Generic)


data ErrCoinSelection = ErrCoinSelection

data CoinSelFinalResult = CoinSelFinalResult
    { inputs  :: NonEmpty (TxIn, TxOut)
    -- ^ Picked inputs
    , outputs :: NonEmpty TxOut
    -- ^ Picked outputs
    , change  :: [Coin]
    -- ^ Resulting changes
    } deriving (Show, Generic)


-- | Largest-first input selection policy
largestFirst
    :: forall m. MonadRandom m
    => CoinSelectionOptions
    -> UTxO
    -> ExceptT ErrCoinSelection m CoinSelFinalResult
largestFirst _ = undefined


----------------------------------------------------------------------------
--                        Fee related                                     --
----------------------------------------------------------------------------

newtype Fee = Fee { getFee :: Quantity "lovelace" Natural }

estimateCardanoFee :: Tx -> Word64
estimateCardanoFee _ = 1765

estimateMaxTxInputs
  :: ByteString
    -- ^ Maximum size of a transaction
  -> Word64
estimateMaxTxInputs _ = 100
