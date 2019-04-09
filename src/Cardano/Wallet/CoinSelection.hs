{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the API of Coin Selection algorithm and Fee Calculation
-- For more information refer to:
-- https://iohk.io/blog/self-organisation-in-coin-selection/


module Cardano.Wallet.CoinSelection
    ( CoinSelectionOptions (..)
    , CoinSelectionError(..)
    , CoinSelection(..)
    , FeeOptions (..)
    , FeeError(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..) )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )


newtype CoinSelectionOptions = CoinSelectionOptions
    { maximumNumberOfInputs
        :: Word64
    } deriving (Generic)

data CoinSelectionError =
    NotEnoughMoney Word64 Word64
    -- ^ UTxO exhausted during input selection
    -- We record the balance of the UTxO as well as the size of the payment
    -- we tried to make.
    | UtxoNotEnoughFragmented Word64 Word64
    -- ^ UTxO is not enough fragmented for the number of transaction outputs
    -- We record the number of UTxO entries as well as the number of the
    -- outputs of the transaction.
    | MaximumInputsReached Word64
    -- ^ When trying to construct a transaction, the max number of allowed
    -- inputs was reached.
    deriving (Show, Eq)

data CoinSelection = CoinSelection
    { inputs  :: [(TxIn, TxOut)]
      -- ^ Picked inputs
    , outputs :: [TxOut]
      -- ^ Picked outputs
    , change  :: [Coin]
      -- ^ Resulting changes
    } deriving (Show, Eq)

-- NOTE
-- We don't check for duplicates when combining selections because we assume
-- they are constructed from independent elements. In practice, we could nub
-- the list or use a `Set` ?
instance Semigroup CoinSelection where
    a <> b = CoinSelection
        { inputs = inputs a <> inputs b
        , outputs = outputs a <> outputs b
        , change = change a <> change b
        }

instance Monoid CoinSelection where
    mempty = CoinSelection [] [] []


data FeeOptions = FeeOptions
    { estimate
      :: Int
      -> [Coin]
      -> Coin
      -- ^ Estimate fees based on number of inputs and values of the outputs
    , dustThreshold
      :: Coin
      -- ^ Change addresses below the given threshold will be evicted
      -- from the created transaction. Setting 'dustThreshold' to 0
      -- removes output equal to 0
    } deriving (Generic)

data FeeError =
     CannotCoverFee Word64
    -- ^ UTxO exhausted during fee covering
    -- We record what amount missed to cover the fee
    | OutOfBoundFee Word64 Word64
    -- ^ Excessive actual fee compared to estimated fee
    -- We record the actual fee as well as estimated fee
    deriving (Show, Eq)
