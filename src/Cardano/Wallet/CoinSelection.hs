{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

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
    ( Coin (..), TxIn, TxOut (..), UTxO )
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


defaultCoinSelectionOptions :: CoinSelectionOptions
defaultCoinSelectionOptions = CoinSelectionOptions
    { estimateFee = \_ _ -> Coin 0
    , dustThreshold = Coin 0
    , maximumNumberOfInputs = 100
    }

data CoinSelectionError =
    UtxoExhausted Word64 Word64
    -- ^ UTxO exhausted during input selection
    -- We record the balance of the UTxO as well as the size of the payment
    -- we tried to make.

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
    } deriving (Show)

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


----------------------------------------------------------------------------
--                        Fee related                                     --
----------------------------------------------------------------------------

newtype Fee = Fee { getFee :: Quantity "lovelace" Natural }

adjustForFees
    :: CoinSelectionOptions
    -> ( Coin -> UTxO -> Maybe (TxIn, TxOut) )
    -> CoinSelection
    -> CoinSelection
adjustForFees _opt _pickUtxo selection = do
    let inps = inputs selection
    let outs = outputs selection
    let chgs = change selection

    -- here will come estimateFee and other stuff
    -- and will change inps, outs and chgs

    -- let neInps = case inps of
    --         [] -> fail "adjustForFees: empty list of inputs"
    --         i:is -> i :| is
    -- let neOuts = case outs of
    --         [] -> fail "adjustForFees: empty list of outputs"
    --         o:os -> o :| os

    CoinSelection inps outs chgs
