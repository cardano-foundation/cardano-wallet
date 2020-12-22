{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Provides the API of Coin Selection algorithm and Fee Calculation
-- This module contains the implementation of adjusting coin selection for a fee.
-- The sender pays for the fee and additional inputs are picked randomly.
-- For more information refer to:
-- https://iohk.io/blog/self-organisation-in-coin-selection/

module Cardano.Wallet.Primitive.CoinSelection
    (
      -- * Coin Selection
      CoinSelection(..)
    , inputBalance
    , outputBalance
    , changeBalance
    , feeBalance
    , totalBalance
    , ErrCoinSelection (..)
    , CoinSelectionOptions (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn, TxOut (..), txOutCoin )
import Cardano.Wallet.Primitive.Types.UTxO
    ( balance' )
import Data.List
    ( foldl' )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64, Word8 )
import Fmt
    ( Buildable (..), blockListF, blockListF', listF, nameF )
import GHC.Generics
    ( Generic )

{-------------------------------------------------------------------------------
                                Coin Selection
-------------------------------------------------------------------------------}

data CoinSelection = CoinSelection
    { inputs  :: [(TxIn, TxOut)]
      -- ^ Picked inputs
    , withdrawal :: Word64
      -- ^ An available withdrawal amount, counting as an extra input
    , reclaim :: Word64
      -- ^ Claim back a deposit, counting as a an extra input
    , outputs :: [TxOut]
      -- ^ Picked outputs
    , change  :: [Coin]
      -- ^ Resulting changes
    , deposit :: Word64
      -- ^ A deposit counting as an extra output
    } deriving (Generic, Show, Eq)

-- NOTE
-- We don't check for duplicates when combining selections because we assume
-- they are constructed from independent elements. In practice, we could nub
-- the list or use a `Set` ?
instance Semigroup CoinSelection where
    a <> b = CoinSelection
        { inputs = inputs a <> inputs b
        , withdrawal = withdrawal a + withdrawal b
        , reclaim = reclaim a + reclaim b
        , outputs = outputs a <> outputs b
        , change = change a <> change b
        , deposit = deposit a + deposit b
        }

instance Monoid CoinSelection where
    mempty = CoinSelection [] 0 0 [] [] 0

instance Buildable CoinSelection where
    build (CoinSelection inps draw back outs chngs depo) = mempty
        <> nameF "inputs" (blockListF' "-" inpsF inps)
        <> nameF "withdrawal" (build draw)
        <> nameF "reclaim" (build back)
        <> nameF "outputs" (blockListF outs)
        <> nameF "change" (listF chngs)
        <> nameF "deposit" (build depo)
      where
        inpsF (txin, txout) = build txin <> " (~ " <> build txout <> ")"

newtype CoinSelectionOptions = CoinSelectionOptions
    { maximumNumberOfInputs
        :: Word8 -> Word8
            -- ^ Maximum number of inputs allowed for a given number of outputs
    } deriving (Generic)

-- | Calculate the sum of all input values
inputBalance :: CoinSelection -> Word64
inputBalance cs =
    foldl' (\total -> addTxOut total . snd) 0 (inputs cs)
    +
    -- NOTE
    -- reclaim and withdrawal can only count towards the input balance if and
    -- only if there's already a transaction input.
    if null (inputs cs) then 0 else withdrawal cs + reclaim cs

-- | Calculate the sum of all output values
outputBalance :: CoinSelection -> Word64
outputBalance cs =
    foldl' addTxOut 0 (outputs cs)
    +
    deposit cs

-- | Calculate the sum of all output values
changeBalance :: CoinSelection -> Word64
changeBalance = foldl' addCoin 0 . change

feeBalance :: CoinSelection -> Word64
feeBalance sel = inputBalance sel - outputBalance sel - changeBalance sel

-- | Total UTxO balance + withdrawal.
totalBalance :: Quantity "lovelace" Word64 -> [(TxIn, TxOut)] -> Word64
totalBalance (Quantity withdraw) inps = balance' inps + withdraw

addTxOut :: Integral a => a -> TxOut -> a
addTxOut total = addCoin total . txOutCoin

addCoin :: Integral a => a -> Coin -> a
addCoin total c = total + (fromIntegral (unCoin c))

data ErrCoinSelection
    = ErrNotEnoughMoney Word64 Word64
    -- ^ UTxO exhausted during input selection
    -- We record the balance of the UTxO as well as the size of the payment
    -- we tried to make.
    | ErrMaximumInputsReached Word64
    -- ^ When trying to construct a transaction, the max number of allowed
    -- inputs was reached.
    | ErrInputsDepleted
    -- ^ When trying to construct a transaction, the available inputs are depleted
    -- even when UTxO is properly fragmented and with enough funds to cover payment
    deriving (Show, Eq)
