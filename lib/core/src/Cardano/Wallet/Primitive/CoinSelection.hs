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
    , ErrCoinSelection (..)
    , CoinSelectionOptions (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..) )
import Data.List
    ( foldl' )
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
    , outputs :: [TxOut]
      -- ^ Picked outputs
    , change  :: [Coin]
      -- ^ Resulting changes
    , reserve :: Maybe Coin
      -- ^ A coin reserve, from a reward balance or a deposit.
    } deriving (Generic, Show, Eq)

-- NOTE
-- We don't check for duplicates when combining selections because we assume
-- they are constructed from independent elements. In practice, we could nub
-- the list or use a `Set` ?
instance Semigroup CoinSelection where
    a <> b = CoinSelection
        { inputs = inputs a <> inputs b
        , outputs = outputs a <> outputs b
        , change = change a <> change b
        , reserve = case (reserve a, reserve b) of
            (Nothing, Nothing) -> Nothing
            (Just (Coin ca), Nothing) -> Just (Coin ca)
            (Nothing, Just (Coin cb)) -> Just (Coin cb)
            (Just (Coin ca), Just (Coin cb)) -> Just (Coin (ca + cb))
        }

instance Monoid CoinSelection where
    mempty = CoinSelection [] [] [] Nothing

instance Buildable CoinSelection where
    build (CoinSelection inps outs chngs rsv) = mempty
        <> nameF "inputs" (blockListF' "-" inpsF inps)
        <> nameF "outputs" (blockListF outs)
        <> nameF "change" (listF chngs)
        <> nameF "reserve" (maybe "ø" build rsv)
      where
        inpsF (txin, txout) = build txin <> " (~ " <> build txout <> ")"

data CoinSelectionOptions e = CoinSelectionOptions
    { maximumNumberOfInputs
        :: Word8 -> Word8
            -- ^ Maximum number of inputs allowed for a given number of outputs
    , validate
        :: CoinSelection -> Either e ()
            -- ^ Returns any backend-specific error regarding coin selection
    } deriving (Generic)

-- | Calculate the sum of all input values
inputBalance :: CoinSelection -> Word64
inputBalance cs =
    maybe 0 getCoin (reserve cs)
    +
    foldl' (\total -> addTxOut total . snd) 0 (inputs cs)

-- | Calculate the sum of all output values
outputBalance :: CoinSelection -> Word64
outputBalance = foldl' addTxOut 0 . outputs

-- | Calculate the sum of all output values
changeBalance :: CoinSelection -> Word64
changeBalance = foldl' addCoin 0 . change

feeBalance :: CoinSelection -> Word64
feeBalance sel = inputBalance sel - outputBalance sel - changeBalance sel

addTxOut :: Integral a => a -> TxOut -> a
addTxOut total = addCoin total . coin

addCoin :: Integral a => a -> Coin -> a
addCoin total c = total + (fromIntegral (getCoin c))

data ErrCoinSelection e
    = ErrNotEnoughMoney Word64 Word64
    -- ^ UTxO exhausted during input selection
    -- We record the balance of the UTxO as well as the size of the payment
    -- we tried to make.
    | ErrUtxoNotEnoughFragmented Word64 Word64
    -- ^ UTxO is not enough fragmented for the number of transaction outputs
    -- We record the number of UTxO entries as well as the number of the
    -- outputs of the transaction.
    | ErrMaximumInputsReached Word64
    -- ^ When trying to construct a transaction, the max number of allowed
    -- inputs was reached.
    | ErrInputsDepleted
    -- ^ When trying to construct a transaction, the available inputs are depleted
    -- even when UTxO is properly fragmented and with enough funds to cover payment
    | ErrInvalidSelection e
    -- ^ Somewhat, we ended up generating an invalid coin selection because of
    -- inputs passed down to the coin selection function, or because a target
    -- backend has extra-limitations not covered by our coin selection
    -- algorithm.
    deriving (Show, Eq)
