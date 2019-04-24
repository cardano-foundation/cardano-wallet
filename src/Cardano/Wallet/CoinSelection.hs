{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the API of Coin Selection algorithm and Fee Calculation
-- This module contains the implementation of adjusting coin selection for a fee.
-- The sender pays for the fee and additional inputs are picked randomly.
-- For more information refer to:
-- https://iohk.io/blog/self-organisation-in-coin-selection/

module Cardano.Wallet.CoinSelection
    (
      -- * Coin Selection
      CoinSelectionOptions (..)
    , CoinSelectionError(..)
    , CoinSelection(..)

    -- * Helpers
    , shuffle
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxIn, TxOut (..) )
import Control.Monad
    ( forM_ )
import Crypto.Number.Generate
    ( generateBetween )
import Data.Vector.Mutable
    ( IOVector )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), blockListF, blockListF', listF, nameF )
import GHC.Generics
    ( Generic )

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

{-------------------------------------------------------------------------------
                                Coin Selection
-------------------------------------------------------------------------------}

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

instance Buildable CoinSelection where
    build (CoinSelection inps outs chngs) = mempty
        <> nameF "inputs" (blockListF' "-" inpsF inps)
        <> nameF "outputs" (blockListF outs)
        <> nameF "change" (listF chngs)
      where
        inpsF (txin, txout) = build txin <> " (~ " <> build txout <> ")"

{-------------------------------------------------------------------------------
                                   Helpers
-------------------------------------------------------------------------------}

-- | Shuffles a list of elements.
--
-- >>> shuffle (outputs coinSel)
-- [...]
--
shuffle :: [a] -> IO [a]
shuffle = modifyInPlace $ \v -> do
    let (lo, hi) = (0, MV.length v - 1)
    forM_ [lo .. hi] $ \i -> do
      j <- fromInteger <$> generateBetween (fromIntegral lo) (fromIntegral hi)
      swapElems v i j
  where
    swapElems :: IOVector a -> Int -> Int -> IO ()
    swapElems v i j = do
        x <- MV.read v i
        y <- MV.read v j
        MV.write v i y
        MV.write v j x

    modifyInPlace :: forall a. (IOVector a -> IO ()) -> [a] -> IO [a]
    modifyInPlace f xs = do
        v' <- V.thaw $ V.fromList xs
        f v'
        V.toList <$> V.freeze v'
