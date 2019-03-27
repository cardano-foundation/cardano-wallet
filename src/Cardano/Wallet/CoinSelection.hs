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

data CoinSelFinalResult = CoinSelFinalResult
    { inputs  :: NonEmpty (TxIn, TxOut)
      -- ^ Picked inputs
    , outputs :: NonEmpty TxOut
      -- ^ Picked outputs
    , change  :: [Coin]
      -- ^ Resulting changes
    } deriving (Show, Generic)


data CoinSelOneGoResult = CoinSelOneGoResult
    { coinSelRequest :: TxOut
      -- ^ The output as it was requested
    , coinSelOutput  :: TxOut
      -- ^ The output as it should appear in the final transaction
      -- This may be different from the requested output if recipient pays fees.
    , coinSelChange  :: [Coin]
      -- ^ Change outputs (if any)
      -- These are not outputs, to keep this agnostic to a choice of change addr
    , coinSelInputs  :: SelectedUtxo
      -- | The UTxO entries that were used for this output
    }

data SelectedUtxo  = SelectedUtxo
    { selectedEntries :: ![(TxIn, TxOut)]
    , selectedBalance :: !Coin
    , selectedSize    :: !Word64
    }

emptySelectedUtxo :: SelectedUtxo
emptySelectedUtxo = SelectedUtxo [] (Coin 0) 0

select
    :: (TxIn, TxOut)
    -> SelectedUtxo
    -> SelectedUtxo
select io@(_,o) SelectedUtxo{..} =
    let currentBalance = getCoin selectedBalance
        entryValue = (getCoin . coin) o
    in SelectedUtxo
       { selectedEntries = io : selectedEntries
       , selectedBalance = Coin $ currentBalance + entryValue
       , selectedSize    = selectedSize + 1
       }


----------------------------------------------------------------------------
--                        Fee related                                     --
----------------------------------------------------------------------------

newtype Fee = Fee { getFee :: Quantity "lovelace" Natural }

adjustForFees
    :: CoinSelectionOptions
    -> ( Coin -> UTxO -> Maybe (TxIn, TxOut) )
    -> [CoinSelOneGoResult]
    -> CoinSelFinalResult
adjustForFees _opt _pickUtxo results = do
    let inps = concatMap (selectedEntries . coinSelInputs) results
    let outs = map coinSelOutput results
    let chgs = concatMap coinSelChange results

    -- here will come estimateFee and other stuff
    -- and will change inps, outs and chgs

    let neInps = case inps of
            [] -> fail "adjustForFees: empty list of inputs"
            i:is -> i :| is
    let neOuts = case outs of
            [] -> fail "adjustForFees: empty list of outputs"
            o:os -> o :| os

    CoinSelFinalResult neInps neOuts chgs
