{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- An extra interface for operation on transactions (e.g. creating witnesses,
-- estimating size...). This makes it possible to decouple those operations from
-- our wallet layer, keeping the implementation flexible to various backends.

module Cardano.Wallet.Transaction
    (
    -- * Interface
      TransactionLayer(..)

    -- * Errors
    , ErrMkStdTx (..)

    -- * Backend helpers
    , estimateMaxNumberOfInputsBase
    , EstimateMaxNumberOfInputsParams(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, Passphrase, XPrv )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Hash (..), Tx, TxIn (..), TxOut (..), TxWitness (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word8 )

import qualified Data.ByteString as BS

data TransactionLayer t = TransactionLayer
    { mkStdTx
        :: (Address -> Maybe (Key 'AddressK XPrv, Passphrase "encryption"))
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkStdTx (Tx t, [TxWitness])
        -- ^ Construct a standard transaction
        --
        -- " Standard " here refers to the fact that we do not deal with redemption,
        -- multisignature transactions, etc.
        --
        -- This expects as a first argument a mean to compute or lookup private
        -- key corresponding to a particular address.

    , estimateSize :: CoinSelection -> Quantity "byte" Int
        -- ^ Estimate the size of a 'CoinSelection', in bytes. This operation is
        -- seemingly coupled to the binary representation of a 'Transaction'.
        -- This estimation is therefore only a best-effort here as many of the
        -- encoding values actually depends on the value of parameters at
        -- runtime.
        --
        -- For instance, with a CBOR encoding, an amount of `50` lovelace would
        -- be encoded using 2 bytes, whereas an amount of `1000000` would be
        -- encoded using 4 bytes. In Byron, we have only one piece of unknown
        -- from the 'CoinSelection' and it's the value of the 'crc32' computed
        -- on the address payload, which can be 1,2,3 or 5 bytes and we therefore
        -- always consider the worst-case scenario of a 5-byte crc.
        -- As a consequence, our estimate may be slightly bigger than the actual
        -- transaction fee (up-to 4 extra bytes per change output).

    , estimateMaxNumberOfInputs :: Quantity "byte" Word16 -> Word8 -> Word8
        -- ^ Calculate a "theoretical" maximum number of inputs given a maximum
        -- transaction size and desired number of outputs.
        --
        -- The actual transaction size cannot be known until it has been fully
        -- determined by coin selection.
        --
        -- This estimate will err on the side of permitting more inputs,
        -- resulting in a transaction which may be too large.

    }

-- | Possible signing error
data ErrMkStdTx
    = ErrKeyNotFoundForAddress Address
    -- ^ We tried to sign a transaction with inputs that are unknown to us?
    | ErrInvalidTx
    -- ^ when transaction with 0 amount is tried (not valid in Byron)
    deriving (Eq, Show)

-- | Backend-specific variables used by 'estimateMaxNumberOfInputsBase'.
data EstimateMaxNumberOfInputsParams t = EstimateMaxNumberOfInputsParams
    { estMeasureTx :: [TxIn] -> [TxOut] -> [TxWitness] -> Int
        -- ^ Finds the size of a serialized transaction.
    , estAddressSample :: Address -- ^ Address to use in tx output
    , estBlockHashSize :: Int -- ^ Block ID size
    , estTxWitnessSize :: Int -- ^ Tx Witness size
    }

-- | This is called by the 'TransactionLayer' implementation. It uses the
-- serialization functions to calculate the size of an empty transaction
-- compared to a transaction with one input. The estimation is based on that.
--
-- It doesn't account for transaction outputs, and assumes there is a single Tx
-- output.
--
-- All the values used are the smaller ones. For example, the shortest adress
-- type and shortest witness type are chosen to use for the estimate.
estimateMaxNumberOfInputsBase
    :: EstimateMaxNumberOfInputsParams t
    -- ^ Backend-specific variables used in the estimation
    -> Quantity "byte" Word16
    -- ^ Transaction max size in bytes
    -> Word8
    -- ^ Number of outputs in transaction
    -> Word8
    -- ^ Maximum number of inputs, estimated
estimateMaxNumberOfInputsBase
    EstimateMaxNumberOfInputsParams{..} (Quantity txSize) numOutputs =
    clamp $ max 0 (fromIntegral txSize - fixedSize) `div` inputSize
  where
    -- The fixed size covers the headers of a signed transaction with a single
    -- output.
    fixedSize = sizeOfTx [] []

    -- inputSize is the size of each additional input of a signed transaction.
    inputSize = sizeOfTx [txIn] [wit] - fixedSize

    -- Serialize a "representative" Tx with the given inputs and read its size.
    sizeOfTx ins = estMeasureTx ins outs

    outs = replicate (fromIntegral numOutputs) txout
    txout = TxOut estAddressSample minBound
    txIn = TxIn (Hash $ chaff estBlockHashSize) 0
    wit = TxWitness (chaff estTxWitnessSize)

    -- Make a bytestring of length n
    chaff n = BS.replicate n 0

    -- convert down to a smaller int without wrapping
    clamp :: Int -> Word8
    clamp = fromIntegral . min (fromIntegral $ maxBound @Word8)
