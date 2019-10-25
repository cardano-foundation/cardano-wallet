{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
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
    , ErrValidateSelection
    , ErrDecodeSignedTx (..)

    -- * Backend helpers
    , estimateMaxNumberOfInputsBase
    , EstimateMaxNumberOfInputsParams(..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), KeyToAddress (..), Passphrase, XPrv, dummyAddress )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Hash (..), Tx, TxIn (..), TxOut (..), TxWitness (..) )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word16, Word8 )

import qualified Data.ByteString as BS

data TransactionLayer t k = TransactionLayer
    { mkStdTx
        :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
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

    , validateSelection
      :: CoinSelection -> Either (ErrValidateSelection t) ()
      -- ^ Validate coin selection regarding rules that may be specific to a
      -- particular backend implementation.
      --
      -- For example, Byron nodes do not allow null output amounts. Jörmungandr
      -- on its side doesn't support more than 255 inputs or outputs.

    , decodeSignedTx
        :: ByteString -> Either ErrDecodeSignedTx (Tx t, [TxWitness])
        -- ^ Decode an externally-signed transaction to the chain producer
    }

-- | A type family for validations that are specific to a particular backend
-- type. This demands an instantiation of the family for a particular backend:
--
--     type instance (ErrValidateSelection MyBackend) = MyCustomError
--
type family ErrValidateSelection t

-- | Error while trying to decode externally signed transaction
data ErrDecodeSignedTx
    = ErrDecodeSignedTxWrongPayload Text
    | ErrDecodeSignedTxNotSupported
    deriving (Show, Eq)

-- | Possible signing error
newtype ErrMkStdTx
    = ErrKeyNotFoundForAddress Address
    -- ^ We tried to sign a transaction with inputs that are unknown to us?
    deriving (Eq, Show)

-- | Backend-specific variables used by 'estimateMaxNumberOfInputsBase'.
data EstimateMaxNumberOfInputsParams t = EstimateMaxNumberOfInputsParams
    { estMeasureTx :: [TxIn] -> [TxOut] -> [TxWitness] -> Int
        -- ^ Finds the size of a serialized transaction.
    , estBlockHashSize :: Int
        -- ^ Block ID size
    , estTxWitnessSize :: Int
        -- ^ Tx Witness size
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
    :: forall t n k. (KeyToAddress n k)
    => EstimateMaxNumberOfInputsParams t
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
    txout = TxOut baseAddr minBound
    baseAddr = dummyAddress @n @k
    txIn = TxIn (Hash $ chaff estBlockHashSize) 0
    wit = TxWitness (chaff estTxWitnessSize)

    -- Make a bytestring of length n
    chaff n = BS.replicate n 0

    -- convert down to a smaller int without wrapping
    clamp :: Int -> Word8
    clamp = fromIntegral . min (fromIntegral $ maxBound @Word8)
