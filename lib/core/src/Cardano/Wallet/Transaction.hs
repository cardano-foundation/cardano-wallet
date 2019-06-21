{-# LANGUAGE DataKinds #-}

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
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Key, Passphrase, XPrv )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address, Tx, TxIn, TxOut, TxWitness )
import Data.Quantity
    ( Quantity )

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
    }

-- | Possible signing error
data ErrMkStdTx
    = ErrKeyNotFoundForAddress Address
    -- ^ We tried to sign a transaction with inputs that are unknown to us?
    | ErrInvalidTx
    -- ^ when transaction with 0 amount is tried (not valid in Byron)
    deriving (Eq, Show)
