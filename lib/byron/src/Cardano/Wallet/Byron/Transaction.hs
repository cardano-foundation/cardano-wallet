{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0

module Cardano.Wallet.Byron.Transaction
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Wallet.Byron.Compatibility
    ( Byron )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Passphrase, XPrv )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), PoolId, SealedTx (..), Tx (..), TxIn (..), TxOut (..) )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word8 )
import GHC.Stack
    ( HasCallStack )

newTransactionLayer
    :: forall k t.
        ( t ~ IO Byron
        )
    => TransactionLayer t k
newTransactionLayer = TransactionLayer
    { mkStdTx = _mkStdTx
    , mkDelegationJoinTx = _mkDelegationJoinTx
    , mkDelegationQuitTx = _mkDelegationQuitTx
    , decodeSignedTx = _decodeSignedTx
    , estimateSize = _estimateSize
    , estimateMaxNumberOfInputs = _estimateMaxNumberOfInputs
    , validateSelection = _validateSelection
    }
  where
    _mkStdTx
        :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkStdTx =
        notImplemented "mkStdTx"

    _mkDelegationJoinTx
      :: PoolId
      -> (k 'AddressK XPrv, Passphrase "encryption") -- reward account
      -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
      -> [(TxIn, TxOut)]
      -> [TxOut]
      -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationJoinTx =
        notImplemented "mkDelegationJoinTx"

    _mkDelegationQuitTx
      :: (k 'AddressK XPrv, Passphrase "encryption") -- reward account
      -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
      -> [(TxIn, TxOut)]
      -> [TxOut]
      -> Either ErrMkTx (Tx, SealedTx)
    _mkDelegationQuitTx =
        notImplemented "mkDelegationQuitTx"

    _estimateSize
      :: CoinSelection
      -> Quantity "byte" Int
    _estimateSize =
        notImplemented "estimateSize"

    _estimateMaxNumberOfInputs
      :: Quantity "byte" Word16
      -> Word8 -> Word8
    _estimateMaxNumberOfInputs =
        notImplemented "estimateMaxNumberOfInputs"

    _validateSelection
        :: CoinSelection
        -> Either (ErrValidateSelection t) ()
    _validateSelection =
        notImplemented "validateSelection"

    _decodeSignedTx
      :: ByteString
      -> Either ErrDecodeSignedTx (Tx, SealedTx)
    _decodeSignedTx =
        notImplemented "decodeSignedTx"

type instance ErrValidateSelection (IO Byron) = ()

--------------------------------------------------------------------------------
-- Temporary
--

notImplemented :: HasCallStack => String -> a
notImplemented what = error ("Not implemented: " <> what)
