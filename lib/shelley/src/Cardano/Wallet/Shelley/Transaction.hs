{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Working with Shelley transactions.

module Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( ProtocolMagic (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley )
import Cardano.Wallet.Transaction
    ( ErrValidateSelection, TransactionLayer (..) )
import Data.Proxy
    ( Proxy )
import Fmt
    ( Buildable (..) )
import GHC.Stack
    ( HasCallStack )

newTransactionLayer
    :: forall (n :: NetworkDiscriminant) k t.
        ( t ~ IO Shelley)
    => Proxy n
    -> ProtocolMagic
    -> TransactionLayer t k
newTransactionLayer _proxy _protocolMagic = TransactionLayer
    { mkStdTx = notImplemented "mkStdTx"
    , mkDelegationJoinTx = notImplemented "mkDelegationJoinTx"
    , mkDelegationQuitTx = notImplemented "mkDelegationQuitTx"
    , decodeSignedTx = notImplemented "decodeSignedTx"
    , estimateSize = notImplemented "estimateSize"
    , estimateMaxNumberOfInputs = notImplemented "estimateMaxNumberOfInputs"
    , validateSelection = notImplemented "validateSelection"
    , allowUnbalancedTx = notImplemented "allowUnbalancedTx"
    }

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

-- | Transaction with 0 output amount is tried
data ErrInvalidTxOutAmount -- FIXME: = ErrInvalidTxOutAmount

instance Buildable ErrInvalidTxOutAmount where
    build _ = "Invalid coin selection: at least one output is null."

type instance ErrValidateSelection (IO Shelley) = ErrInvalidTxOutAmount

notImplemented :: HasCallStack => String -> a
notImplemented what = error ("Not implemented: " <> what)
