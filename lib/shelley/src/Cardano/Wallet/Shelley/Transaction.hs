{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Working with Shelley transactions.

module Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer
    , genesisBlockFromTxOuts
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , BlockchainParameters (..)
    , Hash (..)
    , ProtocolMagic (..)
    , SlotId (..)
    , Tx (..)
    , TxOut (..)
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley )
import Cardano.Wallet.Transaction
    ( ErrValidateSelection, TransactionLayer (..) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Fmt
    ( Buildable (..) )
import GHC.Stack
    ( HasCallStack )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS


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

-- | Construct a ("fake") genesis block from genesis transaction outputs.
--
-- The genesis data on haskell nodes is not a block at all, unlike the block0 on
-- jormungandr. This function is a method to deal with the discrepancy.
genesisBlockFromTxOuts :: BlockchainParameters -> [TxOut] -> Block
genesisBlockFromTxOuts bp outs = Block
    { delegations  = []
    , header = BlockHeader
        { slotId =
            SlotId 0 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ getGenesisBlockHash bp
        , parentHeaderHash =
            Hash (BS.replicate 32 0)
        }
    , transactions = mkTx <$> outs
    }
  where
    mkTx out@(TxOut (Address bytes) _) =
        Tx (Hash $ blake2b256 bytes) [] [out]

blake2b256 :: ByteString -> ByteString
blake2b256 =
    BA.convert . hash @_ @Blake2b_256

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
