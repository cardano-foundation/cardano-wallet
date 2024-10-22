{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Indirection module that re-exports types
-- used for reading data from the blockchain,
-- from all eras.
--
-- TODO: Match this up with the @Read@ hierarchy.
module Cardano.Wallet.Deposit.Read
    ( Read.IsEra
    , Read.EraValue (..)
    , Read.Conway

    , Read.SlotNo
    , Read.ChainPoint (..)
    , Read.Slot
    , Read.WithOrigin (..)
    , Read.slotFromChainPoint

    , Address
    , KeyHash
    , NetworkTag (..)
    , mkEnterpriseAddress

    , Ix
    , Read.TxIn
    , Read.TxOut
    , address
    , Read.Value
    , Read.lessOrEqual
    , UTxO

    , Read.TxId
    , Read.Tx (..)
    , Read.utxoFromEraTx

    , Read.Block
    , Read.getChainPoint
    , Read.getEraBHeader
    , Read.getEraSlotNo
    , Read.getEraTransactions
    , mockNextBlock
    , Read.mockRawHeaderHash

    , Read.GenesisData
    , Read.GenesisHash
    , Read.mockGenesisDataMainnet

    , Read.NetworkId (Read.Mainnet, Read.Testnet)
    , Read.getNetworkId
    ) where

import Prelude

import Cardano.Wallet.Address.Encoding
    ( Credential (..)
    , EnterpriseAddr (..)
    , KeyHash
    , NetworkTag (..)
    , compactAddrFromEnterpriseAddr
    )
import Cardano.Wallet.Read.Block.Gen
    ( mkBlockEra
    )
import Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    )
import Data.Map
    ( Map
    )

import qualified Cardano.Wallet.Read as Read

{-----------------------------------------------------------------------------
    Type definitions
    with dummies
------------------------------------------------------------------------------}

-- | Synonym for readability.
-- The ledger specifications define @Addr@.
-- Byron addresses are represented by @Addr_bootstrap@.
type Address = Read.CompactAddr

-- | Make an enterprise address from a given network and key hash.
mkEnterpriseAddress :: NetworkTag -> KeyHash -> Address
mkEnterpriseAddress network =
    compactAddrFromEnterpriseAddr
    . EnterpriseAddrC network
    . KeyHashObj

type Ix = Read.TxIx

address :: Read.TxOut -> Address
address = Read.getCompactAddr

type UTxO = Map Read.TxIn Read.TxOut

{-----------------------------------------------------------------------------
    Block
------------------------------------------------------------------------------}
-- | Create a new block from a sequence of transaction.
mockNextBlock
    :: Read.ChainPoint -> [Read.Tx Read.Conway] -> Read.Block Read.Conway
mockNextBlock old txs =
    mkBlockEra BlockParameters{slotNumber,blockNumber,txs}
  where
    blockNumber = Read.BlockNo $ Read.unSlotNo slotNumber
    slotNumber = case old of
        Read.GenesisPoint -> Read.SlotNo 0
        Read.BlockPoint{slotNo = n} -> succ n
