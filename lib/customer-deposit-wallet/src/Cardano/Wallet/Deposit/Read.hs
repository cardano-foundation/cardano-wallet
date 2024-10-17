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
    , mkEnterpriseAddress
    , mockAddress

    , Ix
    , Read.TxIn
    , Read.TxOut
    , address
    , Read.Value
    , Read.lessOrEqual
    , UTxO

    , Read.TxId
    , Tx
    , Read.utxoFromEraTx

    , TxBody
    , TxWitness

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

    -- * Dummy Values useful for testing
    , dummyAddress
    ) where

import Prelude

import Cardano.Wallet.Read.Block.Gen
    ( mkBlockEra
    )
import Cardano.Wallet.Read.Block.Gen.BlockParameters
    ( BlockParameters (..)
    )
import Data.ByteString
    ( ByteString
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromJust
    )
import Data.Word
    ( Word8
    )

import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as SBS

{-----------------------------------------------------------------------------
    Type definitions
    with dummies
------------------------------------------------------------------------------}

-- | Synonym for readability.
-- The ledger specifications define @Addr@.
-- Byron addresses are represented by @Addr_bootstrap@.
type Address = Read.CompactAddr

mockAddress :: Show a => a -> Address
mockAddress = mkEnterpriseAddress . B8.pack . show

-- 28 Bytes.
type KeyHash = ByteString

mkEnterpriseAddress :: KeyHash -> Address
mkEnterpriseAddress keyHash =
    fromJust . Read.fromShortByteString . SBS.pack
        $ [tagEnterprise] <> take 28 (BS.unpack keyHash <> repeat 0)

tagEnterprise :: Word8
tagEnterprise = 0b01100001

dummyAddress :: Address
dummyAddress = mockAddress (0 :: Int)

type Ix = Read.TxIx

address :: Read.TxOut -> Address
address = Read.getCompactAddr

type UTxO = Map Read.TxIn Read.TxOut

type Tx = Read.Tx Read.Conway

type TxBody = ()

type TxWitness = ()

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
