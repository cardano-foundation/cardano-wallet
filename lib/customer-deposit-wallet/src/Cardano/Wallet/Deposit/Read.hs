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

    , Network (..)
    , Read.SlotNo
    , Read.ChainPoint (..)
    , Slot
    , WithOrigin (..)
    , slotFromChainPoint

    , Address
    , KeyHash
    , mkEnterpriseAddress
    , mockAddress

    , Ix
    , Read.TxIn
    , Read.TxOut
    , address
    , Read.Value
    , UTxO

    , Read.TxId
    , Tx
    , Read.utxoFromEraTx

    , TxBody
    , TxWitness

    , BlockNo
    , Block (..)
    , getChainPoint
    , mockNextBlock
    , BHeader (..)
    , Read.mockRawHeaderHash
    , BHBody (..)

    , GenesisData
    , GenesisHash

    -- * Dummy Values useful for testing
    , dummyAddress
    ) where

import Prelude

import Cardano.Wallet.Read.Chain
    ( Slot
    , WithOrigin (..)
    , slotFromChainPoint
    )
import Cardano.Wallet.Read.Tx
    ( TxIx
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
import Numeric.Natural
    ( Natural
    )

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Wallet.Read as Read
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Short as SBS
-- import qualified Ouroboros.Consensus.Cardano.Block as O

{-----------------------------------------------------------------------------
    Type definitions
    with dummies
------------------------------------------------------------------------------}
data Network = Testnet | Mainnet

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

type Ix = TxIx

address :: Read.TxOut -> Address
address = Read.getCompactAddr

type UTxO = Map Read.TxIn Read.TxOut

type Tx = Read.Tx Read.Conway

type TxBody = ()

type TxWitness = ()

{-----------------------------------------------------------------------------
    Block
------------------------------------------------------------------------------}
type BlockNo = Natural

-- type Block = O.CardanoBlock O.StandardCrypto
data Block era = Block
    { blockHeader :: BHeader
    , transactions :: [Read.Tx era]
    }

data BHeader = BHeader
    { blockHeaderBody :: BHBody
    , blockHeaderSignature :: Sig
    }
    deriving (Eq, Ord, Show)

type Sig = ()

data BHBody = BHBody
    { prev :: Maybe HashHeader
    , blockno :: BlockNo
    , slotNo :: Read.SlotNo
    , bhash :: HashBBody
    }
    deriving (Eq, Ord, Show)

type HashHeader = Read.RawHeaderHash
type HashBBody = ()

getChainPoint :: Read.IsEra era => Block era -> Read.ChainPoint
getChainPoint block =
    Read.BlockPoint
        { Read.slotNo = slot
        , Read.headerHash =
            Read.mockRawHeaderHash
            $ fromIntegral $ fromEnum slot
        }
  where
    bhBody = blockHeaderBody $ blockHeader block
    slot = slotNo bhBody

-- | Create a new block from a sequence of transaction.
mockNextBlock :: Read.ChainPoint -> [Read.Tx Read.Conway] -> Block Read.Conway
mockNextBlock old txs =
    Block
        { blockHeader = BHeader
            { blockHeaderBody = BHBody
                { prev
                , blockno
                , slotNo
                , bhash = ()
                }
            , blockHeaderSignature = ()
            }
        , transactions = txs
        }
  where
    blockno = toEnum $ fromEnum slotNo
    slotNo = case old of
        Read.GenesisPoint -> 0
        Read.BlockPoint{slotNo = n} -> succ n
    prev = case old of
        Read.GenesisPoint -> Nothing
        Read.BlockPoint{headerHash} -> Just headerHash

{-----------------------------------------------------------------------------
    Genesis
------------------------------------------------------------------------------}

-- GenesisData is not part of the ledger specification proper
type GenesisData = Byron.GenesisData
type GenesisHash = Byron.GenesisHash
