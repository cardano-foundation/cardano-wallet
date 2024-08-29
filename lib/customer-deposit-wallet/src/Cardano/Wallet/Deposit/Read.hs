{-# LANGUAGE BinaryLiterals #-}

-- | Indirection module that re-exports types
-- used for reading data from the blockchain,
-- from all eras.
--
-- TODO: Match this up with the @Read@ hierarchy.
module Cardano.Wallet.Deposit.Read
    ( Network (..)
    , Slot
    , toSlot
    , Read.SlotNo
    , ChainPoint (..)

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
    , BHeader (..)
    , BHBody (..)

    , GenesisData
    , GenesisHash

    -- * Dummy Values useful for testing
    , dummyAddress
    , dummyBHeader
    ) where

import Prelude

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
    ( Word64
    , Word8
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

-- Spec: type Slot = Natural
type Slot = Read.SlotNo

toSlot :: Word64 -> Slot
toSlot = W.SlotNo

data ChainPoint
    = Origin
    | At Slot
    deriving (Eq, Ord, Show)

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

type BlockNo = Natural

-- type Block = O.CardanoBlock O.StandardCrypto
data Block = Block
    { blockHeader :: BHeader
    , transactions :: [Read.Tx Read.Conway]
    }
    deriving (Eq, Show)

data BHeader = BHeader
    { blockHeaderBody :: BHBody
    , blockHeaderSignature :: Sig
    }
    deriving (Eq, Ord, Show)

dummyBHeader :: BHeader
dummyBHeader = BHeader
    { blockHeaderBody = dummyBHBody
    , blockHeaderSignature = ()
    }

type Sig = ()

data BHBody = BHBody
    { prev :: Maybe HashHeader
    , blockno :: BlockNo
    , slot :: Slot
    , bhash :: HashBBody
    }
    deriving (Eq, Ord, Show)

type HashHeader = ()
type HashBBody = ()

dummyBHBody :: BHBody
dummyBHBody = BHBody
    { prev = Nothing
    , blockno = 128
    , slot = Read.SlotNo 42
    , bhash = ()
    }

-- GenesisData is not part of the ledger specification proper
type GenesisData = Byron.GenesisData
type GenesisHash = Byron.GenesisHash
