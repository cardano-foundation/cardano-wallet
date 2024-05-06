-- | Indirection module that re-exports types
-- used for reading data from the blockchain,
-- from all eras.
--
-- TODO: Match this up with the @Read@ hierarchy.
module Cardano.Wallet.Deposit.Read
    ( Network (..)
    , Slot
    , ChainPoint (..)

    , Addr
    , Address
    , fromRawAddress
    , toRawAddress
    , mockAddress

    , Ix
    , TxIn
    , TxOut
    , address
    , Value
    , UTxO

    , TxId
    , Tx
    , W.txScriptInvalid
    , W.collateralInputs
    , W.inputs
    , W.outputs
    , W.collateralOutput

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

import Data.ByteString
    ( ByteString
    )
import Numeric.Natural
    ( Natural
    )

import qualified Cardano.Chain.Genesis as Byron
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Address as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as W
import qualified Cardano.Wallet.Primitive.Types.UTxO as W
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
-- import qualified Ouroboros.Consensus.Cardano.Block as O

{-----------------------------------------------------------------------------
    Type definitions
    with dummies
------------------------------------------------------------------------------}
data Network = Testnet | Mainnet

-- Spec: type Slot = Natural
type Slot = W.SlotNo

data ChainPoint
    = Origin
    | At Slot
    deriving (Eq, Ord, Show)

-- newtype Addr = Addr { getAddressBytes :: ByteString }
--    deriving (Eq, Show)
type Addr = W.Address

-- | Synonym for readability.
-- The ledger specifications define @Addr@.
-- Byron addresses are represented by @Addr_bootstrap@.
type Address = Addr

fromRawAddress :: BS.ByteString -> Address
fromRawAddress = W.Address

toRawAddress :: Address -> BS.ByteString
toRawAddress (W.Address a) = a

mockAddress :: Show a => a -> Address
mockAddress = W.Address . B8.pack . show

dummyAddress :: Address
dummyAddress = W.Address . BS.pack $ replicate 32 0

type Ix = Natural

-- type TxIn = (TxId, Ix)
type TxIn = W.TxIn

-- type TxOut = (Addr, Value)
type TxOut = W.TxOut

address :: TxOut -> Address
address = TxOut.address

type Value = W.TokenBundle

-- type UTxO = Map TxIn TxOut
type UTxO = W.UTxO

type TxId = ByteString

type Tx = W.Tx

type TxBody = ()

type TxWitness = ()

type BlockNo = Natural

-- type Block = O.CardanoBlock O.StandardCrypto
data Block = Block
    { blockHeader :: BHeader
    , transactions :: [Tx]
    }
    deriving (Eq, Ord, Show)

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
    , slot = 42
    , bhash = ()
    }

-- GenesisData is not part of the ledger specification proper
type GenesisData = Byron.GenesisData
type GenesisHash = Byron.GenesisHash
