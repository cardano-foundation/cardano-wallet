{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget
    , Tx (..)
    , block0
    , genesisParameters
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( unXPub )
import Cardano.Wallet.DB.Sqlite
    ( PersistTx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..), WalletKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , DecodeAddress (..)
    , DefineTx
    , EncodeAddress (..)
    , EpochLength (..)
    , FeePolicy (..)
    , Hash (..)
    , SlotLength (..)
    , StartTime (..)
    , TxIn (..)
    , TxOut (..)
    , slotMinBound
    )
import Control.DeepSeq
    ( NFData )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.Bifunctor
    ( bimap )
import Data.ByteArray.Encoding
    ( Base (Base16), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Fmt
    ( Buildable (..), blockListF' )
import GHC.Generics
    ( Generic )

import qualified Cardano.Byron.Codec.Cbor as CBOR
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

data DummyTarget

data Tx = Tx
    { inputs :: ![TxIn]
    , outputs :: ![TxOut]
    } deriving (Show, Generic, Ord, Eq)

instance NFData Tx

instance PersistTx DummyTarget where
    resolvedInputs = flip zip (repeat Nothing) . inputs
    mkTx _ inps = Tx (fst <$> inps)

instance KeyToAddress DummyTarget SeqKey where
    keyToAddress =
        Address . BS.take 8 . convertToBase Base16 . unXPub . getRawKey

instance KeyToAddress DummyTarget RndKey where
    keyToAddress k = Address
        $ CBOR.toStrictByteString
        $ CBOR.encodeAddress (getRawKey k)
            [ CBOR.encodeDerivationPathAttr pwd acctIx addrIx ]
      where
        (acctIx, addrIx) = derivationPath k
        pwd = payloadPassphrase k

instance EncodeAddress DummyTarget where
    encodeAddress _ = T.decodeUtf8 . convertToBase Base16 . unAddress

instance DecodeAddress DummyTarget where
    decodeAddress _ = bimap decodingError Address
        . convertFromBase Base16
        . T.encodeUtf8
      where
        decodingError _ = TextDecodingError
            "Unable to decode Address: expected Base16 encoding"

deriving instance Eq (SeqState DummyTarget)

instance DefineTx DummyTarget where
    type Tx DummyTarget = Tx
    txId = Hash . blake2b256 . B8.pack . show
      where
        blake2b256 :: ByteString -> ByteString
        blake2b256 =
            BA.convert . hash @_ @Blake2b_256
    inputs = inputs
    outputs = outputs

instance Buildable DummyTarget where
    build _ = mempty

instance Buildable Tx where
    build (Tx ins outs) = mempty
        <> blockListF' "~>" build ins
        <> blockListF' "<~" build outs

block0 :: Block Tx
block0 = Block
    { header = BlockHeader
        { slotId = slotMinBound
        , blockHeight = Quantity 0
        , headerHash = Hash "dummy-block0-hash"
        , parentHeaderHash = Hash "genesis"
        }
    , transactions = []
    }

genesisParameters  :: BlockchainParameters
genesisParameters = BlockchainParameters
    { getGenesisBlockHash = Hash "genesis"
    , getGenesisBlockDate = StartTime $ posixSecondsToUTCTime 0
    , getFeePolicy = LinearFee (Quantity 14) (Quantity 42)
    , getSlotLength = SlotLength 1
    , getEpochLength = EpochLength 21600
    , getTxMaxSize = Quantity 8192
    , getEpochStability = Quantity 2160
    }
