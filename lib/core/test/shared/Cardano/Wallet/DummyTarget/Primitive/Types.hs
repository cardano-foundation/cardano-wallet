{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget
    , Tx (..)
    , block0
    , genesisParameters
    ) where

import Prelude

import Cardano.Wallet.DB.Sqlite
    ( PersistTx (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DefineTx
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
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Fmt
    ( Buildable (..), blockListF' )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8

data DummyTarget

data Tx = Tx
    { inputs :: ![TxIn]
    , outputs :: ![TxOut]
    } deriving (Show, Generic, Ord, Eq)

instance NFData Tx

instance PersistTx DummyTarget where
    resolvedInputs = flip zip (repeat Nothing) . inputs
    mkTx _ inps = Tx (fst <$> inps)

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
    , getFeePolicy = LinearFee (Quantity 14) (Quantity 42) (Quantity 5)
    , getSlotLength = SlotLength 1
    , getEpochLength = EpochLength 21600
    , getTxMaxSize = Quantity 8192
    , getEpochStability = Quantity 2160
    }
