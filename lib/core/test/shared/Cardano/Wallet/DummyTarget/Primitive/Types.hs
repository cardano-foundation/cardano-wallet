{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget
    , block0
    , dummyNetworkParameters
    , dummyGenesisParameters
    , dummyProtocolParameters
    , dummyTimeInterpreter
    , genesisHash
    , mockHash
    , mkTxId
    , mkTx
    ) where

import Prelude

import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, singleEraInterpreter )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , ChimericAccount (..)
    , Coin (..)
    , EpochLength (..)
    , FeePolicy (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , StartTime (..)
    , Tx (..)
    , TxIn (..)
    , TxMetadata (..)
    , TxOut (..)
    , TxParameters (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Crypto.Hash
    ( Blake2b_256, hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )
import Fmt
    ( Buildable (..) )

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8

data DummyTarget

instance Buildable DummyTarget where
    build _ = mempty

genesisHash :: Hash "Genesis"
genesisHash = Hash (B8.replicate 32 '0')

block0 :: Block
block0 = Block
    { header = BlockHeader
        { slotNo = SlotNo 0
        , blockHeight = Quantity 0
        , headerHash = mockHash $ SlotNo 0
        , parentHeaderHash = coerce genesisHash
        }
    , transactions = []
    , delegations = []
    }

dummyGenesisParameters :: GenesisParameters
dummyGenesisParameters = GenesisParameters
    { getGenesisBlockHash = genesisHash
    , getGenesisBlockDate = StartTime $ posixSecondsToUTCTime 0
    , getEpochStability = Quantity 2160
    }

dummySlottingParameters :: SlottingParameters
dummySlottingParameters = SlottingParameters
    { getSlotLength = SlotLength 1
    , getEpochLength = EpochLength 21600
    , getActiveSlotCoefficient = ActiveSlotCoefficient 1
    }

dummyTimeInterpreter :: Monad m => TimeInterpreter m
dummyTimeInterpreter = pure
    . runIdentity
    . singleEraInterpreter
        (getGenesisBlockDate dummyGenesisParameters)
        dummySlottingParameters

dummyTxParameters :: TxParameters
dummyTxParameters = TxParameters
    { getFeePolicy = LinearFee (Quantity 14) (Quantity 42) (Quantity 5)
    , getTxMaxSize = Quantity 8192
    }

dummyNetworkParameters :: NetworkParameters
dummyNetworkParameters = NetworkParameters
    { genesisParameters = dummyGenesisParameters
    , slottingParameters = dummySlottingParameters
    , protocolParameters = dummyProtocolParameters
    }

dummyProtocolParameters :: ProtocolParameters
dummyProtocolParameters = ProtocolParameters
    { decentralizationLevel = minBound
    , txParameters = dummyTxParameters
    , desiredNumberOfStakePools = 100
    , minimumUTxOvalue = Coin 0
    , hardforkEpochNo = Nothing
    }

-- | Construct a @Tx@, computing its hash using the dummy @mkTxId@.
mkTx :: [(TxIn, Coin)] -> [TxOut] -> Map ChimericAccount Coin -> Maybe TxMetadata -> Tx
mkTx ins outs wdrls md = Tx (mkTxId ins outs wdrls md) ins outs wdrls md

-- | txId calculation for testing purposes.
mkTxId :: [(TxIn, Coin)] -> [TxOut] -> Map ChimericAccount Coin -> Maybe TxMetadata -> Hash "Tx"
mkTxId ins outs wdrls md = mockHash (ins, outs, wdrls, md)

-- | Construct a good-enough hash for testing
mockHash :: Show a => a -> Hash whatever
mockHash = Hash . blake2b256 . B8.pack . show
  where
     blake2b256 :: ByteString -> ByteString
     blake2b256 =
         BA.convert . hash @_ @Blake2b_256
