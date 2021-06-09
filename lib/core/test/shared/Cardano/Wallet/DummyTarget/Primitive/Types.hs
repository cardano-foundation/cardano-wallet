{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.DummyTarget.Primitive.Types
    ( -- * Dummy values
      block0
    , dummyNetworkParameters
    , dummyGenesisParameters
    , dummySlottingParameters
    , dummyTimeInterpreter
    , genesisHash
    , mockHash
    , mkTxId
    , mkTx

      -- * Mocks
    , dummyNetworkLayer
    ) where

import Prelude

import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter, hoistTimeInterpreter, mkSingleEraInterpreter )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , EpochLength (..)
    , FeePolicy (..)
    , GenesisParameters (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , StartTime (..)
    , TxParameters (..)
    , emptyEraInfo
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxIn (..), TxMetadata (..), TxOut (..) )
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

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8

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
    }

dummySlottingParameters :: SlottingParameters
dummySlottingParameters = SlottingParameters
    { getSlotLength = SlotLength 1
    , getEpochLength = EpochLength 21600
    , getActiveSlotCoefficient = ActiveSlotCoefficient 1
    , getSecurityParameter = Quantity 2160
    }

dummyTimeInterpreter :: Monad m => TimeInterpreter m
dummyTimeInterpreter = hoistTimeInterpreter (pure . runIdentity)
    $ mkSingleEraInterpreter
        (getGenesisBlockDate dummyGenesisParameters)
        dummySlottingParameters

dummyTxParameters :: TxParameters
dummyTxParameters = TxParameters
    { getFeePolicy = LinearFee (Quantity 14) (Quantity 42)
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
    , stakeKeyDeposit = Coin 0
    , eras = emptyEraInfo
    }

-- | Construct a @Tx@, computing its hash using the dummy @mkTxId@.
mkTx
    :: Maybe Coin
    -> [(TxIn, Coin)]
    -> [TxOut]
    -> Map RewardAccount Coin
    -> Maybe TxMetadata
    -> Tx
mkTx fees ins outs wdrls md =
    Tx (mkTxId ins outs wdrls md) fees ins outs wdrls md

-- | txId calculation for testing purposes.
mkTxId
    :: [(TxIn, Coin)]
    -> [TxOut]
    -> Map RewardAccount Coin
    -> Maybe TxMetadata -> Hash "Tx"
mkTxId ins outs wdrls md = mockHash (ins, outs, wdrls, md)

-- | Construct a good-enough hash for testing
mockHash :: Show a => a -> Hash whatever
mockHash = Hash . blake2b256 . B8.pack . show
  where
     blake2b256 :: ByteString -> ByteString
     blake2b256 =
         BA.convert . hash @_ @Blake2b_256

dummyNetworkLayer :: NetworkLayer m a
dummyNetworkLayer = NetworkLayer
    { nextBlocks = error "nextBlocks: not implemented"
    , initCursor = error "initCursor: not implemented"
    , destroyCursor = error "destroyCursor: not implemented"
    , cursorSlotNo = error "cursorSlotNo: not implemented"
    , currentNodeEra = error "currentNodeEra: not implemented"
    , currentNodeTip = error "currentNodeTip: not implemented"
    , watchNodeTip = error "watchNodeTip: not implemented"
    , currentProtocolParameters = error "currentProtocolParameters: not implemented"
    , currentSlottingParameters = error "currentSlottingParameters: not implemented"
    , postTx = error "postTx: not implemented"
    , stakeDistribution = error "stakeDistribution: not implemented"
    , getCachedRewardAccountBalance = error "getRewardCachedAccountBalance: not implemented"
    , fetchRewardAccountBalances = error "fetchRewardAccountBalances: not implemented"
    , timeInterpreter = error "timeInterpreter: not implemented"
    , syncProgress = error "syncProgress: not implemented"
    }
