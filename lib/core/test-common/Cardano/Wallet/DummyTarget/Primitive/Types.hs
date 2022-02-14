{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Cardano.Wallet.DummyTarget.Primitive.Types
    ( -- * Dummy values
      block0
    , dummyNetworkParameters
    , dummyGenesisParameters
    , dummySlottingParameters
    , dummyTimeInterpreter
    , dummyGenesisHash
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
    , ExecutionUnits (..)
    , FeePolicy (..)
    , GenesisParameters (..)
    , MinimumUTxOValue (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , SlotLength (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , StartTime (..)
    , TokenBundleMaxSize (..)
    , TxParameters (..)
    , emptyEraInfo
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..), mockHash )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..)
    , TxIn (..)
    , TxMetadata (..)
    , TxOut (..)
    , TxScriptValidity (..)
    , TxSize (..)
    )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime )

import qualified Data.ByteString.Char8 as B8

dummyGenesisHash :: Hash "Genesis"
dummyGenesisHash = Hash (B8.replicate 32 '1')

block0 :: Block
block0 = Block
    { header = BlockHeader
        { slotNo = SlotNo 0
        , blockHeight = Quantity 0
        , headerHash = Hash $ getHash dummyGenesisHash
        , parentHeaderHash = Nothing
        }
    , transactions = []
    , delegations = []
    }

dummyGenesisParameters :: GenesisParameters
dummyGenesisParameters = GenesisParameters
    { getGenesisBlockHash = dummyGenesisHash
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
    , getTokenBundleMaxSize = TokenBundleMaxSize (TxSize 4000)
    , getMaxExecutionUnits = ExecutionUnits 0 0
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
    , minimumUTxOvalue = MinimumUTxOValue $ Coin 0
    , stakeKeyDeposit = Coin 0
    , eras = emptyEraInfo
    , maximumCollateralInputCount = 3
    , minimumCollateralPercentage = 100
    , executionUnitPrices = Nothing
    }

-- | Construct a @Tx@, computing its hash using the dummy @mkTxId@.
mkTx
    :: Maybe Coin
    -> [(TxIn, Coin)]
    -> [(TxIn, Coin)]
    -> [TxOut]
    -> Map RewardAccount Coin
    -> Maybe TxMetadata
    -> Maybe TxScriptValidity
    -> Tx
mkTx fees ins cins outs wdrls md validity =
    Tx
      { txId = (mkTxId ins outs wdrls md)
      , fee = fees
      , resolvedCollateral = cins
      , resolvedInputs = ins
      , outputs = outs
      , withdrawals = wdrls
      , metadata = md
      , scriptValidity = validity
      }

-- | txId calculation for testing purposes.
mkTxId
    :: [(TxIn, Coin)]
    -> [TxOut]
    -> Map RewardAccount Coin
    -> Maybe TxMetadata -> Hash "Tx"
mkTxId ins outs wdrls md = mockHash (ins, outs, wdrls, md)

dummyNetworkLayer :: NetworkLayer m a
dummyNetworkLayer = NetworkLayer
    { chainSync = error "chainSync: not implemented"
    , lightSync = Nothing
    , currentNodeEra = error "currentNodeEra: not implemented"
    , currentNodeTip = error "currentNodeTip: not implemented"
    , watchNodeTip = error "watchNodeTip: not implemented"
    , currentProtocolParameters = error "currentProtocolParameters: not implemented"
    , currentNodeProtocolParameters = error "currentNodeProtocolParameters: not implemented"
    , currentSlottingParameters = error "currentSlottingParameters: not implemented"
    , postTx = error "postTx: not implemented"
    , stakeDistribution = error "stakeDistribution: not implemented"
    , getCachedRewardAccountBalance = error "getRewardCachedAccountBalance: not implemented"
    , fetchRewardAccountBalances = error "fetchRewardAccountBalances: not implemented"
    , timeInterpreter = error "timeInterpreter: not implemented"
    , syncProgress = error "syncProgress: not implemented"
    }
