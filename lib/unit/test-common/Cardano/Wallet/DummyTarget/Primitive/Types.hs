{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}

module Cardano.Wallet.DummyTarget.Primitive.Types
    ( -- * Dummy values
      block0
    , dummyNetworkParameters
    , dummyGenesisParameters
    , dummyNodeProtocolParameters
    , dummyProtocolParameters
    , dummySlottingParameters
    , dummyTimeInterpreter
    , dummyGenesisHash
    , mkTxId
    , mkTx

      -- * Mocks
    , dummyNetworkLayer

      -- * Realistic values
    , babbageMainnetProtocolParameters
    ) where

import Prelude

import Cardano.Wallet.Network
    ( NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , hoistTimeInterpreter
    , mkSingleEraInterpreter
    )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , EpochLength (..)
    , ExecutionUnitPrices (..)
    , ExecutionUnits (..)
    , FeePolicy (..)
    , GenesisParameters (..)
    , LinearFunction (..)
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
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    , mockHash
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..)
    , TxMetadata (..)
    , TxScriptValidity (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR
    )
import Data.Functor.Identity
    ( Identity (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Time.Clock.POSIX
    ( posixSecondsToUTCTime
    )
import GHC.Stack
    ( HasCallStack
    )

import qualified Cardano.Api.Ledger as C
import qualified Cardano.Api.Shelley as C
import qualified Data.ByteString.Char8 as B8

{-----------------------------------------------------------------------------
    Dummy values
------------------------------------------------------------------------------}
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
    , getEpochLength = EpochLength 21_600
    , getActiveSlotCoefficient = ActiveSlotCoefficient 1
    , getSecurityParameter = Quantity 2_160
    }

dummyTimeInterpreter :: Monad m => TimeInterpreter m
dummyTimeInterpreter = hoistTimeInterpreter (pure . runIdentity)
    $ mkSingleEraInterpreter
        (getGenesisBlockDate dummyGenesisParameters)
        dummySlottingParameters

dummyTxParameters :: TxParameters
dummyTxParameters = TxParameters
    { getFeePolicy = LinearFee $ LinearFunction { intercept = 14, slope = 42 }
    , getTxMaxSize = Quantity 8_192
    , getTokenBundleMaxSize = TokenBundleMaxSize (TxSize 2_000)
    , getMaxExecutionUnits = ExecutionUnits 10 14
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
    , stakeKeyDeposit = Coin 0
    , eras = emptyEraInfo
    , maximumCollateralInputCount = 3
    , minimumCollateralPercentage = 150
    , executionUnitPrices =
        Just $ ExecutionUnitPrices
            { pricePerStep = 7.21e-5
            , pricePerMemoryUnit = 0.057_7
            }
    }

-- | Dummy parameters that are consistent with the @dummy*@ parameters.
dummyNodeProtocolParameters :: C.ProtocolParameters
dummyNodeProtocolParameters = C.ProtocolParameters
    { C.protocolParamProtocolVersion = (8,0)
    , C.protocolParamDecentralization =Just 1
    , C.protocolParamExtraPraosEntropy = Nothing
    , C.protocolParamMaxBlockHeaderSize = 1_100
    , C.protocolParamMaxBlockBodySize = 90_112
    , C.protocolParamMaxTxSize = 8_192
    , C.protocolParamTxFeeFixed = 14 -- B
    , C.protocolParamTxFeePerByte = 42 -- A
    , C.protocolParamMinUTxOValue = Nothing
    , C.protocolParamStakeAddressDeposit = C.Coin 0
    , C.protocolParamStakePoolDeposit = C.Coin 500_000_000
    , C.protocolParamMinPoolCost = C.Coin 340_000_000
    , C.protocolParamPoolRetireMaxEpoch = C.EpochInterval 18
    , C.protocolParamStakePoolTargetNum = 100
    , C.protocolParamPoolPledgeInfluence = 0.3 -- a0
    , C.protocolParamMonetaryExpansion = 0.003 -- rho
    , C.protocolParamTreasuryCut = 0.20 -- tau
    , C.protocolParamUTxOCostPerByte = Just $ C.Coin 43_10
    , C.protocolParamCostModels = mempty
    , C.protocolParamPrices =
        Just $ C.ExecutionUnitPrices
            { C.priceExecutionSteps = 7.21e-5
            , C.priceExecutionMemory = 0.057_7
            }
    , C.protocolParamMaxTxExUnits =
        Just $ C.ExecutionUnits
            { C.executionSteps = 10
            , C.executionMemory = 14
            }
    , C.protocolParamMaxBlockExUnits =
        Just $ C.ExecutionUnits
            { C.executionSteps = 20
            , C.executionMemory = 62
            }
    , C.protocolParamMaxValueSize = Just 2_000
    , C.protocolParamCollateralPercent = Just 150
    , C.protocolParamMaxCollateralInputs = Just 3
    }

dummyNetworkLayer :: HasCallStack => NetworkLayer m a
dummyNetworkLayer = NetworkLayer
    { chainSync = err "chainSync"
    , fetchNextBlock = err "fetchNextBlock"
    , currentNodeEra = err "currentNodeEra"
    , currentNodeTip = err "currentNodeTip"
    , watchNodeTip = err "watchNodeTip"
    , currentProtocolParameters = err "currentProtocolParameters"
    , currentProtocolParametersInRecentEras
        = err "currentProtocolParametersInRecentEras"
    , currentSlottingParameters = err "currentSlottingParameters"
    , postTx = err "postTx"
    , stakeDistribution = err "stakeDistribution"
    , getCachedRewardAccountBalance = err "getRewardCachedAccountBalance"
    , fetchRewardAccountBalances = err "fetchRewardAccountBalances"
    , timeInterpreter = err "timeInterpreter"
    , syncProgress = err "syncProgress"
    }
  where
    err subject = error $
        "`" <> subject <> "` is not implemented in the dummy network layer."

{-----------------------------------------------------------------------------
    Convenience functions
------------------------------------------------------------------------------}
-- | Construct a @Tx@, computing its hash using the dummy @mkTxId@.
mkTx
    :: Maybe TxCBOR
    -> Maybe Coin
    -> [(TxIn, Maybe TxOut)]
    -> [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> Maybe TxOut
    -> Map RewardAccount Coin
    -> Maybe TxMetadata
    -> Maybe TxScriptValidity
    -> Tx
mkTx cbor fees ins cins outs cout wdrls md validity =
    Tx
      { txId = (mkTxId ins outs wdrls md)
      , txCBOR = cbor
      , fee = fees
      , resolvedInputs = ins
      , resolvedCollateralInputs = cins
      , outputs = outs
      , collateralOutput = cout
      , withdrawals = wdrls
      , metadata = md
      , scriptValidity = validity
      }

-- | txId calculation for testing purposes.
mkTxId
    :: [(TxIn, Maybe TxOut)]
    -> [TxOut]
    -> Map RewardAccount Coin
    -> Maybe TxMetadata -> Hash "Tx"
mkTxId ins outs wdrls md = mockHash (ins, outs, wdrls, md)

{-----------------------------------------------------------------------------
    Realistic values
------------------------------------------------------------------------------}
-- | Data from mainnet on 2023-03-17.
-- NOTE: Does not include Plutus cost model (todo).
babbageMainnetProtocolParameters :: C.ProtocolParameters
babbageMainnetProtocolParameters = C.ProtocolParameters
    { C.protocolParamProtocolVersion = (8,0)
    , C.protocolParamDecentralization = Just 0
    , C.protocolParamExtraPraosEntropy = Nothing
    , C.protocolParamMaxBlockHeaderSize = 1_100
    , C.protocolParamMaxBlockBodySize = 90_112
    , C.protocolParamMaxTxSize = 16_384
    , C.protocolParamTxFeeFixed = 155_381 -- B
    , C.protocolParamTxFeePerByte = 44 -- A
    , C.protocolParamMinUTxOValue = Just $ C.Coin 1_000_000
    , C.protocolParamStakeAddressDeposit = C.Coin 2_000_000
    , C.protocolParamStakePoolDeposit = C.Coin 500_000_000
    , C.protocolParamMinPoolCost = C.Coin 340_000_000
    , C.protocolParamPoolRetireMaxEpoch = C.EpochInterval 18
    , C.protocolParamStakePoolTargetNum = 500
    , C.protocolParamPoolPledgeInfluence = 0.3 -- a0
    , C.protocolParamMonetaryExpansion = 0.003 -- rho
    , C.protocolParamTreasuryCut = 0.20 -- tau
    , C.protocolParamUTxOCostPerByte = Just $ C.Coin 4_310
    , C.protocolParamCostModels =
        mempty
        -- TODO: Include a Plutus cost model here.
    , C.protocolParamPrices =
        Just $ C.ExecutionUnitPrices
            { C.priceExecutionSteps = 7.21e-5
            , C.priceExecutionMemory = 0.057_7
            }
    , C.protocolParamMaxTxExUnits =
        Just $ C.ExecutionUnits
            { C.executionSteps = 10_000_000_000
            , C.executionMemory = 14_000_000
            }
    , C.protocolParamMaxBlockExUnits =
        Just $ C.ExecutionUnits
            { C.executionSteps = 20_000_000_000
            , C.executionMemory = 62_000_000
            }
    , C.protocolParamMaxValueSize = Just 5_000
    , C.protocolParamCollateralPercent = Just 150
    , C.protocolParamMaxCollateralInputs = Just 3
    }
