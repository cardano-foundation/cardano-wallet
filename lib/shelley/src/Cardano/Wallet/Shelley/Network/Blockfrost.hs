{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Network Layer implementation that uses Blockfrost API
--
module Cardano.Wallet.Shelley.Network.Blockfrost
    ( withNetworkLayer
    , Log

    -- * Blockfrost <-> Cardano translation
    , blockToBlockHeader

    -- * Internal
    , getPoolPerformanceEstimate
    , eraByEpoch
    ) where

import Prelude

import qualified Blockfrost.Client as BF
import qualified Cardano.Api.Shelley as Node
import qualified Data.Sequence as Seq

import Cardano.Api
    ( AnyCardanoEra )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Tracer
    ( Tracer )
import Cardano.BM.Tracing
    ( HasSeverityAnnotation (getSeverityAnnotation) )
import Cardano.Pool.Rank
    ( RewardParams (..) )
import Cardano.Pool.Rank.Likelihood
    ( BlockProduction (..), PerformanceEstimate (..), estimatePoolPerformance )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , DecentralizationLevel (..)
    , EpochNo (EpochNo)
    , ExecutionUnitPrices (..)
    , ExecutionUnits (..)
    , FeePolicy (LinearFee)
    , LinearFunction (..)
    , MinimumUTxOValue (..)
    , ProtocolParameters (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , TokenBundleMaxSize (..)
    , TxParameters (..)
    , emptyEraInfo
    , executionMemory
    , executionSteps
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin, unCoin) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxSize (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( forever, (<=<) )
import Control.Monad.Error.Class
    ( MonadError, liftEither, throwError )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, withExceptT )
import Data.Bits
    ( Bits )
import Data.Functor
    ( (<&>) )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Quantity
    ( MkPercentageError (PercentageOutOfBoundsError)
    , Quantity (..)
    , mkPercentage
    )
import Data.Text.Class
    ( FromText (fromText), TextDecodingError (..), ToText (..) )
import Data.Traversable
    ( for )
import Fmt
    ( pretty )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, StandardCrypto )
import UnliftIO
    ( throwIO )
import UnliftIO.Async
    ( async, link )
import UnliftIO.Exception
    ( Exception )


{-------------------------------------------------------------------------------
    NetworkLayer
-------------------------------------------------------------------------------}
data BlockfrostError
    = ClientError BF.BlockfrostError
    | NoSlotError BF.Block
    | IntegralCastError String
    | NoBlockHeight BF.Block
    | InvalidBlockHash BF.BlockHash TextDecodingError
    | InvalidDecentralizationLevelPercentage Double
    | UnknownEraForEpoch EpochNo
    deriving (Show, Eq)

newtype BlockfrostException = BlockfrostException BlockfrostError
    deriving stock (Show)
    deriving anyclass (Exception)

data Log = MsgWatcherUpdate BlockHeader BracketLog

instance ToText Log where
    toText = \case
        MsgWatcherUpdate blockHeader bracketLog ->
            "Update watcher with tip: " <> pretty blockHeader <>
            ". Callback " <> toText bracketLog <> ". "

instance HasSeverityAnnotation Log where
    getSeverityAnnotation = \case
      MsgWatcherUpdate _ _ -> Info

withNetworkLayer
    :: Tracer IO Log
    -> BF.Project
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -> IO a
withNetworkLayer tr project k = k NetworkLayer
    { chainSync = \_tr _chainFollower -> pure ()
    , lightSync = Nothing
    , currentNodeTip
    , currentNodeEra
    , currentProtocolParameters
    , currentSlottingParameters = undefined
    , watchNodeTip
    , postTx = undefined
    , stakeDistribution = undefined
    , getCachedRewardAccountBalance = undefined
    , fetchRewardAccountBalances = undefined
    , timeInterpreter = undefined
    , syncProgress = undefined
    }
  where
    currentNodeTip :: IO BlockHeader
    currentNodeTip = runBlockfrost BF.getLatestBlock
    -- ^ TODO: use cached value while retrying

    watchNodeTip :: (BlockHeader -> IO ()) -> IO ()
    watchNodeTip callback = link =<< async (pollNodeTip callback)
      where
        pollNodeTip :: (BlockHeader -> IO ()) -> IO ()
        pollNodeTip cb = forever $ do
            header <- runBlockfrost BF.getLatestBlock
            bracketTracer (MsgWatcherUpdate header >$< tr) $ cb header
            threadDelay 2_000_000

    currentProtocolParameters :: IO ProtocolParameters
    currentProtocolParameters = runBlockfrost BF.getLatestEpochProtocolParams

    currentNodeEra :: IO AnyCardanoEra
    currentNodeEra = handleBlockfrostError $ do
        BF.EpochInfo {_epochInfoEpoch} <- liftBlockfrost BF.getLatestEpoch
        epoch <- fromBlockfrostM _epochInfoEpoch
        eraByEpoch epoch

    handleBlockfrostError :: ExceptT BlockfrostError IO a -> IO a
    handleBlockfrostError =
        either (throwIO . BlockfrostException) pure <=< runExceptT

    runBlockfrost ::
        forall b w. FromBlockfrost b w => BF.BlockfrostClientT IO b -> IO w
    runBlockfrost =
        handleBlockfrostError . (fromBlockfrostM @b @w <=< liftBlockfrost)

    liftBlockfrost :: BF.BlockfrostClientT IO a -> ExceptT BlockfrostError IO a
    liftBlockfrost =
        withExceptT ClientError . ExceptT . BF.runBlockfrostClientT project

blockToBlockHeader ::
    forall m. MonadError BlockfrostError m => BF.Block -> m BlockHeader
blockToBlockHeader block@BF.Block{..} = do
    slotNo <- case _blockSlot of
        Just s -> pure $ SlotNo $ fromIntegral $ BF.unSlot s
        Nothing -> throwError $ NoSlotError block
    blockHeight <- case _blockHeight of
        Just height -> pure $ Quantity $ fromIntegral height
        Nothing -> throwError $ NoBlockHeight block
    headerHash <- parseBlockHeader _blockHash
    parentHeaderHash <- for _blockPreviousBlock parseBlockHeader
    pure BlockHeader { slotNo, blockHeight, headerHash, parentHeaderHash }
  where
    parseBlockHeader :: BF.BlockHash -> m (Hash "BlockHeader")
    parseBlockHeader blockHash =
        case fromText (BF.unBlockHash blockHash) of
            Right hash -> pure hash
            Left tde -> throwError $ InvalidBlockHash blockHash tde

class FromBlockfrost b w where
    fromBlockfrost :: b -> Either BlockfrostError w

fromBlockfrostM
    :: FromBlockfrost b w => MonadError BlockfrostError m => b -> m w
fromBlockfrostM = liftEither . fromBlockfrost

instance FromBlockfrost BF.Block BlockHeader where
    fromBlockfrost block@BF.Block{..} = do
        slotNo <- _blockSlot <?> NoSlotError block >>= fromBlockfrostM
        blockHeight <-
            _blockHeight <?> NoBlockHeight block >>=
                (Quantity <$>) . (<?#> "BlockHeight")
        headerHash <- parseBlockHeader _blockHash
        parentHeaderHash <- for _blockPreviousBlock parseBlockHeader
        pure BlockHeader { slotNo, blockHeight, headerHash, parentHeaderHash }
      where
        parseBlockHeader blockHash =
            case fromText (BF.unBlockHash blockHash) of
                Right hash -> pure hash
                Left tde -> throwError $ InvalidBlockHash blockHash tde

instance FromBlockfrost BF.ProtocolParams ProtocolParameters where
    fromBlockfrost BF.ProtocolParams{..} = do
        decentralizationLevel <-
            let percentage = mkPercentage $
                    toRational _protocolParamsDecentralisationParam
            in case percentage of
                Left PercentageOutOfBoundsError ->
                    throwError $ InvalidDecentralizationLevelPercentage
                        _protocolParamsDecentralisationParam
                Right level -> pure $ DecentralizationLevel level
        minFeeA <-
            _protocolParamsMinFeeA <?#> "MinFeeA"
        minFeeB <-
            _protocolParamsMinFeeB <?#> "MinFeeB"
        maxTxSize <-
            _protocolParamsMaxTxSize <?#> "MaxTxSize"
        maxValSize <-
            BF.unQuantity _protocolParamsMaxValSize <?#> "MaxValSize"
        maxTxExSteps <-
            BF.unQuantity _protocolParamsMaxTxExSteps <?#> "MaxTxExSteps"
        maxBlockExSteps <-
            BF.unQuantity _protocolParamsMaxBlockExSteps <?#> "MaxBlockExSteps"
        maxBlockExMem <-
            BF.unQuantity _protocolParamsMaxBlockExMem <?#> "MaxBlockExMem"
        maxTxExMem <-
            BF.unQuantity _protocolParamsMaxTxExMem <?#> "MaxTxExMem"
        desiredNumberOfStakePools <-
            _protocolParamsNOpt <?#> "NOpt"
        minimumUTxOvalue <-
            MinimumUTxOValueCostPerWord . Coin <$>
                intCast @_ @Integer _protocolParamsCoinsPerUtxoWord
                    <?#> "CoinsPerUtxoWord"
        stakeKeyDeposit <-
            Coin <$>
                intCast @_ @Integer _protocolParamsKeyDeposit <?#> "KeyDeposit"
        maxCollateralInputs <-
            _protocolParamsMaxCollateralInputs <?#> "MaxCollateralInputs"
        collateralPercent <-
            _protocolParamsCollateralPercent <?#> "CollateralPercent"
        protoMajorVer <-
            _protocolParamsProtocolMajorVer <?#> "ProtocolMajorVer"
        protoMinorVer <-
            _protocolParamsProtocolMinorVer <?#> "ProtocolMinorVer"
        maxBlockHeaderSize <-
            _protocolParamsMaxBlockHeaderSize <?#> "MaxBlockHeaderSize"
        maxBlockBodySize <-
            _protocolParamsMaxBlockSize <?#> "MaxBlockBodySize"
        eMax <-
            _protocolParamsEMax <?#> "EMax"
        nOpt <-
            _protocolParamsNOpt <?#> "NOpt"

        pure ProtocolParameters
            { eras = emptyEraInfo
            , txParameters = TxParameters
                { getFeePolicy =
                    LinearFee $ LinearFunction
                        { intercept = fromIntegral minFeeB
                        , slope = fromIntegral minFeeA
                        }
                , getTxMaxSize =
                    Quantity maxTxSize
                , getTokenBundleMaxSize =
                    TokenBundleMaxSize $ TxSize maxValSize
                , getMaxExecutionUnits =
                    ExecutionUnits
                        { executionSteps = maxTxExSteps
                        , executionMemory = maxTxExMem
                        }
                }
            , executionUnitPrices = Just $ ExecutionUnitPrices
                { pricePerStep = toRational _protocolParamsPriceStep
                , pricePerMemoryUnit = toRational _protocolParamsPriceMem
                }
            , maximumCollateralInputCount = maxCollateralInputs
            , minimumCollateralPercentage = collateralPercent
            , currentNodeProtocolParameters = Just Node.ProtocolParameters
                { protocolParamProtocolVersion =
                    (protoMajorVer, protoMinorVer)
                , protocolParamDecentralization =
                    toRational _protocolParamsDecentralisationParam
                , protocolParamExtraPraosEntropy = Nothing
                , protocolParamMaxBlockHeaderSize = maxBlockHeaderSize
                , protocolParamMaxBlockBodySize = maxBlockBodySize
                , protocolParamMaxTxSize = intCast maxTxSize
                , protocolParamTxFeeFixed = minFeeB
                , protocolParamTxFeePerByte = minFeeA
                , protocolParamMinUTxOValue =
                    Just $ Node.Lovelace $ intCast _protocolParamsMinUtxo
                , protocolParamStakeAddressDeposit =
                    Node.Lovelace $
                        intCast @_ @Integer _protocolParamsKeyDeposit
                , protocolParamStakePoolDeposit =
                    Node.Lovelace $
                        intCast @_ @Integer _protocolParamsPoolDeposit
                , protocolParamMinPoolCost =
                    Node.Lovelace $
                        intCast @_ @Integer _protocolParamsMinPoolCost
                , protocolParamPoolRetireMaxEpoch = Node.EpochNo eMax
                , protocolParamStakePoolTargetNum = nOpt
                , protocolParamPoolPledgeInfluence =
                    toRational _protocolParamsA0
                , protocolParamMonetaryExpansion = toRational _protocolParamsRho
                , protocolParamTreasuryCut = toRational _protocolParamsTau
                , protocolParamUTxOCostPerWord =
                    Just $ Node.Lovelace $
                        intCast _protocolParamsCoinsPerUtxoWord
                , protocolParamCostModels =
                    mempty
                    -- Cost models aren't available via BF
                    -- TODO: Hardcode or retrieve from elswhere.
                    -- https://input-output.atlassian.net/browse/ADP-1572
                , protocolParamPrices =
                    Just $ Node.ExecutionUnitPrices
                        { priceExecutionSteps =
                            toRational _protocolParamsPriceStep
                        , priceExecutionMemory =
                            toRational _protocolParamsPriceMem
                        }
                , protocolParamMaxTxExUnits =
                    Just $ Node.ExecutionUnits
                        { executionSteps = maxTxExSteps
                        , executionMemory = maxTxExMem
                        }
                , protocolParamMaxBlockExUnits =
                    Just $ Node.ExecutionUnits
                        { executionSteps = maxBlockExSteps
                        , executionMemory = maxBlockExMem
                        }
                , protocolParamMaxValueSize = Just maxValSize
                , protocolParamCollateralPercent = Just collateralPercent
                , protocolParamMaxCollateralInputs =
                    Just $ intCast maxCollateralInputs
                }
            , ..
            }

instance FromBlockfrost BF.Slot SlotNo where
    fromBlockfrost = fmap SlotNo . (<?#> "SlotNo") . BF.unSlot

instance FromBlockfrost BF.Epoch EpochNo where
    fromBlockfrost = pure . fromIntegral


{-
┌───────┬───────┬─────────┐
│ Epoch │ Major │   Era   │
├───────┼───────┼─────────┤
│  ...  │   6   │ Alonzo  │
│  298  │   6   │ Alonzo  │
├───────┼───────┼─────────┤
│  297  │   5   │ Alonzo  │
│  ...  │   5   │ Alonzo  │
│  290  │   5   │ Alonzo  │
├───────┼───────┼─────────┤
│  289  │   4   │  Mary   │
│  ...  │   4   │  Mary   │
│  251  │   4   │  Mary   │
├───────┼───────┼─────────┤
│  250  │   3   │ Allegra │
│  ...  │   3   │ Allegra │
│  236  │   3   │ Allegra │
├───────┼───────┼─────────┤
│  235  │   2   │ Shelley │
│  ...  │   2   │ Shelley │
│  202  │   2   │ Shelley │
├───────┼───────┼─────────┤
│  201  │   1   │  Byron  │
│  ...  │   1   │  Byron  │
└───────┴───────┴─────────┘
-}
eraByEpoch :: MonadError BlockfrostError m => EpochNo -> m AnyCardanoEra
eraByEpoch epoch =
    case dropWhile ((> epoch) . snd) (reverse eraBoundaries) of
        (era, _) : _ -> pure era
        _ -> throwError $ UnknownEraForEpoch epoch

eraBoundaries :: [(Node.AnyCardanoEra, EpochNo)]
eraBoundaries = [minBound .. maxBound] <&> \era -> (era, epochEraStartsAt era)
  where
    -- When new era is added this function reminds to update itself:
    -- "Pattern match(es) are non-exhaustive"
    epochEraStartsAt :: Node.AnyCardanoEra -> EpochNo
    epochEraStartsAt = EpochNo . \case
        Node.AnyCardanoEra Node.AlonzoEra  -> 290
        Node.AnyCardanoEra Node.MaryEra    -> 251
        Node.AnyCardanoEra Node.AllegraEra -> 236
        Node.AnyCardanoEra Node.ShelleyEra -> 202
        Node.AnyCardanoEra Node.ByronEra   -> 0

-- | Raises an error in case of an absent value
(<?>) :: MonadError e m => Maybe a -> e -> m a
(<?>) Nothing e = throwError e
(<?>) (Just a) _ = pure a

infixl 8 <?>
{-# INLINE (<?>) #-}

-- | Casts integral values safely or raises an `IntegralCastError`
(<?#>) ::
    ( MonadError BlockfrostError m
    , Integral a, Integral b
    , Bits a, Bits b
    ) =>
    a -> String -> m b
(<?#>) a e = intCastMaybe a <?> IntegralCastError e

infixl 8 <?#>
{-# INLINE (<?#>) #-}


{-------------------------------------------------------------------------------
    Stake Pools
-------------------------------------------------------------------------------}
-- | Estimate the performance of a stake pool based on
-- the past 50 epochs (or less if the pool is younger than that).
--
-- Uses 'estimatePoolPerformance' from "Cardano.Pool.Rank.Likelihood"
-- for this purpose.
getPoolPerformanceEstimate
    :: BF.MonadBlockfrost m
    => SlottingParameters
    -> DecentralizationLevel
    -> RewardParams
    -> BF.PoolId
    -> m PerformanceEstimate
getPoolPerformanceEstimate sp dl rp pid = do
    hist <- BF.getPoolHistory' pid get50 BF.Descending
    pure
        . estimatePoolPerformance sp dl
        . Seq.fromList . map toBlockProduction
        $ hist
  where
    get50 = BF.Paged { BF.countPerPage = 50, BF.pageNumber = 1 }
    toBlockProduction p = BlockProduction
        { blocksProduced = fromIntegral $ BF._poolHistoryBlocks p
        , stakeRelative =
            fromIntegral (BF._poolHistoryActiveStake p)
            / fromIntegral (unCoin $ totalStake rp)
            -- _poolHistoryActiveSize would be incorrect here
        }
