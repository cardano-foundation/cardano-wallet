{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    ) where

import Prelude

import qualified Blockfrost.Client as BF

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
    , SlotNo (SlotNo)
    , SlottingParameters (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( forever )
import Control.Monad.Error.Class
    ( MonadError, throwError )
import Control.Monad.Trans.Except
    ( ExceptT (ExceptT), runExceptT )
import Data.Bifunctor
    ( first )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.Quantity
    ( Quantity (..) )
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

import qualified Data.Sequence as Seq

{-------------------------------------------------------------------------------
    NetworkLayer
-------------------------------------------------------------------------------}
data BlockfrostError
    = ClientError BF.BlockfrostError
    | NoSlotError BF.Block
    | NoBlockHeight BF.Block
    | InvalidBlockHash BF.BlockHash TextDecodingError
    deriving (Show)

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
    , currentNodeEra = undefined
    , currentProtocolParameters = undefined
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
    currentNodeTip = runExceptT fetchLatestBlockHeader >>= \case
        -- TODO: use cached value while retrying
        Left err -> throwIO (BlockfrostException err)
        Right header -> pure header

    watchNodeTip :: (BlockHeader -> IO ()) -> IO ()
    watchNodeTip callback = link =<< async (pollNodeTip callback)
      where
        pollNodeTip :: (BlockHeader -> IO ()) -> IO ()
        pollNodeTip cb = forever $ do
            runExceptT fetchLatestBlockHeader >>= \case
                Left err -> throwIO (BlockfrostException err)
                Right header ->
                    bracketTracer (MsgWatcherUpdate header >$< tr) $ cb header
            threadDelay 2_000_000

    fetchLatestBlockHeader :: ExceptT BlockfrostError IO BlockHeader
    fetchLatestBlockHeader =
        runBlockfrost BF.getLatestBlock >>= blockToBlockHeader

    runBlockfrost :: BF.BlockfrostClientT IO a -> ExceptT BlockfrostError IO a
    runBlockfrost =
        ExceptT . (first ClientError <$>) . BF.runBlockfrostClientT project

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
    fromBlockfrost :: forall m. MonadError BlockfrostError m => b -> m w

instance FromBlockfrost BF.Block BlockHeader where
    fromBlockfrost block@BF.Block{..} = do
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
        let intToQuantity :: (Num q, Integral i) => i -> Quantity s q
            intToQuantity = Quantity . fromIntegral
            txParameters = TxParameters
                { getFeePolicy =  LinearFee
                    (intToQuantity _protocolParamsMinFeeA)
                    (intToQuantity _protocolParamsMinFeeB)
                , getTxMaxSize =
                    intToQuantity _protocolParamsMaxTxSize
                , getTokenBundleMaxSize =
                    TokenBundleMaxSize $ TxSize $ fromIntegral $
                        BF.unQuantity _protocolParamsMaxValSize
                , getMaxExecutionUnits =
                    ExecutionUnits
                        { executionSteps = fromIntegral $
                            BF.unQuantity _protocolParamsMaxTxExSteps
                        , executionMemory = fromIntegral $
                            BF.unQuantity _protocolParamsMaxTxExMem
                        }
                }
            desiredNumberOfStakePools = fromIntegral _protocolParamsNOpt
            minimumUTxOvalue = MinimumUTxOValueCostPerWord $ Coin $
                fromIntegral _protocolParamsCoinsPerUtxoWord
            stakeKeyDeposit = Coin $ fromIntegral _protocolParamsKeyDeposit
            eras = emptyEraInfo
            maximumCollateralInputCount =
                fromIntegral _protocolParamsMaxCollateralInputs
            minimumCollateralPercentage =
                fromIntegral _protocolParamsCollateralPercent
            executionUnitPrices = Just $ ExecutionUnitPrices
                { pricePerStep = toRational _protocolParamsPriceStep
                , pricePerMemoryUnit = toRational _protocolParamsPriceMem
                }
        currentNodeProtocolParameters <- undefined
        pure ProtocolParameters {..}


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
