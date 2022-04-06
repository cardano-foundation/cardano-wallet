{-# LANGUAGE BangPatterns #-}
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
    , getStakePoolsSummary'

    -- * Blockfrost -> Cardano translation
    , fromBlockfrost
    ) where

import Prelude

import qualified Blockfrost.Client as BF
import qualified Cardano.Api.Shelley as Node
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Cardano.Api
    ( AnyCardanoEra, NetworkId (Mainnet, Testnet) )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Tracing
    ( HasSeverityAnnotation (..), Tracer, traceWith )
import Cardano.Pool.Rank
    ( RewardInfoPool (..), RewardParams (..), StakePoolsSummary (..) )
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
    , PoolId (..)
    , ProtocolParameters (..)
    , SlotNo (..)
    , SlottingParameters (..)
    , TokenBundleMaxSize (..)
    , TxParameters (..)
    , decodePoolIdBech32
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
import Data.Foldable
    ( toList )
import Data.Functor
    ( (<&>) )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.IntCast
    ( intCast, intCastMaybe )
import Data.Quantity
    ( MkPercentageError (PercentageOutOfBoundsError)
    , Quantity (..)
    , clipToPercentage
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

data Log
    = MsgWatcherUpdate BlockHeader BracketLog
    | MsgRewardInfoPool PoolId RewardInfoPool

instance ToText Log where
    toText = \case
        MsgWatcherUpdate blockHeader bracketLog ->
            "Update watcher with tip: " <> pretty blockHeader <>
            ". Callback " <> toText bracketLog <> ". "
        MsgRewardInfoPool pid info ->
            "Fetched reward info for pool " <> pretty pid <>
            ". Info is: " <> pretty info <> ". "

instance HasSeverityAnnotation Log where
    getSeverityAnnotation = \case
      MsgWatcherUpdate _ _ -> Info
      MsgRewardInfoPool _ _ -> Debug

withNetworkLayer
    :: Tracer IO Log
    -> NetworkId
    -> BF.Project
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -> IO a
withNetworkLayer tr net project k = k NetworkLayer
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
        liftEither $ eraByEpoch net epoch

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


{- Epoch-to-Era translation is not available in the Blockfrost API.

For the Mainnet we're hardcoding the following history
in order to work around this limiation:

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
eraByEpoch :: NetworkId -> EpochNo -> Either BlockfrostError AnyCardanoEra
eraByEpoch = \case
    Mainnet -> \epoch ->
        case dropWhile ((> epoch) . snd) (reverse eraBoundaries) of
            (era, _) : _ -> Right era
            _ -> Left $ UnknownEraForEpoch epoch
    Testnet _ -> \_ -> error
        "In light-mode era to epoch conversions are only available for the \
        \mainnet (translation uses a hard-coded history of hard forks). \
        \It doesn't seem viable to hardcode eras for other networks yet."

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

-- | Retrieve a 'StakePoolsSummary'.
--
-- TODO: This summary is from the "Cardano.Pool.Rank" module;
-- the one from the "Cardano.Wallet.Primitive.Types" will be
-- removed.
--
-- TODO: For reasons of performance, this summary does not
-- fill in the 'performanceEstimate' field of each stake pool.
-- As it is not needed for the new reward computation,
-- it will be removed in the future.
getStakePoolsSummary'
    :: BF.MonadBlockfrost m
    => Tracer m Log
    -> m StakePoolsSummary
getStakePoolsSummary' tr = do
    let mkPoolMap pids = Map.fromList [ (fromPoolId p,p) | p <- pids ]
    pools <- mkPoolMap <$> getAllPages BF.listPools'

    rp <- getRewardParams
    let getPool pid = do
            info <- fromPoolInfo rp <$> BF.getPool pid
            traceWith tr $ MsgRewardInfoPool (fromPoolId pid) info
            pure info

    StakePoolsSummary rp <$> mapM getPool pools

getRewardParams :: BF.MonadBlockfrost m => m RewardParams
getRewardParams = do
    BF.ProtocolParams{..} <- BF.getLatestEpochProtocolParams
    BF.Network{..} <- BF.getNetworkInfo
    BF.EpochInfo{..} <- BF.getLatestEpoch
    let BF.NetworkSupply{..} = _networkSupply
        eta
            | _protocolParamsDecentralisationParam >= 0.8 = 1
            | otherwise = 1 -- wrong value
                -- NOTE: The correct value is  blocksMade % expectedBlocks
                -- But since we are long after d = 1, we ignore that here.
        deltaR1 = rationalToCoinViaFloor $
            min 1 eta
                * realToFrac _protocolParamsRho
                * fromIntegral _supplyReserves
        rationalToCoinViaFloor :: Rational -> Coin
        rationalToCoinViaFloor = Coin . floor
        rPot = fromLovelaces _epochInfoFees <> deltaR1
    pure RewardParams
        { nOpt = fromIntegral _protocolParamsNOpt
        , a0 = realToFrac _protocolParamsA0
        , r = rPot
        , totalStake = fromLovelaces _supplyTotal
        }

fromPoolInfo :: RewardParams -> BF.PoolInfo -> RewardInfoPool
fromPoolInfo rp BF.PoolInfo{..} = RewardInfoPool
    { stakeRelative = clipToPercentage $
        fromIntegral _poolInfoActiveStake / _totalStake
    , ownerPledge =
        fromLovelaces _poolInfoDeclaredPledge
    , ownerStake =
        fromLovelaces _poolInfoLivePledge
    , ownerStakeRelative = clipToPercentage $
        fromIntegral _poolInfoLivePledge / _totalStake
    , cost =
        fromLovelaces _poolInfoFixedCost 
    , margin = clipToPercentage $
        realToFrac _poolInfoMarginCost
    , performanceEstimate =
        1.0 -- assume perfect performance here
    }
  where
    _totalStake = fromIntegral . unCoin $ totalStake rp

{-------------------------------------------------------------------------------
    Type conversions
-------------------------------------------------------------------------------}
fromLovelaces :: BF.Lovelaces -> Coin
fromLovelaces = Coin . toEnum . fromIntegral

-- BF.PoolId is Bech32 encoded
fromPoolId :: BF.PoolId -> PoolId
fromPoolId (BF.PoolId pid) = case decodePoolIdBech32 pid of
    Right a -> a
    Left e -> error $ show e

{-------------------------------------------------------------------------------
    Utility functions
-------------------------------------------------------------------------------}
-- | Retrieve multiple items even though they are only presented
-- in pages of 100.
getAllPages :: Monad m => (BF.Paged -> BF.SortOrder -> m [a]) -> m [a]
getAllPages query = toList <$> go 1 Seq.empty
  where
    go !j !ys = do
        xs <- query
            BF.Paged{ BF.countPerPage = 100, BF.pageNumber = j }
            BF.Ascending
        let gonext
                | length xs < 100 = pure
                | otherwise       = go (j+1)
        gonext $ ys <> Seq.fromList xs
