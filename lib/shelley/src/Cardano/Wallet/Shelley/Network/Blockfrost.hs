{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Network Layer implementation that uses Blockfrost API
module Cardano.Wallet.Shelley.Network.Blockfrost
    ( withNetworkLayer,
        Log,

        -- * Internal
        getPoolPerformanceEstimate,
        eraByEpoch
    )
where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..)
    , AnyPlutusScriptVersion (AnyPlutusScriptVersion)
    , CardanoEraStyle (LegacyByronEra, ShelleyBasedEra)
    , ExecutionUnitPrices (priceExecutionMemory, priceExecutionSteps)
    , ExecutionUnits (executionMemory, executionSteps)
    , NetworkId (..)
    , PlutusScriptVersion (PlutusScriptV1)
    , TxMetadata (TxMetadata)
    , TxMetadataValue (..)
    , cardanoEraStyle
    )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Tracer
    ( Tracer )
import Cardano.BM.Tracing
    ( HasSeverityAnnotation (getSeverityAnnotation), traceWith )
import Cardano.Pool.Rank
    ( RewardParams (..) )
import Cardano.Pool.Rank.Likelihood
    ( BlockProduction (..), PerformanceEstimate (..), estimatePoolPerformance )
import Cardano.Slotting.Slot
    ( unEpochSize )
import Cardano.Wallet.Api.Types
    ( decodeStakeAddress, encodeAddress, encodeStakeAddress )
import Cardano.Wallet.Logging
    ( BracketLog, bracketTracer )
import Cardano.Wallet.Network
    ( ChainFollower, NetworkLayer (..) )
import Cardano.Wallet.Network.Light
    ( LightSyncSource (..) )
import Cardano.Wallet.Primitive.BlockSummary
    ( BlockEvents (..)
    , ChainEvents
    , LightSummary
    , Sublist
    , fromBlockEvents
    , unsafeMkSublist
    , wholeList
    )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException
    , TimeInterpreter
    , TimeInterpreterLog
    , mkTimeInterpreter
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (Syncing) )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (ActiveSlotCoefficient)
    , Block (..)
    , BlockHeader (..)
    , ChainPoint (..)
    , DecentralizationLevel
    , DelegationCertificate (..)
    , EpochLength (EpochLength)
    , EpochNo (..)
    , ExecutionUnitPrices (..)
    , ExecutionUnits (..)
    , FeePolicy (LinearFee)
    , GenesisParameters (..)
    , LinearFunction (..)
    , MinimumUTxOValue (..)
    , NetworkParameters (..)
    , PoolId
    , ProtocolParameters (..)
    , SlotLength (SlotLength)
    , SlotNo (..)
    , SlottingParameters (..)
    , StakePoolsSummary (..)
    , StartTime
    , TokenBundleMaxSize (..)
    , TxParameters (..)
    , WithOrigin (At)
    , emptyEraInfo
    , executionMemory
    , executionSteps
    , fromFederationPercentage
    , genesisParameters
    , getGenesisBlockDate
    , header
    , slottingParameters
    , stabilityWindowByron
    , stabilityWindowShelley
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin, unCoin) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxIn (..), TxOut (..), TxScriptValidity (..), TxSize (..) )
import Cardano.Wallet.Shelley.Network.Blockfrost.Conversion
    ( bfBlockHeader
    , fromBfAddress
    , fromBfEpoch
    , fromBfLovelaces
    , fromBfPoolId
    )
import Cardano.Wallet.Shelley.Network.Blockfrost.Error
    ( BlockfrostError (..), (<?#>) )
import Cardano.Wallet.Shelley.Network.Blockfrost.Monad
    ( BFM )
import Cardano.Wallet.Shelley.Network.Discriminant
    ( SomeNetworkDiscriminant (..), networkDiscriminantToId )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async.Lifted
    ( concurrently, forConcurrently, mapConcurrently )
import Control.Monad
    ( forever, join, unless )
import Control.Monad.Error.Class
    ( MonadError, liftEither, throwError )
import Control.Monad.IO.Class
    ( MonadIO (liftIO) )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Align
    ( align )
import Data.Bifunctor
    ( first )
import Data.Bitraversable
    ( bitraverse )
import Data.Function
    ( (&) )
import Data.Functor
    ( void, (<&>) )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.IntCast
    ( intCast )
import Data.List
    ( partition )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map
    ( Map )
import Data.Maybe
    ( catMaybes, fromMaybe, listToMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( MkPercentageError (PercentageOutOfBoundsError)
    , Percentage
    , Quantity (..)
    , clipToPercentage
    , mkPercentage
    )
import Data.Ratio
    ( (%) )
import Data.Scientific
    ( isInteger )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (fromText), ToText (..) )
import Data.These
    ( These (That, These, This) )
import Data.Traversable
    ( for )
import Fmt
    ( pretty )
import GHC.OldList
    ( sortOn )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, StandardCrypto )
import Ouroboros.Consensus.HardFork.History.EraParams
    ( EraParams (..), SafeZone (StandardSafeZone, UnsafeIndefiniteSafeZone) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..), EraEnd (..), EraSummary (..), Summary (..) )
import Text.Read
    ( readEither )
import UnliftIO.Async
    ( async, link )
import UnliftIO.STM
    ( TChan
    , atomically
    , dupTChan
    , newBroadcastTChanIO
    , readTChan
    , tryReadTChan
    , writeTChan
    )

import qualified Blockfrost.Client as BF
import qualified Cardano.Api.Shelley as Node
import qualified Cardano.Slotting.Time as ST
import qualified Cardano.Wallet.Network.Light as LN
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Shelley.Network.Blockfrost.Fixture as Fixture
import qualified Cardano.Wallet.Shelley.Network.Blockfrost.Monad as BFM
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Ouroboros.Consensus.HardFork.History.Qry as HF
import qualified Ouroboros.Consensus.Util.Counting as UC

{-------------------------------------------------------------------------------
    NetworkLayer
-------------------------------------------------------------------------------}

data Log
    = MsgTipReceived BlockHeader
    | MsgTipWatcherRegistered
    | MsgTipWatcherNotified BracketLog
    | MsgTimeInterpreterLog TimeInterpreterLog
    | MsgLightLayerLog LN.LightLayerLog
    | MsgAccountNotFound Text
    | MsgGetAddressTxs BlockHeader BlockHeader (Either Address RewardAccount)
    | MsgFetchTransaction BF.TxHash
    | MsgFetchDelegation BF.TxHash
    | MsgFetchedLatestBlockHeader BlockHeader
    | MsgCurrentSlottingParameters
    | MsgEraByLatestEpoch AnyCardanoEra EpochNo
    | MsgFetchNetworkRewardAccountBalances
    | MsgIsConsensus ChainPoint Bool
    | MsgBlockHeaderAtHeight Integer (Maybe BlockHeader)
    | MsgBlockHeaderAt ChainPoint (Maybe BlockHeader)
    | MsgNextBlockHeader BlockHeader (Maybe BlockHeader)
    | MsgGotNextBlocks Int

instance ToText Log where
    toText = \case
        MsgTipReceived blockHeader ->
            "New tip received " <> pretty blockHeader
        MsgTipWatcherNotified startFinish ->
            "Tip watcher callback: " <> pretty startFinish
        MsgTipWatcherRegistered ->
            "Tip watcher registered"
        MsgTimeInterpreterLog til ->
            toText til
        MsgLightLayerLog l ->
            toText l
        MsgAccountNotFound account ->
            "Reward account not found: " <> pretty account
        MsgGetAddressTxs f t a ->
            T.unwords
                [ "Getting transactions from"
                , pretty f
                , "till"
                , pretty t
                , "for the"
                , either
                    (("address " <>) . pretty)
                    (("reward account " <>) . pretty)
                    a
                ]
        MsgFetchTransaction h ->
            "Fetching transaction: " <> pretty (BF.unTxHash h)
        MsgFetchDelegation h ->
            "Fetching delegation: " <> pretty (BF.unTxHash h)
        MsgFetchedLatestBlockHeader bh ->
            "Fetched latest block header: " <> pretty bh
        MsgCurrentSlottingParameters ->
            "Fetching current slotting parameters..."
        MsgEraByLatestEpoch era epoch ->
            T.unwords
                [ "Latest Cardano era is"
                , T.pack $ show era
                , ", determined by the latest epoch"
                , pretty epoch
                ]
        MsgFetchNetworkRewardAccountBalances ->
            "Fetching network reward account balances..."
        MsgIsConsensus cp b ->
            T.unwords
                [ "ChainPoint"
                , pretty cp
                , if b then "does" else "doesn't"
                , "belong to consensus"
                ]
        MsgBlockHeaderAtHeight height mbh ->
            "Fetched BlockHeader at height " <> pretty height
                <> ": " <> pretty mbh
        MsgBlockHeaderAt cp mbh ->
            "Fetched BlockHeader at " <> pretty cp
                <> ": " <> pretty mbh
        MsgNextBlockHeader prev next ->
            "Fetched next block header: " <> pretty next
            <> " for the previous one: " <> pretty prev
        MsgGotNextBlocks n ->
            "Fetched " <> pretty n <> " next blocks"

instance HasSeverityAnnotation Log where
    getSeverityAnnotation = \case
        MsgTipReceived{} -> Info
        MsgTipWatcherNotified{} -> Debug
        MsgTipWatcherRegistered -> Debug
        MsgTimeInterpreterLog {} -> Info
        MsgLightLayerLog l -> getSeverityAnnotation l
        MsgAccountNotFound {} -> Warning
        MsgGetAddressTxs {} -> Debug
        MsgFetchTransaction {} -> Debug
        MsgFetchDelegation {} -> Debug
        MsgFetchedLatestBlockHeader {} -> Debug
        MsgCurrentSlottingParameters -> Debug
        MsgEraByLatestEpoch {} -> Debug
        MsgFetchNetworkRewardAccountBalances -> Debug
        MsgIsConsensus {} -> Debug
        MsgBlockHeaderAtHeight {} -> Debug
        MsgBlockHeaderAt {} -> Debug
        MsgNextBlockHeader{} -> Debug
        MsgGotNextBlocks {} -> Debug

withNetworkLayer
    :: Tracer IO Log
    -> SomeNetworkDiscriminant
    -> NetworkParameters
    -> BF.Project
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -> IO a
withNetworkLayer tr network np project k = do
    bfConfig <- BFM.newClientConfig project
    tipBroadcast <- newBroadcastTChanIO
    link =<< async (pollNodeTip bfConfig tipBroadcast)
    k NetworkLayer
        { chainSync = \_tr _chainFollower -> pure ()
        , lightSync = Just $ blockfrostLightSync network bfConfig
        , currentNodeTip = currentNodeTip bfConfig
        , currentNodeEra = currentNodeEra bfConfig
        , currentProtocolParameters = currentProtocolParameters bfConfig
        , currentSlottingParameters = currentSlottingParameters bfConfig
        , watchNodeTip = subscribeNodeTip tipBroadcast
        , postTx = undefined
        , stakeDistribution = stakePoolsSummary bfConfig
        , getCachedRewardAccountBalance =
            getCachedRewardAccountBalance bfConfig
        , fetchRewardAccountBalances =
            fetchNetworkRewardAccountBalances network bfConfig
        , timeInterpreter =
            timeInterpreterFromStartTime getGenesisBlockDate
        , syncProgress = syncProgress bfConfig
        }
  where
    NetworkParameters
        { genesisParameters = GenesisParameters{getGenesisBlockDate} } = np

    networkId = networkDiscriminantToId network

    currentNodeTip :: BF.ClientConfig -> IO BlockHeader
    currentNodeTip bfConfig = do
        tip <- BFM.run bfConfig do
            liftEither . bfBlockHeader =<< BF.getLatestBlock
        traceWith tr $ MsgFetchedLatestBlockHeader tip
        pure tip

    pollNodeTip :: BF.ClientConfig -> TChan BlockHeader -> IO ()
    pollNodeTip bfConfig nodeTip = do
        lastTip <- atomically $ dupTChan nodeTip
        link =<< async =<< forever do
            header <- BFM.run bfConfig do
                liftEither . bfBlockHeader =<< BF.getLatestBlock
            atomically do
                lastHeader <- tryReadTChan lastTip
                unless (lastHeader == Just header) do
                    writeTChan nodeTip header
            traceWith tr $ MsgTipReceived header
            threadDelay 30_000_000 -- 30 seconds

    subscribeNodeTip :: TChan BlockHeader -> (BlockHeader -> IO ()) -> IO ()
    subscribeNodeTip nodeTipChan callback = do
        traceWith tr MsgTipWatcherRegistered
        atomically (dupTChan nodeTipChan) >>= \chan -> forever do
            header <- atomically $ readTChan chan
            bracketTracer (MsgTipWatcherNotified >$< tr) (callback header)

    currentProtocolParameters :: BF.ClientConfig -> IO ProtocolParameters
    currentProtocolParameters bfConfig =
        BFM.run bfConfig $ liftEither . fromBlockfrostPP networkId
            =<< BF.getLatestEpochProtocolParams

    currentSlottingParameters :: BF.ClientConfig -> IO SlottingParameters
    currentSlottingParameters bfConfig = BFM.run bfConfig do
        liftIO $ traceWith tr MsgCurrentSlottingParameters
        BF.EpochInfo{_epochInfoEpoch} <- BF.getLatestEpoch
        let EraParams{..} = eraParams
                $ epochEraSummary networkId
                $ fromBfEpoch _epochInfoEpoch
        epochLen <- unEpochSize eraEpochSize <?#> "EpochSize"
        getSecurityParameter <- case eraSafeZone of
            StandardSafeZone wo -> Quantity <$> wo <?#> "StandardSafeZone"
            UnsafeIndefiniteSafeZone -> error "UnsafeIndefiniteSafeZone"
        getActiveSlotCoefficient <-
            ActiveSlotCoefficient . BF._genesisActiveSlotsCoefficient <$>
                BF.getLedgerGenesis
        pure SlottingParameters
            { getSlotLength = SlotLength $ ST.getSlotLength eraSlotLength
            , getEpochLength = EpochLength epochLen
            , getActiveSlotCoefficient
            , getSecurityParameter
            }

    currentNodeEra :: BF.ClientConfig -> IO AnyCardanoEra
    currentNodeEra bfConfig = do
        (era, epoch) <- BFM.run bfConfig do
            BF.EpochInfo{_epochInfoEpoch} <- BF.getLatestEpoch
            let latestEpoch = fromBfEpoch _epochInfoEpoch
            latestEra <- liftEither $ eraByEpoch networkId latestEpoch
            pure (latestEra, latestEpoch)
        traceWith tr $ MsgEraByLatestEpoch era epoch
        pure era

    timeInterpreterFromStartTime ::
        StartTime -> TimeInterpreter (ExceptT PastHorizonException IO)
    timeInterpreterFromStartTime startTime =
        mkTimeInterpreter (MsgTimeInterpreterLog >$< tr) startTime $
            pure $ HF.mkInterpreter $ Fixture.networkSummary networkId

    fetchNetworkRewardAccountBalances
        :: SomeNetworkDiscriminant
        -> BF.ClientConfig
        -> Set RewardAccount
        -> IO (Map RewardAccount Coin)
    fetchNetworkRewardAccountBalances
        (SomeNetworkDiscriminant (Proxy :: Proxy nd)) bfConfig accounts = do
            traceWith tr MsgFetchNetworkRewardAccountBalances
            BFM.run bfConfig $ Map.fromList . catMaybes <$>
                for (Set.toList accounts) \rewardAccount -> do
                    let addr = BF.mkAddress $ encodeStakeAddress @nd rewardAccount
                    BFM.maybe404 (BF.getAccount addr)
                        >>= traverse \BF.AccountInfo{..} ->
                            (rewardAccount,) . Coin <$>
                                fromIntegral @_ @Integer _accountInfoRewardsSum
                                    <?#> "AccountInfoRewardsSum"

    getCachedRewardAccountBalance :: BF.ClientConfig -> RewardAccount -> IO Coin
    getCachedRewardAccountBalance bfConfig account =
        fromMaybe (Coin 0) . Map.lookup account
            <$> fetchNetworkRewardAccountBalances network bfConfig
                (Set.singleton account)

    blockfrostLightSync
        :: SomeNetworkDiscriminant
        -> BF.ClientConfig
        -> ChainFollower
            IO
            ChainPoint
            BlockHeader
            (Either (NonEmpty Block) (LightSummary IO))
        -> IO ()
    blockfrostLightSync
        (SomeNetworkDiscriminant (Proxy :: Proxy nd))
        bfConfig
        follower = do
            let runBF :: forall a. BFM a -> IO a
                runBF = BFM.run bfConfig
            AnyCardanoEra era <- currentNodeEra bfConfig
            let sp = slottingParameters np
            let stabilityWindow =
                    fromIntegral $ getQuantity case cardanoEraStyle era of
                        LegacyByronEra -> stabilityWindowByron sp
                        ShelleyBasedEra _ -> stabilityWindowShelley sp

                isConsensus :: ChainPoint -> IO Bool
                isConsensus cp = do
                    res <- runBF case cp of
                        ChainPointAtGenesis -> pure True
                        ChainPoint (SlotNo slot) blockHeaderHash -> do
                            BF.Block{_blockHash = BF.BlockHash bfHeaderHash} <-
                                BF.getBlockSlot (BF.Slot (toInteger slot))
                            pure $ bfHeaderHash == toText blockHeaderHash
                    traceWith tr $ MsgIsConsensus cp res
                    pure res

                getBlockHeaderAtHeight :: Integer -> IO (Maybe BlockHeader)
                getBlockHeaderAtHeight height = do
                    mbh <- either (error . show) Just . bfBlockHeader
                        <$> (runBF . BF.getBlock) (Left height)
                    traceWith tr $ MsgBlockHeaderAtHeight height mbh
                    pure mbh

                getNextBlockHeader :: BlockHeader -> IO (Maybe BlockHeader)
                getNextBlockHeader prev@BlockHeader{headerHash} = runBF do
                    let blockHash = BF.BlockHash (toText headerHash)
                    blocks <- BF.getNextBlocks' (Right blockHash) (BF.paged 1 1)
                    nextHeader <- for (listToMaybe blocks) do
                        liftEither . bfBlockHeader
                    liftIO $ traceWith tr $ MsgNextBlockHeader prev nextHeader
                    pure nextHeader

                getBlockHeaderAt :: ChainPoint -> IO (Maybe BlockHeader)
                getBlockHeaderAt cp = do
                    mbh <- runBF case cp of
                        ChainPointAtGenesis ->
                            pure . Just $ Fixture.genesisBlockHeader networkId
                        ChainPoint (SlotNo slot) blockHeaderHash -> do
                            b@BF.Block{_blockHash = BF.BlockHash bfHeaderHash} <-
                                BF.getBlockSlot (BF.Slot (toInteger slot))
                            pure $
                                if bfHeaderHash == toText blockHeaderHash
                                    then either (error . show) Just $
                                        bfBlockHeader b
                                    else Nothing
                    traceWith tr $ MsgBlockHeaderAt cp mbh
                    pure mbh

                getNextBlocks :: ChainPoint -> IO (Maybe [Block])
                getNextBlocks cp = do
                    bs <- runBF $ fetchNextBlocks tr network case cp of
                            ChainPoint _slotNo hash -> hash
                            ChainPointAtGenesis -> headerHash $
                                Fixture.genesisBlockHeader networkId
                    traceWith tr $ MsgGotNextBlocks $ length bs
                    pure case bs of
                        [] -> Nothing
                        x -> Just x

                getAddressTxs ::
                    BlockHeader ->
                    BlockHeader ->
                    Either Address RewardAccount ->
                    IO ChainEvents
                getAddressTxs bhFrom bhTo addrOrAcc = do
                    traceWith tr $ MsgGetAddressTxs bhFrom bhTo addrOrAcc
                    runBF $ fromBlockEvents <$> case addrOrAcc of
                        Left address -> do
                            txs <- BF.allPages \paged ->
                                BFM.empty404 $ BF.getAddressTransactions'
                                    (BF.Address (encodeAddress @nd address))
                                    paged
                                    BF.Ascending
                                    (Just $ headerToIndex bhFrom)
                                    (Just $ headerToIndex bhTo)
                            forConcurrently txs \BF.AddressTransaction{..} -> do
                                (bftx, tx) <-
                                    fetchTransaction tr network _addressTransactionTxHash
                                txIndex <-
                                    _addressTransactionTxIndex
                                        <?#> "_addressTransactionTxIndex"
                                txBlockEvents
                                    bftx
                                    (unsafeMkSublist [((txIndex, 0), tx)])
                                    (wholeList [])
                        Right account -> do
                            let address = BF.Address $ encodeStakeAddress @nd account
                            regTxHashes <-
                                fmap BF._accountRegistrationTxHash
                                    <$> BFM.empty404 (BF.getAccountRegistrations address)
                            delTxHashes <-
                                fmap BF._accountDelegationTxHash
                                    <$> BFM.empty404 (BF.getAccountDelegations address)
                            blockEventsRegDeleg <-
                                forConcurrently (regTxHashes <> delTxHashes) \hash -> do
                                    (tx@BF.Transaction{_transactionIndex}, dcerts) <-
                                        concurrently
                                            (BF.getTx hash)
                                            (fetchDelegation tr network hash)
                                    txIndex <- _transactionIndex <?#> "_transactionIndex"
                                    txBlockEvents
                                        tx
                                        (wholeList [])
                                        ( unsafeMkSublist $
                                            (\(n, dc) -> ((txIndex, n), dc))
                                                <$> zip [0 ..] dcerts
                                        )
                            ws <- BFM.empty404 (BF.getAccountWithdrawals address)
                            blockEventsWithdraw <-
                                forConcurrently ws \BF.AccountWithdrawal{..} -> do
                                    (bftx@BF.Transaction{_transactionIndex}, tx) <-
                                        fetchTransaction tr network _accountWithdrawalTxHash
                                    txIndex <- _transactionIndex <?#> "_transactionIndex"
                                    txBlockEvents
                                        bftx
                                        (unsafeMkSublist [((txIndex, 0), tx)])
                                        (wholeList [])
                            pure $ blockEventsRegDeleg <> blockEventsWithdraw
                  where
                    txBlockEvents ::
                        BF.Transaction ->
                        Sublist Tx ->
                        Sublist DelegationCertificate ->
                        BFM BlockEvents
                    txBlockEvents BF.Transaction{..} txs ds = do
                        slot <-
                            At . SlotNo <$> toInteger _transactionSlot
                                <?#> "_transactionSlot"
                        blockHeight <-
                            Quantity <$> _transactionBlockHeight
                                <?#> "_transactionBlockHeight"
                        pure
                            BlockEvents
                                { slot
                                , blockHeight
                                , transactions = txs
                                , delegations = ds
                                }

                    headerToIndex :: BlockHeader -> BF.BlockIndex
                    headerToIndex BlockHeader{blockHeight} =
                        BF.BlockIndex
                            { blockIndexHeight = intCast $ getQuantity blockHeight
                            , blockIndexIndex = Nothing
                            }

                lightSyncSource =
                    LightSyncSource
                        { stabilityWindow
                        , getHeader = header
                        , getTip = currentNodeTip bfConfig
                        , isConsensus
                        , getBlockHeaderAtHeight
                        , getNextBlockHeader
                        , getBlockHeaderAt
                        , getNextBlocks
                        , getAddressTxs
                        }
            void $ LN.lightSync (MsgLightLayerLog >$< tr) lightSyncSource follower

    syncProgress :: BF.ClientConfig -> SlotNo -> IO SyncProgress
    syncProgress bfConfig s = BFM.run bfConfig do
        BF.Block {_blockSlot} <- BF.getLatestBlock
        let latestSlot = maybe 0 BF.unSlot _blockSlot
            currentSlot = fromIntegral (unSlotNo s)
            percentage = currentSlot % latestSlot
        pure $ Syncing $ Quantity $ clipToPercentage percentage

    stakePoolsSummary :: BF.ClientConfig -> Coin -> IO StakePoolsSummary
    stakePoolsSummary bfConfig _coin = do
        protocolParameters <- currentProtocolParameters bfConfig
        BFM.run bfConfig do
            BF.Network{_networkStake = BF.NetworkStake{_stakeLive}} <-
                BF.getNetworkInfo
            totalLiveStake <- fromBfLovelaces _stakeLive
            pools <- mapConcurrently BF.getPool =<< BF.listPools
            stake <- poolsStake totalLiveStake pools
            pure StakePoolsSummary
                { nOpt = intCast $ desiredNumberOfStakePools protocolParameters
                , rewards = Map.empty
                -- TODO: Update to new `Cardano.Pool.Rank.StakePoolsSummary`
                -- ADP-1509
                , stake
                }
          where
            poolsStake
                :: MonadError BlockfrostError m
                => Coin
                -> [BF.PoolInfo]
                -> m (Map PoolId Percentage)
            poolsStake total = fmap Map.fromList . traverse \BF.PoolInfo{..} ->
                (,) <$> liftEither (fromBfPoolId _poolInfoPoolId) <*> do
                    live <- fromBfLovelaces _poolInfoLiveStake
                    let ratio = Coin.toInteger live % Coin.toInteger total
                    case mkPercentage ratio of
                        Right percentage -> pure percentage
                        Left PercentageOutOfBoundsError ->
                            throwError $ PoolStakePercentageError total live

fetchNextBlocks
    :: forall m
     . ( MonadError BlockfrostError m
       , BF.MonadBlockfrost m
       , MonadBaseControl IO m
       )
    => Tracer IO Log
    -> SomeNetworkDiscriminant
    -> Hash "BlockHeader"
    -> m [Block]
fetchNextBlocks tr nd hash = do
    let blockHash = BF.BlockHash $ toText hash
    BF.getNextBlocks (Right blockHash) >>= traverse \block@BF.Block{..} -> do
        header <- liftEither $ bfBlockHeader block
        txhs <- fetchTxHashes blockHash _blockTxCount
        transactions <- fmap snd <$> mapConcurrently (fetchTransaction tr nd) txhs
        delegations <- join <$> mapConcurrently (fetchDelegation tr nd) txhs
        pure Block{header, transactions, delegations}

fetchTxHashes
    :: forall m
     . BF.MonadBlockfrost m
    => BF.BlockHash
    -> Integer
    -> m [BF.TxHash]
fetchTxHashes blockHash = fmap concat . traverse fetchPage . pages
  where
    fetchPage :: BF.Paged -> m [BF.TxHash]
    fetchPage page = BF.getBlockTxs' (Right blockHash) page BF.Ascending

    pages :: Integer -> [BF.Paged]
    pages count =
        pageNumbers <&> \pageNumber -> BF.Paged{countPerPage, pageNumber}
      where
        countPerPage :: Int = 100
        pageNumbers = [1 .. lastPage]
        lastPage :: Int = fromIntegral $
            let (numWholePages, numLast) = quotRem count (intCast countPerPage)
                in if numLast == 0 then numWholePages else succ numWholePages

fetchDelegation
    :: forall m
     . ( MonadError BlockfrostError m
       , BF.MonadBlockfrost m
       , MonadBaseControl IO m
       )
    => Tracer IO Log
    -> SomeNetworkDiscriminant
    -> BF.TxHash
    -> m [DelegationCertificate]
fetchDelegation tr (SomeNetworkDiscriminant (Proxy :: Proxy nd)) hash = do
    liftIO $ traceWith tr $ MsgFetchDelegation hash
    delegations <- concurrently (BF.getTxDelegations hash) (BF.getTxStakes hash)
    certs <- liftEither $ for (uncurry align delegations) \case
        This txDelegation -> pure <$> parseTxDelegation txDelegation
        That txStake -> pure <$> parseTxStake txStake
        These txDelegation txStake ->
            (\d s -> [d, s])
                <$> parseTxDelegation txDelegation
                <*> parseTxStake txStake
    pure $ snd <$> sortOn fst (concat certs)
  where
    parseTxDelegation BF.TransactionDelegation{..} = do
        let addr = BF.unAddress _transactionDelegationAddress
        rewardAccount <-
            first (InvalidAddress addr) $ decodeStakeAddress @nd addr
        poolId <- fromBfPoolId _transactionDelegationPoolId
        pure
            ( _transactionDelegationCertIndex
            , CertDelegateFull rewardAccount poolId
            )
    parseTxStake BF.TransactionStake{..} = do
        let addr = BF.unAddress _transactionStakeAddress
        rewardAccount <-
            first (InvalidAddress addr) $ decodeStakeAddress @nd addr
        let action =
                if _transactionStakeRegistration
                    then CertRegisterKey
                    else CertDelegateNone
        pure (_transactionStakeCertIndex, action rewardAccount)

fetchTransaction
    :: forall m
     . (MonadError BlockfrostError m, BF.MonadBlockfrost m)
    => Tracer IO Log
    -> SomeNetworkDiscriminant
    -> BF.TxHash
    -> m (BF.Transaction, Tx)
fetchTransaction tr nd hash = do
    liftIO $ traceWith tr $ MsgFetchTransaction hash
    transaction <- BF.getTx hash
    utxos <- BF.getTxUtxos hash
    withdrawals <- BF.getTxWithdrawals hash
    metadata <- BF.getTxMetadataJSON hash
    (transaction,)
        <$> assembleTransaction nd transaction utxos withdrawals metadata

assembleTransaction
    :: forall m
     . MonadError BlockfrostError m
    => SomeNetworkDiscriminant
    -> BF.Transaction
    -> BF.TransactionUtxos
    -> [BF.TransactionWithdrawal]
    -> [BF.TransactionMetaJSON]
    -> m Tx
assembleTransaction
    network@(SomeNetworkDiscriminant (Proxy :: Proxy nd))
    BF.Transaction{..}
    BF.TransactionUtxos{..}
    txWithdrawals
    metadataJSON = liftEither do
        txId <- parseTxHash _transactionHash
        let fee = Just $ Coin $ fromIntegral _transactionFees
        (resolvedInputs, resolvedCollateralInputs) <-
            fromInputs _transactionUtxosInputs
        outputs <- for _transactionUtxosOutputs \out@BF.UtxoOutput{..} -> do
            address <- fromBfAddress network _utxoOutputAddress
            tokens <- do
                coin <- case [ lovelaces
                             | BF.AdaAmount lovelaces <- _utxoOutputAmount
                             ] of
                    [l] -> fromBfLovelaces l
                    _ -> throwError $ InvalidUtxoOutputAmount out
                pure $ TokenBundle coin mempty -- TODO: Handle native assets
            pure TxOut{..}
        withdrawals <-
            Map.fromList
                <$> for txWithdrawals \BF.TransactionWithdrawal{..} -> do
                    let addr = BF.unAddress _transactionWithdrawalAddress
                    rewardAccount <-
                        first (InvalidAddress addr) $ decodeStakeAddress @nd addr
                    coin <- fromBfLovelaces _transactionWithdrawalAmount
                    pure (rewardAccount, coin)
        metadata <-
            if null metadataJSON
                then pure Nothing
                else Just . TxMetadata . Map.fromList . catMaybes
                    <$> for metadataJSON \BF.TransactionMetaJSON{..} -> do
                        label <-
                            either (throwError . InvalidTxMetadataLabel) pure $
                                readEither (T.unpack _transactionMetaJSONLabel)
                        fmap (label,) <$>
                            for _transactionMetaJSONJSONMetadata
                                (first InvalidTxMetadataValue
                                . unmarshalMetadataValue)
        let scriptValidity =
                Just if _transactionValidContract
                        then TxScriptValid
                        else TxScriptInvalid
        pure Tx
            { txId
            , fee
            , resolvedInputs
            , resolvedCollateralInputs
            , outputs
            , collateralOutput = Nothing
            , withdrawals
            , metadata
            , scriptValidity
            }
      where
        fromInputs
            :: [BF.UtxoInput]
            -> Either BlockfrostError ([(TxIn, Coin)], [(TxIn, Coin)])
        fromInputs utxos =
            bitraverse f f $ partition BF._utxoInputCollateral utxos
          where
            f :: [BF.UtxoInput] -> Either BlockfrostError [(TxIn, Coin)]
            f = traverse \input@BF.UtxoInput{..} -> do
                txHash <- parseTxHash $ BF.unTxHash _utxoInputTxHash
                txIndex <- _utxoInputOutputIndex <?#> "_utxoInputOutputIndex"
                coin <-
                    case [lovelaces | BF.AdaAmount lovelaces <- _utxoInputAmount] of
                        [l] -> fromBfLovelaces l
                        _ -> throwError $ InvalidUtxoInputAmount input
                pure (TxIn txHash txIndex, coin)

        parseTxHash hash =
            either (throwError . InvalidTxHash hash) pure $ fromText hash

unmarshalMetadataValue :: Aeson.Value -> Either String TxMetadataValue
unmarshalMetadataValue = \case
    Aeson.Object hm ->
        TxMetaMap <$> for
            (HashMap.toList hm)
            (bitraverse (unmarshalMetadataValue . Aeson.String)
                unmarshalMetadataValue)
    Aeson.Array vec ->
        TxMetaList . V.toList <$> for vec unmarshalMetadataValue
    Aeson.String txt ->
        Right $ TxMetaText txt
    Aeson.Number sci ->
        if isInteger sci
            then Right (TxMetaNumber (truncate sci))
            else Left "Non-integer metadata value"
    Aeson.Bool b ->
        Left $ "Expected TxMetadataValue but got bool (" <> show b <> ")"
    Aeson.Null ->
        Left "Expected TxMetadataValue but got null"

fromBlockfrostPP
    :: NetworkId
    -> BF.ProtocolParams
    -> Either BlockfrostError ProtocolParameters
fromBlockfrostPP network BF.ProtocolParams{..} = do
    decentralizationLevel <-
        let percentage =
                mkPercentage $ toRational _protocolParamsDecentralisationParam
            in case percentage of
                Left PercentageOutOfBoundsError ->
                    throwError $
                        InvalidDecentralizationLevelPercentage
                            _protocolParamsDecentralisationParam
                Right level -> pure $ fromFederationPercentage level
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
        MinimumUTxOValueCostPerWord . Coin
            <$> intCast @_ @Integer _protocolParamsCoinsPerUtxoWord
            <?#> "CoinsPerUtxoWord"
    stakeKeyDeposit <-
        Coin
            <$> intCast @_ @Integer _protocolParamsKeyDeposit <?#> "KeyDeposit"
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
        , txParameters =
            TxParameters
                { getFeePolicy =
                    LinearFee $
                        LinearFunction
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
        , executionUnitPrices =
            Just $
                ExecutionUnitPrices
                    { pricePerStep = toRational _protocolParamsPriceStep
                    , pricePerMemoryUnit = toRational _protocolParamsPriceMem
                    }
        , maximumCollateralInputCount = maxCollateralInputs
        , minimumCollateralPercentage = collateralPercent
        , currentNodeProtocolParameters =
            Just
                Node.ProtocolParameters
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
                        Just $
                            Node.Lovelace $
                                intCast _protocolParamsCoinsPerUtxoWord
                    , protocolParamCostModels =
                        Map.singleton
                            (AnyPlutusScriptVersion PlutusScriptV1)
                            (Fixture.costModels network)
                    , protocolParamPrices =
                        Just $
                            Node.ExecutionUnitPrices
                                { priceExecutionSteps =
                                    toRational _protocolParamsPriceStep
                                , priceExecutionMemory =
                                    toRational _protocolParamsPriceMem
                                }
                    , protocolParamMaxTxExUnits =
                        Just $
                            Node.ExecutionUnits
                                { executionSteps = maxTxExSteps
                                , executionMemory = maxTxExMem
                                }
                    , protocolParamMaxBlockExUnits =
                        Just $
                            Node.ExecutionUnits
                                { executionSteps = maxBlockExSteps
                                , executionMemory = maxBlockExMem
                                }
                    , protocolParamMaxValueSize = Just maxValSize
                    , protocolParamCollateralPercent = Just collateralPercent
                    , protocolParamMaxCollateralInputs =
                        Just $ intCast maxCollateralInputs
                    }
        , .. }

eraByEpoch :: NetworkId -> EpochNo -> Either BlockfrostError AnyCardanoEra
eraByEpoch networkId epoch =
    dropWhile ((> epoch) . snd) (reverse (Fixture.eraBoundaries networkId)) &
        \case
            (era, _) : _ -> Right era
            _ -> Left $ UnknownEraForEpoch epoch

epochEraSummary :: NetworkId -> EpochNo -> EraSummary
epochEraSummary networkId (EpochNo epoch) =
    go $ getSummary $ Fixture.networkSummary networkId
  where
    go :: UC.NonEmpty xs EraSummary -> EraSummary
    go = \case
        UC.NonEmptyOne era -> era
        UC.NonEmptyCons era@EraSummary{eraEnd=EraUnbounded} _eras -> era
        UC.NonEmptyCons era@EraSummary{eraEnd=EraEnd Bound{boundEpoch}} eras ->
            if boundEpoch > fromIntegral epoch then era else go eras

{-------------------------------------------------------------------------------
    Stake Pools
-------------------------------------------------------------------------------}

{- | Estimate the performance of a stake pool based on
 the past 50 epochs (or less if the pool is younger than that).

 Uses 'estimatePoolPerformance' from "Cardano.Pool.Rank.Likelihood"
 for this purpose.
-}
getPoolPerformanceEstimate
    :: BF.MonadBlockfrost m
    => SlottingParameters
    -> DecentralizationLevel
    -> RewardParams
    -> BF.PoolId
    -> m PerformanceEstimate
getPoolPerformanceEstimate sp dl rp pid = do
    hist <- BF.getPoolHistory' pid get50 BF.Descending
    pure . estimatePoolPerformance sp dl . Seq.fromList $
        map toBlockProduction hist
  where
    get50 = BF.Paged{BF.countPerPage = 50, BF.pageNumber = 1}
    toBlockProduction p =
        BlockProduction
            { blocksProduced = fromIntegral $ BF._poolHistoryBlocks p
            , stakeRelative =
                fromIntegral (BF._poolHistoryActiveStake p)
                    / fromIntegral (unCoin $ totalStake rp)
                    -- _poolHistoryActiveSize would be incorrect here
            }
