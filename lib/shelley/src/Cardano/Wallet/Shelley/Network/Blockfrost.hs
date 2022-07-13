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
    , ExecutionUnitPrices (priceExecutionMemory, priceExecutionSteps)
    , ExecutionUnits (executionMemory, executionSteps)
    , NetworkId (..)
    , PlutusScriptVersion (PlutusScriptV1)
    , ShelleyBasedEra (ShelleyBasedEraAlonzo)
    , TxMetadata (TxMetadata)
    , TxMetadataValue (..)
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
    ( Consensual (..), LightSyncSource (..) )
import Cardano.Wallet.Primitive.BlockSummary
    ( BlockEvents (..)
    , ChainEvents
    , LightSummary
    , Sublist
    , fromBlockEvents
    , unsafeMkSublist
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
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (Coin, unCoin) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash )
import Cardano.Wallet.Primitive.Types.MinimumUTxO
    ( MinimumUTxO, minimumUTxOForShelleyBasedEra )
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
    ( BlockfrostError (..)
    , BlockfrostException (BlockfrostException)
    , throwBlockfrostError
    , (<?#>)
    )
import Cardano.Wallet.Shelley.Network.Blockfrost.Layer
    ( BlockfrostLayer (..), rateLimitedBlockfrostLayer, withRecovery )
import Cardano.Wallet.Shelley.Network.Discriminant
    ( SomeNetworkDiscriminant (..), networkDiscriminantToId )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async.Lifted
    ( concurrently )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( forever, join, unless )
import Control.Monad.Error.Class
    ( throwError )
import Control.Monad.IO.Class
    ( MonadIO (liftIO) )
import Control.Monad.Trans.Except
    ( ExceptT (..) )
import Data.Align
    ( align )
import Data.Bifunctor
    ( first )
import Data.Bitraversable
    ( bitraverse )
import Data.Default
    ( Default (..) )
import Data.Function
    ( (&) )
import Data.Functor
    ( void )
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
    ( catMaybes, fromMaybe )
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
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Slotting.Time as ST
import qualified Cardano.Wallet.Network.Light as LN
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Shelley.Network.Blockfrost.Fixture as Fixture
import qualified Cardano.Wallet.Shelley.Network.Blockfrost.Layer as Layer
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson
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

withNetworkLayer
    :: Tracer IO Log
    -> SomeNetworkDiscriminant
    -> NetworkParameters
    -> BF.Project
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -> IO a
withNetworkLayer tr network np project k = do
    let layerLog = MsgBlockfrostLayer >$< tr
    bfLayer <-
        withRecovery layerLog <$> rateLimitedBlockfrostLayer layerLog project
    tipBroadcast <- newBroadcastTChanIO
    link =<< async (pollNodeTip bfLayer tipBroadcast)
    k NetworkLayer
        { chainSync = \_tr _chainFollower -> pure ()
        , lightSync = Just $ blockfrostLightSync network bfLayer
        , currentNodeTip = currentNodeTip bfLayer
        , currentNodeEra = currentNodeEra bfLayer
        , currentProtocolParameters = currentProtocolParameters bfLayer
        , currentSlottingParameters = currentSlottingParameters bfLayer
        , watchNodeTip = subscribeNodeTip tipBroadcast
        , postTx = undefined
        , stakeDistribution = stakePoolsSummary bfLayer
        , getCachedRewardAccountBalance =
            getCachedRewardAccountBalance bfLayer
        , fetchRewardAccountBalances =
            fetchNetworkRewardAccountBalances network bfLayer
        , timeInterpreter =
            timeInterpreterFromStartTime getGenesisBlockDate
        , syncProgress = syncProgress bfLayer
        }
  where
    NetworkParameters
        { genesisParameters = GenesisParameters{getGenesisBlockDate} } = np

    networkId = networkDiscriminantToId network

    currentNodeTip :: BlockfrostLayer IO -> IO BlockHeader
    currentNodeTip bfLayer = do
        tip <- either (throwIO . BlockfrostException) pure . bfBlockHeader =<<
            bfGetLatestBlock bfLayer
        traceWith tr $ MsgFetchedLatestBlockHeader tip
        pure tip

    pollNodeTip :: BlockfrostLayer IO -> TChan BlockHeader -> IO ()
    pollNodeTip bfLayer nodeTip = do
        lastTip <- atomically $ dupTChan nodeTip
        link =<< async =<< forever do
            header <-
                either (throwIO . BlockfrostException) pure . bfBlockHeader =<<
                    bfGetLatestBlock bfLayer
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

    currentProtocolParameters :: BlockfrostLayer IO -> IO ProtocolParameters
    currentProtocolParameters bfLayer =
         either (throwIO . BlockfrostException) pure . fromBlockfrostPP networkId
            =<< bfGetLatestEpochProtocolParams bfLayer

    currentSlottingParameters :: BlockfrostLayer IO -> IO SlottingParameters
    currentSlottingParameters bfLayer = do
        liftIO $ traceWith tr MsgCurrentSlottingParameters
        BF.EpochInfo{_epochInfoEpoch} <- bfGetLatestEpoch bfLayer
        let EraParams{..} = eraParams
                $ epochEraSummary networkId
                $ fromBfEpoch _epochInfoEpoch
        epochLen <- throwBlockfrostError $
            unEpochSize eraEpochSize <?#> "EpochSize"
        getSecurityParameter <- case eraSafeZone of
            StandardSafeZone wo -> throwBlockfrostError $
                Quantity <$> wo <?#> "StandardSafeZone"
            UnsafeIndefiniteSafeZone -> error "UnsafeIndefiniteSafeZone"
        getActiveSlotCoefficient <-
            ActiveSlotCoefficient . BF._genesisActiveSlotsCoefficient <$>
                bfGetLedgerGenesis bfLayer
        pure SlottingParameters
            { getSlotLength = SlotLength $ ST.getSlotLength eraSlotLength
            , getEpochLength = EpochLength epochLen
            , getActiveSlotCoefficient
            , getSecurityParameter
            }

    currentNodeEra :: BlockfrostLayer IO -> IO AnyCardanoEra
    currentNodeEra bfLayer = do
        (era, epoch) <- do
            BF.EpochInfo{_epochInfoEpoch} <- bfGetLatestEpoch bfLayer
            let latestEpoch = fromBfEpoch _epochInfoEpoch
            latestEra <- either (throwIO . BlockfrostException) pure $
                eraByEpoch networkId latestEpoch
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
        -> BlockfrostLayer IO
        -> Set RewardAccount
        -> IO (Map RewardAccount Coin)
    fetchNetworkRewardAccountBalances
        (SomeNetworkDiscriminant (Proxy :: Proxy nd)) bfLayer accounts = do
            traceWith tr MsgFetchNetworkRewardAccountBalances
            Map.fromList . catMaybes <$>
                for (Set.toList accounts) \rewardAccount -> do
                    let addr = BF.mkAddress $ encodeStakeAddress @nd rewardAccount
                    bfGetAccount bfLayer addr
                        >>= traverse \BF.AccountInfo{..} -> throwBlockfrostError $
                            (rewardAccount,) . Coin <$>
                                fromIntegral @_ @Integer _accountInfoWithdrawableAmount
                                    <?#> "AccountInfoRewardsSum"

    getCachedRewardAccountBalance :: BlockfrostLayer IO -> RewardAccount -> IO Coin
    getCachedRewardAccountBalance bfLayer account =
        fromMaybe (Coin 0) . Map.lookup account
            <$> fetchNetworkRewardAccountBalances network bfLayer
                (Set.singleton account)

    blockfrostLightSync
        :: SomeNetworkDiscriminant
        -> BlockfrostLayer IO
        -> ChainFollower
            IO
            ChainPoint
            BlockHeader
            (Either (NonEmpty Block) (LightSummary IO))
        -> IO ()
    blockfrostLightSync
        (SomeNetworkDiscriminant (Proxy :: Proxy nd))
        bfLayer@BlockfrostLayer{..}
        follower = do
            let getBlockHeaderAtHeight :: Integer -> IO (Consensual BlockHeader)
                getBlockHeaderAtHeight height = do
                    header <-
                        either
                            (throwIO . BlockfrostException)
                            (pure . Consensual)
                        . bfBlockHeader =<< bfGetBlockAtHeight height
                    traceWith tr $ MsgBlockHeaderAtHeight height header
                    pure header

                getNextBlockHeader ::
                    BlockHeader -> IO (Consensual (Maybe BlockHeader))
                getNextBlockHeader prev@BlockHeader{headerHash} = do
                    consensualBlocks <-
                        bfGetBlockAfterHash (BF.BlockHash (toText headerHash))
                    nextHeader <-
                        for consensualBlocks $ traverse $
                            either (throwIO . BlockfrostException) pure
                            .  bfBlockHeader
                    liftIO $ traceWith tr $ MsgNextBlockHeader prev nextHeader
                    pure nextHeader

                getBlockHeaderAt :: ChainPoint -> IO (Consensual BlockHeader)
                getBlockHeaderAt cp = do
                    consensualBlockHeader <- case cp of
                        ChainPointAtGenesis ->
                            pure . Consensual $
                                Fixture.genesisBlockHeader networkId
                        ChainPoint (SlotNo slot) blockHeaderHash -> do
                            bfGetBlockSlot (BF.Slot (toInteger slot)) >>= \case
                                NotConsensual -> pure NotConsensual
                                Consensual b@BF.Block{_blockHash =
                                    BF.BlockHash bfHeaderHash} ->
                                    if bfHeaderHash == toText blockHeaderHash
                                        then
                                            either
                                            (throwIO . BlockfrostException)
                                            (pure . Consensual)
                                            (bfBlockHeader b)
                                        else pure NotConsensual
                    traceWith tr $ MsgBlockHeaderAt cp consensualBlockHeader
                    pure consensualBlockHeader

                getNextBlocks :: ChainPoint -> IO (Consensual [Block])
                getNextBlocks cp = do
                    let b = case cp of
                            ChainPoint _slotNo hash -> hash
                            ChainPointAtGenesis -> headerHash $
                                Fixture.genesisBlockHeader networkId
                    consensualBlocks <- fetchNextBlocks tr network bfLayer b
                    traceWith tr $ uncurry (MsgGotNextBlocks b) $
                        case consensualBlocks of
                            NotConsensual ->
                                (0, Nothing)
                            Consensual bls ->
                                ( length bls
                                , case bls of
                                    [] -> Nothing
                                    h : _ -> Just (header h)
                                )
                    pure consensualBlocks

                getAddressTxs ::
                    BlockHeader ->
                    BlockHeader ->
                    Either Address RewardAccount ->
                    IO ChainEvents
                getAddressTxs bhFrom bhTo addrOrAcc = do
                    traceWith tr $ MsgGetAddressTxs bhFrom bhTo addrOrAcc
                    fromBlockEvents <$> case addrOrAcc of
                        Left address -> do
                            txs <-
                                bfGetAddressTransactions
                                    (BF.Address (encodeAddress @nd address))
                                    (Just $ headerToIndex bhFrom)
                                    (Just $ headerToIndex bhTo)
                            for txs \BF.AddressTransaction{..} -> do
                                (bftx, tx) <-
                                    fetchTransaction
                                        tr
                                        network
                                        bfLayer
                                        _addressTransactionTxHash
                                txIndex <- throwBlockfrostError $
                                    _addressTransactionTxIndex
                                        <?#> "_addressTransactionTxIndex"
                                txBlockEvents
                                    bftx
                                    (unsafeMkSublist [((txIndex, 0), tx)])
                                    (unsafeMkSublist [])
                        Right account -> do
                            let address = BF.Address $ encodeStakeAddress @nd account
                            regTxHashes <-
                                fmap BF._accountRegistrationTxHash
                                    <$> bfGetAccountRegistrations address
                            delTxHashes <-
                                fmap BF._accountDelegationTxHash
                                    <$> bfGetAccountDelegations address
                            blockEventsRegDeleg <-
                                for (regTxHashes <> delTxHashes) \hash -> do
                                    (tx@BF.Transaction{_transactionIndex}, dcerts) <-
                                        concurrently
                                            (bfGetTx hash)
                                            (fetchDelegation tr network bfLayer hash)
                                    txIndex <- throwBlockfrostError $
                                        _transactionIndex <?#> "_transactionIndex"
                                    txBlockEvents
                                        tx
                                        ( unsafeMkSublist [] )
                                        ( unsafeMkSublist $
                                            (\(n, dc) -> ((txIndex, n), dc))
                                                <$> zip [0 ..] dcerts
                                        )
                            ws <- bfGetAccountWithdrawals address
                            blockEventsWithdraw <-
                                for ws \BF.AccountWithdrawal{..} -> do
                                    (bftx@BF.Transaction{_transactionIndex}, tx) <-
                                        fetchTransaction
                                            tr
                                            network
                                            bfLayer
                                            _accountWithdrawalTxHash
                                    txIndex <- throwBlockfrostError $
                                        _transactionIndex <?#> "_transactionIndex"
                                    txBlockEvents
                                        bftx
                                        (unsafeMkSublist [((txIndex, 0), tx)])
                                        (unsafeMkSublist [])
                            pure $ blockEventsRegDeleg <> blockEventsWithdraw
                  where
                    txBlockEvents ::
                        BF.Transaction ->
                        Sublist Tx ->
                        Sublist DelegationCertificate ->
                        IO BlockEvents
                    txBlockEvents BF.Transaction{..} txs ds = do
                        slot <- throwBlockfrostError $
                            At . SlotNo <$> toInteger _transactionSlot
                                <?#> "_transactionSlot"
                        blockHeight <- throwBlockfrostError $
                            Quantity <$> _transactionBlockHeight
                                <?#> "_transactionBlockHeight"
                        pure BlockEvents
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
                        { getHeader = header
                        , getTip = currentNodeTip bfLayer
                        , getBlockHeaderAtHeight
                        , getNextBlockHeader
                        , getBlockHeaderAt
                        , getNextBlocks
                        , getAddressTxs
                        }
            void $ LN.lightSync (MsgLightLayerLog >$< tr) lightSyncSource follower

    syncProgress :: BlockfrostLayer IO -> SlotNo -> IO SyncProgress
    syncProgress bfLayer s = do
        BF.Block {_blockSlot} <- bfGetLatestBlock bfLayer
        let latestSlot = maybe 0 BF.unSlot _blockSlot
            currentSlot = fromIntegral (unSlotNo s)
            percentage = currentSlot % latestSlot
        pure $ Syncing $ Quantity $ clipToPercentage percentage

    stakePoolsSummary :: BlockfrostLayer IO -> Coin -> IO StakePoolsSummary
    stakePoolsSummary bfLayer _coin = do
        protocolParameters <- currentProtocolParameters bfLayer
        BF.Network{_networkStake = BF.NetworkStake{_stakeLive}} <-
            bfGetNetworkInfo bfLayer
        totalLiveStake <-
            throwBlockfrostError $ fromBfLovelaces _stakeLive
        pools <- traverse BF.getPool =<< bfListPools bfLayer
        stake <- poolsStake totalLiveStake pools
        pure StakePoolsSummary
            { nOpt = intCast $ desiredNumberOfStakePools protocolParameters
            , rewards = Map.empty
            -- TODO: Update to new `Cardano.Pool.Rank.StakePoolsSummary`
            -- ADP-1509
            , stake
            }
          where
            poolsStake :: Coin -> [BF.PoolInfo] -> IO (Map PoolId Percentage)
            poolsStake total = fmap Map.fromList . traverse \BF.PoolInfo{..} ->
                (,) <$>
                    either (throwIO . BlockfrostException) pure
                    (fromBfPoolId _poolInfoPoolId) <*> do
                        live <- throwBlockfrostError $
                            fromBfLovelaces _poolInfoLiveStake
                        let ratio = Coin.toInteger live % Coin.toInteger total
                        case mkPercentage ratio of
                            Right percentage -> pure percentage
                            Left PercentageOutOfBoundsError ->
                                throwIO . BlockfrostException $
                                    PoolStakePercentageError total live

fetchNextBlocks
    :: Tracer IO Log
    -> SomeNetworkDiscriminant
    -> BlockfrostLayer IO
    -> Hash "BlockHeader"
    -> IO (Consensual [Block])
fetchNextBlocks tr nd bfLayer hash = do
    let prevBlockHash = BF.BlockHash $ toText hash
    bfGetBlocksAfterHash bfLayer prevBlockHash >>= \case
        NotConsensual -> pure NotConsensual
        Consensual bl -> Consensual <$> for bl \block@BF.Block{..} -> do
            header <-
                either (throwIO . BlockfrostException) pure $
                    bfBlockHeader block
            txhs <- bfGetBlockTxs bfLayer _blockHash
            transactions <-
                fmap snd <$> traverse (fetchTransaction tr nd bfLayer) txhs
            delegations <-
                join <$> traverse (fetchDelegation tr nd bfLayer) txhs
            pure Block{header, transactions, delegations}

fetchDelegation
    :: Tracer IO Log
    -> SomeNetworkDiscriminant
    -> BlockfrostLayer IO
    -> BF.TxHash
    -> IO [DelegationCertificate]
fetchDelegation
    tr (SomeNetworkDiscriminant (Proxy :: Proxy nd)) bfLayer hash = do
    liftIO $ traceWith tr $ MsgFetchDelegation hash
    delegations <-
        concurrently
            (bfGetTxDelegations bfLayer hash)
            (bfGetTxStakes bfLayer hash)
    certs <- either (throwIO . BlockfrostException) pure $
        for (uncurry align delegations) \case
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
    :: Tracer IO Log
    -> SomeNetworkDiscriminant
    -> BlockfrostLayer IO
    -> BF.TxHash
    -> IO (BF.Transaction, Tx)
fetchTransaction tr nd BlockfrostLayer{..} hash = do
    liftIO $ traceWith tr $ MsgFetchTransaction hash
    transaction <- bfGetTx hash
    utxos <- bfGetTxUtxos hash
    withdrawals <- bfGetTxWithdrawals hash
    metadata <- bfGetTxMetadataJSON hash
    (transaction,)
        <$> assembleTransaction nd transaction utxos withdrawals metadata

assembleTransaction
    :: SomeNetworkDiscriminant
    -> BF.Transaction
    -> BF.TransactionUtxos
    -> [BF.TransactionWithdrawal]
    -> [BF.TransactionMetaJSON]
    -> IO Tx
assembleTransaction
    network@(SomeNetworkDiscriminant (Proxy :: Proxy nd))
    BF.Transaction{..}
    BF.TransactionUtxos{..}
    txWithdrawals
    metadataJSON = either (throwIO . BlockfrostException) pure do
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
            bitraverse f f $ partition isRegularTxIn utxos
          where
            isRegularTxIn = not . BF._utxoInputCollateral
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
    Aeson.Object km ->
        TxMetaMap <$> for
            (Aeson.toList km)
            (bitraverse (unmarshalMetadataValue . Aeson.String . Aeson.toText)
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
fromBlockfrostPP network pp@BF.ProtocolParams{..} = do
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
    minimumUTxO <-
        getMinimumUTxOFunction pp
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
                        Just $ toRational _protocolParamsDecentralisationParam
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

-- | Selects a minimum UTxO function that is appropriate for the current era.
--
-- TODO: [ADP-1994]
--
-- This function is currently hard-wired to select the Alonzo era minimum UTxO
-- function, which computes a result based on the 'coinsPerUTxOWord' protocol
-- parameter.
--
-- However, the Babbage era will switch to a minimum UTxO function that depends
-- on the 'coinsPerUTxOByte' protocol parameter.
--
-- We should revise this function so that it's capable of selecting a minimum
-- UTxO function that's appropriate for the current era.
--
getMinimumUTxOFunction
    :: BF.ProtocolParams
    -> Either BlockfrostError MinimumUTxO
getMinimumUTxOFunction BF.ProtocolParams {_protocolParamsCoinsPerUtxoWord} =
    minimumUTxOForAlonzoEra . Ledger.Coin
        <$> intCast @_ @Integer _protocolParamsCoinsPerUtxoWord
        <?#> "CoinsPerUtxoWord"
  where
    minimumUTxOForAlonzoEra :: Ledger.Coin -> MinimumUTxO
    minimumUTxOForAlonzoEra coinsPerUTxOWord = minimumUTxOForShelleyBasedEra
        ShelleyBasedEraAlonzo
        def {Alonzo._coinsPerUTxOWord = coinsPerUTxOWord}

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
    :: BlockfrostLayer IO
    -> SlottingParameters
    -> DecentralizationLevel
    -> RewardParams
    -> BF.PoolId
    -> IO PerformanceEstimate
getPoolPerformanceEstimate bfLayer sp dl rp pid = do
    hist <- bfGetPoolHistory bfLayer pid get50 BF.Descending
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

{-------------------------------------------------------------------------------
    Logging
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
    | MsgBlockHeaderAtHeight Integer (Consensual BlockHeader)
    | MsgBlockHeaderAt ChainPoint (Consensual BlockHeader)
    | MsgNextBlockHeader BlockHeader (Consensual (Maybe BlockHeader))
    | MsgGotNextBlocks (Hash "BlockHeader") Int (Maybe BlockHeader)
    | MsgBlockfrostLayer Layer.Log

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
        MsgBlockHeaderAtHeight height cbh ->
            case cbh of
                NotConsensual ->
                    "Block at height " <> pretty height <>
                    " isn't a part of consensus anymore (rollback)."
                Consensual bh ->
                    "Fetched BlockHeader at height " <> pretty height
                        <> ": " <> pretty bh
        MsgBlockHeaderAt cp cbh ->
            case cbh of
                NotConsensual ->
                    "Block at point " <> pretty cp <>
                    " isn't a part of consensus anymore (rollback)."
                Consensual bh ->
                    "Fetched BlockHeader at " <> pretty cp <> ": " <> pretty bh
        MsgNextBlockHeader prev cnext ->
            case cnext of
                NotConsensual ->
                    "Block header " <> pretty prev <>
                    " isn't a part of consensus anymore (rollback)."
                Consensual Nothing ->
                    "Fetched no new block headers after " <> pretty prev
                Consensual (Just next) ->
                    "Fetched next block header " <> pretty next
                    <> " after the previous " <> pretty prev
        MsgGotNextBlocks f n m ->
            "Fetched " <> pretty n <> " blocks after " <> toText f <>
                maybe "." ((", starting from " <>) . pretty) m
        MsgBlockfrostLayer l ->
            toText l

instance HasSeverityAnnotation Log where
    getSeverityAnnotation = \case
        MsgTipReceived{} -> Info
        MsgTipWatcherNotified{} -> Debug
        MsgTipWatcherRegistered -> Notice
        MsgTimeInterpreterLog {} -> Info
        MsgLightLayerLog l -> getSeverityAnnotation l
        MsgAccountNotFound {} -> Warning
        MsgGetAddressTxs {} -> Notice
        MsgFetchTransaction {} -> Notice
        MsgFetchDelegation {} -> Notice
        MsgFetchedLatestBlockHeader {} -> Notice
        MsgCurrentSlottingParameters -> Notice
        MsgEraByLatestEpoch {} -> Notice
        MsgFetchNetworkRewardAccountBalances -> Notice
        MsgIsConsensus {} -> Notice
        MsgBlockHeaderAtHeight {} -> Notice
        MsgBlockHeaderAt {} -> Notice
        MsgNextBlockHeader{} -> Notice
        MsgGotNextBlocks {} -> Notice
        MsgBlockfrostLayer l -> getSeverityAnnotation l
