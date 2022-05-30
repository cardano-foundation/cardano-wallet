{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    eraByEpoch,
    fetchBlock,
    fetchTransaction,
    newClientConfig,
    BFM (..),
    runBFM,
    BlockfrostError (..),
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
    , NetworkMagic (..)
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
    ( HasSeverityAnnotation (getSeverityAnnotation) )
import Cardano.Pool.Rank
    ( RewardParams (..) )
import Cardano.Pool.Rank.Likelihood
    ( BlockProduction (..), PerformanceEstimate (..), estimatePoolPerformance )
import Cardano.Slotting.Slot
    ( unEpochSize )
import Cardano.Wallet.Api.Types
    ( decodeAddress, decodeStakeAddress, encodeAddress, encodeStakeAddress )
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
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (ActiveSlotCoefficient)
    , Block (..)
    , BlockHeader (..)
    , ChainPoint (..)
    , DecentralizationLevel (..)
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
    , StartTime
    , TokenBundleMaxSize (..)
    , TxParameters (..)
    , WithOrigin (At)
    , decodePoolIdBech32
    , emptyEraInfo
    , executionMemory
    , executionSteps
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
import Cardano.Wallet.Shelley.Network.Discriminant
    ( SomeNetworkDiscriminant (..), networkDiscriminantToId )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async.Lifted
    ( concurrently, forConcurrently, mapConcurrently )
import Control.Monad
    ( forever, join, (<=<) )
import Control.Monad.Base
    ( MonadBase )
import Control.Monad.Error.Class
    ( MonadError, liftEither, throwError )
import Control.Monad.IO.Class
    ( MonadIO (liftIO) )
import Control.Monad.Reader
    ( MonadReader, ReaderT (runReaderT), ask, asks )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Data.Align
    ( align )
import Data.Bifunctor
    ( first )
import Data.Bitraversable
    ( bitraverse )
import Data.Bits
    ( Bits )
import Data.Function
    ( (&) )
import Data.Functor
    ( void, (<&>) )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.IntCast
    ( intCast, intCastMaybe )
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
    , Quantity (..)
    , mkPercentage
    )
import Data.Scientific
    ( isInteger )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (fromText), TextDecodingError (..), ToText (..) )
import Data.These
    ( These (That, These, This) )
import Data.Traversable
    ( for )
import Fmt
    ( pretty )
import GHC.OldList
    ( sortOn )
import Money
    ( Discrete' )
import Ouroboros.Consensus.Cardano.Block
    ( CardanoBlock, StandardCrypto )
import Ouroboros.Consensus.HardFork.History.EraParams
    ( EraParams (..), SafeZone (StandardSafeZone, UnsafeIndefiniteSafeZone) )
import Ouroboros.Consensus.HardFork.History.Summary
    ( Bound (..), EraEnd (..), EraSummary (..), Summary (..) )
import Servant.Client
    ( runClientM )
import Text.Read
    ( readEither )
import UnliftIO
    ( throwIO )
import UnliftIO.Async
    ( async, link )
import UnliftIO.Exception
    ( Exception )

import qualified Blockfrost.Client as BF
import qualified Cardano.Api.Shelley as Node
import qualified Cardano.Slotting.Time as ST
import qualified Cardano.Wallet.Network.Light as LN
import qualified Cardano.Wallet.Shelley.Network.Blockfrost.Fixture as Fixture
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

data BlockfrostError
    = ClientError BF.BlockfrostError
    | NoSlotError BF.Block
    | IntegralCastError String
    | InvalidBlockHash BF.BlockHash TextDecodingError
    | InvalidTxMetadataLabel String
    | InvalidTxMetadataValue String
    | InvalidTxHash Text TextDecodingError
    | InvalidAddress Text TextDecodingError
    | InvalidPoolId Text TextDecodingError
    | InvalidDecentralizationLevelPercentage Double
    | InvalidUtxoInputAmount BF.UtxoInput
    | InvalidUtxoOutputAmount BF.UtxoOutput
    | UnknownEraForEpoch EpochNo
    deriving (Show, Eq)

newtype BlockfrostException = BlockfrostException BlockfrostError
    deriving stock (Show)
    deriving anyclass (Exception)

data Log
    = MsgWatcherUpdate BlockHeader BracketLog
    | MsgTimeInterpreterLog TimeInterpreterLog
    | MsgLightLayerLog LN.LightLayerLog

instance ToText Log where
    toText = \case
        MsgWatcherUpdate blockHeader bracketLog ->
            "Update watcher with tip: " <> pretty blockHeader
                <> ". Callback "
                <> toText bracketLog
                <> ". "
        MsgTimeInterpreterLog til ->
            toText til
        MsgLightLayerLog l ->
            toText l

instance HasSeverityAnnotation Log where
    getSeverityAnnotation = \case
        MsgWatcherUpdate _ _ -> Info
        MsgTimeInterpreterLog _ -> Info
        MsgLightLayerLog l -> getSeverityAnnotation l

withNetworkLayer
    :: Tracer IO Log
    -> SomeNetworkDiscriminant
    -> NetworkParameters
    -> BF.Project
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -> IO a
withNetworkLayer tr network np project k = do
    bfConfig <- newClientConfig project
    k NetworkLayer
        { chainSync = \_tr _chainFollower -> pure ()
        , lightSync = Just $ blockfrostLightSync network bfConfig
        , currentNodeTip = currentNodeTip bfConfig
        , currentNodeEra = currentNodeEra bfConfig
        , currentProtocolParameters = currentProtocolParameters bfConfig
        , currentSlottingParameters = currentSlottingParameters bfConfig
        , watchNodeTip = watchNodeTip bfConfig
        , postTx = undefined
        , stakeDistribution = undefined
        , getCachedRewardAccountBalance =
            getCachedRewardAccountBalance bfConfig
        , fetchRewardAccountBalances =
            fetchNetworkRewardAccountBalances network bfConfig
        , timeInterpreter =
            timeInterpreterFromStartTime getGenesisBlockDate
        , syncProgress = undefined
        }
  where
    NetworkParameters
        { genesisParameters = GenesisParameters{getGenesisBlockDate}
        } = np

    networkId = networkDiscriminantToId network

    currentNodeTip :: BF.ClientConfig -> IO BlockHeader
    currentNodeTip bfConfig =
        runBFM bfConfig $ fromBlockfrostM =<< BF.getLatestBlock

    watchNodeTip :: BF.ClientConfig -> (BlockHeader -> IO ()) -> IO ()
    watchNodeTip bfConfig callback = link =<< async (pollNodeTip callback)
      where
        pollNodeTip :: (BlockHeader -> IO ()) -> IO ()
        pollNodeTip cb = forever $ do
            header <- runBFM bfConfig $ fromBlockfrostM =<< BF.getLatestBlock
            bracketTracer (MsgWatcherUpdate header >$< tr) $ cb header
            threadDelay 2_000_000

    currentProtocolParameters :: BF.ClientConfig -> IO ProtocolParameters
    currentProtocolParameters bfConfig =
        runBFM bfConfig $ liftEither . fromBlockfrostPP networkId
            =<< BF.getLatestEpochProtocolParams

    currentSlottingParameters :: BF.ClientConfig -> IO SlottingParameters
    currentSlottingParameters bfConfig = runBFM bfConfig do
        BF.EpochInfo{_epochInfoEpoch} <- BF.getLatestEpoch
        epochNo <- fromBlockfrostM _epochInfoEpoch
        let EraParams{..} = eraParams $ epochEraSummary networkId epochNo
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
    currentNodeEra bfConfig = runBFM bfConfig do
        BF.EpochInfo{_epochInfoEpoch} <- BF.getLatestEpoch
        epoch <- fromBlockfrostM _epochInfoEpoch
        liftEither $ eraByEpoch networkId epoch

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
        (SomeNetworkDiscriminant (Proxy :: Proxy nd)) bfConfig accounts =
            runBFM bfConfig $ Map.fromList <$>
                for (Set.toList accounts) \rewardAccount -> do
                    BF.AccountInfo{..} <-
                        BF.getAccount $
                            BF.mkAddress $ encodeStakeAddress @nd rewardAccount
                    coin <-
                        fromIntegral @_ @Integer _accountInfoRewardsSum
                            <?#> "AccountInfoRewardsSum"
                    pure (rewardAccount, Coin coin)

    getCachedRewardAccountBalance :: BF.ClientConfig ->RewardAccount -> IO Coin
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
                runBF = runBFM bfConfig
            AnyCardanoEra era <- currentNodeEra bfConfig
            let stabilityWindow =
                    fromIntegral . getQuantity $ case cardanoEraStyle era of
                        LegacyByronEra ->
                            stabilityWindowByron $ slottingParameters np
                        ShelleyBasedEra _ ->
                            stabilityWindowShelley $ slottingParameters np

                isConsensus :: ChainPoint -> IO Bool
                isConsensus =
                    runBF . \case
                        ChainPointAtGenesis -> pure True
                        ChainPoint (SlotNo slot) blockHeaderHash -> do
                            BF.Block{_blockHash = BF.BlockHash bfHeaderHash} <-
                                BF.getBlockSlot (BF.Slot (toInteger slot))
                            pure $ bfHeaderHash == toText blockHeaderHash

                getBlockHeaderAtHeight :: Integer -> IO (Maybe BlockHeader)
                getBlockHeaderAtHeight height =
                    either (error . show) Just . fromBlockfrost
                        <$> (runBF . BF.getBlock) (Left height)

                getBlockHeaderAt :: ChainPoint -> IO (Maybe BlockHeader)
                getBlockHeaderAt = runBF . \case
                    ChainPointAtGenesis -> do
                        block <-
                            BF.getBlock $ Right $ genesisBlockHash network
                        pure . either (error . show) Just $
                            fromBlockfrost block
                    ChainPoint (SlotNo slot) blockHeaderHash -> do
                        b@BF.Block{_blockHash = BF.BlockHash bfHeaderHash} <-
                            BF.getBlockSlot (BF.Slot (toInteger slot))
                        pure $
                            if bfHeaderHash == toText blockHeaderHash
                                then either (error . show) Just $
                                        fromBlockfrost b
                                else Nothing

                getNextBlocks :: ChainPoint -> IO (Maybe [Block])
                getNextBlocks = runBF . \case
                    ChainPointAtGenesis ->
                        Just . pure <$> fetchGenesisBlock network
                    ChainPoint _slotNo hash ->
                        -- Only one block is fetched for now,
                        -- even though the type allows for a list of blocks
                        Just . pure <$> fetchBlock network hash

                getAddressTxs ::
                    BlockHeader ->
                    BlockHeader ->
                    Either Address RewardAccount ->
                    IO ChainEvents
                getAddressTxs bhFrom bhTo = runBF . fmap fromBlockEvents . \case
                    Left address -> do
                        txs <- BF.allPages \paged ->
                            BF.getAddressTransactions'
                                (BF.Address (encodeAddress @nd address))
                                paged
                                BF.Ascending
                                (Just $ headerToIndex bhFrom)
                                (Just $ headerToIndex bhTo)
                        forConcurrently txs \BF.AddressTransaction{..} -> do
                            (bftx, tx) <-
                                fetchTransaction network _addressTransactionTxHash
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
                                <$> BF.getAccountRegistrations address
                        delTxHashes <-
                            fmap BF._accountDelegationTxHash
                                <$> BF.getAccountDelegations address
                        blockEventsRegDeleg <-
                            forConcurrently (regTxHashes <> delTxHashes) \hash -> do
                                (tx@BF.Transaction{_transactionIndex}, dcerts) <-
                                    concurrently
                                        do BF.getTx hash
                                        do fetchDelegation network hash
                                txIndex <- _transactionIndex <?#> "_transactionIndex"
                                txBlockEvents
                                    tx
                                    (wholeList [])
                                    ( unsafeMkSublist $
                                        (\(n, dc) -> ((txIndex, n), dc))
                                            <$> zip [0 ..] dcerts
                                    )
                        ws <- BF.getAccountWithdrawals address
                        blockEventsWithdraw <-
                            forConcurrently ws \BF.AccountWithdrawal{..} -> do
                                (bftx@BF.Transaction{_transactionIndex}, tx) <-
                                    fetchTransaction network _accountWithdrawalTxHash
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
                        , getBlockHeaderAt
                        , getNextBlocks
                        , getAddressTxs
                        }
            void $ LN.lightSync (MsgLightLayerLog >$< tr) lightSyncSource follower

genesisBlockHash :: SomeNetworkDiscriminant -> BF.BlockHash
genesisBlockHash nd = case networkDiscriminantToId nd of
    Mainnet ->
        BF.BlockHash
            "5f20df933584822601f9e3f8c024eb5eb252fe8cefb24d1317dc3d432e940ebb"
    Testnet (NetworkMagic 1097911063) ->
        --  Magic of the current public testnet
        BF.BlockHash
            "96fceff972c2c06bd3bb5243c3921533\
            \3be6d56aaf4823073dca31afe5038471"
    Testnet magic ->
        error $
            "Genesis block hash isn't hardcoded for the Testnet " <> show magic

fetchGenesisBlock
    :: forall m
     . ( MonadError BlockfrostError m
       , BF.MonadBlockfrost m
       , MonadBaseControl IO m
       )
    => SomeNetworkDiscriminant
    -> m Block
fetchGenesisBlock nd = fetchBlock' nd (genesisBlockHash nd)

fetchBlock
    :: forall m
     . ( MonadError BlockfrostError m
       , BF.MonadBlockfrost m
       , MonadBaseControl IO m
       )
    => SomeNetworkDiscriminant
    -> Hash "BlockHeader"
    -> m Block
fetchBlock nd hash = fetchBlock' nd $ BF.BlockHash $ toText hash

fetchBlock'
    :: forall m
     . ( MonadError BlockfrostError m
       , BF.MonadBlockfrost m
       , MonadBaseControl IO m
       )
    => SomeNetworkDiscriminant
    -> BF.BlockHash
    -> m Block
fetchBlock' nd blockHash = do
    block@BF.Block{..} <- BF.getBlock $ Right blockHash
    header <- fromBlockfrostM block
    txHashes <- fetchTxHashes blockHash _blockTxCount
    transactions <- fmap snd <$> mapConcurrently (fetchTransaction nd) txHashes
    delegations <- join <$> mapConcurrently (fetchDelegation nd) txHashes
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
    => SomeNetworkDiscriminant
    -> BF.TxHash
    -> m [DelegationCertificate]
fetchDelegation (SomeNetworkDiscriminant (Proxy :: Proxy nd)) hash = do
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
        poolId <- fromBlockfrostM _transactionDelegationPoolId
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
    => SomeNetworkDiscriminant
    -> BF.TxHash
    -> m (BF.Transaction, Tx)
fetchTransaction nd hash = do
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
    (SomeNetworkDiscriminant (Proxy :: Proxy nd))
    BF.Transaction{..}
    BF.TransactionUtxos{..}
    txWithdrawals
    metadataJSON = liftEither do
        txId <- parseTxHash _transactionHash
        let fee = Just $ Coin $ fromIntegral _transactionFees
        (resolvedInputs, resolvedCollateralInputs) <-
            fromInputs _transactionUtxosInputs
        outputs <- for _transactionUtxosOutputs \out@BF.UtxoOutput{..} -> do
            let outAddr = BF.unAddress _utxoOutputAddress
            address <-
                either (throwError . InvalidAddress outAddr) pure $
                    decodeAddress @nd outAddr
            tokens <- do
                coin <- case [ lovelaces
                             | BF.AdaAmount lovelaces <- _utxoOutputAmount
                             ] of
                    [l] -> fromBlockfrost l
                    _ -> throwError $ InvalidUtxoOutputAmount out
                pure $ TokenBundle coin mempty -- TODO: Handle native assets
            pure TxOut{..}
        withdrawals <-
            Map.fromList
                <$> for txWithdrawals \BF.TransactionWithdrawal{..} -> do
                    let addr = BF.unAddress _transactionWithdrawalAddress
                    rewardAccount <-
                        first (InvalidAddress addr) $ decodeStakeAddress @nd addr
                    coin <- fromBlockfrost _transactionWithdrawalAmount
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
                        [l] -> fromBlockfrost l
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

class FromBlockfrost b w where
    fromBlockfrost :: b -> Either BlockfrostError w

fromBlockfrostM ::
    FromBlockfrost b w => MonadError BlockfrostError m => b -> m w
fromBlockfrostM = liftEither . fromBlockfrost

instance FromBlockfrost BF.Block BlockHeader where
    fromBlockfrost block@BF.Block{..} = do
        slotNo <- _blockSlot <?> NoSlotError block >>= fromBlockfrostM
        blockHeight <- Quantity <$> fromMaybe 0 _blockHeight <?#> "BlockHeight"
        headerHash <- parseBlockHeader _blockHash
        parentHeaderHash <- for _blockPreviousBlock parseBlockHeader
        pure BlockHeader{slotNo, blockHeight, headerHash, parentHeaderHash}
      where
        parseBlockHeader blockHash =
            case fromText (BF.unBlockHash blockHash) of
                Right hash -> pure hash
                Left tde -> throwError $ InvalidBlockHash blockHash tde

fromBlockfrostPP
    :: NetworkId
    -> BF.ProtocolParams
    -> Either BlockfrostError ProtocolParameters
fromBlockfrostPP network BF.ProtocolParams{..} = do
    decentralizationLevel <-
        let percentage =
                mkPercentage $
                    toRational _protocolParamsDecentralisationParam
            in case percentage of
                Left PercentageOutOfBoundsError ->
                    throwError $
                        InvalidDecentralizationLevelPercentage
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

instance FromBlockfrost BF.TxHash (Hash "Tx") where
    fromBlockfrost txHash = first (InvalidTxHash hash) $ fromText hash
      where
        hash = BF.unTxHash txHash

instance FromBlockfrost BF.Slot SlotNo where
    fromBlockfrost = fmap SlotNo . (<?#> "SlotNo") . BF.unSlot

instance FromBlockfrost BF.PoolId PoolId where
    fromBlockfrost poolId = first (InvalidPoolId addr) (decodePoolIdBech32 addr)
      where
        addr = BF.unPoolId poolId

instance FromBlockfrost BF.Epoch EpochNo where
    fromBlockfrost = pure . fromIntegral

-- type Lovelaces = Discrete' "ADA" '(1000000, 1)
instance FromBlockfrost (Discrete' "ADA" '(1000000, 1)) Coin where
    fromBlockfrost lovelaces =
        Coin <$> (intCast @_ @Integer lovelaces <?#> "Lovelaces")

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

-- | Raises an error in case of an absent value
(<?>) :: MonadError e m => Maybe a -> e -> m a
(<?>) Nothing e = throwError e
(<?>) (Just a) _ = pure a

infixl 8 <?>

{-# INLINE (<?>) #-}

-- | Casts integral values safely or raises an `IntegralCastError`
(<?#>)
    :: (MonadError BlockfrostError m, Integral a, Integral b, Bits a, Bits b)
    => a
    -> String
    -> m b
(<?#>) a e = intCastMaybe a <?> IntegralCastError e

infixl 8 <?#>

{-# INLINE (<?#>) #-}

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

newtype BFM a = BFM (ReaderT BF.ClientConfig (ExceptT BlockfrostError IO) a)
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadBase IO
        , MonadBaseControl IO
        , MonadReader BF.ClientConfig
        , MonadError BlockfrostError
        )

instance BF.MonadBlockfrost BFM where
    getConf = ask
    liftBlockfrostClient act = BFM do
        env <- asks fst
        liftIO (runClientM act env)
            >>= either (throwError . ClientError . BF.fromServantClientError) pure

newClientConfig :: BF.Project -> IO BF.ClientConfig
newClientConfig prj = (,prj) <$> BF.newEnvByProject prj

runBFM :: BF.ClientConfig -> BFM a -> IO a
runBFM cfg (BFM c) = handleBlockfrostError (runReaderT c cfg)

handleBlockfrostError :: ExceptT BlockfrostError IO a -> IO a
handleBlockfrostError =
    either (throwIO . BlockfrostException) pure <=< runExceptT
