{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Network Layer for talking to Haskell re-written nodes.
--
-- Good to read before / additional resources:
--
-- - Module's documentation in `ouroboros-network/typed-protocols/src/Network/TypedProtocols.hs`
-- - Data Diffusion and Peer Networking in Shelley (see: https://raw.githubusercontent.com/wiki/cardano-foundation/cardano-wallet/data_diffusion_and_peer_networking_in_shelley.pdf)
--     - In particular sections 4.1, 4.2, 4.6 and 4.8
module Cardano.Wallet.Network.Implementation
    ( withNetworkLayer
    , NetworkParams (..)
    , Observer (query, startObserving, stopObserving)
    , newObserver
    , ObserverLog (..)

      -- * Logging
    , Log (..)
    ) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    , NodeToClientVersion (..)
    , SlotNo (..)
    )
import Cardano.Api.Shelley
    ( toConsensusGenTx
    )
import Cardano.BM.Data.Severity
    ( Severity (..)
    )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..)
    , HasSeverityAnnotation (..)
    )
import Cardano.BM.Extra
    ( BracketLog
    , bracketTracer
    , produceTimings
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    , nodeSocketFile
    )
import Cardano.Wallet.Network
    ( ChainFollowLog (..)
    , ChainFollower
    , ChainSyncLog (..)
    , ErrFetchBlock (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    , mapChainFollower
    , mapChainSyncLog
    , withFollowStatsMonitoring
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( FetchBlockCmd (..)
    , LSQ (..)
    , LocalStateQueryCmd (..)
    , LocalTxSubmissionCmd (..)
    , PipeliningStrategy
    , chainSyncFetchNextBlock
    , chainSyncFollowTip
    , chainSyncWithBlocks
    , localStateQuery
    , localTxSubmission
    , send
    )
import Cardano.Wallet.Network.Implementation.Types
    ( fromOuroborosPoint
    , fromOuroborosTip
    , toOuroborosPoint
    )
import Cardano.Wallet.Network.Implementation.UnliftIO
    ( coerceHandlers
    )
import Cardano.Wallet.Primitive.Ledger.Byron
    ( byronCodecConfig
    )
import Cardano.Wallet.Primitive.Ledger.Read.Block.Header
    ( getBlockHeader
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromPoint
    , fromTip'
    , nodeToClientVersions
    , toCardanoEra
    , unsealShelleyTx
    )
import Cardano.Wallet.Primitive.Slotting
    ( TimeInterpreter
    , TimeInterpreterLog
    , currentRelativeTime
    , mkTimeInterpreter
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress (..)
    , SyncTolerance
    )
import Cardano.Wallet.Primitive.Types.Block
    ( BlockHeader
    )
import Cardano.Wallet.Primitive.Types.GenesisParameters
    ( GenesisParameters (..)
    )
import Cardano.Wallet.Primitive.Types.NetworkParameters
    ( NetworkParameters (..)
    )
import Cardano.Wallet.Primitive.Types.ProtocolParameters
    ( ProtocolParameters
    )
import Cardano.Wallet.Primitive.Types.SlottingParameters
    ( SlottingParameters
    )
import Cardano.Wallet.Primitive.Types.StakePoolSummary
    ( StakePoolsSummary (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx (..)
    )
import Control.Concurrent.Class.MonadSTM
    ( MonadSTM
    , STM
    , TMVar
    , TQueue
    , TVar
    , atomically
    , isEmptyTMVar
    , modifyTVar'
    , newEmptyTMVarIO
    , newTMVarIO
    , newTQueue
    , newTQueueIO
    , newTVarIO
    , putTMVar
    , readTMVar
    , readTVar
    , readTVarIO
    , retry
    , takeTMVar
    , tryReadTMVar
    , writeTQueue
    , writeTVar
    )
import Control.Monad
    ( forever
    , unless
    , void
    , when
    )
import Control.Monad.Class.MonadAsync
    ( MonadAsync
    )
import Control.Monad.Class.MonadST
    ( MonadST
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow
    )
import Control.Monad.Class.MonadTimer
    ( MonadTimer
    , threadDelay
    )
import Control.Monad.Except
    ( runExcept
    )
import Control.Monad.IO.Unlift
    ( MonadIO
    , MonadUnliftIO
    , liftIO
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , throwE
    )
import Control.Retry
    ( RetryAction (..)
    , RetryPolicyM
    , RetryStatus (..)
    , capDelay
    , fibonacciBackoff
    , recoveringDynamic
    )
import Control.Tracer
    ( Tracer (..)
    , nullTracer
    , traceWith
    )
import Data.ByteString.Lazy
    ( ByteString
    )
import Data.Either
    ( fromRight
    )
import Data.Function
    ( (&)
    )
import Data.Functor
    ( ($>)
    )
import Data.Functor.Contravariant
    ( Contravariant (..)
    , (>$<)
    )
import Data.List
    ( isInfixOf
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Map
    ( Map
    , (!)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Set
    ( Set
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time.Clock
    ( DiffTime
    )
import Data.Void
    ( Void
    )
import Fmt
    ( Buildable (..)
    , fmt
    , hexF
    , listF
    , mapF
    , pretty
    , (+|)
    , (|+)
    )
import GHC.Stack
    ( HasCallStack
    )
import Internal.Cardano.Write.Tx
    ( MaybeInRecentEra (..)
    )
import Network.Mux
    ( MuxError (..)
    , MuxErrorType (..)
    , WithMuxBearer (..)
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    , CardanoEras
    , CodecConfig (..)
    , GenTx
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( QueryHardFork (..)
    )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( MismatchEraInfo
    )
import Ouroboros.Consensus.HardFork.History.Qry
    ( Interpreter
    , PastHorizonException (..)
    )
import Ouroboros.Consensus.Ledger.Query
    ( Query (..)
    )
import Ouroboros.Consensus.Ledger.SupportsMempool
    ( ApplyTxErr
    )
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs
    , Codecs' (..)
    , DefaultCodecs
    , clientCodecs
    , defaultCodecs
    )
import Ouroboros.Consensus.Node.NetworkProtocolVersion
    ( HasNetworkProtocolVersion (..)
    , SupportedNetworkProtocolVersion (..)
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )
import Ouroboros.Consensus.Shelley.Ledger.Config
    ( CodecConfig (..)
    )
import Ouroboros.Network.Block
    ( Point
    , Tip (..)
    )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv
    , runPeer
    , runPipelinedPeer
    )
import Ouroboros.Network.Mux
    ( MiniProtocolCb (..)
    , MuxMode (..)
    , OuroborosApplicationWithMinimalCtx
    , RunMiniProtocol (..)
    , RunMiniProtocolWithMinimalCtx
    )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , Handshake
    , LocalAddress
    , NetworkConnectTracers (..)
    , NodeToClientProtocols (..)
    , NodeToClientVersionData
    , connectTo
    , localSnocket
    , nodeToClientProtocols
    , withIOManager
    )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( chainSyncClientPeer
    )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( chainSyncClientPeerPipelined
    )
import Ouroboros.Network.Protocol.Handshake.Version
    ( combineVersions
    , simpleSingletonVersions
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( localStateQueryClientPeer
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( LocalStateQuery
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( localTxSubmissionClientPeer
    )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission (..)
    , SubmitResult (..)
    )
import System.IO.Error
    ( isDoesNotExistError
    , isResourceVanishedError
    )
import UnliftIO.Async
    ( async
    , link
    , race
    )
import UnliftIO.Concurrent
    ( ThreadId
    )
import UnliftIO.Exception
    ( Handler (..)
    , IOException
    )

import qualified Cardano.Wallet.Network.LocalStateQuery as LSQ
import qualified Cardano.Wallet.Primitive.SyncProgress as SP
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Cardano.Wallet.Read as Read
import qualified Codec.CBOR.Term as CBOR
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Internal.Cardano.Write.Tx as Write

{- HLINT ignore "Use readTVarIO" -}
{- HLINT ignore "Use newTVarIO" -}
{- HLINT ignore "Use newEmptyTMVarIO" -}

-- | Create an instance of 'NetworkLayer' by connecting to a local node.
withNetworkLayer
    :: HasCallStack
    => Tracer IO Log
    -- ^ Logging of network layer startup
    -> PipeliningStrategy (CardanoBlock StandardCrypto)
    -- ^ pipelining value by the block heigh
    -> NetworkParameters
    -- ^ Initial blockchain parameters
    -> CardanoNodeConn
    -- ^ Socket for communicating with the node
    -> NodeToClientVersionData
    -- ^ Codecs for the node's client
    -> SyncTolerance
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -- ^ Callback function with the network layer
    -> IO a
withNetworkLayer tr pipeliningStrategy np conn ver tol action = do
    trTimings <- traceQueryTimings tr
    withNodeNetworkLayerBase
        (tr <> trTimings)
        pipeliningStrategy
        np
        conn
        ver
        tol
        action

-- | Network parameters and protocol parameters for the node's current tip.
data NetworkParams = NetworkParams
    { protocolParams :: MaybeInRecentEra Write.PParams
    , protocolParamsLegacy :: ProtocolParameters
    , slottingParamsLegacy :: SlottingParameters
    }
    deriving (Eq, Show)

withNodeNetworkLayerBase
    :: HasCallStack
    => Tracer IO Log
    -> PipeliningStrategy (CardanoBlock StandardCrypto)
    -> NetworkParameters
    -> CardanoNodeConn
    -> NodeToClientVersionData
    -> SyncTolerance
    -> (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a)
    -> IO a
withNodeNetworkLayerBase
    tr
    pipeliningStrategy
    np
    conn
    versionData
    tol
    action = do
        -- NOTE: We keep client connections running for accessing the node tip,
        -- submitting transactions, querying parameters and delegations/rewards.
        --
        -- It is safe to retry when the connection is lost here because this client
        -- doesn't really do anything but sending messages to get the node's tip.
        -- It doesn't rely on the intersection to be up-to-date.
        let handlers cl = retryOnConnectionLost (MsgConnectionStatus cl >$< tr)

        -- FIXME: Would be nice to remove these multiple vars.
        -- Not as trivial as it seems, since we'd need to preserve the @debounce@
        -- behaviour.
        (readNodeTip, networkParamsVar, interpreterVar, eraVar, txSubmissionQ) <-
            connectNodeClient (handlers ClientNodeTip)

        queryRewardQ <-
            connectDelegationRewardsClient
                (handlers ClientDelegationRewards)

        rewardsObserver <-
            newRewardBalanceFetcher tr readNodeTip queryRewardQ

        let readCurrentNodeEra = atomically $ readTMVar eraVar

        action
            NetworkLayer
                { chainSync = \trFollowLog follower -> do
                    let withStats =
                            withFollowStatsMonitoring
                                trFollowLog
                                (_syncProgress interpreterVar)
                    withStats $ \trChainSyncLog -> do
                        let mapB = getBlockHeader getGenesisBlockHash
                            mapP = fromOuroborosPoint
                        let blockHeader = fromTip' gp
                        let client =
                                mkWalletClient
                                    (mapChainSyncLog mapB mapP >$< trChainSyncLog)
                                    pipeliningStrategy
                                    (mapChainFollower
                                        toOuroborosPoint
                                        mapP
                                        blockHeader
                                        id
                                        follower
                                    )
                                    cfg
                        traceWith trFollowLog MsgStartFollowing
                        let trChainSync = MsgConnectionStatus ClientChainSync >$< tr
                            retryHandlers = handlers ClientChainSync
                        connectClient trChainSync retryHandlers client versionData conn
                , fetchNextBlock =
                    _fetchNextBlock (handlers ClientFetchBlock)
                , currentNodeTip =
                    fromOuroborosTip <$> atomically readNodeTip
                , currentNodeEra =
                    -- NOTE: Is not guaranteed to be consistent with @currentNodeTip@
                    readCurrentNodeEra
                , watchNodeTip =
                    _watchNodeTip readNodeTip
                , currentProtocolParameters =
                    protocolParamsLegacy
                        <$> atomically (readTMVar networkParamsVar)
                , currentProtocolParametersInRecentEras =
                    protocolParams <$> atomically (readTMVar networkParamsVar)
                , currentSlottingParameters =
                    slottingParamsLegacy
                        <$> atomically (readTMVar networkParamsVar)
                , postTx =
                    _postTx txSubmissionQ readCurrentNodeEra
                , stakeDistribution =
                    _stakeDistribution queryRewardQ
                , getCachedRewardAccountBalance =
                    _getCachedRewardAccountBalance rewardsObserver
                , fetchRewardAccountBalances =
                    fetchRewardAccounts tr queryRewardQ
                , timeInterpreter =
                    _timeInterpreter (contramap MsgInterpreterLog tr) interpreterVar
                , syncProgress = _syncProgress interpreterVar
                }
      where
        gp@GenesisParameters
            { getGenesisBlockHash
            , getGenesisBlockDate
            } = genesisParameters np
        sp = slottingParameters np
        cfg = codecConfig sp

        connectNodeClient
            :: HasCallStack
            => RetryHandlers
            -> IO
                ( STM IO (Tip (CardanoBlock StandardCrypto))
                , TMVar IO NetworkParams
                , TMVar IO (CardanoInterpreter StandardCrypto)
                , TMVar IO AnyCardanoEra
                , TQueue
                    IO
                    ( LocalTxSubmissionCmd
                        (GenTx (CardanoBlock StandardCrypto))
                        (ApplyTxErr (CardanoBlock StandardCrypto))
                        IO
                    )
                )
        connectNodeClient handlers = do
            networkParamsVar <- newEmptyTMVarIO
            interpreterVar <- newEmptyTMVarIO
            eraVar <- newEmptyTMVarIO
            txSubmissionQ <- newTQueueIO
            (mkProtocols, readTip) <-
                mkWalletToNodeProtocols
                    tr
                    np
                    (atomically . repsertTMVar networkParamsVar)
                    (atomically . repsertTMVar interpreterVar)
                    (atomically . repsertTMVar eraVar)
                    txSubmissionQ
            let trNodeTip = MsgConnectionStatus ClientNodeTip >$< tr
                ouroborosApp :: NodeToClientVersion -> WalletOuroborosApplication IO
                ouroborosApp = nodeToClientProtocols =<< mkProtocols
            link
                =<< async
                    (connectClient trNodeTip handlers ouroborosApp versionData conn)
            pure (readTip, networkParamsVar, interpreterVar, eraVar, txSubmissionQ)

        connectDelegationRewardsClient
            :: (HasCallStack)
            => RetryHandlers
            -> IO (TQueue IO (LocalStateQueryCmd (CardanoBlock StandardCrypto) IO))
        connectDelegationRewardsClient handlers = do
            q <- atomically newTQueue
            let client = mkDelegationRewardsClient tr cfg q
                trRewardsClient = MsgConnectionStatus ClientDelegationRewards >$< tr
            link
                =<< async
                    (connectClient trRewardsClient handlers client versionData conn)
            pure q

        runFetchBlockClient
            :: forall block
              . ( block ~ CardanoBlock (StandardCrypto)
                )
            => RetryHandlers
            -> TQueue IO (FetchBlockCmd IO block (Point block))
            -> IO ()
        runFetchBlockClient retryHandlers blockQ = do
            let ouroborosApp
                    :: NodeToClientVersion -> WalletOuroborosApplication IO
                ouroborosApp = mkFetchBlockClient cfg blockQ
            connectClient nullTracer retryHandlers ouroborosApp versionData conn

        _fetchNextBlock retryHandlers pt = do
            let pt' = toOuroborosPoint pt
            blockQ <- newTQueueIO
            let runNodeToClient =
                    runFetchBlockClient retryHandlers blockQ
                awaitResult =
                    withReturnResult $ \respond ->
                        atomically
                            $ writeTQueue blockQ
                            $ FetchBlockCmd pt' respond

            -- We close the node-to-client connection as soon as
            -- we have received the result.
            emblock <- race runNodeToClient awaitResult
            pure $ case emblock of
                Right (Just block) -> Right block
                _ -> Left $ ErrNoBlockAt pt
          where
            withReturnResult :: ((a -> IO ()) -> IO ()) -> IO a
            withReturnResult cont = do
                var <- newEmptyTMVarIO
                cont $ atomically . putTMVar var
                atomically $ readTMVar var

        -- NOTE1: only shelley transactions can be submitted like this, because they
        -- are deserialised as shelley transactions before submitting.
        --
        -- NOTE2: It is not ideal to query the current era again here because we
        -- should in practice use the same era as the one used to construct the
        -- transaction. However, when turning transactions to 'SealedTx', we loose
        -- all form of type-level indicator about the era. The 'SealedTx' type
        -- shouldn't be needed anymore since we've dropped jormungandr, so we could
        -- instead carry a transaction from cardano-api types with proper typing.
        _postTx txSubmissionQueue readCurrentEra tx = do
            liftIO $ traceWith tr $ MsgPostTx tx
            preferredEra <- liftIO readCurrentEra
            let cmd =
                    CmdSubmitTx . toConsensusGenTx
                        $ unsealShelleyTx preferredEra tx
            liftIO (send txSubmissionQueue cmd) >>= \case
                SubmitSuccess -> pure ()
                SubmitFail e -> throwE $ ErrPostTxValidationError $ T.pack $ show e

        _stakeDistribution queue coin = do
            liftIO $ traceWith tr $ MsgWillQueryRewardsForStake coin

            mres <- bracketQuery "stakePoolsSummary" tr
                $ queue `send` SomeLSQ (LSQ.stakeDistribution coin)

            -- The result will be Nothing if query occurs during the byron era
            traceWith tr $ MsgFetchStakePoolsData mres
            case mres of
                Just res@StakePoolsSummary{rewards, stake} -> do
                    liftIO
                        $ traceWith tr
                        $ MsgFetchStakePoolsDataSummary
                            (Map.size stake)
                            (Map.size rewards)
                    return res
                Nothing -> pure $ StakePoolsSummary 0 mempty mempty

        _watchNodeTip readTip callback = do
            observeForever readTip $ \tip -> do
                let tip' = fromOuroborosTip tip
                bracketTracer (contramap (MsgWatcherUpdate tip') tr)
                    $ callback tip'

        -- TODO(#2042): Make wallets call manually, with matching stopObserving.
        _getCachedRewardAccountBalance rewardsObserver k = do
            startObserving rewardsObserver k
            fromMaybe (W.Coin 0) <$> query rewardsObserver k

        _timeInterpreter
            :: HasCallStack
            => Tracer IO TimeInterpreterLog
            -> TMVar IO (CardanoInterpreter sc)
            -> TimeInterpreter (ExceptT PastHorizonException IO)
        _timeInterpreter tr' var = do
            let readInterpreter = liftIO $ atomically $ readTMVar var
            mkTimeInterpreter tr' getGenesisBlockDate readInterpreter

        _syncProgress
            :: TMVar IO (CardanoInterpreter sc) -> SlotNo -> IO SyncProgress
        _syncProgress var slot =
            atomically (tryReadTMVar var) >>= \case
                -- If the wallet has been started, but not yet been able to connect
                -- to the node, we don't have an interpreter summary, and can't
                -- calculate the syncProgress using a @SlotNo@.
                --
                -- If we want to guarantee the availability of @SyncProgress@, we
                -- could consider storing @UTCTime@ along with the follower tip in
                -- question, but that would make chain-following dependent on
                -- a TimeInterpreter.
                Nothing -> pure NotResponding
                Just i -> do
                    let ti = mkTimeInterpreter nullTracer getGenesisBlockDate (pure i)
                    -- Getting a past horizon error here should be unlikely, but
                    -- could happen if we switch from a in-sync node to a
                    -- not-in-sync node, either by restarting the wallet, or
                    -- restarting the node using the same socket but different db.
                    fromRight NotResponding . runExcept . SP.syncProgress tol ti slot
                        <$> currentRelativeTime ti

{-------------------------------------------------------------------------------
    NetworkClient
    Node-to-client mini-protocol descriptions
-------------------------------------------------------------------------------}

-- | A protocol client that will never leave the initial state.
doNothingProtocol
    :: MonadTimer m
    => RunMiniProtocolWithMinimalCtx
        'InitiatorMode
        LocalAddress
        ByteString
        m
        Void
        Void
doNothingProtocol =
    InitiatorProtocolOnly $ MiniProtocolCb $ const $ const $ forever $ threadDelay 1_000_000

type WalletOuroborosApplication m =
    OuroborosApplicationWithMinimalCtx
        'InitiatorMode -- Initiator ~ Client (as opposed to Responder / Server)
        LocalAddress -- Address type
        ByteString -- Concrete representation for bytes string
        m -- Underlying monad the wallet runs in
        Void -- Return type of a network client. Void means the client never exits.
        Void -- Irrelevant for initiator. Return type of 'ResponderMode' app.

type WalletNodeToClientProtocols m =
    NodeToClientProtocols
        'InitiatorMode -- Initiator ~ Client (as opposed to Responder / Server)
        LocalAddress
        ByteString -- Concrete representation for bytes string
        m -- Underlying monad the wallet runs in
        Void -- Return type of a network client. Void means the client never exits.
        Void -- Irrelevant for initiator. Return type of 'ResponderMode' app.

-- | Construct a network client with the given communication channel, for the
-- purposes of syncing blocks to a single wallet.
mkWalletClient
    :: forall m block
     . ( block ~ CardanoBlock (StandardCrypto)
       , MonadThrow m
       , MonadST m
       , MonadTimer m
       , MonadAsync m
       )
    => Tracer m (ChainSyncLog block (Point block))
    -> PipeliningStrategy block
    -> ChainFollower m (Point block) (Tip block) (NonEmpty block)
    -> CodecConfig block
    -> NodeToClientVersion
    -> WalletOuroborosApplication m
mkWalletClient tr pipeliningStrategy follower cfg nodeToClientVer =
    nodeToClientProtocols protocols nodeToClientVer
  where
    protocols =
        NodeToClientProtocols
            { localTxSubmissionProtocol = doNothingProtocol
            , localStateQueryProtocol = doNothingProtocol
            , localTxMonitorProtocol = doNothingProtocol
            , localChainSyncProtocol =
                InitiatorProtocolOnly
                    $ MiniProtocolCb
                    $ \_ channel -> do
                        let codec = cChainSyncCodec $ codecs nodeToClientVer cfg
                        runPipelinedPeer nullTracer codec channel
                            $ chainSyncClientPeerPipelined
                            $ chainSyncWithBlocks tr pipeliningStrategy follower
            }

-- | Construct a network client with the given communication channel,
-- for the purpose of fetching single blocks.
mkFetchBlockClient
    :: forall m block
     . ( block ~ CardanoBlock (StandardCrypto)
       , MonadThrow m
       , MonadST m
       , MonadTimer m
       )
    => CodecConfig block
    -> TQueue m (FetchBlockCmd m block (Point block))
    -> NodeToClientVersion
    -> WalletOuroborosApplication m
mkFetchBlockClient cfg blockQ nodeToClientVer =
    nodeToClientProtocols protocols nodeToClientVer
  where
    protocols =
        NodeToClientProtocols
            { localTxSubmissionProtocol = doNothingProtocol
            , localStateQueryProtocol = doNothingProtocol
            , localTxMonitorProtocol = doNothingProtocol
            , localChainSyncProtocol =
                InitiatorProtocolOnly
                    $ MiniProtocolCb
                    $ \_ channel -> do
                        let codec = cChainSyncCodec $ codecs nodeToClientVer cfg
                        runPeer nullTracer codec channel
                            $ chainSyncClientPeer
                            $ chainSyncFetchNextBlock blockQ
            }

-- | Construct a network client with the given communication channel, for the
-- purposes of querying delegations and rewards.
mkDelegationRewardsClient
    :: forall m
     . (MonadThrow m, MonadST m, MonadTimer m, MonadIO m)
    => Tracer m Log
    -- ^ Base trace for underlying protocols
    -> CodecConfig (CardanoBlock StandardCrypto)
    -> TQueue m (LocalStateQueryCmd (CardanoBlock StandardCrypto) m)
    -- ^ Communication channel with the LocalStateQuery client
    -> NodeToClientVersion
    -> WalletOuroborosApplication m
mkDelegationRewardsClient tr cfg queryRewardQ nodeToClientVer =
    nodeToClientProtocols protocols nodeToClientVer
  where
    protocols =
        NodeToClientProtocols
            { localChainSyncProtocol = doNothingProtocol
            , localTxSubmissionProtocol = doNothingProtocol
            , localTxMonitorProtocol = doNothingProtocol
            , localStateQueryProtocol =
                InitiatorProtocolOnly $ MiniProtocolCb $ \_ channel -> do
                    let tr' = MsgLocalStateQuery DelegationRewardsClient >$< tr
                        codecs' = serialisedCodecs nodeToClientVer cfg
                        codec = cStateQueryCodec codecs'
                    runPeer tr' codec channel
                        $ localStateQueryClientPeer
                        $ localStateQuery queryRewardQ
            }

type CardanoInterpreter sc = Interpreter (CardanoEras sc)

-- | Construct node protocols with the given communication channels,
-- for the purpose of:
--
--  * Tracking the node tip
--  * Tracking the latest protocol parameters state.
--  * Querying the history interpreter as necessary.
--  * Submitting transactions
mkWalletToNodeProtocols
    :: forall m
     . (HasCallStack, MonadUnliftIO m, MonadThrow m, MonadST m, MonadTimer m)
    => Tracer m Log
    -- ^ Base trace for underlying protocols
    -> NetworkParameters
    -- ^ Initial blockchain parameters
    -> (NetworkParams -> m ())
    -- ^ Notifier callback for when parameters for tip change.
    -> (CardanoInterpreter StandardCrypto -> m ())
    -- ^ Notifier callback for when time interpreter is updated.
    -> (AnyCardanoEra -> m ())
    -- ^ Notifier callback for when the era is updated
    -> TQueue
        m
        ( LocalTxSubmissionCmd
            (GenTx (CardanoBlock StandardCrypto))
            (ApplyTxErr (CardanoBlock StandardCrypto))
            m
        )
    -> m
        ( NodeToClientVersion -> WalletNodeToClientProtocols m
        , STM m (Tip (CardanoBlock StandardCrypto))
        )
mkWalletToNodeProtocols
    tr
    np
    onPParamsUpdate
    onInterpreterUpdate
    onEraUpdate
    txSubmissionQ = do
        (localStateQueryQ :: TQueue m (LocalStateQueryCmd (CardanoBlock StandardCrypto) m)) <-
            atomically newTQueue

        tipVar <- newTVarIO (Just $ AnyCardanoEra ByronEra, TipGenesis)

        onPParamsUpdate' <-
            debounce $ \networkParams@NetworkParams{..} -> do
                traceWith tr
                    $ MsgProtocolParameters
                        protocolParamsLegacy
                        slottingParamsLegacy
                onPParamsUpdate networkParams

        let queryParams =
                NetworkParams
                    <$> LSQ.protocolParams
                    <*> LSQ.protocolParamsLegacy
                    <*> (LSQ.slottingParamsLegacy np)

        let queryInterpreter = LSQry (QueryHardFork GetInterpreter)

        let cfg = codecConfig (slottingParameters np)

        -- NOTE: These are updated every block. This is far more often than
        -- necessary.
        --
        -- By blocking (with `send`) we ensure we don't queue multiple queries.
        let onTipUpdate _tip = do
                let qry = (,,)
                        <$> queryParams
                        <*> queryInterpreter
                        <*> LSQ.currentEra
                (pparams, int, e) <- localStateQueryQ `send` (SomeLSQ qry)
                onPParamsUpdate' pparams
                onInterpreterUpdate int
                onEraUpdate e

        link =<< async (observeForever (readTVar tipVar) onTipUpdate)

        let ntcProtocols v =
                NodeToClientProtocols
                    { localChainSyncProtocol =
                        InitiatorProtocolOnly $ MiniProtocolCb
                            $ \_ channel -> do
                            let codec = cChainSyncCodec $ codecs v cfg
                            runPeer nullTracer codec channel
                                $ chainSyncClientPeer
                                $ chainSyncFollowTip toCardanoEra
                                $ curry (atomically . writeTVar tipVar)
                    , localStateQueryProtocol =
                        InitiatorProtocolOnly $ MiniProtocolCb
                            $ \_ channel -> do
                            let codec = cStateQueryCodec $ serialisedCodecs v cfg
                                client = localStateQuery localStateQueryQ
                                peer = localStateQueryClientPeer client
                                tr' = MsgLocalStateQuery TipSyncClient >$< tr
                            runPeer tr' codec channel peer
                    , localTxSubmissionProtocol =
                        InitiatorProtocolOnly $ MiniProtocolCb
                            $ \_ channel -> do
                            let bn2cVer = codecVersion v
                                codec = cTxSubmissionCodec (clientCodecs cfg bn2cVer v)
                                trTxSubmission = MsgTxSubmission >$< tr
                                client = localTxSubmission txSubmissionQ
                                peer = localTxSubmissionClientPeer client
                            runPeer trTxSubmission codec channel peer
                    , localTxMonitorProtocol = doNothingProtocol
                    }
        pure (ntcProtocols, snd <$> readTVar tipVar)

-- FIXME: We can remove the era from the tip sync client now.

{-------------------------------------------------------------------------------
    Thread for observing
    Reward Account Balance
-------------------------------------------------------------------------------}
newRewardBalanceFetcher
    :: Tracer IO Log
    -- ^ Used to convert tips for logging
    -> STM IO (Tip (CardanoBlock StandardCrypto))
    -- ^ STM action for observing the current tip
    -> TQueue IO (LocalStateQueryCmd (CardanoBlock StandardCrypto) IO)
    -> IO (Observer IO W.RewardAccount W.Coin)
newRewardBalanceFetcher tr readNodeTip queryRewardQ = do
    (ob, refresh) <- newObserver (contramap MsgObserverLog tr) fetch
    link =<< async (observeForever readNodeTip refresh)
    return ob
  where
    fetch
        :: Tip (CardanoBlock StandardCrypto)
        -> Set W.RewardAccount
        -> IO (Maybe (Map W.RewardAccount W.Coin))
    fetch _tip accounts | Set.null accounts = pure (Just mempty)
    fetch _tip accounts = do
        -- NOTE: We no longer need the tip to run LSQ queries. The local state
        -- query client will automatically acquire the latest tip.
        Just <$> fetchRewardAccounts tr queryRewardQ accounts

fetchRewardAccounts
    :: Tracer IO Log
    -> TQueue IO (LocalStateQueryCmd (CardanoBlock StandardCrypto) IO)
    -> Set W.RewardAccount
    -> IO (Map W.RewardAccount W.Coin)
fetchRewardAccounts tr queryRewardQ accounts = do
    liftIO
        $ traceWith tr
        $ MsgFetchRewardAccountBalance accounts

    bracketQuery "queryRewards" tr
        $ queryRewardQ `send` SomeLSQ (LSQ.fetchRewardAccounts accounts)

-- | Monitors values for keys, and allows clients to @query@ them.
--
-- Designed to be used for observing reward balances, where we want to cache the
-- balances of /all/ the wallets' accounts on tip change, and allow wallet
-- workers to @query@ the cache later, often, and whenever they want.
--
-- NOTE: One could imagine replacing @query@ getter with a push-based approach.
data Observer m key value = Observer
    { startObserving :: key -> m ()
    , stopObserving :: key -> m ()
    , query :: key -> m (Maybe value)
    }

-- | Given a way to fetch values for a set of keys, create:
-- 1. An @Observer@ for consuming values
-- 2. A refresh action
--
-- The @env@ parameter can be used to pass in information needed for refreshing,
-- like the current tip when fetching rewards.
--
-- If the given @fetch@ function returns @Nothing@ the the cache will not be
-- updated.
--
-- If it returns @Just values@, the cache will be set to @values@.
newObserver
    :: forall m key value env
     . (MonadSTM m, Ord key, Eq value)
    => Tracer m (ObserverLog key value)
    -> (env -> Set key -> m (Maybe (Map key value)))
    -> m (Observer m key value, env -> m ())
newObserver tr fetch = do
    cacheVar <- newTVarIO Map.empty
    toBeObservedVar <- newTVarIO Set.empty
    return (observer cacheVar toBeObservedVar, refresh cacheVar toBeObservedVar)
  where
    observer
        :: TVar m (Map key value)
        -> TVar m (Set key)
        -> Observer m key value
    observer cacheVar observedKeysVar =
        Observer
            { startObserving = \k -> do
                wasAdded <- atomically $ do
                    notAlreadyThere <- Set.notMember k <$> readTVar observedKeysVar
                    modifyTVar' observedKeysVar (Set.insert k)
                    return notAlreadyThere
                when wasAdded $ traceWith tr $ MsgAddedObserver k
            , stopObserving = \k -> do
                atomically $ do
                    modifyTVar' observedKeysVar (Set.delete k)
                    modifyTVar' cacheVar (Map.delete k)
                traceWith tr $ MsgRemovedObserver k
            , query = \k -> Map.lookup k <$> readTVarIO cacheVar
            }

    refresh
        :: TVar m (Map key value)
        -> TVar m (Set key)
        -> env
        -> m ()
    refresh cacheVar observedKeysVar env = do
        keys <- readTVarIO observedKeysVar
        oldValues <- readTVarIO cacheVar
        traceWith tr $ MsgWillFetch keys
        mvalues <- fetch env keys

        case mvalues of
            Nothing -> pure ()
            Just values -> do
                traceWith tr $ MsgDidFetch values
                when (oldValues /= values)
                    $ traceWith tr
                    $ MsgDidChange values
                atomically $ writeTVar cacheVar values

{-------------------------------------------------------------------------------
    Codecs
-------------------------------------------------------------------------------}

codecVersion
    :: NodeToClientVersion
    -> BlockNodeToClientVersion (CardanoBlock StandardCrypto)
codecVersion version = verMap ! version
  where
    verMap = supportedNodeToClientVersions (Proxy @(CardanoBlock StandardCrypto))

codecConfig :: SlottingParameters -> CodecConfig (CardanoBlock c)
codecConfig sp =
    CardanoCodecConfig
        (byronCodecConfig sp)
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig
        ShelleyCodecConfig

-- | A group of codecs which will deserialise block data.
codecs
    :: MonadST m
    => NodeToClientVersion
    -> CodecConfig (CardanoBlock StandardCrypto)
    -> ClientCodecs (CardanoBlock StandardCrypto) m
codecs nodeToClientVersion cfg =
    clientCodecs cfg (codecVersion nodeToClientVersion) nodeToClientVersion

-- | A group of codecs which won't deserialise block data. Often only the block
-- headers are needed. It's more efficient and easier not to deserialise.
serialisedCodecs
    :: MonadST m
    => NodeToClientVersion
    -> CodecConfig (CardanoBlock StandardCrypto)
    -> DefaultCodecs (CardanoBlock StandardCrypto) m
serialisedCodecs nodeToClientVersion cfg =
    defaultCodecs cfg (codecVersion nodeToClientVersion) nodeToClientVersion

{-------------------------------------------------------------------------------
    I/O -- Connect to the cardano-node process.
-------------------------------------------------------------------------------}
-- Connect a client to a network.
-- See `mkWalletClient` to construct a network client interface.
connectClient
    :: Tracer IO ConnectionStatusLog
    -> RetryHandlers
    -> (NodeToClientVersion -> WalletOuroborosApplication IO)
    -> NodeToClientVersionData
    -> CardanoNodeConn
    -> IO ()
connectClient tr handlers client vData conn = withIOManager $ \manager ->
    connectTo (localSnocket manager) tracers versions (nodeSocketFile conn)
        & recoveringNodeConnection tr handlers
  where
    versions =
        combineVersions
            [ simpleSingletonVersions version vData (client version)
            | version <- nodeToClientVersions
            ]
    tracers =
        NetworkConnectTracers
            { nctMuxTracer = nullTracer
            , nctHandshakeTracer = contramap MsgHandshakeTracer tr
            }

recoveringNodeConnection
    :: Tracer IO ConnectionStatusLog -> RetryHandlers -> IO a -> IO a
recoveringNodeConnection tr handlers action =
    recoveringDynamic policy (coerceHandlers handlers) $ \status -> do
        traceWith tr $ MsgCouldntConnect (rsIterNumber status)
        action
  where
    -- .25s -> .25s -> .5s → .75s → 1.25s → 2s
    policy :: RetryPolicyM IO
    policy = fibonacciBackoff 250_000 & capDelay 2_000_000

-- | Shorthand for the list of exception handlers used with 'recovering'.
type RetryHandlers = [RetryStatus -> Handler IO RetryAction]

-- | Handlers that are retrying on every connection lost.
retryOnConnectionLost :: Tracer IO ConnectionStatusLog -> RetryHandlers
retryOnConnectionLost tr =
    [ \_retryStatus ->
        Handler
            $ handleIOException
            $ MsgConnectionLost . ReasonException >$< tr
    , \_retryStatus ->
        Handler
            $ handleMuxError
            $ MsgConnectionLost >$< tr
    ]

-- When the node's connection vanished, we may also want to handle things in a
-- slightly different way depending on whether we are a waller worker or just
-- the node's tip thread.
handleIOException :: Tracer IO IOException -> IOException -> IO RetryAction
handleIOException tr e = traceWith tr e $> retryAction
  where
    retryAction
        -- There's a race-condition when starting the wallet and the node at the
        -- same time: the socket might not be there yet when we try to open it.
        -- In such case, we simply retry a bit later and hope it's there.
        | isDoesNotExistError e = ConsultPolicy
        -- I/O error where the operation failed because the resource vanished.
        -- This happens when, for example, attempting to write to a closed
        -- socket or attempting to write to a named pipe that was deleted.
        | isResourceVanishedError e = ConsultPolicy
        -- If the nonblocking UNIX domain socket connection cannot be completed
        -- immediately (i.e. connect() returns EAGAIN), try again. This happens
        -- because the node's listen queue is quite short.
        | "resource exhausted" `isInfixOf` show e = ConsultPolicy
        | otherwise = DontRetry

handleMuxError :: Tracer IO ReasonConnectionLost -> MuxError -> IO RetryAction
handleMuxError tr muxErr = do
    traceWith tr (ReasonMuxError muxErr)
    case errorType muxErr of
        MuxUnknownMiniProtocol -> pure DontRetry
        MuxDecodeError -> pure DontRetry
        MuxIngressQueueOverRun -> pure DontRetry
        MuxInitiatorOnly -> pure DontRetry
        MuxShutdown _ -> pure DontRetry -- fixme: #2212 consider cases
        MuxCleanShutdown -> pure DontRetry
        MuxIOException e -> handleIOException (ReasonException >$< tr) e
        MuxBearerClosed -> pure ConsultPolicy
        -- MuxSDU*Timeout errors arise because the bandwidth of the
        -- interprocess communication socket dropped unexpectedly,
        -- and the socket library decided to cut off the connection
        -- after ~ 30 seconds.
        -- Chances are that the system is overloaded, let's retry in 30 seconds.
        MuxSDUReadTimeout -> pure $ ConsultPolicyOverrideDelay 30_000_000
        MuxSDUWriteTimeout -> pure $ ConsultPolicyOverrideDelay 30_000_000

{-------------------------------------------------------------------------------
    Helper functions of the Control.* and STM variety
-------------------------------------------------------------------------------}

-- | Return a function to run an action only if its single parameter has changed
-- since the previous time it was called.
debounce :: (Eq a, MonadSTM m) => (a -> m ()) -> m (a -> m ())
debounce action = do
    mvar <- newTMVarIO Nothing
    pure $ \cur -> do
        prev <- atomically $ takeTMVar mvar
        unless (Just cur == prev) $ action cur
        atomically $ putTMVar mvar (Just cur)

-- | Trigger an action initially, and when the value changes.
--
-- There's no guarantee that we will see every intermediate value.
observeForever :: (MonadSTM m, Eq a) => STM m a -> (a -> m ()) -> m ()
observeForever readVal action = go Nothing
  where
    go old = do
        new <- atomically $ do
            new <- readVal
            unless (old /= Just new) retry
            return new
        action new
        go (Just new)

-- | Put if empty, replace if not empty.
repsertTMVar :: TMVar IO a -> a -> STM IO ()
repsertTMVar var x = do
    e <- isEmptyTMVar var
    unless e $ void $ takeTMVar var
    putTMVar var x

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Convenience function to trace around a local state query.
-- See 'addTimings'.
bracketQuery
    :: MonadUnliftIO m
    => String
    -> Tracer m Log
    -> m a
    -> m a
bracketQuery label tr = bracketTracer (contramap (MsgQuery label) tr)

-- | A tracer transformer which processes 'MsgQuery' logs to make new
-- 'MsgQueryTime' logs, so that we can get logs like:
--
-- >>> Query getAccountBalance took 51.664463s
traceQueryTimings :: Tracer IO Log -> IO (Tracer IO Log)
traceQueryTimings tr = produceTimings msgQuery trDiffTime
  where
    trDiffTime = contramap (uncurry MsgQueryTime) tr
    msgQuery = \case
        MsgQuery l b -> Just (l, b)
        _ -> Nothing

-- | Consider a "slow query" to be something that takes 200ms or more.
isSlowQuery :: String -> DiffTime -> Bool
isSlowQuery _label = (>= 0.2)

data ReasonConnectionLost
    = ReasonMuxError MuxError
    | ReasonException IOException

data ConnectionStatusLog where
    MsgCouldntConnect
        :: Int -> ConnectionStatusLog
    MsgConnectionLost
        :: ReasonConnectionLost -> ConnectionStatusLog
    MsgHandshakeTracer
        :: WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace
        -> ConnectionStatusLog

instance ToText ConnectionStatusLog where
    toText = \case
        MsgCouldntConnect n ->
            T.concat
                [ "Couldn't connect to node (x"
                , toText (n + 1)
                , "). Retrying in a bit..."
                ]
        MsgConnectionLost reason -> case reason of
            ReasonMuxError MuxError{errorType, errorMsg} ->
                "Node connection lost because of the mux error ("
                    <> T.pack (show errorType)
                    <> "): "
                    <> T.pack errorMsg
            ReasonException e ->
                "Node connection lost because of the exception: "
                    <> T.pack (show e)
        MsgHandshakeTracer (WithMuxBearer conn h) ->
            pretty conn <> " " <> T.pack (show h)

instance HasPrivacyAnnotation ConnectionStatusLog
instance HasSeverityAnnotation ConnectionStatusLog where
    getSeverityAnnotation = \case
        MsgCouldntConnect 0 -> Debug
        MsgCouldntConnect 1 -> Notice
        MsgCouldntConnect{} -> Warning
        MsgHandshakeTracer{} -> Debug
        MsgConnectionLost reason -> case reason of
            ReasonException ie | isResourceVanishedError ie -> Warning
            ReasonException _ie -> Debug
            ReasonMuxError muxError ->
                case errorType muxError of
                    MuxUnknownMiniProtocol -> Debug
                    MuxDecodeError -> Debug
                    MuxIngressQueueOverRun -> Debug
                    MuxInitiatorOnly -> Debug
                    MuxIOException _ -> Debug
                    MuxSDUReadTimeout -> Debug
                    MuxSDUWriteTimeout -> Debug
                    MuxShutdown _ -> Debug
                    MuxCleanShutdown -> Debug
                    MuxBearerClosed -> Warning

data Client
    = ClientChainSync
    | ClientLocalTxSubmission
    | ClientFetchBlock
    | ClientNodeTip
    | ClientDelegationRewards

renderClientName :: Client -> T.Text
renderClientName = \case
    ClientChainSync -> "Chain Sync"
    ClientLocalTxSubmission -> "Local TX Submission"
    ClientFetchBlock -> "Fetch block"
    ClientNodeTip -> "Node Tip"
    ClientDelegationRewards -> "Delegation Rewards"

data Log where
    MsgConnectionStatus :: Client -> ConnectionStatusLog -> Log
    MsgTxSubmission
        :: (Show tx, Show err)
        => TraceSendRecv (LocalTxSubmission tx err)
        -> Log
    MsgLocalStateQuery
        :: QueryClientName
        -> ( TraceSendRecv
                ( LocalStateQuery
                    (CardanoBlock StandardCrypto)
                    (Point (CardanoBlock StandardCrypto))
                    (Query (CardanoBlock StandardCrypto))
                )
           )
        -> Log
    MsgPostTx :: W.SealedTx -> Log
    MsgNodeTip :: BlockHeader -> Log
    MsgProtocolParameters :: ProtocolParameters -> SlottingParameters -> Log
    MsgLocalStateQueryError :: QueryClientName -> String -> Log
    MsgLocalStateQueryEraMismatch
        :: MismatchEraInfo (CardanoEras StandardCrypto) -> Log
    MsgFetchRewardAccountBalance :: Set W.RewardAccount -> Log
    MsgDestroyCursor :: ThreadId -> Log
    MsgWillQueryRewardsForStake :: W.Coin -> Log
    MsgFetchStakePoolsData :: Maybe StakePoolsSummary -> Log
    MsgFetchStakePoolsDataSummary
        :: Int
        -> Int
        -> Log
        -- ^ Number of pools in stake distribution, and rewards map,
        -- respectively.
    MsgWatcherUpdate :: Read.ChainTip -> BracketLog -> Log
    MsgInterpreter :: CardanoInterpreter StandardCrypto -> Log
    -- TODO: Combine ^^ and vv
    MsgInterpreterLog :: TimeInterpreterLog -> Log
    MsgQuery :: String -> BracketLog -> Log
    MsgQueryTime :: String -> DiffTime -> Log
    MsgObserverLog :: ObserverLog W.RewardAccount W.Coin -> Log

data QueryClientName
    = TipSyncClient
    | DelegationRewardsClient
    deriving (Show, Eq)

type HandshakeTrace = TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)

instance ToText Log where
    toText = \case
        MsgConnectionStatus client statusLog ->
            renderClientName client <> " node client: " <> toText statusLog
        MsgTxSubmission msg ->
            T.pack (show msg)
        MsgPostTx tx ->
            "Posting transaction, serialized as:\n" +| hexF (serialisedTx tx) |+ ""
        MsgLocalStateQuery client msg ->
            T.pack (show client <> " " <> show msg)
        MsgNodeTip bh ->
            T.unwords
                [ "Network node tip is"
                , pretty bh
                ]
        MsgProtocolParameters pparams sparams ->
            T.unlines
                [ "Protocol parameters for tip are:"
                , pretty pparams
                , "Slotting parameters for tip are:"
                , pretty sparams
                ]
        MsgLocalStateQueryError client e ->
            T.pack
                $ mconcat
                    [ "Error when querying local state parameters for "
                    , show client
                    , ": "
                    , e
                    ]
        MsgLocalStateQueryEraMismatch mismatch ->
            "Local state query for the wrong era - this is fine. "
                <> T.pack (show mismatch)
        MsgFetchRewardAccountBalance accts ->
            T.unwords
                [ "Querying the reward account balance for"
                , fmt $ listF accts
                ]
        MsgDestroyCursor threadId ->
            T.unwords
                [ "Destroying cursor connection at"
                , T.pack (show threadId)
                ]
        MsgWillQueryRewardsForStake c ->
            "Will query non-myopic rewards using the stake " <> pretty c
        MsgFetchStakePoolsData d ->
            "Fetched pool data from node tip using LSQ: " <> pretty d
        MsgFetchStakePoolsDataSummary inStake inRewards ->
            mconcat
                [ "Fetched pool data from node tip using LSQ. Got "
                , T.pack (show inStake)
                , " pools in the stake distribution, and "
                , T.pack (show inRewards)
                , " pools in the non-myopic member reward map."
                ]
        MsgWatcherUpdate tip b ->
            "Update watcher with tip: "
                <> Read.prettyChainTip tip
                <> ". Callback "
                <> toText b
                <> "."
        MsgQuery label msg ->
            T.pack label <> ": " <> toText msg
        MsgQueryTime qry diffTime ->
            "Query "
                <> T.pack qry
                <> " took "
                <> T.pack (show diffTime)
                <> if isSlowQuery qry diffTime then " (too slow)" else ""
        MsgInterpreter interpreter ->
            "Updated the history interpreter: " <> T.pack (show interpreter)
        MsgInterpreterLog msg -> toText msg
        MsgObserverLog msg -> "Reward observer: " <> toText msg

instance HasPrivacyAnnotation Log
instance HasSeverityAnnotation Log where
    getSeverityAnnotation = \case
        MsgConnectionStatus _client msg -> getSeverityAnnotation msg
        MsgTxSubmission{} -> Info
        MsgPostTx{} -> Debug
        MsgLocalStateQuery{} -> Debug
        MsgNodeTip{} -> Debug
        MsgProtocolParameters{} -> Info
        MsgLocalStateQueryError{} -> Error
        MsgLocalStateQueryEraMismatch{} -> Debug
        MsgDestroyCursor{} -> Debug
        MsgWillQueryRewardsForStake{} -> Info
        MsgFetchStakePoolsData{} -> Debug
        MsgFetchStakePoolsDataSummary{} -> Info
        MsgWatcherUpdate{} -> Debug
        MsgInterpreter{} -> Debug
        MsgQuery _ msg -> getSeverityAnnotation msg
        MsgQueryTime qry dt
            | isSlowQuery qry dt -> Notice
            | otherwise -> Debug
        MsgInterpreterLog msg -> getSeverityAnnotation msg
        MsgFetchRewardAccountBalance{} -> Debug
        MsgObserverLog (MsgDidChange _) -> Notice
        MsgObserverLog{} -> Debug

data ObserverLog key value
    = MsgWillFetch (Set key)
    | MsgDidFetch (Map key value)
    | MsgDidChange (Map key value)
    | MsgAddedObserver key
    | MsgRemovedObserver key
    deriving (Eq, Show)

instance
    (Ord key, Buildable key, Buildable value)
    => ToText (ObserverLog key value)
    where
    toText (MsgWillFetch keys) =
        mconcat
            [ "Will fetch values for keys "
            , fmt $ listF keys
            ]
    toText (MsgDidFetch m) =
        mconcat
            [ "Did fetch values "
            , fmt $ mapF m
            ]
    toText (MsgDidChange m) =
        mconcat
            [ "New values: "
            , fmt $ mapF m
            ]
    toText (MsgAddedObserver key) =
        mconcat
            [ "Started observing values for key "
            , pretty key
            ]
    toText (MsgRemovedObserver key) =
        mconcat
            [ "Stopped observing values for key "
            , pretty key
            ]
