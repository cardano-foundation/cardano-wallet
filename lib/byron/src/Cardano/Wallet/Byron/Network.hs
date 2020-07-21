{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Network Layer for talking to Haskell re-written nodes.
--
-- Good to read before / additional resources:
--
-- - Module's documentation in `ouroboros-network/typed-protocols/src/Network/TypedProtocols.hs`
-- - Data Diffusion and Peer Networking in Shelley (see: https://raw.githubusercontent.com/wiki/input-output-hk/cardano-wallet/data_diffusion_and_peer_networking_in_shelley.pdf)
--     - In particular sections 4.1, 4.2, 4.6 and 4.8
module Cardano.Wallet.Byron.Network
    ( -- * Top-Level Interface
      pattern Cursor
    , withNetworkLayer

      -- * Logging
    , NetworkLayerLog (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Chain.Byron.API
    ( ApplyMempoolPayloadErr (..) )
import Cardano.Wallet.Byron.Compatibility
    ( Byron
    , byronCodecConfig
    , fromChainHash
    , fromTip
    , protocolParametersFromUpdateState
    , toGenTx
    , toPoint
    )
import Cardano.Wallet.Network
    ( Cursor, ErrPostTx (..), NetworkLayer (..), mapCursor )
import Cardano.Wallet.Primitive.Slotting
    ( singleEraInterpreter )
import Control.Concurrent
    ( ThreadId )
import Control.Concurrent.Async
    ( Async, async, asyncThreadId, cancel, link )
import Control.Exception
    ( IOException )
import Control.Monad
    ( forever, unless, (>=>) )
import Control.Monad.Catch
    ( Handler (..) )
import Control.Monad.Class.MonadAsync
    ( MonadAsync )
import Control.Monad.Class.MonadST
    ( MonadST )
import Control.Monad.Class.MonadSTM
    ( MonadSTM
    , TQueue
    , atomically
    , newTMVarM
    , newTQueue
    , newTVar
    , putTMVar
    , readTVar
    , takeTMVar
    , writeTVar
    )
import Control.Monad.Class.MonadThrow
    ( MonadThrow )
import Control.Monad.Class.MonadTimer
    ( MonadTimer, threadDelay )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( throwE )
import Control.Retry
    ( RetryPolicyM, RetryStatus (..), capDelay, fibonacciBackoff, recovering )
import Control.Tracer
    ( Tracer, contramap, nullTracer, traceWith )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( runIdentity )
import Data.List
    ( isInfixOf )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Void
    ( Void )
import Fmt
    ( pretty )
import GHC.Stack
    ( HasCallStack )
import Network.Mux
    ( MuxError (..), MuxErrorType (..), WithMuxBearer )
import Ouroboros.Consensus.Byron.Ledger
    ( ByronBlock (..)
    , ByronNodeToClientVersion (ByronNodeToClientVersion1)
    , GenTx
    , Query (..)
    )
import Ouroboros.Consensus.Byron.Node
    ()
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), DefaultCodecs, clientCodecs, defaultCodecs )
import Ouroboros.Network.Block
    ( Point (..)
    , SlotNo (..)
    , Tip (..)
    , genesisPoint
    , getTipPoint
    , pointHash
    , pointSlot
    )
import Ouroboros.Network.Client.Wallet
    ( ChainSyncCmd (..)
    , LocalStateQueryCmd (..)
    , LocalTxSubmissionCmd (..)
    , chainSyncFollowTip
    , chainSyncWithBlocks
    , localStateQuery
    , localTxSubmission
    , send
    )
import Ouroboros.Network.CodecCBORTerm
    ( CodecCBORTerm )
import Ouroboros.Network.Driver.Simple
    ( TraceSendRecv, runPeer, runPipelinedPeer )
import Ouroboros.Network.Mux
    ( MuxMode (..)
    , MuxPeer (..)
    , OuroborosApplication (..)
    , RunMiniProtocol (..)
    )
import Ouroboros.Network.NodeToClient
    ( ConnectionId (..)
    , Handshake
    , LocalAddress
    , NetworkConnectTracers (..)
    , NodeToClientProtocols (..)
    , NodeToClientVersion (..)
    , NodeToClientVersionData (..)
    , connectTo
    , localSnocket
    , nodeToClientProtocols
    , withIOManager
    )
import Ouroboros.Network.Point
    ( fromWithOrigin )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( chainSyncClientPeer )
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined
    ( chainSyncClientPeerPipelined )
import Ouroboros.Network.Protocol.Handshake.Version
    ( DictVersion (..), simpleSingletonVersions )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( localStateQueryClientPeer )
import Ouroboros.Network.Protocol.LocalStateQuery.Type
    ( AcquireFailure, LocalStateQuery )
import Ouroboros.Network.Protocol.LocalTxSubmission.Client
    ( localTxSubmissionClientPeer )
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
    ( LocalTxSubmission, SubmitResult (..) )
import System.IO.Error
    ( isDoesNotExistError )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.CBOR.Term as CBOR
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{- HLINT ignore "Use readTVarIO" -}

-- | Network layer cursor for Byron. Mostly useless since the protocol itself is
-- stateful and the node's keep track of the associated connection's cursor.
data instance Cursor (m Byron) = Cursor
    (Async ())
    (Point ByronBlock)
    (TQueue m (ChainSyncCmd ByronBlock m))

-- | Create an instance of the network layer
withNetworkLayer
    :: Tracer IO NetworkLayerLog
        -- ^ Logging of network layer startup
        -- FIXME: Use a typed message instead of a 'Text'
    -> W.NetworkParameters
        -- ^ Initial blockchain parameters
    -> FilePath
        -- ^ Socket for communicating with the node
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
        -- ^ Codecs for the node's client
    -> (NetworkLayer IO (IO Byron) ByronBlock -> IO a)
        -- ^ Callback function with the network layer
    -> IO a
withNetworkLayer tr np addrInfo versionData action = do
    localTxSubmissionQ <- atomically newTQueue

    -- NOTE: We keep a client connection running for accessing the node tip,
    -- submitting transactions, and querying parameters.
    -- It is safe to retry when the connection is lost here because this client
    -- doesn't really do anything but sending dummy messages to get the node's
    -- tip. It doesn't rely on the intersection to be up-to-date.
    nodeTipVar <- atomically $ newTVar TipGenesis
    protocolParamsVar <- atomically $ newTVar $ W.protocolParameters np
    nodeTipClient <- mkTipSyncClient tr np
        localTxSubmissionQ
        (atomically . writeTVar nodeTipVar)
        (atomically . writeTVar protocolParamsVar)
    let handlers = retryOnConnectionLost tr
    link =<< async
        (connectClient tr handlers nodeTipClient versionData addrInfo)

    action
        NetworkLayer
            { currentNodeTip = liftIO $ _currentNodeTip nodeTipVar
            , nextBlocks = _nextBlocks
            , initCursor = _initCursor
            , destroyCursor = _destroyCursor
            , cursorSlotNo = _cursorSlotNo
            , getProtocolParameters = atomically $ readTVar protocolParamsVar
            , postTx = _postTx localTxSubmissionQ
            , stakeDistribution = _stakeDistribution
            , getAccountBalance = _getAccountBalance
            , timeInterpreter = pure . runIdentity . singleEraInterpreter gp
            , watchNodeTip = _watchNodeTip
            }
  where
    gp@W.GenesisParameters
        { getGenesisBlockHash
        } = W.genesisParameters np

    _initCursor headers = do
        chainSyncQ <- atomically newTQueue
        client <- mkWalletClient gp chainSyncQ
        let handlers = failOnConnectionLost tr
        thread <- async (connectClient tr handlers client versionData addrInfo)
        link thread
        let points = reverse $ genesisPoint :
                (toPoint getGenesisBlockHash <$> headers)
        let findIt = chainSyncQ `send` CmdFindIntersection points
        traceWith tr $ MsgFindIntersection headers
        res <- findIt
        case res of
            Just intersection -> do
                traceWith tr
                    $ MsgIntersectionFound
                    $ fromChainHash getGenesisBlockHash
                    $ pointHash intersection
                pure $ Cursor thread intersection chainSyncQ
            _ -> fail $ unwords
                [ "initCursor: intersection not found? This can't happen"
                , "because we always give at least the genesis point."
                , "Here are the points we gave: " <> show headers
                ]

    _destroyCursor (Cursor thread _ _) = do
        liftIO $ traceWith tr $ MsgDestroyCursor (asyncThreadId thread)
        cancel thread

    _nextBlocks (Cursor thread _ chainSyncQ) = do
        let toCursor point = Cursor thread point chainSyncQ
        liftIO $ mapCursor toCursor <$> chainSyncQ `send` CmdNextBlocks

    _cursorSlotNo (Cursor _ point _) = do
        fromWithOrigin (SlotNo 0) $ pointSlot point

    _getAccountBalance _ =
        pure (Quantity 0)

    _currentNodeTip nodeTipVar =
        fromTip getGenesisBlockHash <$> atomically (readTVar nodeTipVar)

    _postTx localTxSubmissionQ tx = do
        liftIO $ traceWith tr $ MsgPostSealedTx tx
        result <- liftIO $ localTxSubmissionQ `send` CmdSubmitTx (toGenTx tx)
        case result of
            SubmitSuccess -> pure ()
            SubmitFail err -> throwE $ ErrPostTxBadRequest $ T.pack (show err)

    _stakeDistribution =
        notImplemented "stakeDistribution"

    _watchNodeTip =
        notImplemented "watchNodeTip"

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type NetworkClient m = OuroborosApplication
    'InitiatorMode
        -- Initiator ~ Client (as opposed to Responder / Server)
    LocalAddress
        -- Address type
    ByteString
        -- Concrete representation for bytes string
    m
        -- Underlying monad we run in
    Void
        -- Return type of a network client. Void indicates that the client
        -- never exits.
    Void
        -- Irrelevant for initiator. Return type of 'ResponderMode' application.

-- | Construct a network client with the given communication channel, for the
-- purposes of syncing blocks to a single wallet.
mkWalletClient
    :: (MonadThrow m, MonadST m, MonadTimer m, MonadAsync m)
    => W.GenesisParameters
        -- ^ Static blockchain parameters
    -> TQueue m (ChainSyncCmd ByronBlock m)
        -- ^ Communication channel with the ChainSync client
    -> m (NetworkClient m)
mkWalletClient gp chainSyncQ = do
    responsesBuffer <- atomically newTQueue
    pure $ nodeToClientProtocols (const $ pure $ NodeToClientProtocols
        { localChainSyncProtocol =
            let
                fromTip' = fromTip getGenesisBlockHash
                codec = cChainSyncCodec codecs
            in
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPipelinedPeer nullTracer codec channel
                $ chainSyncClientPeerPipelined
                $ chainSyncWithBlocks csTr fromTip' chainSyncQ responsesBuffer

        , localTxSubmissionProtocol =
            doNothingProtocol

        , localStateQueryProtocol =
            doNothingProtocol
        })
        NodeToClientV_2
  where
    W.GenesisParameters
        { getGenesisBlockHash
        } = gp

    codecs :: MonadST m => ClientCodecs ByronBlock m
    codecs = clientCodecs (byronCodecConfig gp) ByronNodeToClientVersion1

    -- A low-level DEBUG chain sync tracer.
    --
    -- TODO: Exists for shelley, but not here. Might be suitable to add when we
    -- merge the two code-bases.
    csTr = nullTracer

-- | Construct a network client with the given communication channel, for the
-- purpose of:
--
--  * Submitting transactions
--  * Tracking the node tip
--  * Tracking the latest protocol parameters state.
mkTipSyncClient
    :: (MonadThrow m, MonadST m, MonadTimer m)
    => Tracer m NetworkLayerLog
        -- ^ Base trace for underlying protocols
    -> W.NetworkParameters
        -- ^ Initial blockchain parameters
    -> TQueue m
        (LocalTxSubmissionCmd (GenTx ByronBlock) ApplyMempoolPayloadErr m)
        -- ^ Communication channel with the LocalTxSubmission client
    -> (Tip ByronBlock -> m ())
        -- ^ Notifier callback for when tip changes
    -> (W.ProtocolParameters -> m ())
        -- ^ Notifier callback for when parameters for tip change.
    -> m (NetworkClient m)
mkTipSyncClient tr np localTxSubmissionQ onTipUpdate onProtocolParamsUpdate = do
    localStateQueryQ <- atomically newTQueue

    onProtocolParamsUpdate' <- debounce $ \pp -> do
        traceWith tr $ MsgProtocolParameters pp
        onProtocolParamsUpdate pp

    let
        queryLocalState pt = do
            st <- localStateQueryQ `send`
                CmdQueryLocalState pt GetUpdateInterfaceState
            handleLocalState st

        handleLocalState = \case
            Left (e :: AcquireFailure) ->
                traceWith tr $ MsgLocalStateQueryError $ show e
            Right ls ->
                onProtocolParamsUpdate' $ protocolParametersFromUpdateState ls

    onTipUpdate' <- debounce $ \tip -> do
        traceWith tr $
            MsgNodeTip $ fromTip getGenesisBlockHash tip
        onTipUpdate tip
        queryLocalState (getTipPoint tip)

    pure $ nodeToClientProtocols (const $ pure $ NodeToClientProtocols
        { localChainSyncProtocol =
            let
                codec = cChainSyncCodec codecs
            in
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPeer nullTracer codec channel
                $ chainSyncClientPeer
                $ chainSyncFollowTip onTipUpdate'

        , localTxSubmissionProtocol =
            let
                tr' = contramap MsgTxSubmission tr
                codec = cTxSubmissionCodec codecs
            in
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPeer tr' codec channel
                $ localTxSubmissionClientPeer
                $ localTxSubmission localTxSubmissionQ

        , localStateQueryProtocol =
            let
                tr' = contramap MsgLocalStateQuery tr
                codec = cStateQueryCodec codecs
            in
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPeer tr' codec channel
                $ localStateQueryClientPeer
                $ localStateQuery localStateQueryQ
        })
        NodeToClientV_2
  where
    gp@W.GenesisParameters
        { getGenesisBlockHash
        } = W.genesisParameters np

    codecs :: MonadST m => DefaultCodecs ByronBlock m
    codecs = defaultCodecs (byronCodecConfig gp) ByronNodeToClientVersion1

debounce :: (Eq a, MonadSTM m) => (a -> m ()) -> m (a -> m ())
debounce action = do
    mvar <- newTMVarM Nothing
    pure $ \cur -> do
        prev <- atomically $ takeTMVar mvar
        unless (Just cur == prev) $ action cur
        atomically $ putTMVar mvar (Just cur)

-- | A protocol client that will never leave the initial state.
doNothingProtocol
    :: MonadTimer m => RunMiniProtocol 'InitiatorMode ByteString m a Void
doNothingProtocol =
    InitiatorProtocolOnly $ MuxPeerRaw $
    const $ forever $ threadDelay 1e6

-- Connect a client to a network, see `mkWalletClient` to construct a network
-- client interface.
--
-- >>> connectClient (mkWalletClient tr gp queue) mainnetVersionData addrInfo
connectClient
    :: Tracer IO NetworkLayerLog
    -> [RetryStatus -> Handler IO Bool]
    -> NetworkClient IO
    -> (NodeToClientVersionData, CodecCBORTerm Text NodeToClientVersionData)
    -> FilePath
    -> IO ()
connectClient tr handlers client (vData, vCodec) addr = withIOManager $ \iocp -> do
    let vDict = DictVersion vCodec
    let versions = simpleSingletonVersions NodeToClientV_2 vData vDict client
    let tracers = NetworkConnectTracers
            { nctMuxTracer = nullTracer
            , nctHandshakeTracer = contramap MsgHandshakeTracer tr
            }
    let socket = localSnocket iocp addr
    recovering policy handlers $ \status -> do
        traceWith tr $ MsgCouldntConnect (rsIterNumber status)
        connectTo socket tracers versions addr
  where
    -- .25s -> .25s -> .5s → .75s → 1.25s → 2s
    policy :: RetryPolicyM IO
    policy = fibonacciBackoff 250_000 & capDelay 2_000_000

-- | Handlers that are retrying on every connection lost.
retryOnConnectionLost
    :: Tracer IO NetworkLayerLog
    -> [RetryStatus -> Handler IO Bool]
retryOnConnectionLost tr =
    [ const $ Handler $ handleIOException tr' True
    , const $ Handler $ handleMuxError tr' True
    ]
  where
    tr' = contramap MsgConnectionLost tr

-- | Handlers that are failing if the connection is lost
failOnConnectionLost
    :: Tracer IO NetworkLayerLog
    -> [RetryStatus -> Handler IO Bool]
failOnConnectionLost tr =
    [ const $ Handler $ handleIOException tr' False
    , const $ Handler $ handleMuxError tr' False
    ]
  where
    tr' = contramap MsgConnectionLost tr

-- When the node's connection vanished, we may also want to handle things in a
-- slightly different way depending on whether we are a waller worker or just
-- the node's tip thread.
handleIOException
    :: Tracer IO (Maybe IOException)
    -> Bool -- ^ 'True' = retry on 'ResourceVanishedError'
    -> IOException
    -> IO Bool
handleIOException tr onResourceVanished e
    -- There's a race-condition when starting the wallet and the node at the
    -- same time: the socket might not be there yet when we try to open it.
    -- In such case, we simply retry a bit later and hope it's there.
    | isDoesNotExistError e =
        pure True

    | isResourceVanishedError e = do
        traceWith tr $ Just e
        pure onResourceVanished

    | otherwise =
        pure False
  where
    isResourceVanishedError = isInfixOf "resource vanished" . show

handleMuxError
    :: Tracer IO (Maybe IOException)
    -> Bool -- ^ 'True' = retry on 'ResourceVanishedError'
    -> MuxError
    -> IO Bool
handleMuxError tr onResourceVanished = pure . errorType >=> \case
    MuxUnknownMiniProtocol -> pure False
    MuxDecodeError -> pure False
    MuxIngressQueueOverRun -> pure False
    MuxInitiatorOnly -> pure False
    MuxSDUReadTimeout -> pure False
    MuxSDUWriteTimeout -> pure False
    MuxShutdown -> pure False
    MuxIOException e ->
        handleIOException tr onResourceVanished e
    MuxBearerClosed -> do
        traceWith tr Nothing
        pure onResourceVanished

--------------------------------------------------------------------------------
--
-- Temporary

notImplemented :: HasCallStack => String -> a
notImplemented what = error ("Not implemented: " <> what)

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data NetworkLayerLog
    = MsgCouldntConnect Int
    | MsgConnectionLost (Maybe IOException)
    | MsgTxSubmission
        (TraceSendRecv
            (LocalTxSubmission (GenTx ByronBlock) ApplyMempoolPayloadErr))
    | MsgLocalStateQuery
        (TraceSendRecv (LocalStateQuery ByronBlock (Query ByronBlock)))
    | MsgHandshakeTracer
        (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace)
    | MsgFindIntersection [W.BlockHeader]
    | MsgIntersectionFound (W.Hash "BlockHeader")
    | MsgFindIntersectionTimeout
    | MsgPostSealedTx W.SealedTx
    | MsgNodeTip W.BlockHeader
    | MsgProtocolParameters W.ProtocolParameters
    | MsgLocalStateQueryError String
    | MsgDestroyCursor ThreadId

type HandshakeTrace = TraceSendRecv (Handshake NodeToClientVersion CBOR.Term)

instance ToText NetworkLayerLog where
    toText = \case
        MsgCouldntConnect n -> T.unwords
            [ "Couldn't connect to node (x" <> toText (n + 1) <> ")."
            , "Retrying in a bit..."
            ]
        MsgConnectionLost Nothing  ->
            "Connection lost with the node."
        MsgConnectionLost (Just e) -> T.unwords
            [ toText (MsgConnectionLost Nothing)
            , T.pack (show e)
            ]
        MsgTxSubmission msg ->
            T.pack (show msg)
        MsgHandshakeTracer msg ->
            T.pack (show msg)
        MsgFindIntersectionTimeout ->
            "Couldn't find an intersection in a timely manner. Retrying..."
        MsgFindIntersection points -> T.unwords
            [ "Looking for an intersection with the node's local chain with:"
            , T.intercalate ", " (pretty <$> points)
            ]
        MsgIntersectionFound point -> T.unwords
            [ "Intersection found:", pretty point ]
        MsgPostSealedTx (W.SealedTx bytes) -> T.unwords
            [ "Posting transaction, serialized as:"
            , T.decodeUtf8 $ convertToBase Base16 bytes
            ]
        MsgLocalStateQuery msg ->
            T.pack (show msg)
        MsgNodeTip bh -> T.unwords
            [ "Network node tip is"
            , pretty bh
            ]
        MsgProtocolParameters params -> T.unlines
            [ "Protocol parameters for tip are:"
            , pretty params
            ]
        MsgLocalStateQueryError e -> T.unwords
            [ "Error when querying local state parameters:"
            , T.pack e
            ]
        MsgDestroyCursor threadId -> T.unwords
            [ "Destroying cursor connection at"
            , T.pack (show threadId)
            ]

instance HasPrivacyAnnotation NetworkLayerLog
instance HasSeverityAnnotation NetworkLayerLog where
    getSeverityAnnotation = \case
        MsgCouldntConnect 0        -> Debug
        MsgCouldntConnect 1        -> Notice
        MsgCouldntConnect{}        -> Warning
        MsgConnectionLost{}        -> Warning
        MsgTxSubmission{}          -> Info
        MsgHandshakeTracer{}       -> Info
        MsgFindIntersectionTimeout -> Warning
        MsgFindIntersection{}      -> Info
        MsgIntersectionFound{}     -> Info
        MsgPostSealedTx{}          -> Debug
        MsgLocalStateQuery{}       -> Debug
        MsgNodeTip{}               -> Debug
        MsgProtocolParameters{}    -> Info
        MsgLocalStateQueryError{}  -> Error
        MsgDestroyCursor{}         -> Notice
