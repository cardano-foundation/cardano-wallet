{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
module Cardano.Wallet.Shelley.Network
    ( -- * Top-Level Interface
      pattern Cursor
    , withNetworkLayer

      -- * Logging
    , NetworkLayerLog
    ) where

import Prelude


import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.Network
    ( Cursor, ErrPostTx (..), NetworkLayer (..), mapCursor )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , ShelleyBlock
    , TPraosStandardCrypto
    , fromChainHash
    , fromPParams
    , fromSlotNo
    , fromTip
    , toGenTx
    , toPoint
    )
import Control.Concurrent.Async
    ( async, link )
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
    ( AppType (..), MuxError (..), MuxErrorType (..), WithMuxBearer )
import Ouroboros.Consensus.Network.NodeToClient
    ( ClientCodecs, Codecs' (..), DefaultCodecs, clientCodecs, defaultCodecs )
import Ouroboros.Consensus.Shelley.Ledger
    ( CodecConfig (ShelleyCodecConfig)
    , GenTx
    , Query (..)
    , ShelleyNodeToClientVersion (..)
    )
import Ouroboros.Consensus.Shelley.Node
    ()
import Ouroboros.Network.Block
    ( Point
    , SlotNo (..)
    , Tip (..)
    , castTip
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
    ( MuxPeer (..), OuroborosApplication (..), RunMiniProtocol (..) )
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
import qualified Ouroboros.Consensus.Shelley.Ledger as OC

{- HLINT ignore "Use readTVarIO" -}

-- | Network layer cursor for Shelley. Mostly useless since the protocol itself is
-- stateful and the node's keep track of the associated connection's cursor.
data instance Cursor (m Shelley) = Cursor
    (Point ShelleyBlock)
    (TQueue m (ChainSyncCmd ShelleyBlock m))

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
    -> (NetworkLayer IO (IO Shelley) ShelleyBlock -> IO a)
        -- ^ Callback function with the network layer
    -> IO a
withNetworkLayer tr gbp addrInfo versionData action = do
    localTxSubmissionQ <- atomically newTQueue

    -- NOTE: We keep a client connection running for accessing the node tip,
    -- submitting transactions, and querying parameters.
    -- It is safe to retry when the connection is lost here because this client
    -- doesn't really do anything but sending dummy messages to get the node's
    -- tip. It doesn't rely on the intersection to be up-to-date.
    nodeTipVar <- atomically $ newTVar TipGenesis
    txParamsVar <- atomically $ newTVar (W.txParameters gbp)
    nodeTipClient <- mkTipSyncClient tr gbp
        localTxSubmissionQ
        (atomically . writeTVar nodeTipVar)
        (atomically . writeTVar txParamsVar)
    let handlers = retryOnConnectionLost tr
    link =<< async
        (connectClient tr handlers (const nodeTipClient) versionData addrInfo)

    action
        NetworkLayer
            { currentNodeTip = liftIO $ _currentNodeTip nodeTipVar
            , nextBlocks = _nextBlocks
            , initCursor = _initCursor
            , cursorSlotId = _cursorSlotId
            , getTxParameters = atomically $ readTVar txParamsVar
            , postTx = _postTx localTxSubmissionQ
            , stakeDistribution = _stakeDistribution
            , getAccountBalance = _getAccountBalance
            }
  where
    bp@W.GenesisParameters
        { getGenesisBlockHash
        , getEpochLength
        } = W.genesisParameters gbp

    _initCursor headers = do
        chainSyncQ <- atomically newTQueue
        client <- mkWalletClient bp chainSyncQ
        let handlers = failOnConnectionLost tr
        link =<< async
            (connectClient tr handlers (const client) versionData addrInfo)
        let points = reverse $ genesisPoint :
                (toPoint getGenesisBlockHash getEpochLength <$> headers)
        let findIt = chainSyncQ `send` CmdFindIntersection points
        traceWith tr $ MsgFindIntersection headers
        res <- findIt
        case res of
            Just intersection -> do
                traceWith tr
                    $ MsgIntersectionFound
                    $ fromChainHash getGenesisBlockHash
                    $ pointHash intersection
                pure $ Cursor intersection chainSyncQ
            _ -> fail $ unwords
                [ "initCursor: intersection not found? This can't happen"
                , "because we always give at least the genesis point."
                , "Here are the points we gave: " <> show headers
                ]

    _nextBlocks (Cursor _ chainSyncQ) = do
        let toCursor point = Cursor point chainSyncQ
        liftIO $ mapCursor toCursor <$> chainSyncQ `send` CmdNextBlocks

    _cursorSlotId (Cursor point _) = do
        fromSlotNo getEpochLength $ fromWithOrigin (SlotNo 0) $ pointSlot point

    _getAccountBalance _ =
        pure (Quantity 0)

    _currentNodeTip nodeTipVar =
        fromTip getGenesisBlockHash getEpochLength <$> atomically (readTVar nodeTipVar)

    _postTx localTxSubmissionQ tx = do
        liftIO $ traceWith tr $ MsgPostSealedTx tx
        result <- liftIO $ localTxSubmissionQ `send` CmdSubmitTx (toGenTx tx)
        case result of
            SubmitSuccess -> pure ()
            SubmitFail err -> throwE $ ErrPostTxBadRequest $ T.pack (show err)

    _stakeDistribution =
        notImplemented "stakeDistribution"

--------------------------------------------------------------------------------
--
-- Network Client

-- | Type representing a network client running two mini-protocols to sync
-- from the chain and, submit transactions.
type NetworkClient m = OuroborosApplication
    'InitiatorApp
        -- Initiator ~ Client (as opposed to Responder / Server)
    ByteString
        -- Concrete representation for bytes string
    m
        -- Underlying monad we run in
    Void
        -- Return type of a network client. Void indicates that the client
        -- never exits.
    Void
        -- Irrelevant for 'InitiatorApplication'. Return type of 'Responder'
        -- application.

-- | Construct a network client with the given communication channel, for the
-- purposes of syncing blocks to a single wallet.
mkWalletClient
    :: (MonadThrow m, MonadST m, MonadTimer m, MonadAsync m)
    => W.GenesisParameters
        -- ^ Static blockchain parameters
    -> TQueue m (ChainSyncCmd ShelleyBlock m)
        -- ^ Communication channel with the ChainSync client
    -> m (NetworkClient m)
mkWalletClient bp chainSyncQ = do
    stash <- atomically newTQueue
    pure $ nodeToClientProtocols NodeToClientProtocols
        { localChainSyncProtocol =
            let
                fromTip' =
                    fromTip getGenesisBlockHash getEpochLength
            in
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPipelinedPeer nullTracer codec channel
                $ chainSyncClientPeerPipelined
                $ chainSyncWithBlocks fromTip' chainSyncQ stash

        , localTxSubmissionProtocol =
            doNothingProtocol

        , localStateQueryProtocol =
            doNothingProtocol
        }
        NodeToClientV_2
  where
    W.GenesisParameters
        { getEpochLength
        , getGenesisBlockHash
        } = bp

    codec = cChainSyncCodec codecs

codecs :: MonadST m => ClientCodecs ShelleyBlock m
codecs = clientCodecs ShelleyCodecConfig ShelleyNodeToClientVersion1

serialisedCodecs :: MonadST m => DefaultCodecs ShelleyBlock m
serialisedCodecs = defaultCodecs ShelleyCodecConfig ShelleyNodeToClientVersion1

-- | Construct a network client with the given communication channel, for the
-- purpose of:
--
--  * Submitting transactions
--  * Tracking the node tip
--  * Tracking the latest protocol parameters state.
mkTipSyncClient
    :: forall m. (MonadThrow m, MonadST m, MonadTimer m)
    => Tracer m NetworkLayerLog
        -- ^ Base trace for underlying protocols
    -> W.NetworkParameters
        -- ^ Initial blockchain parameters
    -> TQueue m (LocalTxSubmissionCmd (GenTx ShelleyBlock) (OC.ApplyTxError TPraosStandardCrypto) m)
        -- ^ Communication channel with the LocalTxSubmission client
    -> (Tip ShelleyBlock -> m ())
        -- ^ Notifier callback for when tip changes
    -> (W.TxParameters -> m ())
        -- ^ Notifier callback for when parameters for tip change.
    -> m (NetworkClient m)
mkTipSyncClient tr gbp localTxSubmissionQ onTipUpdate onTxParamsUpdate = do
    localStateQueryQ <- atomically newTQueue

    (onTxParamsUpdate' :: W.TxParameters -> m ()) <- debounce $ \txParams -> do
        traceWith tr $ MsgTxParameters txParams
        onTxParamsUpdate txParams

    let
        queryLocalState :: Point ShelleyBlock -> m ()
        queryLocalState pt = do
            st <- localStateQueryQ `send` CmdQueryLocalState pt OC.GetCurrentPParams
            handleLocalState st

        handleLocalState = \case
            Left (e :: AcquireFailure) ->
                traceWith tr $ MsgLocalStateQueryError $ show e
            Right ls ->
                onTxParamsUpdate' $ fromPParams ls

        W.GenesisParameters
             { getGenesisBlockHash
             , getEpochLength
             } = W.genesisParameters gbp

    onTipUpdate' <- debounce @(Tip ShelleyBlock) @m $ \tip' -> do
        let tip = castTip tip'
        traceWith tr $ MsgNodeTip $ fromTip getGenesisBlockHash getEpochLength tip
        onTipUpdate tip
        queryLocalState (getTipPoint tip)

    pure $ nodeToClientProtocols NodeToClientProtocols
        { localChainSyncProtocol =
            let
                codec = cChainSyncCodec $ serialisedCodecs @m
            in
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPeer nullTracer codec channel
                $ chainSyncClientPeer
                $ chainSyncFollowTip onTipUpdate'

        , localTxSubmissionProtocol =
            let
                tr' = contramap MsgTxSubmission tr
                codec = cTxSubmissionCodec serialisedCodecs
            in
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPeer tr' codec channel
                $ localTxSubmissionClientPeer
                $ localTxSubmission localTxSubmissionQ

        , localStateQueryProtocol =
            let
                tr' = contramap MsgLocalStateQuery tr
                codec = cStateQueryCodec serialisedCodecs
            in
            InitiatorProtocolOnly $ MuxPeerRaw
                $ \channel -> runPeer tr' codec channel
                $ localStateQueryClientPeer
                $ localStateQuery localStateQueryQ
        }
        NodeToClientV_2


-- | Return a function to run an action only if its single parameter has changed
-- since the previous time it was called.
debounce :: (Eq a, MonadSTM m) => (a -> m ()) -> m (a -> m ())
debounce action = do
    mvar <- newTMVarM Nothing
    pure $ \cur -> do
        prev <- atomically $ takeTMVar mvar
        unless (Just cur == prev) $ action cur
        atomically $ putTMVar mvar (Just cur)

-- | A protocol client that will never leave the initial state.
doNothingProtocol
    :: MonadTimer m => RunMiniProtocol 'InitiatorApp ByteString m a Void
doNothingProtocol =
    InitiatorProtocolOnly $ MuxPeerRaw $
    const $ forever $ threadDelay 1e6

-- Connect a client to a network, see `mkWalletClient` to construct a network
-- client interface.
--
-- >>> connectClient (mkWalletClient tr bp queue) mainnetVersionData addrInfo
connectClient
    :: Tracer IO NetworkLayerLog
    -> [RetryStatus -> Handler IO Bool]
    -> (ConnectionId LocalAddress -> NetworkClient IO)
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
    | MsgTxSubmission (TraceSendRecv (LocalTxSubmission (GenTx ShelleyBlock) (OC.ApplyTxError TPraosStandardCrypto)))
    | MsgLocalStateQuery (TraceSendRecv (LocalStateQuery ShelleyBlock (Query ShelleyBlock)))
    | MsgHandshakeTracer (WithMuxBearer (ConnectionId LocalAddress) HandshakeTrace)
    | MsgFindIntersection [W.BlockHeader]
    | MsgIntersectionFound (W.Hash "BlockHeader")
    | MsgFindIntersectionTimeout
    | MsgPostSealedTx W.SealedTx
    | MsgNodeTip W.BlockHeader
    | MsgTxParameters W.TxParameters
    | MsgLocalStateQueryError String

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
        MsgTxParameters params -> T.unwords
            [ "TxParams for tip are:"
            , pretty params
            ]
        MsgLocalStateQueryError e -> T.unwords
            [ "Error when querying local state parameters:"
            , T.pack e
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
        MsgTxParameters{}          -> Info
        MsgLocalStateQueryError{}  -> Error
