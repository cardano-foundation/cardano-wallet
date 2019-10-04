{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module allows the wallet to retrieve blocks from a known @Jormungandr@
-- node. This is done by providing a @NetworkLayer@ with some logic building on
-- top of an underlying @JormungandrClient@ HTTP client.
--
-- It also provides facilities for starting the @jormungandr@ node backend
-- process. The Jormungandr 'NetworkLayer' implementation uses the HTTP REST API
-- of this backend.
--
module Cardano.Wallet.Jormungandr.Network
    (
    -- * Starting the network layer
      JormungandrBackend (..)
    , JormungandrConnParams (..)
    , withNetworkLayer

    -- * Launching the node backend
    , JormungandrConfig (..)
    , withJormungandr
    , connParamsPort

    -- * Errors
    , ErrGetBlock (..)
    , ErrGetBlockchainParams (..)
    , ErrGetDescendants (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , ErrStartup (..)
    , ErrUnexpectedNetworkFailure (..)

    -- * Internal constructors
    , mkRawNetworkLayer
    , BaseUrl (..)
    , Scheme (..)
    ) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, logInfo )
import Cardano.Launcher
    ( Command (..)
    , ProcessHasExited
    , StdStream (..)
    , transformLauncherTrace
    , withBackendProcess
    )
import Cardano.Wallet.Jormungandr.Api.Client
    ( BaseUrl (..)
    , ErrGetBlock (..)
    , ErrGetBlockchainParams (..)
    , ErrGetDescendants (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , ErrUnexpectedNetworkFailure (..)
    , JormungandrClient
    , LiftError (..)
    , Scheme (..)
    , defaultManagerSettings
    , getBlockHeader
    , getBlocks
    , getInitialBlockchainParameters
    , getTipId
    , mkJormungandrClient
    , newManager
    , postMessage
    )
import Cardano.Wallet.Jormungandr.Binary
    ( convertBlock, runGetOrFail )
import Cardano.Wallet.Jormungandr.BlockHeaders
    ( BlockHeaders (..)
    , appendBlockHeaders
    , blockHeadersTip
    , dropAfterSlotId
    , emptyBlockHeaders
    , greatestCommonBlockHeader
    , updateUnstableBlocks
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, genConfigFile, localhostBaseUrl )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx )
import Cardano.Wallet.Network
    ( Cursor, NetworkLayer (..), NextBlocksResult (..), defaultRetryPolicy )
import Cardano.Wallet.Network.Ports
    ( PortNumber, getRandomPort, waitForPort )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), SlotId (..) )
import Control.Concurrent.MVar.Lifted
    ( MVar, modifyMVar, newMVar, readMVar )
import Control.Exception
    ( Exception, bracket )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE, withExceptT )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32 )
import System.Directory
    ( removeFile )
import System.FilePath
    ( (</>) )

import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Char as C
import qualified Data.Text as T
import qualified Data.Yaml as Yaml

-- | Whether to start Jormungandr with the given config, or to connect to an
-- already running Jormungandr REST API using the given parameters.
data JormungandrBackend
    = UseRunning JormungandrConnParams
    | Launch JormungandrConfig
    deriving (Show, Eq)

-- | Parameters for connecting to a Jormungandr REST API.
data JormungandrConnParams = JormungandrConnParams
    { _genesisHash :: Hash "Genesis"
    , _restApi :: BaseUrl
    } deriving (Show, Eq)

-- | A subset of the Jormungandr configuration parameters, used for starting the
-- Jormungandr node backend.
data JormungandrConfig = JormungandrConfig
    { _stateDir :: FilePath
    , _genesisBlock :: FilePath
    , _secretFile :: FilePath
    , _restApiPort :: Maybe PortNumber
    , _minSeverity :: Severity
    , _outputStream :: StdStream
    } deriving (Show, Eq)

-- | Starts the network layer and runs the given action with a
-- 'NetworkLayer'. The caller is responsible for handling errors which may have
-- occurred while starting the Node.
withNetworkLayer
    :: forall n a t. (t ~ Jormungandr n)
    => Trace IO Text
    -- ^ Logging
    -> JormungandrBackend
    -- ^ How Jörmungandr is started.
    -> (Either ErrStartup (JormungandrConnParams, NetworkLayer IO t (Block Tx)) -> IO a)
    -- ^ The action to run. It will be passed the connection parameters used,
    -- and a network layer if startup was successful.
    -> IO a
withNetworkLayer _ (UseRunning cp) action = withNetworkLayerConn cp action
withNetworkLayer tr (Launch lj) action = withNetworkLayerLaunch tr lj action

withNetworkLayerLaunch
    :: forall n a t. (t ~ Jormungandr n)
    => Trace IO Text
    -- ^ Logging of node startup.
    -> JormungandrConfig
    -- ^ Configuration for starting Jörmungandr.
    -> (Either ErrStartup (JormungandrConnParams, NetworkLayer IO t (Block Tx)) -> IO a)
    -- ^ The action to run. It will be passed the connection parameters used,
    -- and a network layer if startup was successful.
    -> IO a
withNetworkLayerLaunch tr lj action = do
    res <- withJormungandr tr lj $ \cp -> withNetworkLayerConn cp action
    either (action . Left) pure res

withNetworkLayerConn
    :: forall n a t. (t ~ Jormungandr n)
    => JormungandrConnParams
    -- ^ Parameters for connecting to Jörmungandr node which is already running.
    -> (Either ErrStartup (JormungandrConnParams, NetworkLayer IO t (Block Tx)) -> IO a)
    -- ^ Action to run with the network layer.
    -> IO a
withNetworkLayerConn cp@(JormungandrConnParams block0H baseUrl) action =
    runExceptT go >>= action . fmap (cp,)
  where
    go = withExceptT ErrStartupGetBlockchainParameters new
    new = newNetworkLayer baseUrl block0H

-- | Creates a new 'NetworkLayer' connecting to an underlying 'Jormungandr'
-- backend target.
newNetworkLayer
    :: forall n t. (t ~ Jormungandr n)
    => BaseUrl
    -> Hash "Genesis"
    -> ExceptT ErrGetBlockchainParams IO (NetworkLayer IO t (Block Tx))
newNetworkLayer baseUrl block0H = do
    mgr <- liftIO $ newManager defaultManagerSettings
    st <- newMVar emptyBlockHeaders
    let jor = mkJormungandrClient mgr baseUrl
    g0 <- getInitialBlockchainParameters jor (coerce block0H)
    return (convertBlock <$> mkRawNetworkLayer g0 st jor)

-- | Wrap a Jormungandr client into a 'NetworkLayer' common interface.
--
-- This version provides the full, raw blocks from
-- "Cardano.Wallet.Jormungandr.Binary".
mkRawNetworkLayer
    :: forall m n t block.
        ( MonadBaseControl IO m
        , t ~ Jormungandr n
        , block ~ J.Block
        )
    => (block, BlockchainParameters)
    -> MVar BlockHeaders
    -> JormungandrClient m
    -> NetworkLayer m t block
mkRawNetworkLayer (block0, bp) st j = NetworkLayer
    { networkTip =
        _networkTip

    , nextBlocks =
        _nextBlocks

    , initCursor =
        _initCursor

    , cursorSlotId =
        _cursorSlotId

    , postTx =
        postMessage j

    , staticBlockchainParameters =
        (block0, bp)
    }
  where
    -- security parameter, the maximum number of unstable blocks
    k :: Quantity "block" Word32
    k = getEpochStability bp

    _networkTip :: ExceptT ErrNetworkTip m BlockHeader
    _networkTip = modifyMVar st $ \bs -> do
        let tip = withExceptT liftE $ getTipId j
        bs' <- withExceptT liftE $ updateUnstableBlocks k tip (getBlockHeader j) bs
        ExceptT . pure $ case blockHeadersTip bs' of
            Just t -> Right (bs', t)
            Nothing -> Left ErrNetworkTipNotFound

    _initCursor :: BlockHeader -> Cursor t
    _initCursor bh = Cursor $
        -- FIXME The empty hash looks weird? Why not creating the BlockHeaders
        -- from the constructor?
        --
        -- TODO:
        -- Change blockheights to be consistent everywhere.
        appendBlockHeaders k emptyBlockHeaders
            [ ( Hash ""
              , bh
              , Quantity $ fromIntegral $ getQuantity $ blockHeight bh
              )
            ]

    _cursorSlotId :: Cursor t -> SlotId
    _cursorSlotId (Cursor unstable) =
        -- FIXME Why the default here?
        maybe (SlotId 0 0) slotId $ blockHeadersTip unstable

    _nextBlocks
        :: Cursor t
        -> ExceptT ErrGetBlock m (NextBlocksResult t block)
    _nextBlocks cursor@(Cursor localChain) = do
        lift (runExceptT _networkTip) >>= \case
            Right _ -> do
                unstable <- readMVar st
                case direction cursor unstable of
                    Stay ->
                        pure AwaitReply

                    Forward -> do
                        let Just localTip = blockHeadersTip localChain
                        let Just nodeTip  = blockHeadersTip unstable
                        rollForward nodeTip <$> getBlocks j localTip

                    Backward point ->
                        pure $ rollBackward point

            Left ErrNetworkTipNotFound ->
                pure AwaitReply

            Left (ErrNetworkTipNetworkUnreachable e) ->
                throwE (ErrGetBlockNetworkUnreachable e)
      where
        rollForward
            :: BlockHeader
            -> [(Hash "BlockHeader", block)]
            -> NextBlocksResult t block
        rollForward tip bs =
            RollForward (cursorForward k bs cursor) tip (snd <$> bs)

        rollBackward
            :: BlockHeader
            -> NextBlocksResult t block
        rollBackward point =
            RollBackward (cursorBackward point cursor)

{-------------------------------------------------------------------------------
                             Jormungandr Cursor
-------------------------------------------------------------------------------}


-- Use a block headers sequence as the wallet state. This can easily be
-- intersected with the global node state.
data instance Cursor (Jormungandr n) = Cursor BlockHeaders

-- | Direction in which to move the local chain.
data Direction
    = Forward
    | Backward BlockHeader
    | Stay
    deriving (Show, Eq)

-- If there is intersection, then the decision is simple. Otherwise, find
-- whether the local chain is behind or ahead of the node chain.
direction
    :: forall n t. (t ~ Jormungandr n)
    => Cursor t
    -- ^ Local wallet unstable blocks
    -> BlockHeaders
    -- ^ Node's unstable blocks
    -> Direction
direction (Cursor lbs) ubs = case greatestCommonBlockHeader ubs lbs of
    Just bh
        -- Local tip is the greatest common block
        | Just bh == blockHeadersTip lbs -> Forward
        -- Common block is not the local tip
        | otherwise -> Backward bh
    Nothing
        -- Local tip is way back in stable blocks.
        | tipSlot lbs < tipSlot ubs -> Forward
        -- Node is behind local chain. Wait for it to catch up.
        | otherwise -> Stay
  where
    tipSlot = maybe (SlotId 0 0) slotId . blockHeadersTip

-- | Pushes the received blockheaders onto the local state.
cursorForward
    :: forall n t block. (t ~ Jormungandr n, block ~ J.Block)
    => Quantity "block" Word32
    -- ^ Epoch Stability, a.k.a 'k'
    -> [(Hash "BlockHeader", block)]
    -- ^ New blocks received
    -> Cursor t
    -- ^ Current cursor / local state
    -> Cursor t
cursorForward k bs (Cursor cursor) =
    Cursor $ appendBlockHeaders k cursor bs'
  where
    Quantity curHeight = getBlockHeight cursor
    bs' =
        [ (h, J.convertBlockHeader (J.header b), Quantity (curHeight + i))
        | (i, (h, b)) <- zip [1..] bs
        ]

-- | Clears local state after the rollback point.
cursorBackward
    :: forall n t. (t ~ Jormungandr n)
    => BlockHeader
    -> Cursor t
    -> Cursor t
cursorBackward point (Cursor cursor) =
    Cursor (dropAfterSlotId (slotId point) cursor)

{-------------------------------------------------------------------------------
                                Backend launcher
-------------------------------------------------------------------------------}

-- | Launches a Jörmungandr node backend with the given configuration
withJormungandr
    :: Trace IO Text
    -- ^ Logging
    -> JormungandrConfig
    -- ^ Launch configuration
    -> (JormungandrConnParams -> IO a)
    -- ^ Action to run while node is running.
    -> IO (Either ErrStartup a)
withJormungandr tr (JormungandrConfig stateDir block0File secretsFile mPort logSeverity output) cb =
    bracket setupConfig cleanupConfig startBackend
  where
    nodeConfigFile = stateDir </> "jormungandr-config.yaml"
    setupConfig = do
        apiPort <- maybe getRandomPort pure mPort
        p2pPort <- getRandomPort
        let baseUrl = localhostBaseUrl $ fromIntegral apiPort
        genConfigFile stateDir p2pPort baseUrl
            & Yaml.encodeFile nodeConfigFile
        logInfo tr $ mempty
            <> "Generated Jörmungandr's configuration to: "
            <> T.pack nodeConfigFile
        pure (apiPort, baseUrl)
    cleanupConfig _ = removeFile nodeConfigFile

    startBackend (apiPort, baseUrl) = parseBlock0H block0File >>= \case
        Right block0H -> do
            let args =
                    [ "--genesis-block", block0File
                    , "--config", nodeConfigFile
                    , "--secret", secretsFile
                    , "--log-level", C.toLower <$> show logSeverity
                    ]
            let cmd = Command "jormungandr" args (return ()) output
            let tr' = transformLauncherTrace tr
            res <- withBackendProcess tr' cmd $
                waitForPort defaultRetryPolicy apiPort >>= \case
                    True -> Right <$> cb (JormungandrConnParams block0H baseUrl)
                    False -> pure $ Left ErrStartupNodeNotListening
            pure $ either (Left . ErrStartupCommandExited) id res

        Left _ -> pure $ Left $ ErrStartupGenesisBlockFailed block0File

parseBlock0H :: FilePath -> IO (Either ErrStartup (Hash "Genesis"))
parseBlock0H file = parse <$> BL.readFile file
  where
    parse bytes = case runGetOrFail J.getBlockId bytes of
        Right (_, _, block0H) -> Right (coerce block0H)
        Left _ -> Left (ErrStartupGenesisBlockFailed file)

-- | Extract the port number from the base URL part of the connection params.
connParamsPort :: JormungandrConnParams -> Int
connParamsPort (JormungandrConnParams _ url) = baseUrlPort url

data ErrStartup
    = ErrStartupNodeNotListening
    | ErrStartupGetBlockchainParameters ErrGetBlockchainParams
    | ErrStartupGenesisBlockFailed FilePath
    | ErrStartupCommandExited ProcessHasExited
    deriving (Show, Eq)

instance Exception ErrStartup
