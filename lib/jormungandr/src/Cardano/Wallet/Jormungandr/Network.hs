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
    , newNetworkLayer

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

import Cardano.BM.Trace
    ( Trace )
import Cardano.CLI
    ( Port (..), waitForService )
import Cardano.Launcher
    ( Command (..), ProcessHasExited, StdStream (..), withBackendProcess )
import Cardano.Wallet.Jormungandr.Api.Client
    ( BaseUrl (..)
    , ErrGetAccountState (..)
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
    , getAccountState
    , getBlockHeader
    , getBlocks
    , getInitialBlockchainParameters
    , getStakeDistribution
    , getTipId
    , mkJormungandrClient
    , newManager
    , postMessage
    )
import Cardano.Wallet.Jormungandr.Api.Types
    ( AccountState (currentBalance)
    , ApiStakeDistribution (pools)
    , ApiT (..)
    , StakeApiResponse (epoch, stake)
    )
import Cardano.Wallet.Jormungandr.Binary
    ( runGetOrFail )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, localhostBaseUrl )
import Cardano.Wallet.Logging
    ( transformTextTrace )
import Cardano.Wallet.Network
    ( Cursor
    , ErrGetAccountBalance (..)
    , NetworkLayer (..)
    , NextBlocksResult (..)
    , defaultRetryPolicy
    , waitForNetwork
    )
import Cardano.Wallet.Network.BlockHeaders
    ( BlockHeaders (..)
    , appendBlockHeaders
    , blockHeadersAtGenesis
    , blockHeadersBase
    , blockHeadersTip
    , dropAfterSlotId
    , emptyBlockHeaders
    , greatestCommonBlockHeader
    , updateUnstableBlocks
    )
import Cardano.Wallet.Network.Ports
    ( PortNumber, getRandomPort, waitForPort )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , BlockchainParameters (..)
    , ChimericAccount (..)
    , EpochNo
    , Hash (..)
    , PoolId
    , SlotId (..)
    )
import Control.Concurrent.MVar.Lifted
    ( MVar, modifyMVar, newMVar, readMVar )
import Control.Exception
    ( Exception )
import Control.Monad
    ( void )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE, withExceptT )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.Coerce
    ( coerce )
import Data.Map.Strict
    ( Map )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Word
    ( Word32, Word64 )
import System.FilePath
    ( (</>) )

import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map

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
    , _genesisBlock :: Either (Hash "Genesis") FilePath
    , _restApiPort :: Maybe PortNumber
    , _outputStream :: StdStream
    , _extraArgs :: [String]
    } deriving (Show, Eq)

-- | Starts the network layer and runs the given action with a
-- 'NetworkLayer'. The caller is responsible for handling errors which may have
-- occurred while starting the Node.
withNetworkLayer
    :: forall a t. (t ~ Jormungandr)
    => Trace IO Text
    -- ^ Logging
    -> JormungandrBackend
    -- ^ How Jörmungandr is started.
    -> (Either ErrStartup (JormungandrConnParams, NetworkLayer IO t J.Block) -> IO a)
    -- ^ The action to run. It will be passed the connection parameters used,
    -- and a network layer if startup was successful.
    -> IO a
withNetworkLayer tr (UseRunning cp) action = withNetworkLayerConn tr cp action
withNetworkLayer tr (Launch lj) action = withNetworkLayerLaunch tr lj action

withNetworkLayerLaunch
    :: forall a t. (t ~ Jormungandr)
    => Trace IO Text
    -- ^ Logging of node startup.
    -> JormungandrConfig
    -- ^ Configuration for starting Jörmungandr.
    -> (Either ErrStartup (JormungandrConnParams, NetworkLayer IO t J.Block) -> IO a)
    -- ^ The action to run. It will be passed the connection parameters used,
    -- and a network layer if startup was successful.
    -> IO a
withNetworkLayerLaunch tr lj action = do
    res <- withJormungandr tr lj $ \cp -> withNetworkLayerConn tr cp action
    either (action . Left) pure res

withNetworkLayerConn
    :: forall a t. (t ~ Jormungandr)
    => Trace IO Text
    -- ^ Logging of network layer startup
    -> JormungandrConnParams
    -- ^ Parameters for connecting to Jörmungandr node which is already running.
    -> (Either ErrStartup (JormungandrConnParams, NetworkLayer IO t J.Block) -> IO a)
    -- ^ Action to run with the network layer.
    -> IO a
withNetworkLayerConn tr cp@(JormungandrConnParams block0H baseUrl) action =
    runExceptT go >>= action . fmap (cp,)
  where
    go = withExceptT ErrStartupGetBlockchainParameters new
    new = newNetworkLayer tr baseUrl block0H

-- | Creates a new 'NetworkLayer' connecting to an underlying 'Jormungandr'
-- backend target.
newNetworkLayer
    :: forall t. (t ~ Jormungandr)
    => Trace IO Text
    -> BaseUrl
    -> Hash "Genesis"
    -> ExceptT ErrGetBlockchainParams IO (NetworkLayer IO t J.Block)
newNetworkLayer tr baseUrl block0H = do
    mgr <- liftIO $ newManager defaultManagerSettings
    st <- newMVar emptyBlockHeaders
    let jor = mkJormungandrClient mgr baseUrl
    liftIO $ waitForService "Jörmungandr" tr (Port $ baseUrlPort baseUrl) $
        waitForNetwork (void $ getTipId jor) defaultRetryPolicy
    (block0, bp) <- getInitialBlockchainParameters jor (coerce block0H)
    return (mkRawNetworkLayer (block0, bp) 1000 st jor)

-- | Wrap a Jormungandr client into a 'NetworkLayer' common interface.
--
-- This version provides the full, raw blocks from
-- "Cardano.Wallet.Jormungandr.Binary".
mkRawNetworkLayer
    :: forall m t block.
        ( MonadBaseControl IO m
        , t ~ Jormungandr
        , block ~ J.Block
        )
    => (block, BlockchainParameters)
    -> Word
        -- ^ Batch size when fetching blocks from Jörmungandr
    -> MVar BlockHeaders
    -> JormungandrClient m
    -> NetworkLayer m t block
mkRawNetworkLayer (block0, bp) batchSize st j = NetworkLayer
    { networkTip =
        _networkTip

    , findIntersection =
        _findIntersection

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

    , stakeDistribution =
        _stakeDistribution
    , getAccountBalance =
        _getAccountBalance
    }
  where
    -- security parameter, the maximum number of unstable blocks
    k :: Quantity "block" Word32
    k = getEpochStability bp

    genesis :: Hash "Genesis"
    genesis = getGenesisBlockHash bp

    _networkTip :: ExceptT ErrNetworkTip m BlockHeader
    _networkTip = modifyMVar st $ \bs -> do
        let tip = withExceptT liftE $ getTipId j
        bs' <- withExceptT liftE $ updateUnstableBlocks k tip (getBlockHeader j) bs
        ExceptT . pure $ case blockHeadersTip bs' of
            Just t -> Right (bs', t)
            Nothing -> Left ErrNetworkTipNotFound

    _findIntersection :: Cursor t -> m (Maybe BlockHeader)
    _findIntersection (Cursor localChain) = do
        nodeChain <- readMVar st
        pure (greatestCommonBlockHeader nodeChain localChain)

    _initCursor :: [BlockHeader] -> Cursor t
    _initCursor bhs =
        Cursor $ appendBlockHeaders k emptyBlockHeaders bhs

    _cursorSlotId :: Cursor t -> SlotId
    _cursorSlotId (Cursor unstable) =
        maybe (SlotId 0 0) slotId (blockHeadersTip unstable)

    _stakeDistribution
        :: ExceptT ErrNetworkUnavailable m
            ( EpochNo
            , Map PoolId (Quantity "lovelace" Word64)
            )
    _stakeDistribution = do
        r <- getStakeDistribution j
        let epochNo = getApiT . epoch $ r
        let distr = map (\(ApiT a, ApiT b) -> (a,b)) . pools . stake $ r
        return (epochNo, Map.fromList distr)

    _getAccountBalance (ChimericAccount acc) =
        liftE1
        . fmap currentBalance
        $ getAccountState j (Hash acc)
      where
        liftE1 = withExceptT $ \case
            ErrGetAccountStateNetworkUnreachable err ->
                ErrGetAccountBalanceNetworkUnreachable err
            ErrGetAccountStateAccountNotFound (Hash acc') ->
                ErrGetAccountBalanceAccountNotFound (ChimericAccount acc')

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
                        let Just nodeTip = blockHeadersTip unstable
                        let start = maybe
                                (coerce genesis)
                                headerHash
                                (blockHeadersTip localChain)
                        lift (runExceptT $ getBlocks j batchSize start) >>= \case
                            Right blks ->
                                pure (tryRollForward nodeTip blks)
                            Left (ErrGetBlockNotFound _) ->
                                pure (recover localChain)
                            Left e ->
                                throwE e

                    Backward point ->
                        pure $ rollBackward point

                    Restart ->
                        pure (recover localChain)

            Left ErrNetworkTipNotFound ->
                pure AwaitReply

            Left (ErrNetworkTipNetworkUnreachable e) ->
                throwE (ErrGetBlockNetworkUnreachable e)
      where
        tryRollForward
            :: BlockHeader
            -> [block]
            -> NextBlocksResult t block
        tryRollForward tip = \case
            -- No more blocks to apply, no need to roll forward
            [] -> AwaitReply

            -- There's some time between the moment we fetch blocks and the
            -- moment we have decided to go forward; therefore there's a
            -- concurrency issue lurking around where we could have fetched
            -- blocks that are not a valid continuation of our local chain!
            -- This can happen if the node switch chains just before our local
            --  tip while we were fetching our next blocks.
            next@(b:_)
                -- If the blocks we are about to apply are a continuation of our
                -- local chain, then it's good, we can continue
                | Just (J.parentHeaderHash $ J.header b) == (headerHash <$> blockHeadersTip localChain) ->
                    RollForward (cursorForward k next cursor) tip next

                -- If we are at genesis, we apply them anyway
                | blockHeadersAtGenesis localChain ->
                    RollForward (cursorForward k next cursor) tip next

                -- We need to rollback somewhere, but we don't know where, so we
                -- try rolling back to the oldest header we know.
                | otherwise ->
                    recover localChain

        rollBackward
            :: BlockHeader
            -> NextBlocksResult t block
        rollBackward point =
            RollBackward (cursorBackward point cursor)

        recover
            :: BlockHeaders
            -> NextBlocksResult t block
        recover chain = case (blockHeadersBase chain, blockHeadersTip chain) of
            (Just baseH, Just tipH) | baseH /= tipH ->
                RollBackward (cursorBackward baseH cursor)
            _ ->
                RollBackward $ Cursor emptyBlockHeaders

{-------------------------------------------------------------------------------
                             Jormungandr Cursor
-------------------------------------------------------------------------------}


-- Use a block headers sequence as the wallet state. This can easily be
-- intersected with the global node state.
data instance Cursor Jormungandr = Cursor BlockHeaders

-- | Direction in which to move the local chain.
data Direction
    = Forward
    | Backward BlockHeader
    | Stay
    | Restart
    deriving (Show, Eq)

-- If there is intersection, then the decision is simple. Otherwise, find
-- whether the local chain is behind or ahead of the node chain.
direction
    :: forall t. (t ~ Jormungandr)
    => Cursor t
    -- ^ Local wallet unstable blocks
    -> BlockHeaders
    -- ^ Node's unstable blocks
    -> Direction
direction (Cursor local) node = case greatestCommonBlockHeader node local of
    Just intersection
        -- Local tip and node tip are the same
        | blockHeadersTip local == blockHeadersTip node -> Stay

        -- Local tip is the greatest common block
        | blockHeadersTip local == Just intersection -> Forward

        -- Common block is not the local tip
        | otherwise -> Backward intersection
    Nothing
        | blockHeadersAtGenesis local -> Forward

        -- Local tip is before the node's unstable area, we need to catch up
        | (slotId <$> blockHeadersTip local) < (slotId <$> blockHeadersBase node) -> Forward

        -- We are beyond the node tip, just resync from genesis
        | otherwise -> Restart

-- | Pushes the received blockheaders onto the local state.
cursorForward
    :: forall t block. (t ~ Jormungandr, block ~ J.Block)
    => Quantity "block" Word32
    -- ^ Epoch Stability, a.k.a 'k'
    -> [block]
    -- ^ New blocks received
    -> Cursor t
    -- ^ Current cursor / local state
    -> Cursor t
cursorForward k bs (Cursor cursor) =
    Cursor $ appendBlockHeaders k cursor $ J.convertBlockHeader . J.header <$> bs

-- | Clears local state after the rollback point.
cursorBackward
    :: forall t. (t ~ Jormungandr)
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
withJormungandr tr (JormungandrConfig stateDir block0 mPort output extraArgs) cb = do
    apiPort <- maybe getRandomPort pure mPort
    let baseUrl = localhostBaseUrl $ fromIntegral apiPort
    getGenesisBlockArg block0 >>= \case
        Right (block0H, genesisBlockArg) -> do
            let args = genesisBlockArg ++
                    [ "--rest-listen", "127.0.0.1:" <> show apiPort
                    , "--storage", stateDir </> "chain"
                    ] ++ extraArgs
            let cmd = Command "jormungandr" args (return ()) output
            let tr' = transformTextTrace tr
            res <- withBackendProcess tr' cmd $ do
                waitForPort defaultRetryPolicy apiPort >>= \case
                    True -> Right <$> cb (JormungandrConnParams block0H baseUrl)
                    False -> pure $ Left ErrStartupNodeNotListening
            pure $ either (Left . ErrStartupCommandExited) id res

        Left e -> pure $ Left e

getGenesisBlockArg
    :: Either (Hash "Genesis") FilePath
    -> IO (Either ErrStartup (Hash "Genesis", [String]))
getGenesisBlockArg (Left hash@(Hash b)) = do
    let hexHash = B8.unpack $ convertToBase Base16 b
    pure $ Right (hash, ["--genesis-block-hash", hexHash])
getGenesisBlockArg (Right file) = do
    let mkArg hash = (hash, ["--genesis-block", file])
    fmap mkArg <$> parseBlock0H file

parseBlock0H :: FilePath -> IO (Either ErrStartup (Hash "Genesis"))
parseBlock0H file = parse <$> BL.readFile file
  where
    parse bytes = case runGetOrFail J.getBlockId bytes of
        Right (_, _, block0H) -> Right (coerce block0H)
        Left _ -> Left (ErrStartupInvalidGenesisBlock file)

-- | Extract the port number from the base URL part of the connection params.
connParamsPort :: JormungandrConnParams -> Int
connParamsPort (JormungandrConnParams _ url) = baseUrlPort url

data ErrStartup
    = ErrStartupNodeNotListening
    | ErrStartupCommandExited ProcessHasExited
    | ErrStartupGetBlockchainParameters ErrGetBlockchainParams
    | ErrStartupInvalidGenesisBlock FilePath
    | ErrStartupInvalidGenesisHash String
    deriving (Show, Eq)

instance Exception ErrStartup
