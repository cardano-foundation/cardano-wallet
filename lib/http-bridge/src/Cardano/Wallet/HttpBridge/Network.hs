{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- This module contains the necessary logic to talk to implement the network
-- layer using the cardano-http-bridge as a chain producer.

module Cardano.Wallet.HttpBridge.Network
    ( HttpBridgeLayer(..)
    , mkNetworkLayer
    , newNetworkLayer
    , mkHttpBridgeLayer

    -- * Launching
    , withNetworkLayer
    , HttpBridgeBackend(..)
    , HttpBridgeConfig(..)

    -- * Errors
    , ErrStartup (..)
    ) where

import Prelude

import Cardano.BM.Trace
    ( Trace, logDebug, logInfo )
import Cardano.Launcher
    ( Command (Command)
    , ProcessHasExited
    , StdStream (..)
    , transformLauncherTrace
    , withBackendProcess
    )
import Cardano.Wallet.HttpBridge.Api
    ( ApiT (..)
    , EpochIndex (..)
    , GetBlockByHash
    , GetEpochById
    , GetTipBlockHeader
    , NetworkName (..)
    , PostSignedTx
    , api
    )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, block0, byronBlockchainParameters )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Local (..), Network (..) )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx )
import Cardano.Wallet.Network
    ( Cursor
    , ErrGetBlock (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    , NextBlocksResult (..)
    , defaultRetryPolicy
    )
import Cardano.Wallet.Network.Ports
    ( PortNumber, getRandomPort, waitForPort )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), SlotId (..), TxWitness )
import Control.Arrow
    ( left )
import Control.Exception
    ( Exception )
import Control.Monad
    ( void )
import Control.Monad.Catch
    ( throwM )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), mapExceptT, runExceptT, withExceptT )
import Crypto.Hash
    ( HashAlgorithm, digestFromByteString )
import Data.ByteArray
    ( convert )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..), showT )
import Data.Word
    ( Word64 )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.HTTP.Types.Status
    ( status400, status404, status500 )
import Servant.API
    ( (:<|>) (..) )
import Servant.Client
    ( BaseUrl (..), ClientM, Scheme (Http), client, mkClientEnv, runClientM )
import Servant.Client.Core
    ( Response, ServantError (..), responseBody, responseStatusCode )
import Servant.Extra.ContentTypes
    ( WithHash (..) )
import Servant.Links
    ( Link, safeLink )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Servant.Extra.ContentTypes as Api

data HttpBridgeBackend
    = UseRunning PortNumber
    | Launch HttpBridgeConfig
    deriving (Show, Eq)

data HttpBridgeConfig = HttpBridgeConfig
    { _networkName :: Either Local Network
    , _stateDir :: Maybe FilePath
    , _restApiPort :: Maybe PortNumber
    , _extraArgs :: [String]
    , _outputStream :: StdStream
    } deriving (Show, Eq)

withNetworkLayer
    :: forall n a t. (KnownNetwork n, t ~ HttpBridge n)
    => Trace IO Text
    -> HttpBridgeBackend
    -> (Either ErrStartup (PortNumber, NetworkLayer IO t (Block Tx)) -> IO a)
    -> IO a
withNetworkLayer tr (UseRunning port) cb = do
    nl <- newNetworkLayer @n port
    logInfo tr $ "Using cardano-http-bridge port " <> T.pack (show port)
    cb (Right (port, nl))
withNetworkLayer tr (Launch cfg) cb = do
    res <- withHttpBridge tr cfg $ \port -> withNetworkLayer @n tr (UseRunning port) cb
    either (cb . Left) pure res

withHttpBridge :: Trace IO Text -> HttpBridgeConfig -> (PortNumber -> IO a) -> IO (Either ErrStartup a)
withHttpBridge tr (HttpBridgeConfig network stateDir mPort extraArgs output) cb = do
    nodePort <- maybe getRandomPort pure mPort
    let args = mconcat
                [ [ "start" ]
                , [ "--port", show nodePort ]
                , [ "--template", case network of
                        Left Local -> "local"
                        Right n -> showT n
                  ]
                , maybe [] (\d -> ["--networks-dir", d]) stateDir
                , extraArgs
                ]
    let cmd = Command "cardano-http-bridge" args (return ()) output
    let tr' = transformLauncherTrace tr
    res <- withBackendProcess tr' cmd $ do
        logDebug tr $ "Waiting for cardano-http-bridge port " <> T.pack (show nodePort)
        waitForPort defaultRetryPolicy nodePort >>= \case
            True -> Right <$> cb nodePort
            False -> pure $ Left ErrStartupNodeNotListening
    pure $ either (Left . ErrStartupCommandExited) id res

-- | Constructs a network layer with the given cardano-http-bridge API.
mkNetworkLayer
    :: forall n m t. (Monad m, KnownNetwork (n :: Network), t ~ HttpBridge n)
    => HttpBridgeLayer m
    -> NetworkLayer m t (Block Tx)
mkNetworkLayer httpBridge = NetworkLayer
    { nextBlocks = \(Cursor (BlockHeader sl _ _)) -> do
        nodeTip <- lift $ runExceptT (snd <$> getNetworkTip httpBridge)
        withExceptT ErrGetBlockNetworkUnreachable $
            nextBlocksResult nodeTip <$> rbNextBlocks httpBridge sl
    , initCursor =
        Cursor
    , cursorSlotId = \(Cursor (BlockHeader sl _ _)) ->
        sl
    , networkTip =
        snd <$> getNetworkTip httpBridge
    , postTx =
        postSignedTx httpBridge
    , staticBlockchainParameters =
        (block0, byronBlockchainParameters @n)
    }

-- | Creates a cardano-http-bridge 'NetworkLayer' using the given connection
-- settings.
newNetworkLayer
    :: forall n t. (KnownNetwork (n :: Network), t ~ HttpBridge n)
    => PortNumber
    -> IO (NetworkLayer IO t (Block Tx))
newNetworkLayer port = mkNetworkLayer @n <$> newHttpBridgeLayer @n port

-- | Retrieve a chunk of blocks from cardano-http-bridge.
--
-- It will either return:
--
-- - an epoch pack's worth of blocks (those after the given starting slot); or
-- - all of the unstable blocks after (exclusively) the starting slot, if any.
--
-- Note that, starting from `slotMinBound`, we'll never get the first block of
-- the chain through this method.
rbNextBlocks
    :: Monad m
    => HttpBridgeLayer m -- ^ http-bridge API
    -> SlotId -- ^ Starting point
    -> ExceptT ErrNetworkUnavailable m [Block Tx]
rbNextBlocks bridge start = maybeTip (getNetworkTip bridge) >>= \case
    Just (tipHash, tipHdr) -> do
        epochBlocks <-
            if slotNumber start >= 21599
            then nextStableEpoch $ epochNumber start + 1
            else nextStableEpoch $ epochNumber start
        additionalBlocks <-
            if null epochBlocks
            then unstableBlocks tipHash (slotId tipHdr)
            else pure []
        pure (epochBlocks ++ additionalBlocks)
    Nothing -> pure []
  where
    nextStableEpoch ix = do
        epochBlocks <- getEpoch bridge ix
        pure $ filter (blockIsAfter start) epochBlocks

    -- Returns true iff. the block is from a later slot than the specified slot.
    blockIsAfter :: SlotId -> Block Tx -> Bool
    blockIsAfter s = (> s) . slotId . header

    -- Grab the remaining blocks which aren't packed in epoch files,
    -- starting from the tip.
    unstableBlocks tipHash tip
        | start <= tip = fetchBlocksFromTip bridge start tipHash
        | otherwise    = pure []

    maybeTip = mapExceptT $ fmap $ \case
        Left (ErrNetworkTipNetworkUnreachable e) -> Left e
        Left ErrNetworkTipNotFound -> Right Nothing
        Right tip -> Right (Just tip)

-- Fetch blocks which are not in epoch pack files.
fetchBlocksFromTip
    :: Monad m
    => HttpBridgeLayer m
    -> SlotId
    -> Hash "BlockHeader"
    -> ExceptT ErrNetworkUnavailable m [Block Tx]
fetchBlocksFromTip bridge start tipHash =
    reverse <$> workBackwards tipHash
  where
    workBackwards headerHash = do
        block <- getBlock bridge headerHash
        if start >= slotId (header block) then
            return []
        else do
            blocks <- workBackwards (prevBlockHash (header block))
            pure (block:blocks)

data instance Cursor (HttpBridge n) = Cursor BlockHeader

nextBlocksResult
    :: Either ErrNetworkTip BlockHeader
    -> [Block Tx]
    -> NextBlocksResult (HttpBridge n) (Block Tx)
nextBlocksResult _ [] = AwaitReply
nextBlocksResult nodeTip bs = RollForward (Cursor tip) nodeTip' bs
  where
    tip = header $ last bs
    nodeTip' = either (const tip) id nodeTip

{-------------------------------------------------------------------------------
                            HTTP-Bridge Client
-------------------------------------------------------------------------------}

-- | Endpoints of the cardano-http-bridge API.
data HttpBridgeLayer m = HttpBridgeLayer
    { getBlock
        :: Hash "BlockHeader" -> ExceptT ErrNetworkUnavailable m (Block Tx)
    , getEpoch
        :: Word64 -> ExceptT ErrNetworkUnavailable m [(Block Tx)]
    , getNetworkTip
        :: ExceptT ErrNetworkTip m (Hash "BlockHeader", BlockHeader)
    , postSignedTx
        :: (Tx, [TxWitness]) -> ExceptT ErrPostTx m ()
    }

-- | Exception thrown when an unexpected failure occurs. It's basically a
-- 'ServantError' enriched with some information about the request that was
-- made.
data ErrUnexpectedNetworkFailure
    = ErrUnexpectedNetworkFailure Link ServantError
    deriving (Show)

instance Exception ErrUnexpectedNetworkFailure

-- | Construct a new network layer
mkHttpBridgeLayer
    :: Manager -> BaseUrl -> NetworkName -> HttpBridgeLayer IO
mkHttpBridgeLayer mgr baseUrl networkName = HttpBridgeLayer
    { getBlock = \hash -> ExceptT $ do
        hash' <- hashToApi' hash
        let ctx = safeLink api (Proxy @GetBlockByHash) networkName hash'
        run (getApiT <$> cGetBlock networkName hash') >>= defaultHandler ctx

    , getEpoch = \ep -> ExceptT $ do
        let ep' = EpochIndex ep
        run (map getApiT <$> cGetEpoch networkName ep') >>= \case
            Left (FailureResponse e) | responseStatusCode e == status404 ->
                return $ Right []
            x -> do
                let ctx = safeLink api (Proxy @GetEpochById) networkName ep'
                defaultHandler ctx x

    , getNetworkTip = ExceptT $ do
        let e0 = "Couldn't find Tip"
        run (blockHeaderHash <$> cGetNetworkTip networkName) >>= \case
            Left (FailureResponse e) | responseStatusCode e == status404 ->
                return $ Left ErrNetworkTipNotFound
            Left (FailureResponse e)
                | responseStatusCode e == status500 && responseBody e == e0 ->
                -- Sometimes during initialization, the bridge fails to fetch
                -- the tip block from the storage. This looks like a race
                -- condition but, the real cause is unclear.
                return $ Left ErrNetworkTipNotFound
            x -> do
                let ctx = safeLink api (Proxy @GetTipBlockHeader) networkName
                left ErrNetworkTipNetworkUnreachable <$> defaultHandler ctx x

    , postSignedTx = \tx -> void $ ExceptT $ do
        let e0 = "Failed to send to peers: Blockchain protocol error"
        run (cPostSignedTx networkName (ApiT tx)) >>= \case
            Left (FailureResponse e)
                | responseStatusCode e == status400 -> do
                    let msg = T.decodeUtf8 $ BL.toStrict $ responseBody e
                    return $ Left $ ErrPostTxBadRequest msg
            Left (FailureResponse e)
                | responseStatusCode e == status500 && responseBody e == e0 -> do
                    let msg = T.decodeUtf8 $ BL.toStrict $ responseBody e
                    return $ Left $ ErrPostTxProtocolFailure msg
            x -> do
                let ctx = safeLink api (Proxy @PostSignedTx) networkName
                left ErrPostTxNetworkUnreachable <$> defaultHandler ctx x
    }
  where
    run :: ClientM a -> IO (Either ServantError a)
    run query = runClientM query (mkClientEnv mgr baseUrl)

    defaultHandler
        :: Link
        -> Either ServantError a
        -> IO (Either ErrNetworkUnavailable a)
    defaultHandler ctx = \case
        Right c -> return $ Right c

        -- The node has not started yet or has exited.
        -- This could be recovered from by either waiting for the node
        -- initialise, or restarting the node.
        Left (ConnectionError e) ->
            return $ Left $ ErrNetworkUnreachable e

        Left (FailureResponse r) | isInvalidNetwork r ->
            return $ Left $ ErrNetworkInvalid $ getNetworkName networkName

        -- Other errors (status code, decode failure, invalid content type
        -- headers). These are considered to be programming errors, so crash.
        Left e -> do
            throwM (ErrUnexpectedNetworkFailure ctx e)

    cGetBlock
        :<|> cGetEpoch
        :<|> cGetNetworkTip
        :<|> cPostSignedTx
        = client api

    isInvalidNetwork :: Response -> Bool
    isInvalidNetwork r = responseStatusCode r == status400 &&
        "Invalid network" `BL.isPrefixOf` responseBody r

blockHeaderHash
    :: WithHash algorithm (ApiT BlockHeader)
    -> (Hash "BlockHeader", BlockHeader)
blockHeaderHash (WithHash h (ApiT bh)) =
    (Hash (convert h), bh)

hashToApi :: HashAlgorithm a => Hash h -> Maybe (Api.Hash a b)
hashToApi (Hash h) = Api.Hash <$> digestFromByteString h

-- | Converts a Hash to the Digest type that the Api module requires.
hashToApi'
    :: (MonadFail m, HashAlgorithm algorithm)
    => Hash a
    -> m (Api.Hash algorithm b)
hashToApi' h = case hashToApi h of
    Just h' -> pure h'
    Nothing -> fail "hashToApi: Digest was of the wrong length"

-- | Creates a cardano-http-bridge API with the given connection settings.
newHttpBridgeLayer :: forall n. KnownNetwork n => PortNumber -> IO (HttpBridgeLayer IO)
newHttpBridgeLayer port = do
    mgr <- newManager defaultManagerSettings
    let baseUrl = localhostBaseUrl port
    pure $ mkHttpBridgeLayer mgr baseUrl (NetworkName $ toText $ networkVal @n)

localhostBaseUrl :: PortNumber -> BaseUrl
localhostBaseUrl port = BaseUrl Http "localhost" (fromIntegral port) ""

data ErrStartup
    = ErrStartupNodeNotListening
    | ErrStartupCommandExited ProcessHasExited
    deriving (Show, Eq)

instance Exception ErrStartup
