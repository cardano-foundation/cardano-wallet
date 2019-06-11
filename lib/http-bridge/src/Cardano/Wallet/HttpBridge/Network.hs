{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the necessary logic to talk to implement the network
-- layer using the cardano-http-bridge as a chain producer.

module Cardano.Wallet.HttpBridge.Network
    ( HttpBridgeLayer(..)
    , mkNetworkLayer
    , newNetworkLayer
    , mkHttpBridgeLayer
    ) where

import Prelude

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
    ( HttpBridge )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Network
    ( ErrNetworkTip (..)
    , ErrNetworkUnreachable (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), SlotId (..), Tx, TxWitness )
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
import Control.Monad.Trans.Except
    ( ExceptT (..), mapExceptT )
import Crypto.Hash
    ( HashAlgorithm, digestFromByteString )
import Data.ByteArray
    ( convert )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( ToText (..) )
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
    ( ServantError (..), responseBody, responseStatusCode )
import Servant.Extra.ContentTypes
    ( WithHash (..) )
import Servant.Links
    ( Link, safeLink )

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Servant.Extra.ContentTypes as Api

-- | Constructs a network layer with the given cardano-http-bridge API.
mkNetworkLayer :: Monad m => HttpBridgeLayer m -> NetworkLayer t m
mkNetworkLayer httpBridge = NetworkLayer
    { nextBlocks = rbNextBlocks httpBridge
    , networkTip = getNetworkTip httpBridge
    , postTx = postSignedTx httpBridge
    }

-- | Creates a cardano-http-bridge 'NetworkLayer' using the given connection
-- settings.
newNetworkLayer
    :: forall n. KnownNetwork (n :: Network)
    => Int
    -> IO (NetworkLayer (HttpBridge n) IO)
newNetworkLayer port = mkNetworkLayer <$> newHttpBridgeLayer @n port

-- | Retrieve a chunk of blocks from cardano-http-bridge.
--
-- It will either return:
--
-- - an epoch pack's worth of blocks (those after the given starting slot); or
-- - all of the unstable blocks after (exclusively) the starting slot, if any.
--
-- Note that, starting from the `SlotId 0 0`, we'll never get the first block of
-- the chain through this method.
rbNextBlocks
    :: Monad m
    => HttpBridgeLayer m -- ^ http-bridge API
    -> SlotId -- ^ Starting point
    -> ExceptT ErrNetworkUnreachable m [Block]
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

    -- Predicate returns true iff the block is from the given slot or a later
    -- one.
    blockIsAfter :: SlotId -> Block -> Bool
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
    -> ExceptT ErrNetworkUnreachable m [Block]
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

{-------------------------------------------------------------------------------
                            HTTP-Bridge Client
-------------------------------------------------------------------------------}

-- | Endpoints of the cardano-http-bridge API.
data HttpBridgeLayer m = HttpBridgeLayer
    { getBlock
        :: Hash "BlockHeader" -> ExceptT ErrNetworkUnreachable m Block
    , getEpoch
        :: Word64 -> ExceptT ErrNetworkUnreachable m [Block]
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
        run (blockHeaderHash <$> cGetNetworkTip networkName) >>= \case
            Left (FailureResponse e) | responseStatusCode e == status404 ->
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
        -> IO (Either ErrNetworkUnreachable a)
    defaultHandler ctx = \case
        Right c -> return $ Right c

        -- The node has not started yet or has exited.
        -- This could be recovered from by either waiting for the node
        -- initialise, or restarting the node.
        Left (ConnectionError e) ->
            return $ Left $ ErrNetworkUnreachable e

        -- Other errors (status code, decode failure, invalid content type
        -- headers). These are considered to be programming errors, so crash.
        Left e -> do
            throwM (ErrUnexpectedNetworkFailure ctx e)

    cGetBlock
        :<|> cGetEpoch
        :<|> cGetNetworkTip
        :<|> cPostSignedTx
        = client api

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
newHttpBridgeLayer :: forall n. KnownNetwork n => Int -> IO (HttpBridgeLayer IO)
newHttpBridgeLayer port = do
    mgr <- newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" port ""
    pure $ mkHttpBridgeLayer mgr baseUrl (NetworkName $ toText $ networkVal @n)
