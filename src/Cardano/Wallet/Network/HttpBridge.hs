{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the necessary logic to talk to implement the network
-- layer using the cardano-http-bridge as a chain producer.

module Cardano.Wallet.Network.HttpBridge
    ( HttpBridge(..)
    , mkNetworkLayer
    , newNetworkLayer
    , mkHttpBridge
    ) where

import Prelude

import Cardano.Wallet.Network
    ( ErrNetworkUnreachable (..), NetworkLayer (..) )
import Cardano.Wallet.Network.HttpBridge.Api
    ( ApiT (..), EpochIndex (..), NetworkName (..), api )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), SignedTx, SlotId (..) )
import Control.Monad
    ( void )
import Control.Monad.Catch
    ( throwM )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Crypto.Hash
    ( HashAlgorithm, digestFromByteString )
import Data.ByteArray
    ( convert )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Servant.API
    ( (:<|>) (..) )
import Servant.Client
    ( BaseUrl (..), ClientM, Scheme (Http), client, mkClientEnv, runClientM )
import Servant.Client.Core
    ( ServantError (..) )
import Servant.Extra.ContentTypes
    ( WithHash (..) )

import qualified Servant.Extra.ContentTypes as Api

-- | Constructs a network layer with the given cardano-http-bridge API.
mkNetworkLayer :: Monad m => HttpBridge m ErrNetworkUnreachable -> NetworkLayer m
mkNetworkLayer httpBridge = NetworkLayer
    { nextBlocks = rbNextBlocks httpBridge
    , networkTip = getNetworkTip httpBridge
    , postTx = postSignedTx httpBridge
    }

-- | Creates a cardano-http-bridge 'NetworkLayer' using the given connection
-- settings.
newNetworkLayer
    :: Text -> Int -> IO (NetworkLayer IO)
newNetworkLayer network port = mkNetworkLayer <$> newHttpBridge network port

-- | Retrieve a chunk of blocks from cardano-http-bridge.
--
-- It will either return:
-- - an epoch pack's worth of blocks (those after the given starting slot); or
-- - all of the unstable blocks after the starting slot, if any.
rbNextBlocks
    :: Monad m
    => HttpBridge m e -- ^ http-bridge API
    -> SlotId -- ^ Starting point
    -> ExceptT e m [Block]
rbNextBlocks network start = do
    (tipHash, tip) <- fmap slotId <$> getNetworkTip network
    epochBlocks <- lift $ nextStableEpoch (epochIndex start)
    additionalBlocks <-
        if null epochBlocks then
            unstableBlocks tipHash tip
        else if length epochBlocks < 1000 then
            lift $ nextStableEpoch (epochIndex start + 1)
        else
            pure []
    pure (epochBlocks ++ additionalBlocks)
  where
    nextStableEpoch ix = do
        epochBlocks <- runExceptT (getEpoch network ix) >>= \case
            Left _ -> pure []
            Right r -> return r
        pure $ filter (blockIsSameOrAfter start) epochBlocks

    -- Predicate returns true iff the block is from the given slot or a later
    -- one.
    blockIsSameOrAfter :: SlotId -> Block -> Bool
    blockIsSameOrAfter s = (>= s) . slotId . header

    -- Grab the remaining blocks which aren't packed in epoch files,
    -- starting from the tip.
    unstableBlocks tipHash tip
        | start <= tip = fetchBlocksFromTip network start tipHash
        | otherwise    = pure []

-- Fetch blocks which are not in epoch pack files.
fetchBlocksFromTip
    :: Monad m
    => HttpBridge m e
    -> SlotId
    -> Hash "BlockHeader"
    -> ExceptT e m [Block]
fetchBlocksFromTip network start tipHash =
    reverse <$> workBackwards tipHash
  where
    workBackwards headerHash = do
        block <- getBlock network headerHash
        let hdr = header block
        if start < slotId hdr then do
            blocks <- workBackwards (prevBlockHash hdr)
            pure (block:blocks)
        else
            pure [block]

{-------------------------------------------------------------------------------
                            HTTP-Bridge Client
-------------------------------------------------------------------------------}

-- | Endpoints of the cardano-http-bridge API.
data HttpBridge m e = HttpBridge
    { getBlock
        :: Hash "BlockHeader" -> ExceptT e m Block
    , getEpoch
        :: Word64 -> ExceptT e m [Block]
    , getNetworkTip
        :: ExceptT e m (Hash "BlockHeader", BlockHeader)
    , postSignedTx
        :: SignedTx -> ExceptT e m ()
    }

-- | Construct a new network layer
mkHttpBridge
    :: Manager -> BaseUrl -> NetworkName -> HttpBridge IO ErrNetworkUnreachable
mkHttpBridge mgr baseUrl network = HttpBridge
    { getBlock = \hash -> do
        hash' <- hashToApi' hash
        run (getApiT <$> cGetBlock network hash')
    , getEpoch = \ep ->
        run (map getApiT <$> cGetEpoch network (EpochIndex ep))
    , getNetworkTip =
        run (blockHeaderHash <$> cGetNetworkTip network)
    , postSignedTx =
        -- TODO: We would be good to parse errors from the node
        -- like "Failed to send to peers: Blockchain protocol error"
        -- and "Transaction failed verification: transaction has no inputs"
        void . run . (cPostSignedTx network) . ApiT
    }
  where
    env = mkClientEnv mgr baseUrl

    run :: ClientM a -> ExceptT ErrNetworkUnreachable IO a
    run query = ExceptT $ runClientM query env >>= \case
        Left (ConnectionError e) -> return $ Left $ ErrNetworkUnreachable e
        Left e -> throwM e
        Right c -> return $ Right c

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
newHttpBridge :: Text -> Int -> IO (HttpBridge IO ErrNetworkUnreachable)
newHttpBridge network port = do
    mgr <- newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" port ""
    pure $ mkHttpBridge mgr baseUrl (NetworkName network)
