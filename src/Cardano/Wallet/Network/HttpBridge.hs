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
    , HttpBridgeError(..)
    , mkNetworkLayer
    , newNetworkLayer
    , mkHttpBridge
    ) where

import Prelude

import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Network.HttpBridge.Api
    ( ApiT (..), EpochIndex (..), NetworkName (..), api )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), SignedTx, SlotId (..) )
import Control.Exception
    ( Exception (..) )
import Control.Monad
    ( void )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE )
import Crypto.Hash
    ( HashAlgorithm, digestFromByteString )
import Data.Bifunctor
    ( first )
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

import qualified Data.Text as T
import qualified Servant.Extra.ContentTypes as Api

-- | Constructs a network layer with the given cardano-http-bridge API.
mkNetworkLayer :: Monad m => HttpBridge m e -> NetworkLayer m e e
mkNetworkLayer httpBridge = NetworkLayer
    { nextBlocks = rbNextBlocks httpBridge
    , networkTip = getNetworkTip httpBridge
    , postTx = postSignedTx httpBridge
    }

-- | Creates a cardano-http-bridge 'NetworkLayer' using the given connection
-- settings.
newNetworkLayer
    :: Text -> Int -> IO (NetworkLayer IO HttpBridgeError HttpBridgeError)
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
    epochBlocks <- lift nextStableEpoch
    lastBlocks <- if null epochBlocks
        then unstableBlocks tipHash tip
        else pure []
    pure (epochBlocks ++ lastBlocks)
  where
    nextStableEpoch = do
        epochBlocks <- runExceptT (getEpoch network (epochIndex start)) >>= \case
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
    :: Manager -> BaseUrl -> NetworkName -> HttpBridge IO HttpBridgeError
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
    run :: ClientM a -> ExceptT HttpBridgeError IO a
    run query = ExceptT $ (first convertError) <$> runClientM query env

    env = mkClientEnv mgr baseUrl

    cGetBlock
        :<|> cGetEpoch
        :<|> cGetNetworkTip
        :<|> cPostSignedTx
        = client api

convertError :: ServantError -> HttpBridgeError
convertError e@(FailureResponse _) =
    NodeUnavailable (displayException e)
convertError (ConnectionError e) =
    NodeUnavailable ("Connection error: " <> T.unpack e)
convertError e@(DecodeFailure _ _) =
    BadResponseFromNode (show e)
convertError (UnsupportedContentType _ _) =
    BadResponseFromNode "UnsupportedContentType"
convertError (InvalidContentTypeHeader _) =
    BadResponseFromNode "InvalidContentTypeHeader"

-- | The things that can go wrong when retrieving blocks.
data HttpBridgeError
    = NodeUnavailable String
      -- ^ Could not connect to or read from the node API.
    | BadResponseFromNode String
      -- ^ The node returned an unexpected response.
    deriving (Show, Eq)

blockHeaderHash
    :: WithHash algorithm (ApiT BlockHeader)
    -> (Hash "BlockHeader", BlockHeader)
blockHeaderHash (WithHash h (ApiT bh)) =
    (Hash (convert h), bh)

hashToApi :: HashAlgorithm a => Hash h -> Maybe (Api.Hash a b)
hashToApi (Hash h) = Api.Hash <$> digestFromByteString h

-- | Converts a Hash to the Digest type that the Api module requires.
hashToApi'
    :: (Monad m, HashAlgorithm algorithm)
    => Hash a
    -> ExceptT HttpBridgeError m (Api.Hash algorithm b)
hashToApi' h = case hashToApi h of
    Just h' -> pure h'
    Nothing -> throwE
        $ BadResponseFromNode "hashToApi: Digest was of the wrong length"

-- | Creates a cardano-http-bridge API with the given connection settings.
newHttpBridge :: Text -> Int -> IO (HttpBridge IO HttpBridgeError)
newHttpBridge network port = do
    mgr <- newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" port ""
    pure $ mkHttpBridge mgr baseUrl (NetworkName network)
