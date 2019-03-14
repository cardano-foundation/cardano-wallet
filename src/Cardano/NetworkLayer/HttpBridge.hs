{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.NetworkLayer.HttpBridge
    ( HttpBridge(..)
    , HttpBridgeError(..)
    , mkNetworkLayer
    , newNetworkLayer
    ) where

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the necessary logic to talk to implement the network
-- layer using the cardano-http-bridge as a chain producer.

module Cardano.NetworkLayer.HttpBridge where

import Prelude

import Cardano.NetworkLayer
    ( NetworkLayer (..) )
import Cardano.NetworkLayer.HttpBridge.Api
    ( ApiT (..), EpochIndex (..), NetworkName (..), api )
import Cardano.Wallet.Primitive
    ( Block (..)
    , BlockHeader (..)
    , Hash (..)
    , Hash (..)
    , SlotId (..)
    , blockIsAfter
    , blockIsBefore
    , blockIsBetween
    , slotIncr
    , slotsPerEpoch
    )
import Control.Exception
    ( Exception (..) )
import Control.Monad.Except
    ( ExceptT (..), runExceptT, throwError )
import Crypto.Hash
    ( HashAlgorithm, digestFromByteString )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.Bifunctor
    ( first )
import Data.ByteArray
    ( convert )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.Word
    ( Word64 )
import Network.HTTP.Client
    ( Manager )
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

-- Note: This will be quite inefficient for at least two reasons.
-- 1. If the number of blocks requested is small, it will fetch the same epoch
--    pack file repeatedly.
-- 2. Fetching the tip block and working backwards is not ideal.
-- We will keep it for now, and it can be improved later.
rbNextBlocks
    :: Monad m
    => HttpBridge m e -- ^ http-bridge API
    -> Word64  -- ^ Number of blocks to retrieve
    -> SlotId -- ^ Starting point
    -> ExceptT e m [Block]
rbNextBlocks net numBlocks start = do
    (tipHash, tip) <- fmap slotId <$> getNetworkTip net
    epochBlocks <- blocksFromPacks net tip
    lastBlocks <- unstableBlocks net tipHash tip epochBlocks
    pure (epochBlocks ++ lastBlocks)
  where
    end = slotIncr numBlocks start

    -- Grab blocks from epoch pack files
    blocksFromPacks network tip = do
        let epochs = epochRange numBlocks start tip
        epochBlocks <- getEpochs network epochs
        pure $ filter (blockIsBetween start end) (concat epochBlocks)

    -- The next slot after the last block.
    slotAfter [] = Nothing
    slotAfter bs = Just . succ . slotId . header . last $ bs

    -- Grab the remaining blocks which aren't packed in epoch files,
    -- starting from the tip.
    unstableBlocks network tipHash tip epochBlocks = do
        let start' = fromMaybe start (slotAfter epochBlocks)

        lastBlocks <- if end > start' && start' <= tip
            then fetchBlocksFromTip network start' tipHash
            else pure []

        pure $ filter (blockIsBefore end) lastBlocks

-- | Calculates which epochs to fetch, given a number of slots, and the start
-- point. It takes into account the latest block available, and that the most
-- recent epoch is never available in a pack file.
epochRange
    :: Word64
        -- ^ Number of slots
    -> SlotId
        -- ^ Start point
    -> SlotId
        -- ^ Latest block available
    -> [Word64]
epochRange numBlocks (SlotId startEpoch startSlot) (SlotId tipEpoch _) =
    [startEpoch .. min (tipEpoch - 1) (startEpoch + fromIntegral numEpochs)]
  where
    numEpochs = (numBlocks + fromIntegral startSlot) `div` slotsPerEpoch

-- | Fetch epoch blocks until one fails.
getEpochs
    :: Monad m
    => HttpBridge m e
    -> [Word64]
    -> ExceptT e m [[Block]]
getEpochs network = mapUntilError (getEpoch network)

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
        if blockIsAfter start block then do
            blocks <- workBackwards $ prevBlockHash $ header block
            pure (block:blocks)
        else
            pure [block]


-- * Utility functions for monadic loops

-- | Apply an action to each element of a list, until an action fails, or there
-- are no more elements. This is like mapM, except that it always succeeds and
-- the resulting list might be smaller than the given list.
mapUntilError
    :: Monad m
    => (a -> ExceptT e m b)
       -- ^ Action to run
    -> [a]
       -- ^ Elements
    -> ExceptT e m [b]
       -- ^ Results
mapUntilError action (x:xs) = ExceptT $ runExceptT (action x) >>= \case
    Left _ -> pure $ Right []
    Right r -> runExceptT $ do
        rs <- mapUntilError action xs
        pure (r:rs)
mapUntilError _ [] = pure []


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
    }

-- | Retrieve a block identified by the unique hash of its header.
getBlockByHash :: NetworkName -> Api.Hash Blake2b_256 (ApiT BlockHeader) -> ClientM (ApiT Block)

-- | Retrieve all the blocks for the epoch identified by the given integer ID.
getEpochById :: NetworkName -> EpochIndex -> ClientM [ApiT Block]

-- | Retrieve the header of the latest known block.
getTipBlockHeader :: NetworkName -> ClientM (WithHash Blake2b_256 (ApiT BlockHeader))

getBlockByHash
    :<|> getEpochById
    :<|> getTipBlockHeader
    = client api

-- | Construct a new network layer
mkHttpBridge :: Manager -> BaseUrl -> NetworkName -> HttpBridge IO HttpBridgeError
mkHttpBridge mgr baseUrl network = HttpBridge
    { getBlock = \hash -> do
        hash' <- hashToApi' hash
        run (getApiT <$> getBlockByHash network hash')
    , getEpoch = \ep -> run (map getApiT <$>
        getEpochById network (EpochIndex ep))
    , getNetworkTip = run (blockHeaderHash <$> getTipBlockHeader network)
    }
  where
    run :: ClientM a -> ExceptT HttpBridgeError IO a
    run query = ExceptT $ (first convertError) <$> runClientM query env
    env = mkClientEnv mgr baseUrl

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

instance Exception HttpBridgeError where
    displayException (NodeUnavailable e) =
        "Internal error: cardano-http-bridge returned an error code "
        ++ " or could not be connected to: " ++ e
    displayException (BadResponseFromNode e) =
        "Internal error: cardano-http-bridge returned an "
        ++ " unexpected response: " ++ e

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
    Nothing -> throwError
        $ BadResponseFromNode "hashToApi: Digest was of the wrong length"

-- | Creates a cardano-http-bridge API with the given connection settings.
newHttpBridge :: Text -> Int -> IO (HttpBridge IO HttpBridgeError)
newHttpBridge network port = do
    mgr <- newManager defaultManagerSettings
    let baseUrl = BaseUrl Http "localhost" port ""
    pure $ mkHttpBridge mgr baseUrl (NetworkName network)

-- | Creates a cardano-http-bridge 'NetworkLayer' using the given connection settings.
newNetworkLayer :: Text -> Int -> IO (NetworkLayer IO HttpBridgeError HttpBridgeError)
newNetworkLayer network port = mkNetworkLayer <$> newHttpBridge network port

-- | Constructs a network layer with the given cardano-http-bridge API.
mkNetworkLayer :: Monad m => HttpBridge m e -> NetworkLayer m e e
mkNetworkLayer httpBridge = NetworkLayer
    { nextBlocks = rbNextBlocks httpBridge
    , networkTip = getNetworkTip httpBridge
    }
