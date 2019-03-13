{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.NetworkLayer.HttpBridge where

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains the necessary logic to talk to implement the network
-- layer using the cardano-http-bridge as a chain producer.

import Prelude

import Cardano.NetworkLayer.HttpBridge.Api
    ( ApiT (..), EpochIndex (..), NetworkName, api )
import Cardano.Wallet.Primitive
    ( Block (..)
    , BlockHeader (..)
    , Hash (..)
    , Hash (..)
    , SlotId (..)
    , slotIncr
    , slotsPerEpoch
    )
import Control.Exception
    ( Exception )
import Control.Monad.Except
    ( ExceptT (..), mapExceptT, runExceptT, throwError )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Reader
    ( MonadReader, ReaderT (..), ask, lift )
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
import Data.Word
    ( Word64 )
import Network.HTTP.Client
    ( Manager )
import Numeric.Natural
    ( Natural )
import Servant.API
    ( (:<|>) (..) )
import Servant.Client
    ( BaseUrl, ClientM, client, mkClientEnv, runClientM )
import Servant.Extra.ContentTypes
    ( WithHash (..) )

import qualified Servant.Extra.ContentTypes as Api


newtype RustBackend a = RustBackend
    { runRB :: ReaderT (HttpBridge IO) IO a
    } deriving
        ( Monad
        , Applicative
        , Functor
        , MonadReader (HttpBridge IO)
        , MonadIO
        )

runRustBackend :: HttpBridge IO -> RustBackend a -> IO a
runRustBackend network action = runReaderT (runRB action) network

getNetwork :: RustBackend (HttpBridge IO)
getNetwork = ask

-- | The things that can go wrong when retrieving blocks.
newtype ErrGetNextBlocks
    = GetNextBlocksError String
    deriving (Show, Eq)

instance Exception ErrGetNextBlocks

-- Note: This will be quite inefficient for at least two reasons.
-- 1. If the number of blocks requested is small, it will fetch the same epoch
--    pack file repeatedly.
-- 2. Fetching the tip block and working backwards is not ideal.
-- We will keep it for now, and it can be improved later.
nextBlocks
    :: Natural -- ^ Number of blocks to retrieve
    -> SlotId    -- ^ Starting point
    -> ExceptT ErrGetNextBlocks RustBackend [Block]
nextBlocks numBlocks start = do
    net <- lift getNetwork
    (tipHash, tip) <- fmap slotId <$> runHttpBridge (getNetworkTip net)
    epochBlocks <- blocksFromPacks net tip
    lastBlocks <- unstableBlocks net tipHash tip epochBlocks
    pure (epochBlocks ++ lastBlocks)
  where
    end = slotIncr numBlocks start

    -- Grab blocks from epoch pack files
    blocksFromPacks network tip = do
        let epochs = epochRange numBlocks start tip
        epochBlocks <- runHttpBridge (getEpochs network epochs)
        pure $ filter (blockIsBetween start end) (concat epochBlocks)

    -- The next slot after the last block.
    slotAfter [] = Nothing
    slotAfter bs = Just . succ . slotId . header . last $ bs

    -- Grab the remaining blocks which aren't packed in epoch files,
    -- starting from the tip.
    unstableBlocks network tipHash tip epochBlocks = do
        let start' = fromMaybe start (slotAfter epochBlocks)

        lastBlocks <- if end > start' && start' <= tip
            then runHttpBridge $ fetchBlocksFromTip network start' tipHash
            else pure []

        pure $ filter (blockIsBefore end) lastBlocks

-- | Fetch epoch blocks until one fails.
getEpochs
    :: Monad m
    => HttpBridge m
    -> [Word64]
    -> ExceptT HttpBridgeError m [[Block]]
getEpochs network = mapUntilError (getEpoch network)

-- Fetch blocks which are not in epoch pack files.
fetchBlocksFromTip
    :: Monad m
    => HttpBridge m
    -> SlotId
    -> Hash "BlockHeader"
    -> ExceptT HttpBridgeError m [Block]
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

runHttpBridge
    :: ExceptT HttpBridgeError IO a
    -> ExceptT ErrGetNextBlocks RustBackend a
runHttpBridge =
    mapExceptT (fmap handle . liftIO)
  where
    handle = first (GetNextBlocksError . show)


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


-- * Slotting calculation utilities (TODO: Move in the wallet primitives)

-- | Calculates which epochs to fetch, given a number of slots, and the start
-- point. It takes into account the latest block available, and that the most
-- recent epoch is never available in a pack file.
epochRange
    :: Natural
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

-- | Predicate returns true iff the block is from the given slot or a later one.
blockIsSameOrAfter :: SlotId -> Block -> Bool
blockIsSameOrAfter s = (>= s) . slotId . header

-- | Predicate returns true iff the block is after then given slot
blockIsAfter :: SlotId -> Block -> Bool
blockIsAfter s = (> s) . slotId . header

-- | Predicate returns true iff the block is before the given slot.
blockIsBefore :: SlotId -> Block -> Bool
blockIsBefore s = (< s) . slotId . header

-- | @blockIsBetween start end@ Returns true if the block is in within the
-- interval @[start, end)@.
blockIsBetween :: SlotId -> SlotId -> Block -> Bool
blockIsBetween start end b = blockIsSameOrAfter start b && blockIsBefore end b


{-------------------------------------------------------------------------------
                            HTTP-Bridge Client
-------------------------------------------------------------------------------}

-- | Endpoints of the cardano-http-bridge API.
data HttpBridge m = HttpBridge
    { getBlock
        :: Hash "BlockHeader" -> ExceptT HttpBridgeError m Block
    , getEpoch
        :: Word64 -> ExceptT HttpBridgeError m [Block]
    , getNetworkTip
        :: ExceptT HttpBridgeError m (Hash "BlockHeader", BlockHeader)
    }

newtype HttpBridgeError
    = HttpBridgeError String
    deriving (Show, Eq)

instance Exception HttpBridgeError

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
mkHttpBridge :: Manager -> BaseUrl -> NetworkName -> HttpBridge IO
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
    convertError = HttpBridgeError . show

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
        $ HttpBridgeError "hashToApi: Digest was of the wrong length"
