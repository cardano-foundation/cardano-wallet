{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
--
--
-- This module allows the wallet to retrieve blocks from a known @Jormungandr@
-- node. This is done by providing a @Restorer@ with some logic building on
-- top of an underlying @JormungandrLayer@ HTTP client.
module Cardano.Wallet.Jormungandr.Network
    ( newRestorer
    , newRestorer'
    , mkRestorer
    , JormungandrLayer (..)
    , mkJormungandrLayer

    -- * Exceptions
    , ErrUnexpectedNetworkFailure (..)

    -- * Errors
    , ErrGetDescendants (..)
    , ErrGetBlockchainParams (..)

    -- * Re-export
    , BaseUrl (..)
    , newManager
    , defaultManagerSettings
    , Scheme (..)

    -- * Utils
    , baseUrlToText

    -- * Internal state
    , UnstableBlocks(..)
    , updateUnstableBlocks
    ) where

import Prelude

import Cardano.Wallet
    ( BlockchainParameters (..) )
import Cardano.Wallet.Jormungandr.Api
    ( BlockId (..)
    , GetBlock
    , GetBlockDescendantIds
    , GetTipId
    , PostMessage
    , api
    )
import Cardano.Wallet.Jormungandr.Binary
    ( ConfigParam (..), Message (..), coerceBlock )
import Cardano.Wallet.Jormungandr.Compatibility
    ( softTxMaxSize )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx )
import Cardano.Wallet.Network
    ( ErrGetBlock (..)
    , ErrNetworkTip (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , Restorer (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , Hash (..)
    , SlotId (..)
    , SlotLength (..)
    , TxWitness (..)
    )
import Control.Arrow
    ( left )
import Control.Concurrent.MVar
    ( MVar, newMVar )
import Control.Concurrent.MVar.Lifted
    ( modifyMVar )
import Control.DeepSeq
    ( NFData, ($!!) )
import Control.Exception
    ( Exception )
import Control.Monad
    ( forM, void )
import Control.Monad.Catch
    ( throwM )
import Control.Monad.Trans.Control
    ( MonadBaseControl )
import Control.Monad.Trans.Except
    ( ExceptT (..), throwE, withExceptT )
import Data.Coerce
    ( coerce )
import Data.Maybe
    ( mapMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Sequence
    ( Seq (..), (><) )
import Data.Tuple.Extra
    ( thd3 )
import GHC.Generics
    ( Generic )
import Network.HTTP.Client
    ( Manager, defaultManagerSettings, newManager )
import Network.HTTP.Types.Status
    ( status400 )
import Numeric.Natural
    ( Natural )
import Safe
    ( lastMay, tailSafe )
import Servant.API
    ( (:<|>) (..) )
import Servant.Client
    ( BaseUrl (..)
    , ClientM
    , Scheme (..)
    , client
    , mkClientEnv
    , responseBody
    , responseStatusCode
    , runClientM
    , showBaseUrl
    )
import Servant.Client.Core
    ( ServantError (..) )
import Servant.Links
    ( Link, safeLink )

import qualified Cardano.Wallet.Jormungandr.Binary as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Creates a new 'Restorer' connecting to an underlying 'Jormungandr'
-- backend target.
newRestorer
    :: BaseUrl
    -> IO (Restorer (Block Tx) IO)
newRestorer = fmap snd . newRestorer'

-- | Creates a new 'Restorer' connecting to an underlying 'Jormungandr'
-- backend target. Also returns the internal 'JormungandrLayer' component.
newRestorer'
    :: BaseUrl
    -> IO (JormungandrLayer IO, Restorer (Block Tx) IO)
newRestorer' url = do
    mgr <- newManager defaultManagerSettings
    st <- newMVar emptyUnstableBlocks
    let jor = mkJormungandrLayer mgr url
    return (jor, mkRestorer st jor)

-- | Wrap a Jormungandr client into a 'Restorer' common interface.
mkRestorer
    :: MonadBaseControl IO m
    => MVar UnstableBlocks
    -> JormungandrLayer m
    -> Restorer (Block Tx) m
mkRestorer st j = Restorer
    { networkTip = modifyMVar st $ \bs -> do
        bs' <- updateUnstableBlocks k getTipId' getBlockHeader bs
        ExceptT . pure $ case unstableBlocksTip bs' of
            Just t -> Right (bs', t)
            Nothing -> Left ErrNetworkTipNotFound

    , nextBlocks = \tip -> do
        let count = 10000
        -- Get the descendants of the tip's /parent/.
        -- The first descendant is therefore the current tip itself. We need to
        -- skip it. Hence the 'tail'.
        ids <- tailSafe <$> getDescendantIds j (prevBlockHash tip) count
                `mappingError` \case
            ErrGetDescendantsNetworkUnreachable e ->
                ErrGetBlockNetworkUnreachable e
            ErrGetDescendantsParentNotFound _ ->
                ErrGetBlockNotFound (prevBlockHash tip)
        forM ids (fmap coerceBlock . getBlock j)
    }
  where
    getTipId' = (getTipId j) `mappingError`
        ErrNetworkTipNetworkUnreachable
    getBlock' t = (getBlock j t) `mappingError` \case
        ErrGetBlockNotFound _ ->
            ErrNetworkTipNotFound
        ErrGetBlockNetworkUnreachable e ->
            ErrNetworkTipNetworkUnreachable e
    getBlockHeader t = do
        blk@(J.Block blkHeader _) <- getBlock' t
        let nodeHeight = Quantity $ fromIntegral $ J.chainLength blkHeader
        pure (header (coerceBlock blk), nodeHeight)

    mappingError = flip withExceptT

    -- security parameter, hardcoded for now
    k = 2160

{-------------------------------------------------------------------------------
                   Managing the global unstable blocks state
-------------------------------------------------------------------------------}

-- | A list of block headers and their hashes.
-- The last block in this sequence is the network tip.
-- The first block in this sequence is the block of depth /k/,
-- which is the last unstable block.
data UnstableBlocks = UnstableBlocks
    { getUnstableBlocks :: !(Seq (Hash "BlockHeader", BlockHeader))
    -- ^ Double-ended queue of block headers, and their IDs.
    , blockHeight :: !(Quantity "block" Natural)
    -- ^ The block height of the tip of the sequence.
    } deriving stock (Show, Eq, Generic)

instance NFData UnstableBlocks

emptyUnstableBlocks :: UnstableBlocks
emptyUnstableBlocks = UnstableBlocks mempty (Quantity 0)

-- | Updates the unstable blocks state using the given "fetch" functions.
--
-- This attempts to synchronise the local state with that of the node. The node
-- may be on a different chain to the current unstable blocks, so this function
-- handles switching of chains.
--
-- For example, this is what it would do when the local tip is @a13@, but the
-- node's tip is @b15@, on a different chain.
--
-- @
--                                    local tip ↴
--                 ┌───┬───  ───┬───┬───┬───┬───┐
--  UnstableBlocks │a03│..    ..│a10│a11│a12│a13│
--                 └───┴───  ───┴───┴───┴───┴───┘
--                           ───┬───┬───┬───┬───┬───┬───┐
--  Node backend chain       ...│a10│a11│b12│b13│b14│b15│
--                           ───┴───┴───┴───┴───┴───┴───┘
--                       rollback point ⬏     node tip ⬏
-- @
--
-- To start with, the node says the tip hash is @b15@.
--
-- Work backwards from tip, fetching blocks and adding them to @ac@, and
-- removing overlapping blocks from @ubs@. Overlapping blocks occur when there
-- has been a rollback.
--
-- @
--     ubs                     ac
-- 1.  ───┬───┬───┬───┬───┐    ┌───┐
--     ...│a10│a11│a12│a13│    │b15│
--     ───┴───┴───┴───┴───┘    └───┘
-- 2.  ───┬───┬───┬───┬───┐┌───┬───┐
--     ...│a10│a11│a12│a13││b14│b15│
--     ───┴───┴───┴───┴───┘└───┴───┘
-- 3.  ───┬───┬───┬───┐┌───┬───┬───┐
--     ...│a10│a11│a12││b13│b14│b15│
--     ───┴───┴───┴───┘└───┴───┴───┘
-- 4.  ───┬───┬───┐┌───┬───┬───┬───┐
--     ...│a10│a11││b12│b13│b14│b15│
--     ───┴───┴───┘└───┴───┴───┴───┘
-- @
--
-- Stop once @ubs@ and @ac@ meet with a block which has the same hash.
-- If they never meet, stop after fetching /k/ blocks.
--
-- Finally, to get the new 'UnstableBlocks', append @ac@ to @ubs@, and limit the
-- length to /k/.
--
-- The new block height is the height of the first block that was fetched.
--
-- If any errors occur while this process is running (for example, fetching a
-- block which has been rolled back and lost from the node's state), it will
-- immediately terminate.
updateUnstableBlocks
    :: forall m. Monad m
    => Natural
    -- ^ Maximum number of unstable blocks (/k/).
    -> m (Hash "BlockHeader")
    -- ^ Fetches tip.
    -> (Hash "BlockHeader" -> m (BlockHeader, Quantity "block" Natural))
    -- ^ Fetches block header and its chain height.
    -> UnstableBlocks
    -- ^ Current unstable blocks state.
    -> m UnstableBlocks
updateUnstableBlocks k getTipId' getBlockHeader lbhs = do
    t <- getTipId'
    -- Trace backwards from the tip, accumulating new block headers, and
    -- removing overlapped unstable block headers.
    (lbhs', nbhs) <- fetchBackwards lbhs [] 0 t
    -- The new unstable blocks is the current local blocks up to where they
    -- meet the blocks fetched starting from the tip, with the fetched
    -- blocks appended.
    pure $!! appendUnstableBlocks k lbhs' nbhs
  where
    -- | Fetch blocks backwards starting from the given id. If fetched blocks
    -- overlap the local blocks, the excess local blocks will be dropped.
    fetchBackwards
        :: UnstableBlocks
        -- ^ Current local unstable blocks
        -> [(Hash "BlockHeader", BlockHeader, Quantity "block" Natural)]
        -- ^ Accumulator of fetched blocks
        -> Natural
        -- ^ Accumulator for number of blocks fetched
        -> Hash "BlockHeader"
        -- ^ Starting point for block fetch
        -> m (UnstableBlocks, [(Hash "BlockHeader", BlockHeader, Quantity "block" Natural)])
    fetchBackwards ubs ac len t = do
        (tipHeader, tipHeight) <- getBlockHeader t
        -- Push the remote block.
        let ac' = ((t, tipHeader, tipHeight):ac)
         -- Pop off any overlap.
        let ubs' = dropStartingFromSlot tipHeader ubs
        -- If remote blocks have met local blocks, or if more than k have been
        -- fetched, or we are at the genesis, then stop.
        -- Otherwise, continue from the parent of the current tip.
        let intersected = unstableBlocksTipId ubs' == Just (prevBlockHash tipHeader)
        let bufferFull = len + 1 >= k
        let atGenesis = slotId tipHeader == SlotId 0 0
        if intersected || bufferFull || atGenesis
            then pure (ubs', ac')
            else fetchBackwards ubs' ac' (len + 1) (prevBlockHash tipHeader)

-- | The tip block header of the unstable blocks, if it exists.
unstableBlocksTip :: UnstableBlocks -> Maybe BlockHeader
unstableBlocksTip (UnstableBlocks Empty _) = Nothing
unstableBlocksTip (UnstableBlocks (_ubs :|> (_, bh)) _) = Just bh

-- | The tip block id of the unstable blocks, if it exists.
unstableBlocksTipId :: UnstableBlocks -> Maybe (Hash "BlockHeader")
unstableBlocksTipId (UnstableBlocks Empty _) = Nothing
unstableBlocksTipId (UnstableBlocks (_ubs :|> (t, _)) _) = Just t

-- | Add recently fetched block headers to the unstable blocks. This will drop
-- the oldest block headers to ensure that there are at most /k/ items in the
-- sequence.
appendUnstableBlocks
    :: Natural
    -- ^ Maximum length of sequence.
    -> UnstableBlocks
    -- ^ Current unstable block headers, with rolled back blocks removed.
    -> [(Hash "BlockHeader", BlockHeader, Quantity "block" Natural)]
    -- ^ Newly fetched block headers to add.
    -> UnstableBlocks
appendUnstableBlocks k (UnstableBlocks ubs h) bs =
    UnstableBlocks (ubs `appendBounded` more) h'
  where
    more = Seq.fromList [(a, b) | (a, b, _) <- bs]
    -- New block height is the height of the new tip block.
    h' = maybe h thd3 (lastMay bs)

    -- Concatenate sequences, ensuring that the result is no longer than k.
    appendBounded :: Seq a -> Seq a -> Seq a
    appendBounded a b = Seq.drop excess (a >< b)
        where excess = max 0 (Seq.length a + Seq.length b - fromIntegral k)

-- | Remove unstable blocks which have a slot greater than or equal to the given
-- block header's slot.
dropStartingFromSlot :: BlockHeader -> UnstableBlocks -> UnstableBlocks
dropStartingFromSlot bh (UnstableBlocks bs (Quantity h)) =
    UnstableBlocks bs' (Quantity h')
  where
    isAfter = (>= slotId bh) . slotId . snd
    bs' = Seq.dropWhileR isAfter bs
    h' = h + fromIntegral (max 0 $ Seq.length bs' - Seq.length bs)

{-------------------------------------------------------------------------------
                            Jormungandr Client
-------------------------------------------------------------------------------}

-- | Endpoints of the jormungandr REST API.
data JormungandrLayer m = JormungandrLayer
    { getTipId
        :: ExceptT ErrNetworkUnavailable m (Hash "BlockHeader")
    , getBlock
        :: Hash "BlockHeader"
        -> ExceptT ErrGetBlock m J.Block
    , getDescendantIds
        :: Hash "BlockHeader"
        -> Word
        -> ExceptT ErrGetDescendants m [Hash "BlockHeader"]
    , postMessage
        :: (Tx, [TxWitness])
        -> ExceptT ErrPostTx m ()
    , getInitialBlockchainParameters
        :: Hash "Genesis"
        -> ExceptT ErrGetBlockchainParams m (Block Tx, BlockchainParameters)
    }

-- | Construct a 'JormungandrLayer'-client
--
-- >>> mgr <- newManager defaultManagerSettings
-- >>> j = mkJormungandrLayer mgr (BaseUrl Http "localhost" 8080 "")
--
-- >>> (Right tip) <- runExceptT $ getTipId j
-- >>> tip
-- BlockId (Hash {getHash = "26c640a3de09b74398c14ca0a137ec78"})
--
-- >>> (Right block) <- runExceptT $ getBlock j t
-- >>> block
-- >>> Block {header = BlockHeader {slotId = SlotId {epochNumber = 0, slotNumber = 0}, prevBlockHash = Hash {getHash = "\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL"}}, transactions = [Tx {inputs = [], outputs = [TxOut {address = Address {unAddress = "3$\195xi\193\"h\154\&5\145}\245:O\"\148\163\165/h^\ENQ\245\248\229;\135\231\234E/"}, coin = Coin {getCoin = 14}}]}]}
--
-- At the time of writing, we only have the genesis-block, but we should be
-- able to get its descendants.
--
-- >>> let genesisHash = BlockId (Hash {getHash = "&\198@\163\222\t\183C\152\193L\160\161\&7\236x\245\229\EOT\175\177\167\131\190\b\b/\174\212\177:\179"})
-- >>> runExceptT $ getDescendantIds j t 4
-- Right []
mkJormungandrLayer
    :: Manager -> BaseUrl -> JormungandrLayer IO
mkJormungandrLayer mgr baseUrl = JormungandrLayer
    { getTipId = ExceptT $ do
        let ctx = safeLink api (Proxy @GetTipId)
        run (getBlockId <$> cGetTipId) >>= defaultHandler ctx

    , getBlock = \blockId -> ExceptT $ do
        run (cGetBlock (BlockId blockId)) >>= \case
            Left (FailureResponse e) | responseStatusCode e == status400 ->
                return . Left . ErrGetBlockNotFound $ blockId
            x -> do
                let ctx = safeLink api (Proxy @GetBlock) (BlockId blockId)
                left ErrGetBlockNetworkUnreachable <$> defaultHandler ctx x

    , getDescendantIds = \parentId count -> ExceptT $ do
        run (map getBlockId <$> cGetBlockDescendantIds (BlockId parentId) (Just count))  >>= \case
            Left (FailureResponse e) | responseStatusCode e == status400 ->
                return . Left $ ErrGetDescendantsParentNotFound parentId
            x -> do
                let ctx = safeLink
                        api
                        (Proxy @GetBlockDescendantIds)
                        (BlockId parentId)
                        (Just count)
                left ErrGetDescendantsNetworkUnreachable <$> defaultHandler ctx x

    -- Never returns 'Left ErrPostTxProtocolFailure'. Will currently return
    -- 'Right ()' when submitting correctly formatted, but invalid transactions.
    --
    -- https://github.com/input-output-hk/jormungandr/blob/fe638a36d4be64e0c4b360ba1c041e8fa10ea024/jormungandr/src/rest/v0/message/post.rs#L25-L39
    , postMessage = \tx -> void $ ExceptT $ do
        run (cPostMessage tx) >>= \case
            Left (FailureResponse e)
                | responseStatusCode e == status400 -> do
                    let msg = T.decodeUtf8 $ BL.toStrict $ responseBody e
                    return $ Left $ ErrPostTxBadRequest msg
            x -> do
                let ctx = safeLink api (Proxy @PostMessage)
                left ErrPostTxNetworkUnreachable <$> defaultHandler ctx x

    , getInitialBlockchainParameters = \block0 -> do
        jblock@(J.Block _ msgs) <- ExceptT $ run (cGetBlock (BlockId $ coerce block0)) >>= \case
            Left (FailureResponse e) | responseStatusCode e == status400 ->
                return . Left . ErrGetBlockchainParamsGenesisNotFound $ block0
            x -> do
                let ctx = safeLink api (Proxy @GetBlock) (BlockId $ coerce block0)
                let networkUnreachable = ErrGetBlockchainParamsNetworkUnreachable
                left networkUnreachable <$> defaultHandler ctx x

        let params = mconcat $ mapMaybe getConfigParameters msgs
              where
                getConfigParameters = \case
                    Initial xs -> Just xs
                    _ -> Nothing

        let mpolicy = mapMaybe getPolicy params
              where
                getPolicy = \case
                    ConfigLinearFee x -> Just x
                    _ -> Nothing

        let mduration = mapMaybe getSlotDuration params
              where
                getSlotDuration = \case
                    SlotDuration x -> Just x
                    _ -> Nothing

        let mblock0Date = mapMaybe getBlock0Date params
              where
                getBlock0Date = \case
                    Block0Date x -> Just x
                    _ -> Nothing

        let mepochLength = mapMaybe getSlotsPerEpoch params
              where
                getSlotsPerEpoch = \case
                    SlotsPerEpoch x -> Just x
                    _ -> Nothing

        let mStability = mapMaybe getStability params
              where
                getStability = \case
                   EpochStabilityDepth x -> Just x
                   _ -> Nothing

        case (mpolicy, mduration, mblock0Date, mepochLength, mStability) of
            ([policy],[duration],[block0Date], [epochLength], [stability]) ->
                return
                    ( coerceBlock jblock
                    , BlockchainParameters
                        { getGenesisBlockDate = block0Date
                        , getFeePolicy = policy
                        , getEpochLength = epochLength
                        , getSlotLength = SlotLength duration
                        , getTxMaxSize = softTxMaxSize
                        , getEpochStability = stability
                        }
                    )
            _ ->
                throwE $ ErrGetBlockchainParamsIncompleteParams params
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

        -- Other errors (status code, decode failure, invalid content type
        -- headers). These are considered to be programming errors, so crash.
        Left e -> do
            throwM (ErrUnexpectedNetworkFailure ctx e)

    cGetTipId
        :<|> cGetBlock
        :<|> cGetBlockDescendantIds
        :<|> cPostMessage
        = client api

data ErrUnexpectedNetworkFailure
    = ErrUnexpectedNetworkFailure Link ServantError
    deriving (Show)

instance Exception ErrUnexpectedNetworkFailure

data ErrGetDescendants
    = ErrGetDescendantsNetworkUnreachable ErrNetworkUnavailable
    | ErrGetDescendantsParentNotFound (Hash "BlockHeader")
    deriving (Show, Eq)

data ErrGetBlockchainParams
    = ErrGetBlockchainParamsNetworkUnreachable ErrNetworkUnavailable
    | ErrGetBlockchainParamsGenesisNotFound (Hash "Genesis")
    | ErrGetBlockchainParamsIncompleteParams [ConfigParam]
    deriving (Show, Eq)

{-------------------------------------------------------------------------------
                                     Utils
-------------------------------------------------------------------------------}

-- | Format an API 'BaseUrl', for logging, etc.
baseUrlToText :: BaseUrl -> T.Text
baseUrlToText = T.pack . showBaseUrl
