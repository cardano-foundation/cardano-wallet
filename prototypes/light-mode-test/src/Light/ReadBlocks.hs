{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Implementation of ledger query.
module Light.ReadBlocks where

import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( forM
    )
import Control.Monad.IO.Class
    ( MonadIO
    , liftIO
    )
import Control.Monad.Trans.Except
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromJust
    , fromMaybe
    , listToMaybe
    )
import Data.Set
    ( Set
    )
import Say
    ( sayString
    )
import System.Random
    ( randomRIO
    )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Blockfrost.Client
import qualified Blockfrost.Client as BF
import Light.Types

{-----------------------------------------------------------------------------
    Address Discovery
------------------------------------------------------------------------------}

-- | done. Discover addresses from a query using an address pool.
discoverTransactions
    :: (Enum ix, Ord ix, Ord addr, Ord tx, Monad m, MonadIO m)
    => (addr -> m [tx])
    -> Pool addr ix
    -> m (Set tx, Pool addr ix)
discoverTransactions query pool0 =
    go pool0 (generator pool0 $ toEnum 0)
  where
    go !pool1 old = do
        txs <- query old
        case next pool1 old of
            Nothing -> pure (Set.fromList txs, pool1)
            Just new -> do
                if null txs
                    then go pool1 new
                    else do
                        let pool2 = update old pool1
                        (txs3, pool3) <- go pool2 new
                        pure (Set.fromList txs <> txs3, pool3)

{-----------------------------------------------------------------------------
    BlockSummary
------------------------------------------------------------------------------}

-- | (internal)
-- A 'BlockSummary' represents the data contained in a contiguous sequence
-- of blocks.
--
-- However, instead of storing the sequence of blocks of directly as a Haskell
-- list, the 'BlockSummary' only provides a 'query' function
-- which looks up all transactions associated to a given addresses.
-- In addition, this query function is monadic, which means that it
-- can call out to an external data source.
data BlockSummary m = BlockSummary
    { from :: ChainPoint
    -- ^ Location of the first block in the sequence
    , to :: ChainPoint
    -- ^ Location of the last block in the sequence
    , query :: Address -> m [TransactionUtxos]
    -- ^ Retrieve information contained in the block sequence.
    }

-- | Implement a 'BlockSummary' using the Blockfrost backend.
--
-- FIXME: 'getAddressTransactions'' only retrieves a maximum
-- of 100 transactions at present.
mkBlockSummaryBlockfrost
    :: ChainPoint -> ChainPoint -> BlockSummary BlockfrostClient
mkBlockSummaryBlockfrost from to =
    BlockSummary{from = from, to = to, query = query}
  where
    toHeight Origin = BlockIndex 0 Nothing
    toHeight (At height _) = BlockIndex height Nothing

    query addr = do
        atxs <-
            BF.getAddressTransactions'
                addr
                (Paged 100 1)
                Ascending
                (Just $ toHeight from)
                (Just $ toHeight to)
        forM atxs $ \(AddressTransaction hash _ _) ->
            BF.getTxUtxos hash

{-----------------------------------------------------------------------------
    Drive a ChainFollower using Blockfrost as a backend
------------------------------------------------------------------------------}

-- | (internal)
-- A 'ChainFollower' represents a collection of callbacks
-- that are provided by the wallet and driven by the network code
-- that follows the blockchain.
data ChainFollower m point = ChainFollower
    { readChainPoints :: m [point]
    -- ^ List the checkpoints from which we can resume following the chain.
    , rollForward :: BlockSummary m -> point -> m ()
    -- ^ Forward the chain follower with a sequence of blocks
    , rollBackward :: point -> m point
    -- ^ Rollback to a given point.
    }

-- | done. Drive a 'ChainFollower' using the blockfrost backend.
--
-- TODO: We need to test the logic of this query sequence
-- by mocking the Blockfrost / data source API.

{- HLINT ignore lightSync "Use if" -}
lightSync
    :: ChainFollower BlockfrostClient ChainPoint
    -> BlockfrostClient ()
lightSync follower = do
    pts <- readChainPoints follower
    go $ latest pts
  where
    go old = do
        next <- nextBlockFrom old
        liftIO $ do
            sayString $ "from: " <> show old
            sayString $ "to: " <> show next
        rollForward follower (mkBlockSummaryBlockfrost old next) next
        liftIO delay
        go next

    delay = threadDelay 3_000_000 -- microseconds
    nextBlockFrom Origin = do
        tip <- BF.getLatestBlock
        case _blockHeight tip of
            Nothing -> pure Origin
            Just height -> do
                -- help the chain follower to make a stable checkpoint
                fromBlock <$> (BF.getBlock . Left $ height - stabilityWindow)
    nextBlockFrom prev@(At height _) = do
        b <- hasBeenRolledBack prev
        case b of
            False -> fromBlock <$> BF.getLatestBlock
            True -> do
                -- fall back to behind the stability window
                target <- BF.getBlock . Left $ height - stabilityWindow
                old <- rollBackward follower $ fromBlock target
                nextBlockFrom old
    stabilityWindow = 1_000 :: Integer

-- | Test whether a given 'ChainPoint' has become invalid due
-- to a rollback.
hasBeenRolledBack :: ChainPoint -> BlockfrostClient Bool
hasBeenRolledBack Origin = pure False
hasBeenRolledBack (At _ hash) = do
    (False <$ BF.getBlock (Right hash)) `catchE` \e ->
        if e == BlockfrostNotFound then pure True else throwE e

{-----------------------------------------------------------------------------
    Mock Address Pool
------------------------------------------------------------------------------}

-- | Mock address pool
type Pool addr ix = Map addr ix

-- | Update an address within a pool.
update :: (Ord addr) => addr -> Pool addr ix -> Pool addr ix
update addr = id -- Map.adjust (\(ix,_) -> (ix,Used)) addr

-- | List the next address still in the pool
next :: (Enum ix, Ord addr, Eq ix) => Pool addr ix -> addr -> Maybe addr
next pool addr = do
    ix <- succ <$> Map.lookup addr pool
    if Map.null (Map.filter (ix ==) pool)
        then Nothing
        else pure $ generator pool ix

-- | Map index to pool.
generator :: (Ord addr, Eq ix) => Pool addr ix -> (ix -> addr)
generator pool ix = fst . head $ Map.toList $ Map.filter (ix ==) pool

mkPool :: (Ord addr, Enum ix) => [addr] -> Pool addr ix
mkPool addrs = Map.fromList $ zip addrs $ map toEnum [0 ..]

-- | Generate a new adddress that appears on the chain by
-- following the transactions of the previous one.
-- Note: If we just want a list of addresses that are on chain,
-- this is probably not a good way to do it.
genAddress :: Address -> BlockfrostClient (Maybe Address)
genAddress addr = do
    txs <- BF.getAddressTransactions' addr (BF.Paged 100 1) BF.Ascending Nothing Nothing
    tx <- BF.getTxUtxos . BF._addressTransactionTxHash =<< choose txs
    let choices =
            map _utxoInputAddress (_transactionUtxosInputs tx)
                <> map _utxoOutputAddress (_transactionUtxosOutputs tx)
    case filter (/= addr) choices of
        [] -> pure Nothing
        _ -> liftIO $ Just <$> choose choices

choose :: (MonadIO m) => [a] -> m a
choose xs = (xs !!) <$> randomRIO (0, length xs - 1)
