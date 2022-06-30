{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Shelley.Network.Blockfrost.Layer where

import Prelude

import Blockfrost.Client
    ( AccountDelegation
    , AccountInfo
    , AccountRegistration
    , AccountWithdrawal
    , Address
    , AddressTransaction
    , Block
    , BlockHash
    , BlockIndex
    , EpochInfo
    , Genesis
    , Network
    , Paged (..)
    , PoolHistory
    , PoolId
    , Project
    , ProtocolParams
    , Slot
    , SortOrder (Ascending)
    , Transaction
    , TransactionDelegation
    , TransactionMetaJSON
    , TransactionStake
    , TransactionUtxos
    , TransactionWithdrawal
    , TxHash
    , getAccount
    , getAccountDelegations'
    , getAccountRegistrations'
    , getAccountWithdrawals'
    , getAddressTransactions'
    , getBlock
    , getBlockSlot
    , getBlockTxs'
    , getLatestBlock
    , getLatestEpoch
    , getLatestEpochProtocolParams
    , getLedgerGenesis
    , getNetworkInfo
    , getNextBlocks'
    , getPoolHistory'
    , getTx
    , getTxDelegations
    , getTxMetadataJSON
    , getTxStakes
    , getTxUtxos
    , getTxWithdrawals
    , listPools'
    , paged
    )
import Cardano.BM.Tracing
    ( HasSeverityAnnotation (getSeverityAnnotation)
    , Severity (Debug)
    , Tracer
    , traceWith
    )
import Cardano.Wallet.Network.Light
    ( Consensual )
import Cardano.Wallet.Shelley.Network.Blockfrost.Monad
    ( BFM, consensual404, empty404, maybe404 )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( when )
import Data.IORef
    ( newIORef, readIORef, writeIORef )
import Data.Maybe
    ( listToMaybe )
import Data.Text.Class
    ( ToText (toText) )
import Data.Time.Clock
    ( nominalDiffTimeToSeconds )
import Data.Time.Clock.POSIX
    ( POSIXTime, getPOSIXTime )

import qualified Cardano.Wallet.Shelley.Network.Blockfrost.Monad as BFM

data BlockfrostLayer m = BlockfrostLayer
    { bfGetLatestBlock ::
        m Block
    , bfGetLatestEpoch ::
        m EpochInfo
    , bfGetLatestEpochProtocolParams ::
        m ProtocolParams
    , bfGetPoolHistory ::
        PoolId -> Paged -> SortOrder -> m [PoolHistory]
    , bfGetLedgerGenesis ::
        m Genesis
    , bfGetAccount ::
        Address -> m (Maybe AccountInfo)
    , bfGetBlockSlot ::
        Slot -> m Block
    , bfGetBlockAtHeight ::
        Integer -> m Block
    , bfGetBlockAfterHash ::
        BlockHash -> m (Consensual (Maybe Block))
    , bfGetBlocksAfterHash ::
        BlockHash -> m (Consensual [Block])
    , bfGetBlockTxs ::
        BlockHash -> m [TxHash]
    , bfGetTx ::
        TxHash -> m Transaction
    , bfGetTxStakes ::
        TxHash -> m [TransactionStake]
    , bfGetTxUtxos ::
        TxHash -> m TransactionUtxos
    , bfGetTxWithdrawals ::
        TxHash -> m [TransactionWithdrawal]
    , bfGetTxDelegations ::
        TxHash -> m [TransactionDelegation]
    , bfGetTxMetadataJSON ::
        TxHash -> m [TransactionMetaJSON]
    , bfGetAddressTransactions ::
        Address
        -> Maybe BlockIndex
        -> Maybe BlockIndex
        -> m [AddressTransaction]
    , bfGetAccountRegistrations ::
        Address -> m [AccountRegistration]
    , bfGetAccountDelegations ::
        Address -> m [AccountDelegation]
    , bfGetAccountWithdrawals ::
        Address -> m [AccountWithdrawal]
    , bfGetNetworkInfo ::
        m Network
    , bfListPools ::
        m [PoolId]
    }

blockfrostLayer :: BlockfrostLayer BFM
blockfrostLayer = BlockfrostLayer
    { bfGetLatestBlock = getLatestBlock
    , bfGetLatestEpoch = getLatestEpoch
    , bfGetLatestEpochProtocolParams = getLatestEpochProtocolParams
    , bfGetPoolHistory = getPoolHistory'
    , bfGetLedgerGenesis = getLedgerGenesis
    , bfGetAccount = maybe404 . getAccount
    , bfGetBlockSlot = getBlockSlot
    , bfGetBlockAtHeight = getBlock . Left
    , bfGetBlockAfterHash =
        consensual404 . (listToMaybe <$>) . (`getNextBlocks'` paged 1 1) . Right
    , bfGetBlocksAfterHash =
        consensual404 . allPages' . getNextBlocks' . Right
    , bfGetBlockTxs = \a -> allPages' \p -> getBlockTxs' (Right a) p Ascending
    , bfGetTx = getTx
    , bfGetTxStakes = getTxStakes
    , bfGetTxUtxos = getTxUtxos
    , bfGetTxWithdrawals = getTxWithdrawals
    , bfGetTxDelegations = getTxDelegations
    , bfGetTxMetadataJSON = getTxMetadataJSON
    , bfGetAddressTransactions = \a f t ->
        empty404 $ allPages' \p -> getAddressTransactions' a p Ascending f t
    , bfGetAccountRegistrations = \a ->
        empty404 $ allPages' \p -> getAccountRegistrations' a p Ascending
    , bfGetAccountDelegations = \a ->
        empty404 $ allPages' \p -> getAccountDelegations' a p Ascending
    , bfGetAccountWithdrawals = \a ->
        empty404 $ allPages' \p -> getAccountWithdrawals' a p Ascending
    , bfGetNetworkInfo = getNetworkInfo
    , bfListPools = allPages' (`listPools'` Ascending)
    }

hoistBlockfrostLayer ::
    BlockfrostLayer BFM -> (forall a. BFM a -> IO a) -> BlockfrostLayer IO
hoistBlockfrostLayer BlockfrostLayer{..} nt =
    BlockfrostLayer
    { bfGetLatestBlock = nt bfGetLatestBlock
    , bfGetLatestEpoch = nt bfGetLatestEpoch
    , bfGetLatestEpochProtocolParams = nt bfGetLatestEpochProtocolParams
    , bfGetPoolHistory = ((nt .) .) . bfGetPoolHistory
    , bfGetLedgerGenesis = nt bfGetLedgerGenesis
    , bfGetAccount = nt . bfGetAccount
    , bfGetBlockSlot = nt . bfGetBlockSlot
    , bfGetBlockAtHeight = nt . bfGetBlockAtHeight
    , bfGetBlockAfterHash = nt . bfGetBlockAfterHash
    , bfGetBlocksAfterHash = nt . bfGetBlocksAfterHash
    , bfGetBlockTxs = nt . bfGetBlockTxs
    , bfGetTx = nt . bfGetTx
    , bfGetTxStakes = nt . bfGetTxStakes
    , bfGetTxUtxos = nt . bfGetTxUtxos
    , bfGetTxWithdrawals = nt . bfGetTxWithdrawals
    , bfGetTxDelegations = nt . bfGetTxDelegations
    , bfGetTxMetadataJSON = nt . bfGetTxMetadataJSON
    , bfGetAddressTransactions = ((nt .) .) . bfGetAddressTransactions
    , bfGetAccountRegistrations = nt . bfGetAccountRegistrations
    , bfGetAccountDelegations = nt . bfGetAccountDelegations
    , bfGetAccountWithdrawals = nt . bfGetAccountWithdrawals
    , bfGetNetworkInfo = nt bfGetNetworkInfo
    , bfListPools = nt bfListPools
    }

rateLimitedBlockfrostLayer :: Tracer IO Log -> Project -> IO (BlockfrostLayer IO)
rateLimitedBlockfrostLayer tr project = do
    bfConfig <- BFM.newClientConfig project
    r <- newIORef . (,rps) =<< getPOSIXTime
    pure $ hoistBlockfrostLayer blockfrostLayer \bfm -> do
        (prev, n) <- readIORef r
        time <- getPOSIXTime
        let remaining = if floorSec time /= floorSec prev then rps else n
        when (remaining < 1) do
            let pause = toMicros $ nextSecond time - time
            traceWith tr $ RateLimiting pause
            threadDelay pause
        res <- BFM.run bfConfig bfm
        writeIORef r (time, remaining - 1)
        pure res
  where
    rps :: Int
    rps = 8

    floorSec :: POSIXTime -> Integer
    floorSec = floor . nominalDiffTimeToSeconds

    toMicros :: POSIXTime -> Int
    toMicros = truncate . (* 1_000_000) . nominalDiffTimeToSeconds

    nextSecond :: POSIXTime -> POSIXTime
    nextSecond =
        fromIntegral @Integer . truncate . (+ 1) . nominalDiffTimeToSeconds

newtype Log = RateLimiting Int

instance ToText Log where
    toText = \case
        RateLimiting micros ->
            "Pausing for " <> toText (show micros) <> " microseconds..."

instance HasSeverityAnnotation Log where
    getSeverityAnnotation = \case RateLimiting{} -> Debug

-- | Query all results, until we get less than maximum items per page.
-- Until https://github.com/blockfrost/blockfrost-haskell/issues/26 is fixed.
allPages' :: Monad m => (Paged -> m [a]) -> m [a]
allPages' act = do
    let fetch page' = do
            res <- act page'
            case res of
                xs | length xs < maxPageSize -> pure xs
                xs -> do
                    next <- fetch (nextPage page')
                    pure $ xs <> next
    fetch Paged { countPerPage = maxPageSize, pageNumber = 1 }
  where
    nextPage :: Paged -> Paged
    nextPage p = p { pageNumber = 1 + pageNumber p }
    maxPageSize = 100
