{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Provides wallet layer functions that are used by API layer. Uses both
-- "Cardano.Wallet.DB" and "Cardano.Wallet.Network" to realize its role as
-- being intermediary between the three.

module Cardano.Wallet
    (
    -- * Types
      WalletLayer
    , newWalletLayer
    , BlockchainParameters (..)

    -- * Wallet
    , attachPrivateKey
    , createWallet
    , listUtxoStatistics
    , listWallets
    , readWallet
    , removeWallet
    , restoreWallet
    , updateWallet
    , updateWalletPassphrase
    , ErrWalletAlreadyExists (..)
    , ErrNoSuchWallet (..)
    , ErrListUTxOStatistics (..)
    , ErrUpdatePassphrase (..)

    -- * Address
    , listAddresses

    -- * Transaction
    , createUnsignedTx
    , estimateTxFee
    , listTransactions
    , signTx
    , submitExternalTx
    , submitTx
    , ErrCreateUnsignedTx (..)
    , ErrEstimateTxFee (..)
    , ErrSignTx (..)
    , ErrMkStdTx (..)
    , ErrAdjustForFee (..)
    , ErrCoinSelection (..)
    , ErrSubmitTx (..)
    , ErrSubmitExternalTx (..)
    , ErrPostTx (..)
    , ErrDecodeExternalTx (..)
    , ErrValidateSelection
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , ErrListTransactions (..)
    , ErrNetworkUnavailable (..)
    , ErrStartTimeLaterThanEndTime (..)
    ) where

import Prelude hiding
    ( log )

import Cardano.BM.Trace
    ( Trace, appendName, logDebug, logError, logInfo, logNotice )
import Cardano.Wallet.DB
    ( DBLayer
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.Network
    ( ErrDecodeExternalTx (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (RootK)
    , ErrWrongPassphrase (..)
    , Passphrase
    , WalletKey (..)
    , XPrv
    , checkPassphrase
    , encryptPassphrase
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionOptions (..)
    , ErrCoinSelection (..)
    , shuffle
    )
import Cardano.Wallet.Primitive.Fee
    ( ErrAdjustForFee (..)
    , Fee (..)
    , FeeOptions (..)
    , FeePolicy
    , adjustForFee
    , computeFee
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , applyBlocks
    , availableUTxO
    , blockHeight
    , currentTip
    , getPending
    , getState
    , initWallet
    , newPending
    , updateState
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , DefineTx (..)
    , Direction (..)
    , EpochLength (..)
    , Hash (..)
    , Range (..)
    , SlotId (..)
    , SlotLength (..)
    , SlotParameters (..)
    , SortOrder (..)
    , StartTime (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , TxWitness
    , UTxOStatistics
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , computeUtxoStatistics
    , log10
    , slotDifference
    , slotRangeFromTimeRange
    , slotRatio
    , slotStartTime
    , wholeRange
    )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), ErrValidateSelection, TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Concurrent
    ( ThreadId, forkIO, killThread, threadDelay )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newMVar )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, forM_, when )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), maybeToExceptT )
import Control.Monad.Trans.State
    ( runState, state )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Foldable
    ( fold )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( typed )
import Data.List.NonEmpty
    ( NonEmpty ((:|)) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( toText )
import Data.Time.Clock
    ( UTCTime, getCurrentTime )
import Data.Word
    ( Word16, Word32 )
import Fmt
    ( Buildable, blockListF, pretty, (+|), (+||), (|+), (||+) )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Cardano.Wallet.DB as DB
import qualified Cardano.Wallet.Primitive.CoinSelection.Random as CoinSelection
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                 Types
-------------------------------------------------------------------------------}

data WalletLayer s t (k :: Depth -> * -> *)
    = WalletLayer
        (Trace IO Text)
        (BlockchainParameters t)
        (DBLayer IO s t k)
        (NetworkLayer t IO)
        (TransactionLayer t k)
        WorkerRegistry

-- | Create a new instance of the wallet layer.
newWalletLayer
    :: Trace IO Text
    -> BlockchainParameters t
    -> DBLayer IO s t k
    -> NetworkLayer t IO
    -> TransactionLayer t k
    -> IO (WalletLayer s t k)
newWalletLayer tr bp db nw tl =
    WalletLayer tr bp db nw tl <$> newRegistry

data BlockchainParameters t = BlockchainParameters
    { getGenesisBlock :: Block (Tx t)
        -- ^ Very first block.
    , getGenesisBlockDate :: StartTime
        -- ^ Start time of the chain.
    , getFeePolicy :: FeePolicy
        -- ^ Policy regarding transaction fee.
    , getSlotLength :: SlotLength
        -- ^ Length, in seconds, of a slot.
    , getEpochLength :: EpochLength
        -- ^ Number of slots in a single epoch.
    , getTxMaxSize :: Quantity "byte" Word16
        -- ^ Maximum size of a transaction (soft or hard limit).
    , getEpochStability :: Quantity "block" Word32
        -- ^ Length of the suffix of the chain considered unstable
    } deriving (Generic)

{-------------------------------------------------------------------------------
                                   Wallet
-------------------------------------------------------------------------------}

-- | Initialise and store a new wallet, returning its ID
createWallet
    :: (Show s, NFData s, IsOurs s, DefineTx t)
    => WalletLayer s t k
    -> WalletId
    -> WalletName
    -> s
    -> ExceptT ErrWalletAlreadyExists IO WalletId
createWallet (WalletLayer _ bp db _ _ _) wid wname s = do
    let checkpoint = initWallet (getGenesisBlock bp) s
    currentTime <- liftIO getCurrentTime
    let metadata = WalletMetadata
            { name = wname
            , creationTime = currentTime
            , passphraseInfo = Nothing
            , status = Restoring minBound
            , delegation = NotDelegating
            }
    DB.createWallet db (PrimaryKey wid) checkpoint metadata $> wid

-- | Retrieve the wallet state for the wallet with the given ID.
readWallet
    :: WalletLayer s t k
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (Wallet s t, WalletMetadata)
readWallet w wid = (,)
   <$> readWalletCheckpoint w wid
   <*> readWalletMeta w wid

-- | Retrieve a wallet's most recent checkpoint
readWalletCheckpoint
    :: WalletLayer s t k
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (Wallet s t)
readWalletCheckpoint (WalletLayer _ _ db _ _ _) wid =
    maybeToExceptT (ErrNoSuchWallet wid) $ do
        MaybeT $ DB.readCheckpoint db (PrimaryKey wid)

-- | Retrieve only metadata associated with a particular wallet
readWalletMeta
    :: WalletLayer s t k
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO WalletMetadata
readWalletMeta (WalletLayer _ _ db _ _ _) wid =
    maybeToExceptT (ErrNoSuchWallet wid) $
        MaybeT $ DB.readWalletMeta db (PrimaryKey wid)

-- | Update a wallet's metadata with the given update function.
updateWallet
    :: WalletLayer s t k
    -> WalletId
    -> (WalletMetadata -> WalletMetadata)
    -> ExceptT ErrNoSuchWallet IO ()
updateWallet w@(WalletLayer _ _ db _ _ _) wid modify =
    DB.withLock db $ do
        meta <- readWalletMeta w wid
        DB.putWalletMeta db (PrimaryKey wid) (modify meta)

-- | Change a wallet's passphrase to the given passphrase.
updateWalletPassphrase
  :: WalletKey k
  => WalletLayer s t k
  -> WalletId
  -> (Passphrase "encryption-old", Passphrase "encryption-new")
  -> ExceptT ErrUpdatePassphrase IO ()
updateWalletPassphrase w wid (old, new) =
    withRootKey w wid (coerce old) ErrUpdatePassphraseWithRootKey $ \xprv ->
        withExceptT ErrUpdatePassphraseNoSuchWallet $ do
            let xprv' = changePassphrase old new xprv
            attachPrivateKey w wid (xprv', coerce new)

-- | Retrieve a list of known wallet IDs.
listWallets
    :: WalletLayer s t k
    -> IO [WalletId]
listWallets (WalletLayer _ _ db _ _ _) =
    fmap (\(PrimaryKey wid) -> wid) <$> DB.listWallets db

-- | List the wallet's UTxO statistics.
listUtxoStatistics
    :: forall s t k. (DefineTx t)
    => WalletLayer s t k
    -> WalletId
    -> ExceptT ErrListUTxOStatistics IO UTxOStatistics
listUtxoStatistics w wid = do
    (wal, _) <- withExceptT
        ErrListUTxOStatisticsNoSuchWallet (readWallet w wid)
    let utxo = availableUTxO @s @t wal
    pure $ computeUtxoStatistics log10 utxo

-- | Remove an existing wallet. Note that there's no particular work to
-- be done regarding the restoration worker as it will simply terminate
-- on the next tick when noticing that the corresponding wallet is gone.
removeWallet
    :: WalletLayer s t k
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
removeWallet (WalletLayer _ _ db _ _ re) wid = do
    DB.withLock db . DB.removeWallet db . PrimaryKey $ wid
    liftIO $ cancelWorker re wid

-- | Restore a wallet from its current tip up to the network tip.
--
-- This function returns immediately, starting a worker thread in the
-- background that will fetch and apply remaining blocks until the
-- network tip is reached or until failure.
restoreWallet
    :: DefineTx t
    => WalletLayer s t k
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
restoreWallet w@(WalletLayer tr _ _ nw _ re) wid = do
    (cp, _) <- readWallet w wid
    let workerName = "worker." <> T.take 8 (toText wid)
        t = appendName workerName tr
    liftIO $ logInfo t $ "Restoring wallet "+| wid |+"..."
    worker <- liftIO $ forkIO $ do
        runExceptT (networkTip nw) >>= \case
            Left e -> do
                logError t $ "Failed to get network tip: " +|| e ||+ ""
                restoreSleep w wid (currentTip cp)
            Right tip -> do
                restoreStep w wid (currentTip cp, tip)
    liftIO $ registerWorker re (wid, worker)

-- | Infinite restoration loop. We drain the whole available chain and try
-- to catch up with the node. In case of error, we log it and wait a bit
-- before retrying.
--
-- The function only terminates if the wallet has disappeared from the DB.
restoreStep
    :: forall s t k. (DefineTx t)
    => WalletLayer s t k
    -> WalletId
    -> (BlockHeader, (BlockHeader, Quantity "block" Natural))
    -> IO ()
restoreStep w@(WalletLayer t _ _ nw _ _) wid (localTip, (nodeTip, nodeHeight)) = do
    runExceptT (nextBlocks nw localTip) >>= \case
        Left e -> do
            logError t $ "Failed to get next blocks: " +|| e ||+ "."
            restoreSleep w wid localTip
        Right [] -> do
            logDebug t "Wallet restored."
            restoreSleep w wid localTip
        Right (blockFirst : blocksRest) -> do
            let blocks = blockFirst :| blocksRest
            let nextLocalTip = view #header . NE.last $ blocks
            let measuredTip = (nodeTip ^. #slotId, nodeHeight)
            let action = restoreBlocks w wid blocks measuredTip
            runExceptT action >>= \case
                Left (ErrNoSuchWallet _) ->
                    logNotice t "Wallet is gone! Terminating worker..."
                Right () -> do
                    restoreStep w wid (nextLocalTip, (nodeTip, nodeHeight))

-- | Wait a short delay before querying for blocks again. We also take this
-- opportunity to refresh the chain tip as it has probably increased in
-- order to refine our syncing status.
restoreSleep
    :: forall s t k. (DefineTx t)
    => WalletLayer s t k
    -> WalletId
    -> BlockHeader
    -> IO ()
restoreSleep w@(WalletLayer t bp _ nw _ _) wid localTip = do
    -- NOTE: Conversion functions will treat 'NominalDiffTime' as
    -- picoseconds
    let (SlotLength s) = (bp :: BlockchainParameters t) ^. typed @SlotLength
    let halfSlotLengthDelay = fromEnum s `div` 2000000
    threadDelay halfSlotLengthDelay
    runExceptT (networkTip nw) >>= \case
        Left e -> do
            logError t $ "Failed to get network tip: " +|| e ||+ ""
            restoreSleep w wid localTip
        Right nodeTip ->
            restoreStep w wid (localTip, nodeTip)

-- | Apply the given blocks to the wallet and update the wallet state,
-- transaction history and corresponding metadata.
restoreBlocks
    :: forall s t k. (DefineTx t)
    => WalletLayer s t k
    -> WalletId
    -> NonEmpty (Block (Tx t))
    -> (SlotId, Quantity "block" Natural) -- ^ Network tip and height
    -> ExceptT ErrNoSuchWallet IO ()
restoreBlocks w@(WalletLayer t bp db _ _ _) wid blocks (nodeTip, Quantity nodeHeight) = do
    let (slotFirst, slotLast) =
            ( view #slotId . header . NE.head $ blocks
            , view #slotId . header . NE.last $ blocks
            )
    liftIO $ logInfo t $
        "Applying blocks ["+| slotFirst |+" ... "+| slotLast |+"]"

    -- NOTE:
    -- Not as good as a transaction, but, with the lock, nothing can make
    -- the wallet disappear within these calls, so either the wallet is
    -- there and they all succeed, or it's not and they all fail.
    DB.withLock db $ do
        (wallet, meta) <- readWallet w wid
        let (txs, cps) = NE.unzip $ applyBlocks @s @t blocks wallet
        let newTxs = fold txs

        let calculateMetadata :: SlotId -> WalletMetadata
            calculateMetadata slot = meta { status = status' }
              where
                progress' = slotRatio epochLength slot nodeTip
                status' =
                    if progress' == maxBound
                    then Ready
                    else Restoring progress'

        -- NOTE:
        -- We cast `k` and `nodeHeight` to 'Integer' since at a given point
        -- in time, `k` may be greater than the tip.
        let (Quantity k) = epochStability
        let bhUnstable :: Integer
            bhUnstable = fromIntegral nodeHeight - fromIntegral k
        forM_ (NE.init cps) $ \cp -> do
            let (Quantity bh) = blockHeight cp
            when (fromIntegral bh >= bhUnstable) $
                DB.putCheckpoint db (PrimaryKey wid) cp

        -- NOTE:
        -- Always store the last checkpoint from the batch and all new
        -- transactions.
        let cpLast = NE.last cps
        let Quantity bhLast = blockHeight cpLast
        let meta' = calculateMetadata (view #slotId $ currentTip cpLast)
        DB.putCheckpoint db (PrimaryKey wid) cpLast
        DB.putTxHistory db (PrimaryKey wid) newTxs
        DB.putWalletMeta db (PrimaryKey wid) meta'

        liftIO $ do
            logInfo t $ ""
                +|| pretty (calculateMetadata slotLast)
            logInfo t $ "number of pending transactions: "
                +|| Set.size (getPending cpLast) ||+ ""
            logInfo t $ "number of new transactions: "
                +|| length newTxs ||+ ""
            logInfo t $ "new block height: "
                +|| bhLast ||+ ""
            logDebug t $ "blocks: "
                <> pretty (NE.toList blocks)
            logDebug t $ "transactions: "
                <> pretty (blockListF (snd <$> Map.elems newTxs))
  where
    BlockchainParameters _ _ _ _ epochLength _ epochStability = bp

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

-- | List all addresses of a wallet with their metadata. Addresses
-- are ordered from the most-recently-discovered to the oldest known.
listAddresses
  :: forall s t k. (IsOurs s, CompareDiscovery s, KnownAddresses s, DefineTx t)
  => WalletLayer s t k
  -> WalletId
  -> ExceptT ErrNoSuchWallet IO [(Address, AddressState)]
listAddresses w@(WalletLayer _ _ db _ _ _) wid = do
    (s, txs) <- DB.withLock db $ (,)
        <$> (getState <$> readWalletCheckpoint w wid)
        <*> liftIO (DB.readTxHistory db
            (PrimaryKey wid) Descending wholeRange)
    let maybeIsOurs (TxOut a _) = if fst (isOurs a s)
            then Just a
            else Nothing
    let usedAddrs = Set.fromList $
            concatMap (mapMaybe maybeIsOurs . outputs' . snd) txs
          where
            outputs' (tx, _) = W.outputs @t tx
    let knownAddrs =
            L.sortBy (compareDiscovery s) (knownAddresses s)
    let withAddressState addr =
            (addr, if addr `Set.member` usedAddrs then Used else Unused)
    return $ withAddressState <$> knownAddrs

{-------------------------------------------------------------------------------
                                  Transaction
-------------------------------------------------------------------------------}

coinSelOpts
    :: TransactionLayer t k
    -> Quantity "byte" Word16
    -> CoinSelectionOptions (ErrValidateSelection t)
coinSelOpts tl txMaxSize = CoinSelectionOptions
    { maximumNumberOfInputs = estimateMaxNumberOfInputs tl txMaxSize
    , validate = validateSelection tl
    }

feeOpts
    :: TransactionLayer t k
    -> FeePolicy
    -> FeeOptions
feeOpts tl feePolicy = FeeOptions
    { estimate = computeFee feePolicy . estimateSize tl
    , dustThreshold = minBound
    }

-- | Prepare a transaction and automatically select inputs from the
-- wallet to cover the requested outputs. Note that this only runs
-- coin selection for the given outputs. In order to construct (and
-- sign) an actual transaction, use 'signTx'.
createUnsignedTx
  :: forall e s t k. (DefineTx t, e ~ ErrValidateSelection t)
  => WalletLayer s t k
  -> WalletId
  -> NonEmpty TxOut
  -> ExceptT (ErrCreateUnsignedTx e) IO CoinSelection
createUnsignedTx w@(WalletLayer t bp _ _ tl _) wid recipients = do
    (wal, _) <- withExceptT ErrCreateUnsignedTxNoSuchWallet (readWallet w wid)
    let utxo = availableUTxO @s @t wal
    (sel, utxo') <- withExceptT ErrCreateUnsignedTxCoinSelection $
        CoinSelection.random (coinSelOpts tl txMaxSize) recipients utxo
    liftIO . logInfo t $ "Coins selected for transaction: \n"+| sel |+""
    withExceptT ErrCreateUnsignedTxFee $ do
        debug t "Coins after fee adjustment"
            =<< adjustForFee (feeOpts tl feePolicy) utxo' sel
  where
    txMaxSize = bp ^. typed @(Quantity "byte" Word16)
    feePolicy = bp ^. typed @FeePolicy

-- | Estimate a transaction fee by automatically selecting inputs from
-- the wallet to cover the requested outputs.
estimateTxFee
  :: forall e s t k. (DefineTx t, e ~ ErrValidateSelection t)
  => WalletLayer s t k
  -> WalletId
  -> NonEmpty TxOut
  -> ExceptT (ErrEstimateTxFee e) IO Fee
estimateTxFee w@(WalletLayer _ bp _ _ tl _) wid recipients = do
    (wal, _) <- withExceptT ErrEstimateTxFeeNoSuchWallet (readWallet w wid)
    let utxo = availableUTxO @s @t wal
    (sel, _utxo') <- withExceptT ErrEstimateTxFeeCoinSelection $
        CoinSelection.random (coinSelOpts tl txMaxSize) recipients utxo
    let estimateFee = computeFee feePolicy . estimateSize tl
    pure $ estimateFee sel
  where
    txMaxSize = bp ^. typed @(Quantity "byte" Word16)
    feePolicy = bp ^. typed @FeePolicy

-- | Produce witnesses and construct a transaction from a given
-- selection. Requires the encryption passphrase in order to decrypt
-- the root private key. Note that this doesn't broadcast the
-- transaction to the network. In order to do so, use 'submitTx'.
signTx
  :: (Show s, NFData s, IsOwned s k, GenChange s)
  => WalletLayer s t k
  -> WalletId
  -> Passphrase "encryption"
  -> CoinSelection
  -> ExceptT ErrSignTx IO (Tx t, TxMeta, [TxWitness])
signTx w@(WalletLayer _ _ db _ tl _) wid pwd (CoinSelection ins outs chgs) =
    DB.withLock db $ do
        (wal, _) <- withExceptT ErrSignTxNoSuchWallet $ readWallet w wid
        let (changeOuts, s') = flip runState (getState wal) $
                forM chgs $ \c -> do
                    addr <- state (genChange pwd)
                    return $ TxOut addr c
        allShuffledOuts <- liftIO $ shuffle (outs ++ changeOuts)
        withRootKey w wid pwd ErrSignTxWithRootKey $ \xprv -> do
            let keyFrom = isOwned (getState wal) (xprv, pwd)
            case mkStdTx tl keyFrom ins allShuffledOuts of
                Right (tx, wit) -> do
                    -- Safe because we have a lock and we already fetched the
                    -- wallet within this context.
                    liftIO . unsafeRunExceptT $
                        DB.putCheckpoint db (PrimaryKey wid) (updateState s' wal)
                    let amtChng = fromIntegral $
                            sum (getCoin <$> chgs)
                    let amtInps = fromIntegral $
                            sum (getCoin . coin . snd <$> ins)
                    let meta = TxMeta
                            { status = Pending
                            , direction = Outgoing
                            , slotId = (currentTip wal) ^. #slotId
                            , amount = Quantity (amtInps - amtChng)
                            }
                    return (tx, meta, wit)
                Left e ->
                    throwE $ ErrSignTx e

-- | Broadcast a (signed) transaction to the network.
submitTx
  :: WalletLayer s t k
  -> WalletId
  -> (Tx t, TxMeta, [TxWitness])
  -> ExceptT ErrSubmitTx IO ()
submitTx w@(WalletLayer _ _ db nw _ _) wid (tx, meta, wit)= do
    withExceptT ErrSubmitTxNetwork $ postTx nw (tx, wit)
    DB.withLock db $ withExceptT ErrSubmitTxNoSuchWallet $ do
        (wal, _) <- readWallet w wid
        DB.putCheckpoint db (PrimaryKey wid) (newPending (tx, meta) wal)

-- | Broadcast an externally-signed transaction to the network.
submitExternalTx
  :: WalletLayer s t k
  -> ByteString
  -> ExceptT ErrSubmitExternalTx IO (Tx t)
submitExternalTx (WalletLayer _ _ _ nw _ _) bytes = do
    txWithWit@(tx,_) <- withExceptT ErrSubmitExternalTxDecode $
        decodeExternalTx nw bytes
    withExceptT ErrSubmitExternalTxNetwork $ postTx nw txWithWit
    return tx

-- | List all transactions and metadata from history for a given wallet.
listTransactions
  :: forall s t k. DefineTx t
  => WalletLayer s t k
  -> WalletId
  -> Maybe UTCTime
      -- Inclusive minimum time bound.
  -> Maybe UTCTime
      -- Inclusive maximum time bound.
  -> SortOrder
  -> ExceptT ErrListTransactions IO [TransactionInfo]
listTransactions w@(WalletLayer _ bp db _ _ _) wid mStart mEnd order = do
    maybe (pure []) listTransactionsWithinRange =<< getSlotRange
  where
    -- Transforms the user-specified time range into a slot range. If the
    -- user-specified range terminates before the start of the blockchain,
    -- returns 'Nothing'.
    getSlotRange
        :: ExceptT ErrListTransactions IO (Maybe (Range SlotId))
    getSlotRange = case (mStart, mEnd) of
        (Just start, Just end) | start > end -> do
            let err = ErrStartTimeLaterThanEndTime start end
            throwE (ErrListTransactionsStartTimeLaterThanEndTime err)
        _ ->
            pure $ slotRangeFromTimeRange sp $ Range mStart mEnd

    listTransactionsWithinRange
        :: Range SlotId -> ExceptT ErrListTransactions IO [TransactionInfo]
    listTransactionsWithinRange sr = do
        (wal, _) <- withExceptT ErrListTransactionsNoSuchWallet $
            readWallet w wid
        let tip = currentTip wal ^. #slotId
        liftIO $ assemble tip
            <$> DB.readTxHistory db (PrimaryKey wid) order sr

    -- This relies on DB.readTxHistory returning all necessary transactions
    -- to assemble coin selection information for outgoing payments.
    -- To reliably provide this information, it should be looked up when
    -- applying blocks, but that is future work (issue #573).
    assemble
        :: SlotId
        -> [(Hash "Tx", (Tx t, TxMeta))]
        -> [TransactionInfo]
    assemble tip txs = map mkTxInfo txs
      where
        mkTxInfo (txid, (tx, meta)) = TransactionInfo
            { txInfoId = txid
            , txInfoInputs =
                [(txIn, lookupOutput txIn) | txIn <- W.inputs @t tx]
            , txInfoOutputs = W.outputs @t tx
            , txInfoMeta = meta
            , txInfoDepth =
                slotDifference sp tip (meta ^. #slotId)
            , txInfoTime = txTime (meta ^. #slotId)
            }
        txOuts = Map.fromList
            [ (txid, W.outputs @t tx)
            | (txid, (tx, _)) <- txs ]
        -- Because we only track the UTxO of this wallet, we can only
        -- return this information for outgoing payments.
        lookupOutput (TxIn txid index) =
            Map.lookup txid txOuts >>= atIndex (fromIntegral index)
        atIndex i xs = if i < length xs then Just (xs !! i) else Nothing
        -- Get the approximate time of a transaction, given its 'SlotId'.
        -- We assume that the transaction "happens" at the start of the
        -- slot. This is purely arbitrary and in practice, any time between
        -- the start of a slot and its end could be a valid candidate.
        txTime = slotStartTime sp

    sp :: SlotParameters
    sp = SlotParameters
        (bp ^. typed @EpochLength)
        (bp ^. typed @SlotLength)
        (bp ^. typed @StartTime)

{-------------------------------------------------------------------------------
                                  Key Store
-------------------------------------------------------------------------------}

-- | Attach a given private key to a wallet. The private key is
-- necessary for some operations like signing transactions, or
-- generating new accounts.
attachPrivateKey
    :: WalletLayer s t k
    -> WalletId
    -> (k 'RootK XPrv, Passphrase "encryption")
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKey w@(WalletLayer _ _ db _ _ _) wid (xprv, pwd) = do
   hpwd <- liftIO $ encryptPassphrase pwd
   DB.putPrivateKey db (PrimaryKey wid) (xprv, hpwd)
   DB.withLock db $ do
       meta <- readWalletMeta w wid
       now <- liftIO getCurrentTime
       let modify x =
               x { passphraseInfo = Just (WalletPassphraseInfo now) }
       DB.putWalletMeta db (PrimaryKey wid) (modify meta)

-- | Execute an action which requires holding a root XPrv.
withRootKey
    :: forall e a s t k. ()
    => WalletLayer s t k
    -> WalletId
    -> Passphrase "encryption"
    -> (ErrWithRootKey -> e)
    -> (k 'RootK XPrv -> ExceptT e IO a)
    -> ExceptT e IO a
withRootKey (WalletLayer _ _ db _ _ _) wid pwd embed action = do
    xprv <- withExceptT embed $ do
        lift (DB.readPrivateKey db (PrimaryKey wid)) >>= \case
            Nothing ->
                throwE $ ErrWithRootKeyNoRootKey wid
            Just (xprv, hpwd) -> do
                withExceptT (ErrWithRootKeyWrongPassphrase wid) $ ExceptT $
                    return $ checkPassphrase pwd hpwd
                return xprv
    action xprv

{-------------------------------------------------------------------------------
                                Worker Registry
-------------------------------------------------------------------------------}

-- | A simple registry to keep track of worker threads created for wallet
-- restoration. This way, we can clean up worker threads early and don't have
-- to wait for them to fail with an error message about the wallet being gone.
newtype WorkerRegistry = WorkerRegistry (MVar (Map WalletId ThreadId))

newRegistry :: IO WorkerRegistry
newRegistry = WorkerRegistry <$> newMVar mempty

registerWorker :: WorkerRegistry -> (WalletId, ThreadId) -> IO ()
registerWorker (WorkerRegistry mvar) (wid, threadId) =
    modifyMVar_ mvar (pure . Map.insert wid threadId)

cancelWorker :: WorkerRegistry -> WalletId -> IO ()
cancelWorker (WorkerRegistry mvar) wid =
    modifyMVar_ mvar (Map.alterF alterF wid)
  where
    alterF = \case
        Nothing -> pure Nothing
        Just threadId -> do
            -- NOTE: It is safe to kill a thread that is already dead.
            killThread threadId
            return Nothing

{-------------------------------------------------------------------------------
                                   Errors
-------------------------------------------------------------------------------}

-- | Errors that can occur when creating an unsigned transaction.
data ErrCreateUnsignedTx e
    = ErrCreateUnsignedTxNoSuchWallet ErrNoSuchWallet
    | ErrCreateUnsignedTxCoinSelection (ErrCoinSelection e)
    | ErrCreateUnsignedTxFee ErrAdjustForFee
    deriving (Show, Eq)

-- | Errors that can occur when estimating transaction fees.
data ErrEstimateTxFee e
    = ErrEstimateTxFeeNoSuchWallet ErrNoSuchWallet
    | ErrEstimateTxFeeCoinSelection (ErrCoinSelection e)
    deriving (Show, Eq)

-- | Errors that can occur when listing UTxO statistics.
newtype ErrListUTxOStatistics
    = ErrListUTxOStatisticsNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors that can occur when signing a transaction.
data ErrSignTx
    = ErrSignTx ErrMkStdTx
    | ErrSignTxNoSuchWallet ErrNoSuchWallet
    | ErrSignTxWithRootKey ErrWithRootKey
    deriving (Show, Eq)

-- | Errors that can occur when submitting a signed transaction to the network.
data ErrSubmitTx
    = ErrSubmitTxNetwork ErrPostTx
    | ErrSubmitTxNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors that can occur when submitting an externally-signed transaction
--   to the network.
data ErrSubmitExternalTx
    = ErrSubmitExternalTxNetwork ErrPostTx
    | ErrSubmitExternalTxDecode ErrDecodeExternalTx
    deriving (Show, Eq)

-- | Errors that can occur when trying to change a wallet's passphrase.
data ErrUpdatePassphrase
    = ErrUpdatePassphraseNoSuchWallet ErrNoSuchWallet
    | ErrUpdatePassphraseWithRootKey ErrWithRootKey
    deriving (Show, Eq)

-- | Errors that can occur when trying to perform an operation on a wallet that
-- requires a private key, but where none is attached to the wallet.
data ErrWithRootKey
    = ErrWithRootKeyNoRootKey WalletId
    | ErrWithRootKeyWrongPassphrase WalletId ErrWrongPassphrase
    deriving (Show, Eq)

-- | Errors that can occur when trying to list transactions.
data ErrListTransactions
    = ErrListTransactionsNoSuchWallet ErrNoSuchWallet
    | ErrListTransactionsStartTimeLaterThanEndTime ErrStartTimeLaterThanEndTime
    deriving (Show, Eq)

-- | Indicates that the specified start time is later than the specified end
-- time.
data ErrStartTimeLaterThanEndTime = ErrStartTimeLaterThanEndTime
    { errStartTime :: UTCTime
    , errEndTime :: UTCTime
    } deriving (Show, Eq)

{-------------------------------------------------------------------------------
                                   Utils
-------------------------------------------------------------------------------}

debug :: (Buildable a, MonadIO m) => Trace IO Text -> Text -> a -> m a
debug t msg a =
    liftIO (logDebug t (msg <> pretty a)) $> a
