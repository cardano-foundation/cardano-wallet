{-# LANGUAGE DataKinds #-}
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
-- License: MIT
--
-- Provides the wallet layer functions that are used by API layer and uses both
-- "Cardano.Wallet.DB" and "Cardano.Wallet.Network" to realize its role as being
-- intermediary between the three.

module Cardano.Wallet
    (
    -- * Interface
      WalletLayer (..)
    , BlockchainParameters (..)

    -- * Errors
    , ErrAdjustForFee (..)
    , ErrCoinSelection (..)
    , ErrCreateUnsignedTx (..)
    , ErrEstimateTxFee (..)
    , ErrListTransactions (..)
    , ErrListUTxOStatistics (..)
    , ErrMkStdTx (..)
    , ErrNetworkUnavailable (..)
    , ErrNoSuchWallet (..)
    , ErrPostTx (..)
    , ErrSignTx (..)
    , ErrStartTimeLaterThanEndTime (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrValidateSelection
    , ErrWalletAlreadyExists (..)
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)

    -- * Construction
    , newWalletLayer
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
    ( ErrNetworkUnavailable (..), ErrPostTx (..), NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (RootK)
    , ErrWrongPassphrase (..)
    , Key
    , Passphrase
    , XPrv
    , changePassphrase
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
    , SlotId (..)
    , SlotId (..)
    , SlotLength (..)
    , SlotLength (..)
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
    , slotRatio
    , slotStartTime
    , wholeRange
    )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), ErrValidateSelection, TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Arrow
    ( first )
import Control.Concurrent
    ( ThreadId, forkIO, killThread, threadDelay )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newMVar )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, unless, when )
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
import Data.Coerce
    ( coerce )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty )
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
    ( Word16 )
import Fmt
    ( Buildable, blockListF, pretty, (+|), (+||), (|+), (||+) )

import qualified Cardano.Wallet.DB as DB
import qualified Cardano.Wallet.Primitive.CoinSelection.Random as CoinSelection
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                 Types
-------------------------------------------------------------------------------}

data WalletLayer s t = WalletLayer
    { createWallet
        :: (Show s, NFData s, IsOurs s, DefineTx t)
        => WalletId
        -> WalletName
        -> s
        -> ExceptT ErrWalletAlreadyExists IO WalletId
        -- ^ Initialise and store a new wallet, returning its Id.

    , readWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet s t, WalletMetadata)
        -- ^ Retrieve the wallet state for the wallet with the given ID.

    , updateWallet
        :: WalletId
        -> (WalletMetadata -> WalletMetadata)
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Update the wallet metadata with the given update function.

    , updateWalletPassphrase
        :: WalletId
        -> (Passphrase "encryption-old", Passphrase "encryption-new")
        -> ExceptT ErrUpdatePassphrase IO ()
        -- ^ Change the wallet passphrase to the given passphrase.

    , listWallets
        :: IO [WalletId]
        -- ^ Retrieve a list of known wallets IDs.

    , removeWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Remove an existing wallet. Note that, there's no particular work to
        -- be done regarding the restoration worker as it will simply terminate
        -- on the next tick when noticing that the corresponding wallet is gone.

    , restoreWallet
        :: (DefineTx t)
        => WalletId
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Restore a wallet from its current tip up to a given target
        -- (typically, the network tip).
        --
        -- It returns immediately and fail if the wallet is already beyond the
        -- given tip. It starts a worker in background which will fetch and
        -- apply remaining blocks until failure or, the target slot is reached.

    , listAddresses
        :: (IsOurs s, CompareDiscovery s, KnownAddresses s, DefineTx t)
        => WalletId
        -> ExceptT ErrNoSuchWallet IO [(Address, AddressState)]
        -- ^ List all addresses of a wallet with their metadata. Addresses
        -- are ordered from the most recently discovered to the oldest known.

    , createUnsignedTx
        :: forall e. (DefineTx t, e ~ ErrValidateSelection t)
        => WalletId
        -> NonEmpty TxOut
        -> ExceptT (ErrCreateUnsignedTx e) IO CoinSelection
        -- ^ Prepare a transaction and automatically select inputs from the
        -- wallet to cover the requested outputs. Note that this only run the
        -- coin selection for the given outputs. In order to construct (and
        -- sign) an actual transaction, have a look at 'signTx'.

    , estimateTxFee
        :: forall e. (DefineTx t, e ~ ErrValidateSelection t)
        => WalletId
        -> NonEmpty TxOut
        -> ExceptT (ErrEstimateTxFee e) IO Fee
        -- ^ Estimate a transaction fee by automatically selecting inputs from
        -- the wallet to cover the requested outputs.

    , listUtxoStatistics
        :: (DefineTx t)
        => WalletId
        -> ExceptT ErrListUTxOStatistics IO UTxOStatistics
        -- ^ List the wallet's UTxO statistics.

    , signTx
        :: (Show s, NFData s, IsOwned s, GenChange s)
        => WalletId
        -> Passphrase "encryption"
        -> CoinSelection
        -> ExceptT ErrSignTx IO (Tx t, TxMeta, [TxWitness])
        -- ^ Produce witnesses and construct a transaction from a given
        -- selection. Requires the encryption passphrase in order to decrypt
        -- the root private key. Note that this doesn't broadcast the
        -- transaction to the network. In order to do so, have a look at
        -- 'submitTx'.

    , submitTx
        :: (DefineTx t)
        => WalletId
        -> (Tx t, TxMeta, [TxWitness])
        -> ExceptT ErrSubmitTx IO ()
        -- ^ Broadcast a (signed) transaction to the network.

    , listTransactions
        :: DefineTx t
        => WalletId
        -> Maybe UTCTime
            -- Start time
        -> Maybe UTCTime
            -- End time
        -> SortOrder
        -> ExceptT ErrListTransactions IO [TransactionInfo]
        -- ^ List all transactions and metadata from history for a given wallet.
        --
        -- The result is sorted on 'slotId' in descending order. The most recent
        -- transaction comes first.

    , attachPrivateKey
        :: WalletId
        -> (Key 'RootK XPrv, Passphrase "encryption")
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Attach a given private key to a wallet. The private key is
        -- necessary for some operations like signing transactions or,
        -- generating new accounts.

    }

-- | Errors occuring when creating an unsigned transaction
data ErrCreateUnsignedTx e
    = ErrCreateUnsignedTxNoSuchWallet ErrNoSuchWallet
    | ErrCreateUnsignedTxCoinSelection (ErrCoinSelection e)
    | ErrCreateUnsignedTxFee ErrAdjustForFee
    deriving (Show, Eq)

-- | Errors occuring when estimating transaction fee
data ErrEstimateTxFee e
    = ErrEstimateTxFeeNoSuchWallet ErrNoSuchWallet
    | ErrEstimateTxFeeCoinSelection (ErrCoinSelection e)
    deriving (Show, Eq)

newtype ErrListUTxOStatistics
    = ErrListUTxOStatisticsNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors occuring when signing a transaction
data ErrSignTx
    = ErrSignTx ErrMkStdTx
    | ErrSignTxNoSuchWallet ErrNoSuchWallet
    | ErrSignTxWithRootKey ErrWithRootKey
    deriving (Show, Eq)

-- | Errors occuring when submitting a signed transaction to the network
data ErrSubmitTx
    = ErrSubmitTxNetwork ErrPostTx
    | ErrSubmitTxNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors occuring when trying to change a wallet's passphrase
data ErrUpdatePassphrase
    = ErrUpdatePassphraseNoSuchWallet ErrNoSuchWallet
    | ErrUpdatePassphraseWithRootKey ErrWithRootKey
    deriving (Show, Eq)

-- | Errors occuring when trying to perform an operation on a wallet which
-- requires a private key, but none is attached to the wallet
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
    { startTime :: UTCTime
    , endTime :: UTCTime
    } deriving (Show, Eq)

{-------------------------------------------------------------------------------
                                Worker Registry
-------------------------------------------------------------------------------}

-- | A simple registry to keep track of worker threads created for wallet
-- restoration. This way, we can clean-up workers threads early and don't have
-- to wait for them to fail with a an error message about the wallet being gone.
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
                                 Construction
-------------------------------------------------------------------------------}

data BlockchainParameters t = BlockchainParameters
    { getGenesisBlock :: Block (Tx t)
        -- ^ Very first block
    , getGenesisBlockDate :: StartTime
        -- ^ Start time of the chain
    , getFeePolicy :: FeePolicy
        -- ^ Policy regarding transcation fee
    , getSlotLength :: SlotLength
        -- ^ Length, in seconds, of a slot
    , getEpochLength :: EpochLength
        -- ^ Number of slots in a single epoch
    , getTxMaxSize :: Quantity "byte" Word16
        -- ^ Maximum size of a transaction (soft or hard limit)
    }

-- | Create a new instance of the wallet layer.
newWalletLayer
    :: forall s t. (Buildable (Tx t))
    => Trace IO Text
    -> BlockchainParameters t
    -> DBLayer IO s t
    -> NetworkLayer t IO
    -> TransactionLayer t
    -> IO (WalletLayer s t)
newWalletLayer tracer bp db nw tl = do
    logDebugT $ "Wallet layer starting with: "
        <> "block0: "+| block0 |+ ", "
        <> "fee policy: "+|| feePolicy ||+""
    registry <- newRegistry
    return WalletLayer
        { createWallet = _createWallet
        , readWallet = _readWallet
        , updateWallet = _updateWallet
        , updateWalletPassphrase = _updateWalletPassphrase
        , listWallets = _listWallets
        , removeWallet = _removeWallet registry
        , restoreWallet = _restoreWallet registry
        , listAddresses = _listAddresses
        , createUnsignedTx = _createUnsignedTx
        , estimateTxFee = _estimateTxFee
        , listUtxoStatistics = _listUtxoStatistics
        , signTx = _signTx
        , submitTx = _submitTx
        , attachPrivateKey = _attachPrivateKey
        , listTransactions = _listTransactions
        }
  where
    BlockchainParameters
        block0
        block0Date
        feePolicy
        (SlotLength slotLength)
        slotsPerEpoch
        txMaxSize = bp

    logDebugT :: MonadIO m => Text -> m ()
    logDebugT = liftIO . logDebug tracer

    logInfoT :: MonadIO m => Text -> m ()
    logInfoT = liftIO . logInfo tracer

    debug :: (Buildable a, MonadIO m) => Text -> a -> m a
    debug msg a = logDebugT (msg <> pretty a) $> a

    {---------------------------------------------------------------------------
                                       Wallets
    ---------------------------------------------------------------------------}

    _createWallet
        :: (Show s, NFData s, IsOurs s, DefineTx t)
        => WalletId
        -> WalletName
        -> s
        -> ExceptT ErrWalletAlreadyExists IO WalletId
    _createWallet wid wname s = do
        let checkpoint = initWallet block0 s
        currentTime <- liftIO getCurrentTime
        let metadata = WalletMetadata
                { name = wname
                , creationTime = currentTime
                , passphraseInfo = Nothing
                , status = Restoring minBound
                , delegation = NotDelegating
                }
        DB.createWallet db (PrimaryKey wid) checkpoint metadata $> wid

    _readWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet s t, WalletMetadata)
    _readWallet wid = (,)
        <$> _readWalletCheckpoint wid
        <*> _readWalletMeta wid

    _readWalletMeta
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO WalletMetadata
    _readWalletMeta wid = maybeToExceptT (ErrNoSuchWallet wid) $
        MaybeT $ DB.readWalletMeta db (PrimaryKey wid)

    _readWalletCheckpoint
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet s t)
    _readWalletCheckpoint wid = maybeToExceptT (ErrNoSuchWallet wid) $ do
        MaybeT $ DB.readCheckpoint db (PrimaryKey wid)

    _updateWallet
        :: WalletId
        -> (WalletMetadata -> WalletMetadata)
        -> ExceptT ErrNoSuchWallet IO ()
    _updateWallet wid modify = DB.withLock db $ do
        meta <- _readWalletMeta wid
        DB.putWalletMeta db (PrimaryKey wid) (modify meta)

    _updateWalletPassphrase
        :: WalletId
        -> (Passphrase "encryption-old", Passphrase "encryption-new")
        -> ExceptT ErrUpdatePassphrase IO ()
    _updateWalletPassphrase wid (old, new) = do
        withRootKey wid (coerce old) ErrUpdatePassphraseWithRootKey $ \xprv ->
            withExceptT ErrUpdatePassphraseNoSuchWallet $ do
                let xprv' = changePassphrase old new xprv
                _attachPrivateKey wid (xprv', coerce new)

    _listWallets
        :: IO [WalletId]
    _listWallets =
        fmap (\(PrimaryKey wid) -> wid) <$> DB.listWallets db

    _removeWallet
        :: WorkerRegistry
        -> WalletId
        -> ExceptT ErrNoSuchWallet IO ()
    _removeWallet re wid = do
        DB.withLock db . DB.removeWallet db . PrimaryKey $ wid
        liftIO $ cancelWorker re wid

    _restoreWallet
        :: (DefineTx t)
        => WorkerRegistry
        -> WalletId
        -> ExceptT ErrNoSuchWallet IO ()
    _restoreWallet re wid = do
        (w, _) <- _readWallet wid
        let workerName = "worker." <> T.take 8 (toText wid)
            t = appendName workerName tracer
        liftIO $ logInfo t $ "Restoring wallet "+| wid |+"..."
        worker <- liftIO $ forkIO $ do
            runExceptT (networkTip nw) >>= \case
                Left e -> do
                    logError t $ "Failed to get network tip: " +|| e ||+ ""
                    restoreSleep t wid (currentTip w)
                Right tip -> do
                    restoreStep t wid (currentTip w, tip)
        liftIO $ registerWorker re (wid, worker)

    _listUtxoStatistics
        :: (DefineTx t)
        => WalletId
        -> ExceptT ErrListUTxOStatistics IO UTxOStatistics
    _listUtxoStatistics wid = do
        (w, _) <- withExceptT
            ErrListUTxOStatisticsNoSuchWallet (_readWallet wid)
        let utxo = availableUTxO @s @t w
        pure $ computeUtxoStatistics log10 utxo

    -- | Infinite restoration loop. We drain the whole available chain and try
    -- to catch up with the node. In case of error, we log it and wait a bit
    -- before retrying.
    --
    -- The function only terminates if the wallet has disappeared from the DB.
    restoreStep
        :: (DefineTx t)
        => Trace IO Text
        -> WalletId
        -> (BlockHeader, BlockHeader)
        -> IO ()
    restoreStep t wid (slot, tip) = do
        runExceptT (nextBlocks nw slot) >>= \case
            Left e -> do
                logError t $ "Failed to get next blocks: " +|| e ||+ "."
                restoreSleep t wid slot
            Right [] -> do
                logDebug t "Wallet restored."
                restoreSleep t wid slot
            Right blocks -> do
                let next = view #header . last $ blocks
                runExceptT (restoreBlocks t wid blocks (tip ^. #slotId)) >>=
                    \case
                        Left (ErrNoSuchWallet _) ->
                            logNotice t "Wallet is gone! Terminating worker..."
                        Right () -> do
                            restoreStep t wid (next, tip)

    -- | Wait a short delay before querying for blocks again. We do take this
    -- opportunity to also refresh the chain tip as it has probably increased
    -- in order to refine our syncing status.
    restoreSleep
        :: (DefineTx t)
        => Trace IO Text
        -> WalletId
        -> BlockHeader
        -> IO ()
    restoreSleep t wid slot = do
        -- NOTE: Conversion functions will treat 'NominalDiffTime' as
        -- picoseconds
        let halfSlotLengthDelay = fromEnum slotLength `div` 2000000
        threadDelay halfSlotLengthDelay
        runExceptT (networkTip nw) >>= \case
            Left e -> do
                logError t $ "Failed to get network tip: " +|| e ||+ ""
                restoreSleep t wid slot
            Right tip ->
                restoreStep t wid (slot, tip)

    -- | Apply the given blocks to the wallet and update the wallet state,
    -- transaction history and corresponding metadata.
    restoreBlocks
        :: (DefineTx t)
        => Trace IO Text
        -> WalletId
        -> [Block (Tx t)]
        -> SlotId -- ^ Network tip
        -> ExceptT ErrNoSuchWallet IO ()
    restoreBlocks t wid blocks tip = do
        let (inf, sup) =
                ( view #slotId . header . head $ blocks
                , view #slotId . header . last $ blocks
                )
        liftIO $ logInfo t $ "Applying blocks ["+| inf |+" ... "+| sup |+"]"

        -- NOTE
        -- Not as good as a transaction, but, with the lock, nothing can make
        -- the wallet disappear within these calls, so either the wallet is
        -- there and they all succeed, or it's not and they all fail.
        DB.withLock db $ do
            (cp, meta) <- _readWallet wid
            -- NOTE
            -- We only process non-empty blocks, though we still keep the last
            -- block of the list, even if empty, so that we correctly update the
            -- current tip of the wallet state.
            let nonEmpty = not . null . transactions
            let (h,q) = first (filter nonEmpty) $
                    splitAt (length blocks - 1) blocks
            liftIO $ logDebug t $ pretty (h ++ q)
            let (txs, cp') = applyBlocks @s @t (h ++ q) cp
            let progress = slotRatio slotsPerEpoch sup tip
            let status' = if progress == maxBound
                    then Ready
                    else Restoring progress
            let meta' = meta { status = status' } :: WalletMetadata
            let nPending = Set.size (getPending cp')
            liftIO $ logInfo t $ pretty meta'
            liftIO $ logInfo t $ nPending ||+" transaction(s) pending."
            liftIO $ logInfo t $
                length txs ||+ " new transaction(s) discovered."
            unless (null txs) $ liftIO $ logDebug t $
                pretty $ blockListF (snd <$> Map.elems txs)
            DB.putCheckpoint db (PrimaryKey wid) cp'
            DB.putTxHistory db (PrimaryKey wid) txs
            DB.putWalletMeta db (PrimaryKey wid) meta'

    {---------------------------------------------------------------------------
                                     Addresses
    ---------------------------------------------------------------------------}

    -- NOTE
    -- This implementation is rather inneficient and not intented for frequent
    -- use, in particular for exchanges or "big-players".
    _listAddresses
        :: (IsOurs s, CompareDiscovery s, KnownAddresses s, DefineTx t)
        => WalletId
        -> ExceptT ErrNoSuchWallet IO [(Address, AddressState)]
    _listAddresses wid = do
        (s, txs) <- DB.withLock db $ (,)
            <$> (getState <$> _readWalletCheckpoint wid)
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

    {---------------------------------------------------------------------------
                                    Transactions
    ---------------------------------------------------------------------------}

    coinSelOpts :: CoinSelectionOptions (ErrValidateSelection t)
    coinSelOpts = CoinSelectionOptions
        { maximumNumberOfInputs = estimateMaxNumberOfInputs tl txMaxSize
        , validate = validateSelection tl
        }

    feeOpts :: FeeOptions
    feeOpts = FeeOptions
            { estimate = computeFee feePolicy . estimateSize tl
            , dustThreshold = minBound
            }

    _createUnsignedTx
        :: forall e. (DefineTx t, e ~ ErrValidateSelection t)
        => WalletId
        -> NonEmpty TxOut
        -> ExceptT (ErrCreateUnsignedTx e) IO CoinSelection
    _createUnsignedTx wid recipients = do
        (w, _) <- withExceptT ErrCreateUnsignedTxNoSuchWallet (_readWallet wid)
        let utxo = availableUTxO @s @t w
        (sel, utxo') <- withExceptT ErrCreateUnsignedTxCoinSelection $
            CoinSelection.random coinSelOpts recipients utxo
        logInfoT $ "Coins selected for transaction: \n"+| sel |+""
        withExceptT ErrCreateUnsignedTxFee $ do
            debug "Coins after fee adjustment"
                =<< adjustForFee feeOpts utxo' sel

    _estimateTxFee
        :: forall e. (DefineTx t, e ~ ErrValidateSelection t)
        => WalletId
        -> NonEmpty TxOut
        -> ExceptT (ErrEstimateTxFee e) IO Fee
    _estimateTxFee wid recipients = do
        (w, _) <- withExceptT ErrEstimateTxFeeNoSuchWallet (_readWallet wid)
        let utxo = availableUTxO @s @t w
        (sel, _utxo') <- withExceptT ErrEstimateTxFeeCoinSelection $
            CoinSelection.random coinSelOpts recipients utxo
        let estimateFee = computeFee feePolicy . estimateSize tl
        pure $ estimateFee sel

    _listTransactions
        :: (DefineTx t)
        => WalletId
        -> Maybe UTCTime
        -> Maybe UTCTime
        -> SortOrder
        -> ExceptT ErrListTransactions IO [TransactionInfo]
    _listTransactions wid mStart mEnd order = do
        (w, _) <- withExceptT ErrListTransactionsNoSuchWallet $ _readWallet wid
        case (mStart, mEnd) of
            (Just start, Just end) -> when (start > end) $ throwE $
                ErrListTransactionsStartTimeLaterThanEndTime $
                    ErrStartTimeLaterThanEndTime start end
            _ -> pure ()
        let tip = currentTip w ^. #slotId
        liftIO $ assemble tip
            <$> DB.readTxHistory db (PrimaryKey wid)
                order wholeRange
      where
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
                    slotDifference slotsPerEpoch tip (meta ^. #slotId)
                , txInfoTime = txTime (meta ^. #slotId)
                }
            txOuts = Map.fromList
                [ (txid, W.outputs @t tx)
                | (txid, (tx, _)) <- txs ]
            -- Because we only track UTxO of this wallet, we can only return
            -- this information for outgoing payments.
            lookupOutput (TxIn txid index) =
                Map.lookup txid txOuts >>= atIndex (fromIntegral index)
            atIndex i xs = if i < length xs then Just (xs !! i) else Nothing
            -- Get the approximate time of a transaction, given its 'SlotId'. We
            -- assume that the transaction "happens" at the start of the
            -- slot. This is purely arbitrary and in practice, any time between
            -- the start of a slot and the end could be a validate candidate.
            txTime = slotStartTime
                slotsPerEpoch
                (SlotLength slotLength)
                block0Date

    _signTx
        :: (Show s, NFData s, IsOwned s, GenChange s)
        => WalletId
        -> Passphrase "encryption"
        -> CoinSelection
        -> ExceptT ErrSignTx IO (Tx t, TxMeta, [TxWitness])
    _signTx wid pwd (CoinSelection ins outs chgs) = DB.withLock db $ do
        (w, _) <- withExceptT ErrSignTxNoSuchWallet $ _readWallet wid
        let (changeOuts, s') = flip runState (getState w) $ forM chgs $ \c -> do
                addr <- state genChange
                return $ TxOut addr c
        allShuffledOuts <- liftIO $ shuffle (outs ++ changeOuts)
        withRootKey wid pwd ErrSignTxWithRootKey $ \xprv -> do
            let keyFrom = isOwned (getState w) (xprv, pwd)
            case mkStdTx tl keyFrom ins allShuffledOuts of
                Right (tx, wit) -> do
                    -- Safe because we have a lock and we already fetched the
                    -- wallet within this context.
                    liftIO . unsafeRunExceptT $
                        DB.putCheckpoint db (PrimaryKey wid) (updateState s' w)
                    let amtChng = fromIntegral $
                            sum (getCoin <$> chgs)
                    let amtInps = fromIntegral $
                            sum (getCoin . coin . snd <$> ins)
                    let meta = TxMeta
                            { status = Pending
                            , direction = Outgoing
                            , slotId = (currentTip w) ^. #slotId
                            , amount = Quantity (amtInps - amtChng)
                            }
                    return (tx, meta, wit)
                Left e ->
                    throwE $ ErrSignTx e

    _submitTx
        :: WalletId
        -> (Tx t, TxMeta, [TxWitness])
        -> ExceptT ErrSubmitTx IO ()
    _submitTx wid (tx, meta, wit) = do
        withExceptT ErrSubmitTxNetwork $ postTx nw (tx, wit)
        DB.withLock db $ withExceptT ErrSubmitTxNoSuchWallet $ do
            (w, _) <- _readWallet wid
            DB.putCheckpoint db (PrimaryKey wid) (newPending (tx, meta) w)

    {---------------------------------------------------------------------------
                                     Keystore
    ---------------------------------------------------------------------------}

    _attachPrivateKey
        :: WalletId
        -> (Key 'RootK XPrv, Passphrase "encryption")
        -> ExceptT ErrNoSuchWallet IO ()
    _attachPrivateKey wid (xprv, pwd) = do
        hpwd <- liftIO $ encryptPassphrase pwd
        DB.putPrivateKey db (PrimaryKey wid) (xprv, hpwd)
        DB.withLock db $ do
            meta <- _readWalletMeta wid
            now <- liftIO getCurrentTime
            let modify x =
                    x { passphraseInfo = Just (WalletPassphraseInfo now) }
            DB.putWalletMeta db (PrimaryKey wid) (modify meta)

    -- | Execute an action which requires holding a root XPrv
    withRootKey
        :: forall e a. ()
        => WalletId
        -> Passphrase "encryption"
        -> (ErrWithRootKey -> e)
        -> (Key 'RootK XPrv -> ExceptT e IO a)
        -> ExceptT e IO a
    withRootKey wid pwd embed action = do
        xprv <- withExceptT embed $ do
            lift (DB.readPrivateKey db (PrimaryKey wid)) >>= \case
                Nothing ->
                    throwE $ ErrWithRootKeyNoRootKey wid
                Just (xprv, hpwd) -> do
                    withExceptT (ErrWithRootKeyWrongPassphrase wid) $ ExceptT $
                        return $ checkPassphrase pwd hpwd
                    return xprv
        action xprv
