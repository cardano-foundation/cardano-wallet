{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Provides wallet layer functions that are used by API layer. Uses both
-- "Cardano.Wallet.DB" and "Cardano.Wallet.Network" to realize its role as
-- being intermediary between the three.
--
-- Functions of the wallet layer are often parameterized with variables
-- following the convention below:
--
-- - @s@: A __s__tate used to keep track of known addresses. Typically, possible
--   values for this parameter are described in 'Cardano.Wallet.AddressDiscovery' sub-modules.
--   For instance @SeqState@ or @Rnd State@.
--
-- - @t@: A __t__arget backend which captures details specific to a particular chain
--   producer (binary formats, fee policy, networking layer).
--
-- - @k@: A __k__ey derivation scheme intrisically connected to the underlying discovery
--   state @s@. This describes how the hierarchical structure of a wallet is
--   defined as well as the relationship between secret keys and public
--   addresses.

module Cardano.Wallet
    (
    -- * Developement
    -- $Development

    -- * WalletLayer
        WalletLayer (..)

    -- * Capabilities
    -- $Capabilities
    , HasDBLayer
    , dbLayer
    , HasLogger
    , logger
    , HasNetworkLayer
    , networkLayer
    , HasTransactionLayer
    , transactionLayer
    , HasGenesisData
    , genesisData

    -- * Interface
    -- ** Wallet
    , createWallet
    , attachPrivateKey
    , listUtxoStatistics
    , readWallet
    , deleteWallet
    , restoreWallet
    , updateWallet
    , updateWalletPassphrase
    , walletSyncProgress
    , ErrWalletAlreadyExists (..)
    , ErrNoSuchWallet (..)
    , ErrListUTxOStatistics (..)
    , ErrUpdatePassphrase (..)

    -- ** Address
    , listAddresses

    -- ** Transaction
    , createUnsignedTx
    , estimateTxFee
    , forgetPendingTx
    , listTransactions
    , signTx
    , submitExternalTx
    , submitTx
    , createMigrationSourceData
    , assignMigrationTargetAddresses
    , ErrCreateUnsignedTx (..)
    , ErrEstimateTxFee (..)
    , ErrSignTx (..)
    , ErrMkStdTx (..)
    , ErrAdjustForFee (..)
    , ErrCoinSelection (..)
    , ErrSubmitTx (..)
    , ErrSubmitExternalTx (..)
    , ErrRemovePendingTx (..)
    , ErrPostTx (..)
    , ErrDecodeSignedTx (..)
    , ErrValidateSelection
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , ErrListTransactions (..)
    , ErrNetworkUnavailable (..)
    , ErrStartTimeLaterThanEndTime (..)

    -- ** Root Key
    , withRootKey
    ) where

import Prelude hiding
    ( log )

import Cardano.BM.Trace
    ( Trace, logDebug, logInfo )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrRemovePendingTx (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , sparseCheckpoints
    )
import Cardano.Wallet.Network
    ( ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , FollowAction (..)
    , NetworkLayer (..)
    , follow
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
import Cardano.Wallet.Primitive.CoinSelection.Migration
    ( idealBatchSize, selectCoinsForMigration )
import Cardano.Wallet.Primitive.Fee
    ( ErrAdjustForFee (..)
    , Fee (..)
    , FeeOptions (..)
    , adjustForFee
    , computeFee
    )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..)
    , Wallet
    , applyBlocks
    , availableUTxO
    , blockchainParameters
    , currentTip
    , getState
    , initWallet
    , slotParams
    , updateState
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Direction (..)
    , FeePolicy (LinearFee)
    , Hash (..)
    , Range (..)
    , SlotId (..)
    , SlotParameters (..)
    , SortOrder (..)
    , SyncProgress (..)
    , TransactionInfo (..)
    , Tx
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
    , computeUtxoStatistics
    , log10
    , slotRangeFromTimeRange
    , slotStartTime
    , syncProgressRelativeToTime
    , wholeRange
    )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..)
    , ErrMkStdTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, forM_, void, when )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, mapExceptT, runExceptT, throwE, withExceptT )
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
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( Lens', view, (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime, getCurrentTime )
import Data.Word
    ( Word16 )
import Fmt
    ( Buildable, blockListF, pretty )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.CoinSelection.Random as CoinSelection
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- $Development
-- __Naming Conventions__
--
-- Components inside a particular context `ctx` can be called via dedicated
-- lenses (see Cardano.Wallet#Capabilities). These components are extracted from the context
-- in a @where@ clause according to the following naming convention:
--
-- - @db = ctx ^. dbLayer \@s \\@k@ for the 'DBLayer'.
-- - @tr = ctx ^. logger@ for the Logger.
-- - @nw = ctx ^. networkLayer \@t@ for the 'NetworkLayer'.
-- - @tl = ctx ^. transactionLayer \\@k@ for the 'TransactionLayer'.
-- - @re = ctx ^. workerRegistry@ for the 'WorkerRegistry'.
--
-- __TroubleShooting__
--
-- @
-- • Overlapping instances for HasType (DBLayer IO s t k) ctx
--     arising from a use of ‘myFunction’
--   Matching instances:
-- @
--
-- Occurs when a particular function is missing a top-level constraint
-- (because it uses another function that demands such constraint). Here,
-- `myFunction` needs its surrounding context `ctx` to have a `DBLayer` but
-- the constraint is missing from its host function.
--
-- __Fix__: Add "@HasDBLayer s t k@" as a class-constraint to the surrounding function.
--
-- @
-- • Overlapping instances for HasType (DBLayer IO s t0 k0) ctx
--     arising from a use of ‘myFunction’
--   Matching givens (or their superclasses):
-- @
--
-- Occurs when a function is called in a context where type-level parameters
-- can be inferred. Here, `myFunction` is called but it is unclear
-- whether the parameter `t0` and `k0` of its context are the same as the ones
-- from the function at the call-site.
--
-- __Fix__: Add type-applications at the call-site "@myFunction \@ctx \@s \\@k@"

data WalletLayer s t (k :: Depth -> * -> *)
    = WalletLayer
        (Trace IO Text)
        (Block, BlockchainParameters)
        (NetworkLayer IO t Block)
        (TransactionLayer t k)
        (DBLayer IO s k)
    deriving (Generic)

{-------------------------------------------------------------------------------
                                 Capabilities
-------------------------------------------------------------------------------}

-- $Capabilities
-- Each function in the wallet layer is defined in function of a non-specialized
-- context `ctx`. That context may require some extra capabilities via
-- class-constraints in the function signature. Capabilities are expressed in the
-- form of a "@HasXXX@" class-constraints sometimes with extra type parameters.
--
-- For example:
--
-- @
-- listWallets
--     :: forall ctx s t k.
--         ( HasDBLayer s t k ctx
--         )
--     => ctx
--     -> IO [WalletId]
-- @
--
-- Requires that the given context has an access to a database layer 'DBLayer'
-- parameterized over the wallet state, a network target and a key derivation
-- scheme. Components are pulled from the context generically (i.e. the concrete
-- `ctx` must derive 'Generic') using their associated type. The concrete `ctx`
-- is therefore expected to be a product-type of all the necessary components.
--
-- One can build an interface using only a subset of the wallet layer
-- capabilities and functions, for instance, something to fiddle with wallets
-- and their metadata does not require any networking layer.
type HasDBLayer s k = HasType (DBLayer IO s k)

type HasGenesisData = HasType (Block, BlockchainParameters)

type HasLogger = HasType (Trace IO Text)

-- | This module is only interested in one block-, and tx-type. This constraint
-- hides that choice, for some ease of use.
type HasNetworkLayer t = HasType (NetworkLayer IO t Block)

type HasTransactionLayer t k = HasType (TransactionLayer t k)

dbLayer
    :: forall s k ctx. HasDBLayer s k ctx
    => Lens' ctx (DBLayer IO s k)
dbLayer =
    typed @(DBLayer IO s k)

genesisData
    :: forall ctx. HasGenesisData ctx
    => Lens' ctx (Block, BlockchainParameters)
genesisData =
    typed @(Block, BlockchainParameters)

logger
    :: forall ctx. HasLogger ctx
    => Lens' ctx (Trace IO Text)
logger =
    typed @(Trace IO Text)

networkLayer
    :: forall t ctx. (HasNetworkLayer t ctx)
    => Lens' ctx (NetworkLayer IO t Block)
networkLayer =
    typed @(NetworkLayer IO t Block)

transactionLayer
    :: forall t k ctx. (HasTransactionLayer t k ctx)
    => Lens' ctx (TransactionLayer t k)
transactionLayer =
    typed @(TransactionLayer t k)

{-------------------------------------------------------------------------------
                                   Wallet
-------------------------------------------------------------------------------}

-- | Initialise and store a new wallet, returning its ID.
createWallet
    :: forall ctx s k.
        ( HasGenesisData ctx
        , HasDBLayer s k ctx
        , Show s
        , NFData s
        , IsOurs s
        )
    => ctx
    -> WalletId
    -> WalletName
    -> s
    -> ExceptT ErrWalletAlreadyExists IO WalletId
createWallet ctx wid wname s = db & \DBLayer{..} -> do
    let (hist, cp) = initWallet block0 bp s
    now <- lift getCurrentTime
    let meta = WalletMetadata
            { name = wname
            , creationTime = now
            , passphraseInfo = Nothing
            , delegation = NotDelegating
            }
    mapExceptT atomically $
        initializeWallet (PrimaryKey wid) cp meta hist $> wid
  where
    db = ctx ^. dbLayer @s @k
    (block0, bp) = ctx ^. genesisData

-- | Retrieve the wallet state for the wallet with the given ID.
readWallet
    :: forall ctx s k. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (Wallet s, WalletMetadata, Set Tx)
readWallet ctx wid = db & \DBLayer{..} -> mapExceptT atomically $ do
    let pk = PrimaryKey wid
    cp <- withNoSuchWallet wid $ readCheckpoint pk
    meta <- withNoSuchWallet wid $ readWalletMeta pk
    txs <- lift $ readTxHistory pk Descending wholeRange (Just Pending)
    pure (cp, meta, Set.fromList (fst <$> txs))
  where
    db = ctx ^. dbLayer @s @k

walletSyncProgress :: Wallet s -> IO SyncProgress
walletSyncProgress w = do
    let bp = blockchainParameters w
    let h = currentTip w
    syncProgressRelativeToTime (slotParams bp) h <$> getCurrentTime

-- | Update a wallet's metadata with the given update function.
updateWallet
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> (WalletMetadata -> WalletMetadata)
    -> ExceptT ErrNoSuchWallet IO ()
updateWallet ctx wid modify = db & \DBLayer{..} -> mapExceptT atomically $ do
    meta <- withNoSuchWallet wid $ readWalletMeta (PrimaryKey wid)
    putWalletMeta (PrimaryKey wid) (modify meta)
  where
    db = ctx ^. dbLayer @s @k

-- | Change a wallet's passphrase to the given passphrase.
updateWalletPassphrase
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        , WalletKey k
        )
    => ctx
    -> WalletId
    -> (Passphrase "encryption-old", Passphrase "encryption-new")
    -> ExceptT ErrUpdatePassphrase IO ()
updateWalletPassphrase ctx wid (old, new) =
    withRootKey @ctx @s @k ctx wid (coerce old) ErrUpdatePassphraseWithRootKey
        $ \xprv -> withExceptT ErrUpdatePassphraseNoSuchWallet $ do
            let xprv' = changePassphrase old new xprv
            attachPrivateKey @ctx @s @k ctx wid (xprv', coerce new)

-- | List the wallet's UTxO statistics.
listUtxoStatistics
    :: forall ctx s k. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> ExceptT ErrListUTxOStatistics IO UTxOStatistics
listUtxoStatistics ctx wid = do
    (wal, _, pending) <- withExceptT
        ErrListUTxOStatisticsNoSuchWallet (readWallet @ctx @s @k ctx wid)
    let utxo = availableUTxO @s pending wal
    pure $ computeUtxoStatistics log10 utxo

-- | Restore a wallet from its current tip up to the network tip.
--
-- This function returns immediately, starting a worker thread in the
-- background that will fetch and apply remaining blocks until the
-- network tip is reached or until failure.
restoreWallet
    :: forall ctx s t k.
        ( HasLogger ctx
        , HasNetworkLayer t ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
restoreWallet ctx wid = db & \DBLayer{..} -> do
    cps <- liftIO $ atomically $ listCheckpoints (PrimaryKey wid)
    let forward bs h = run $ restoreBlocks @ctx @s @k ctx wid bs h
    let backward sid = run $ mapExceptT atomically $ rollbackTo (PrimaryKey wid) sid
    void $ liftIO $ follow nw tr cps forward backward (view #header)
  where
    db = ctx ^. dbLayer @s @k
    nw = ctx ^. networkLayer @t
    tr = ctx ^. logger

    run :: ExceptT ErrNoSuchWallet IO () -> IO (FollowAction ErrNoSuchWallet)
    run = fmap (either ExitWith (const Continue)) . runExceptT

-- | Apply the given blocks to the wallet and update the wallet state,
-- transaction history and corresponding metadata.
restoreBlocks
    :: forall ctx s k.
        ( HasLogger ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> NonEmpty Block
    -> BlockHeader
    -> ExceptT ErrNoSuchWallet IO ()
restoreBlocks ctx wid blocks nodeTip = db & \DBLayer{..} -> do
    (cp, meta) <- mapExceptT atomically $ (,)
        <$> withNoSuchWallet wid (readCheckpoint $ PrimaryKey wid)
        <*> withNoSuchWallet wid (readWalletMeta $ PrimaryKey wid)
    let bp = blockchainParameters cp
    let (txs0, cps) = NE.unzip $ applyBlocks @s blocks cp
    let txs = fold txs0
    let k = bp ^. #getEpochStability
    let localTip = currentTip $ NE.last cps

    let unstable = sparseCheckpoints k (nodeTip ^. #blockHeight)
    mapExceptT atomically $ forM_ (NE.init cps) $ \cp' -> do
        let (Quantity h) = currentTip cp ^. #blockHeight
        when (fromIntegral h `elem` unstable) $ do
            liftIO $ logCheckpoint cp'
            putCheckpoint (PrimaryKey wid) cp'

    mapExceptT atomically $ do
        liftIO $ logCheckpoint cp
        putCheckpoint (PrimaryKey wid) (NE.last cps)
        prune (PrimaryKey wid)
        putTxHistory (PrimaryKey wid) txs

    liftIO $ do
        progress <- walletSyncProgress (NE.last cps)
        logInfo tr $
            pretty meta
        logInfo tr $ "syncProgress: " <> pretty progress
        logInfo tr $ "discovered "
            <> pretty (length txs) <> " new transaction(s)"
        logInfo tr $ "local tip: "
            <> pretty localTip
        logDebug tr $ "blocks: "
            <> pretty (NE.toList blocks)
        logDebug tr $ "transactions: "
            <> pretty (blockListF (snd <$> txs))
  where
    db = ctx ^. dbLayer @s @k
    tr = ctx ^. logger
    logCheckpoint :: Wallet s -> IO ()
    logCheckpoint cp = logInfo tr $
        "Creating checkpoint at " <> pretty (currentTip cp)

-- | Remove an existing wallet. Note that there's no particular work to
-- be done regarding the restoration worker as it will simply terminate
-- on the next tick when noticing that the corresponding wallet is gone.
deleteWallet
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
deleteWallet ctx wid = db & \DBLayer{..} -> do
    mapExceptT atomically $ removeWallet (PrimaryKey wid)
  where
    db = ctx ^. dbLayer @s @k

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

-- | List all addresses of a wallet with their metadata. Addresses
-- are ordered from the most-recently-discovered to the oldest known.
listAddresses
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        , IsOurs s
        , CompareDiscovery s
        , KnownAddresses s
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO [(Address, AddressState)]
listAddresses ctx wid = db & \DBLayer{..} -> do
    (s, txs) <- mapExceptT atomically $ (,)
        <$> (getState <$> withNoSuchWallet wid (readCheckpoint $ PrimaryKey wid))
        <*> lift (readTxHistory (PrimaryKey wid) Descending wholeRange Nothing)
    let maybeIsOurs (TxOut a _) = if fst (isOurs a s)
            then Just a
            else Nothing
    let usedAddrs = Set.fromList $
            concatMap (mapMaybe maybeIsOurs . outputs') txs
          where
            outputs' (tx, _) = W.outputs tx
    let knownAddrs =
            L.sortBy (compareDiscovery s) (knownAddresses s)
    let withAddressState addr =
            (addr, if addr `Set.member` usedAddrs then Used else Unused)
    return $ withAddressState <$> knownAddrs
  where
    db = ctx ^. dbLayer @s @k

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
    { estimateFee = computeFee feePolicy . estimateSize tl
    , dustThreshold = minBound
    }

-- | Prepare a transaction and automatically select inputs from the
-- wallet to cover the requested outputs. Note that this only runs
-- coin selection for the given outputs. In order to construct (and
-- sign) an actual transaction, use 'signTx'.
createUnsignedTx
    :: forall ctx s t k e.
        ( HasTransactionLayer t k ctx
        , HasLogger ctx
        , HasDBLayer s k ctx
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> WalletId
    -> NonEmpty TxOut
    -> ExceptT (ErrCreateUnsignedTx e) IO CoinSelection
createUnsignedTx ctx wid recipients = do
    (wal, _, pending) <- withExceptT ErrCreateUnsignedTxNoSuchWallet $
        readWallet @ctx @s @k ctx wid
    let bp = blockchainParameters wal
    let utxo = availableUTxO @s pending wal
    (sel, utxo') <- withExceptT ErrCreateUnsignedTxCoinSelection $ do
        let opts = coinSelOpts tl (bp ^. #getTxMaxSize)
        CoinSelection.random opts recipients utxo
    liftIO . logInfo tr $ "Coins selected for transaction: \n" <> pretty sel
    withExceptT ErrCreateUnsignedTxFee $ do
        debug tr "Coins after fee adjustment"
            =<< adjustForFee (feeOpts tl (bp ^. #getFeePolicy)) utxo' sel
  where
    tl = ctx ^. transactionLayer @t @k
    tr = ctx ^. logger

-- | Estimate a transaction fee by automatically selecting inputs from
-- the wallet to cover the requested outputs.
estimateTxFee
    :: forall ctx s t k e.
        ( HasTransactionLayer t k ctx
        , HasDBLayer s k ctx
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> WalletId
    -> NonEmpty TxOut
    -> ExceptT (ErrEstimateTxFee e) IO Fee
estimateTxFee ctx wid recipients = do
    (wal, _, pending) <- withExceptT ErrEstimateTxFeeNoSuchWallet $
        readWallet @ctx @s @k ctx wid
    let bp = blockchainParameters wal
    let utxo = availableUTxO @s pending wal
    (sel, _utxo') <- withExceptT ErrEstimateTxFeeCoinSelection $ do
        let opts = coinSelOpts tl (bp ^. #getTxMaxSize)
        CoinSelection.random opts recipients utxo
    pure $ computeFee (bp ^. #getFeePolicy) $ estimateSize tl sel
  where
    tl = ctx ^. transactionLayer @t @k

-- | Constructs a set of coin selections that select all funds from the given
--   source wallet, returning them as change.
--
-- If the coin selections returned by this function are used to create
-- transactions from the given wallet to a target wallet, executing those
-- transactions will have the effect of migrating all funds from the given
-- source wallet to the specified target wallet.
--
createMigrationSourceData
    :: forall ctx s t k.
        ( HasTransactionLayer t k ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
       -- ^ The source wallet ID.
    -> ExceptT ErrNoSuchWallet IO [CoinSelection]
createMigrationSourceData ctx wid = do
    (cp, _, pending) <- readWallet @ctx @s @k ctx wid
    let bp = blockchainParameters cp
    let utxo = availableUTxO @s pending cp
    let feePolicy@(LinearFee (Quantity a) _ _) = bp ^. #getFeePolicy
    let feeOptions = (feeOpts tl feePolicy)
            { dustThreshold = Coin $ ceiling a }
    let selOptions = coinSelOpts tl (bp ^. #getTxMaxSize)
    pure $ selectCoinsForMigration feeOptions (idealBatchSize selOptions) utxo
  where
    tl = ctx ^. transactionLayer @t @k

-- | Transform the given set of migration coin selections (for a source wallet)
--   into a set of coin selections that will migrate funds to the specified
--   target wallet.
--
-- Each change entry in the specified set of coin selections is replaced with a
-- corresponding output entry in the returned set, where the output entry has a
-- address corresponding to the target wallet.
--
-- All addresses generated by this function are also registered as pending
-- change addresses for the target wallet.
--
assignMigrationTargetAddresses
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        , GenChange s
        , IsOurs s
        , NFData s
        , Show s
        )
    => ctx
    -> WalletId
    -- ^ The target wallet ID.
    -> ArgGenChange s
    -- ^ Bits necessary to generate change address for a state @s@
    -> [CoinSelection]
    -- ^ Migration data for the source wallet.
    -> ExceptT ErrNoSuchWallet IO [CoinSelection]
assignMigrationTargetAddresses ctx wid argGenChange cs = db & \DBLayer{..} -> do
    mapExceptT atomically $ do
        cp <- withNoSuchWallet wid $ readCheckpoint (PrimaryKey wid)
        let (cs', s') = flip runState (getState cp) $ do
                forM cs $ \sel -> do
                    outs <- forM (change sel) $ \c -> do
                        addr <- state (genChange argGenChange)
                        pure (TxOut addr c)
                    pure (sel { change = [], outputs = outs })
        putCheckpoint (PrimaryKey wid) (updateState s' cp) $> cs'
  where
    db = ctx ^. dbLayer @s @k

-- | Produce witnesses and construct a transaction from a given
-- selection. Requires the encryption passphrase in order to decrypt
-- the root private key. Note that this doesn't broadcast the
-- transaction to the network. In order to do so, use 'submitTx'.
signTx
    :: forall ctx s t k.
        ( HasTransactionLayer t k ctx
        , HasDBLayer s k ctx
        , Show s
        , NFData s
        , IsOwned s k
        , GenChange s
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> Passphrase "encryption"
    -> CoinSelection
    -> ExceptT ErrSignTx IO (Tx, TxMeta, UTCTime, [TxWitness])
signTx ctx wid argGenChange pwd (CoinSelection ins outs chgs) = db & \DBLayer{..} -> do
    withRootKey @_ @s ctx wid pwd ErrSignTxWithRootKey $ \xprv -> do
        mapExceptT atomically $ do
            cp <- withExceptT ErrSignTxNoSuchWallet $ withNoSuchWallet wid $
                readCheckpoint (PrimaryKey wid)
            let (changeOuts, s') = flip runState (getState cp) $
                    forM chgs $ \c -> do
                        addr <- state (genChange argGenChange)
                        return $ TxOut addr c
            allShuffledOuts <- liftIO $ shuffle (outs ++ changeOuts)
            let keyFrom = isOwned (getState cp) (xprv, pwd)
            (tx, wit) <- either
                (throwE . ErrSignTx)
                pure
                (mkStdTx tl keyFrom ins allShuffledOuts )
            withExceptT ErrSignTxNoSuchWallet $
                putCheckpoint (PrimaryKey wid) (updateState s' cp)
            let amtChng = fromIntegral $
                    sum (getCoin <$> chgs)
            let amtInps = fromIntegral $
                    sum (getCoin . coin . snd <$> ins)
            let txSlot =
                    (currentTip cp) ^. #slotId
            let meta = TxMeta
                    { status = Pending
                    , direction = Outgoing
                    , slotId = txSlot
                    , blockHeight = (currentTip cp) ^. #blockHeight
                    , amount = Quantity (amtInps - amtChng)
                    }
            let time = slotStartTime
                    (slotParams (blockchainParameters cp))
                    txSlot
            return (tx, meta, time, wit)
  where
    db = ctx ^. dbLayer @s @k
    tl = ctx ^. transactionLayer @t @k

-- | Broadcast a (signed) transaction to the network.
submitTx
    :: forall ctx s t k.
        ( HasNetworkLayer t ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> (Tx, TxMeta, [TxWitness])
    -> ExceptT ErrSubmitTx IO ()
submitTx ctx wid (tx, meta, wit) = db & \DBLayer{..} -> do
    withExceptT ErrSubmitTxNetwork $ postTx nw (tx, wit)
    mapExceptT atomically $ withExceptT ErrSubmitTxNoSuchWallet $
        putTxHistory (PrimaryKey wid) [(tx, meta)]
  where
    db = ctx ^. dbLayer @s @k
    nw = ctx ^. networkLayer @t

-- | Broadcast an externally-signed transaction to the network.
submitExternalTx
    :: forall ctx t k.
        ( HasNetworkLayer t ctx
        , HasTransactionLayer t k ctx
        )
    => ctx
    -> ByteString
    -> ExceptT ErrSubmitExternalTx IO Tx
submitExternalTx ctx bytes = do
    txWithWit@(tx,_) <- withExceptT ErrSubmitExternalTxDecode $ except $
        decodeSignedTx tl bytes
    withExceptT ErrSubmitExternalTxNetwork $ postTx nw txWithWit
    return tx
  where
    nw = ctx ^. networkLayer @t
    tl = ctx ^. transactionLayer @t @k

-- | Forget pending transaction.
forgetPendingTx
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> Hash "Tx"
    -> ExceptT ErrRemovePendingTx IO ()
forgetPendingTx ctx wid tid = db & \DBLayer{..} -> do
    mapExceptT atomically $ removePendingTx (PrimaryKey wid) tid
  where
    db = ctx ^. dbLayer @s @k

-- | List all transactions and metadata from history for a given wallet.
listTransactions
    :: forall ctx s k. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> Maybe UTCTime
        -- Inclusive minimum time bound.
    -> Maybe UTCTime
        -- Inclusive maximum time bound.
    -> SortOrder
    -> ExceptT ErrListTransactions IO [TransactionInfo]
listTransactions ctx wid mStart mEnd order = db & \DBLayer{..} -> do
    let pk = PrimaryKey wid
    mapExceptT atomically $ do
        cp <- withExceptT ErrListTransactionsNoSuchWallet $
            withNoSuchWallet wid $ readCheckpoint pk

        let tip = currentTip cp
        let sp = slotParams (blockchainParameters cp)

        mapExceptT liftIO (getSlotRange sp) >>= maybe
            (pure [])
            (\r -> assemble sp tip <$> lift (readTxHistory pk order r Nothing))
  where
    db = ctx ^. dbLayer @s @k

    -- Transforms the user-specified time range into a slot range. If the
    -- user-specified range terminates before the start of the blockchain,
    -- returns 'Nothing'.
    getSlotRange
        :: SlotParameters
        -> ExceptT ErrListTransactions IO (Maybe (Range SlotId))
    getSlotRange sp = case (mStart, mEnd) of
        (Just start, Just end) | start > end -> do
            let err = ErrStartTimeLaterThanEndTime start end
            throwE (ErrListTransactionsStartTimeLaterThanEndTime err)
        _ ->
            pure $ slotRangeFromTimeRange sp $ Range mStart mEnd

    -- This relies on DB.readTxHistory returning all necessary transactions
    -- to assemble coin selection information for outgoing payments.
    -- To reliably provide this information, it should be looked up when
    -- applying blocks, but that is future work (issue #573).
    assemble
        :: SlotParameters
        -> BlockHeader
        -> [(Tx, TxMeta)]
        -> [TransactionInfo]
    assemble sp tip txs = map mkTxInfo txs
      where
        mkTxInfo (tx, meta) = TransactionInfo
            { txInfoId = W.txId tx
            , txInfoInputs =
                [(txIn, lookupOutput txIn) | txIn <- W.inputs tx]
            , txInfoOutputs = W.outputs tx
            , txInfoMeta = meta
            , txInfoDepth =
                Quantity $ fromIntegral $ if tipH > txH then tipH - txH else 0
            , txInfoTime = txTime (meta ^. #slotId)
            }
          where
            txH = getQuantity (meta ^. #blockHeight)
            tipH = getQuantity (tip ^. #blockHeight)
        txOuts = Map.fromList
            [ (W.txId tx, W.outputs tx)
            | ((tx, _)) <- txs
            ]
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

{-------------------------------------------------------------------------------
                                  Key Store
-------------------------------------------------------------------------------}

-- | Attach a given private key to a wallet. The private key is
-- necessary for some operations like signing transactions, or
-- generating new accounts.
attachPrivateKey
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, Passphrase "encryption")
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKey ctx wid (xprv, pwd) = db & \DBLayer{..} -> do
    hpwd <- liftIO $ encryptPassphrase pwd
    now <- liftIO getCurrentTime
    mapExceptT atomically $ do
        putPrivateKey (PrimaryKey wid) (xprv, hpwd)
        meta <- withNoSuchWallet wid $ readWalletMeta (PrimaryKey wid)
        let modify x = x { passphraseInfo = Just (WalletPassphraseInfo now) }
        putWalletMeta (PrimaryKey wid) (modify meta)
  where
    db = ctx ^. dbLayer @s @k

-- | Execute an action which requires holding a root XPrv.
withRootKey
    :: forall ctx s k e a. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> Passphrase "encryption"
    -> (ErrWithRootKey -> e)
    -> (k 'RootK XPrv -> ExceptT e IO a)
    -> ExceptT e IO a
withRootKey ctx wid pwd embed action = db & \DBLayer{..} -> do
    xprv <- withExceptT embed $ mapExceptT atomically $ do
        lift (readPrivateKey (PrimaryKey wid)) >>= \case
            Nothing ->
                throwE $ ErrWithRootKeyNoRootKey wid
            Just (xprv, hpwd) -> do
                withExceptT (ErrWithRootKeyWrongPassphrase wid) $ ExceptT $
                    return $ checkPassphrase pwd hpwd
                return xprv
    action xprv
  where
    db = ctx ^. dbLayer @s @k

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
    | ErrSubmitExternalTxDecode ErrDecodeSignedTx
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

withNoSuchWallet
    :: Monad m
    => WalletId
    -> m (Maybe a)
    -> ExceptT ErrNoSuchWallet m a
withNoSuchWallet wid =
    maybeToExceptT (ErrNoSuchWallet wid) . MaybeT
