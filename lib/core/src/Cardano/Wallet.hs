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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
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
    , createIcarusWallet
    , attachPrivateKeyFromPwd
    , attachPrivateKeyFromPwdHash
    , listUtxoStatistics
    , readWallet
    , deleteWallet
    , restoreWallet
    , updateWallet
    , updateWalletPassphrase
    , walletSyncProgress
    , fetchRewardBalance
    , rollbackBlocks
    , checkWalletIntegrity
    , ErrWalletAlreadyExists (..)
    , ErrNoSuchWallet (..)
    , ErrListUTxOStatistics (..)
    , ErrUpdatePassphrase (..)
    , ErrFetchRewards (..)
    , ErrCheckWalletIntegrity (..)
    , ErrWalletNotResponding (..)

    -- ** Address
    , createRandomAddress
    , importRandomAddress
    , listAddresses
    , normalizeDelegationAddress
    , ErrCreateRandomAddress(..)
    , ErrImportRandomAddress(..)

    -- ** Payment
    , selectCoinsExternal
    , selectCoinsForPayment
    , estimateFeeForPayment
    , signPayment
    , ErrSelectCoinsExternal (..)
    , ErrSelectForPayment (..)
    , ErrSignPayment (..)
    , ErrCoinSelection (..)
    , ErrAdjustForFee (..)
    , ErrValidateSelection

    -- ** Migration
    , selectCoinsForMigration
    , ErrSelectForMigration (..)

    -- ** Delegation
    , joinStakePool
    , quitStakePool
    , selectCoinsForDelegation
    , estimateFeeForDelegation
    , signDelegation
    , guardJoin
    , guardQuit
    , ErrJoinStakePool (..)
    , ErrCannotJoin (..)
    , ErrQuitStakePool (..)
    , ErrCannotQuit (..)
    , ErrSelectForDelegation (..)
    , ErrSignDelegation (..)

    -- ** Fee Estimation
    , FeeEstimation (..)
    , estimateFeeForCoinSelection

    -- ** Transaction
    , forgetPendingTx
    , listTransactions
    , submitExternalTx
    , signTx
    , submitTx
    , ErrMkTx (..)
    , ErrSubmitTx (..)
    , ErrSubmitExternalTx (..)
    , ErrRemovePendingTx (..)
    , ErrPostTx (..)
    , ErrDecodeSignedTx (..)
    , ErrListTransactions (..)
    , ErrNetworkUnavailable (..)
    , ErrCurrentNodeTip (..)
    , ErrStartTimeLaterThanEndTime (..)

    -- ** Root Key
    , withRootKey
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)

    -- * Logging
    , WalletLog (..)
    ) where

import Prelude hiding
    ( log )

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrRemovePendingTx (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , sparseCheckpoints
    )
import Cardano.Wallet.Network
    ( ErrCurrentNodeTip (..)
    , ErrGetAccountBalance (..)
    , ErrNetworkUnavailable (..)
    , ErrPostTx (..)
    , FollowAction (..)
    , FollowExit (..)
    , FollowLog (..)
    , NetworkLayer (..)
    , follow
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , DerivationType (..)
    , ErrWrongPassphrase (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint (..)
    , Passphrase
    , PaymentAddress (..)
    , WalletKey (..)
    , checkPassphrase
    , deriveRewardAccount
    , encryptPassphrase
    , liftIndex
    , preparePassphrase
    , toChimericAccount
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , HasRewardAccount (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..)
    , defaultAddressPoolGap
    , mkSeqStateFromRootXPrv
    , mkUnboundedAddressPoolGap
    , shrinkPool
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionOptions (..)
    , ErrCoinSelection (..)
    , feeBalance
    )
import Cardano.Wallet.Primitive.CoinSelection.Migration
    ( depleteUTxO, idealBatchSize )
import Cardano.Wallet.Primitive.Fee
    ( ErrAdjustForFee (..)
    , Fee (..)
    , FeeOptions (..)
    , OnDanglingChange (..)
    , adjustForFee
    , computeCertFee
    , computeFee
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , applyBlocks
    , availableUTxO
    , blockchainParameters
    , currentTip
    , getState
    , initWallet
    , updateState
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Block (..)
    , BlockHeader (..)
    , BlockchainParameters (..)
    , ChimericAccount (..)
    , Coin (..)
    , DelegationCertificate (..)
    , Direction (..)
    , FeePolicy (LinearFee)
    , GenesisBlockParameters (..)
    , Hash (..)
    , IsDelegatingTo (..)
    , PassphraseScheme (..)
    , PoolId
    , Range (..)
    , SealedTx
    , SlotId (..)
    , SlotParameters (..)
    , SortOrder (..)
    , SyncProgress (..)
    , SyncTolerance
    , TransactionInfo (..)
    , Tx
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxParameters (..)
    , TxStatus (..)
    , UTxOStatistics
    , UnsignedTx (..)
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , computeUtxoStatistics
    , dlgCertPoolId
    , fromTransactionInfo
    , log10
    , slotParams
    , slotRangeFromTimeRange
    , slotStartTime
    , syncProgressRelativeToTime
    , wholeRange
    )
import Cardano.Wallet.Transaction
    ( ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , ErrValidateSelection
    , TransactionLayer (..)
    )
import Control.Exception
    ( Exception )
import Control.Monad
    ( forM, forM_, replicateM, unless, when )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), except, mapExceptT, runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), maybeToExceptT )
import Control.Monad.Trans.State.Strict
    ( runStateT, state )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Either
    ( partitionEithers )
import Data.Either.Extra
    ( eitherToMaybe )
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
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( UTCTime, getCurrentTime )
import Data.Vector.Shuffle
    ( shuffle )
import Data.Word
    ( Word16, Word64 )
import Fmt
    ( blockListF, pretty )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import Safe
    ( lastMay )
import Statistics.Quantile
    ( medianUnbiased, quantiles )

import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.CoinSelection.Random as CoinSelection
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector as V

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
        (Tracer IO WalletLog)
        (Block, GenesisBlockParameters, SyncTolerance)
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

type HasGenesisData = HasType (Block, GenesisBlockParameters, SyncTolerance)

type HasLogger msg = HasType (Tracer IO msg)

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
    => Lens' ctx (Block, GenesisBlockParameters, SyncTolerance)
genesisData =
    typed @(Block, GenesisBlockParameters, SyncTolerance)

logger
    :: forall msg ctx. HasLogger msg ctx
    => Lens' ctx (Tracer IO msg)
logger =
    typed @(Tracer IO msg)

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
        , IsOurs s Address
        , IsOurs s ChimericAccount
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
            , delegation = WalletDelegation NotDelegating []
            }
    mapExceptT atomically $
        initializeWallet (PrimaryKey wid) cp meta hist txp $> wid
  where
    db = ctx ^. dbLayer @s @k
    (block0, GenesisBlockParameters bp txp, _) = ctx ^. genesisData

-- | Initialise and store a new legacy Icarus wallet. These wallets are
-- intrinsically sequential, but, in the incentivized testnet, we only have
-- access to the a snapshot of the MainNet.
--
-- To work-around this, we scan the genesis block with an arbitrary big gap and
-- resort to a default gap afterwards.
createIcarusWallet
    :: forall ctx s k n.
        ( HasGenesisData ctx
        , HasDBLayer s k ctx
        , PaymentAddress n k
        , k ~ IcarusKey
        , s ~ SeqState n k
        )
    => ctx
    -> WalletId
    -> WalletName
    -> (k 'RootK XPrv, Passphrase "encryption")
    -> ExceptT ErrWalletAlreadyExists IO WalletId
createIcarusWallet ctx wid wname credentials = db & \DBLayer{..} -> do
    let s = mkSeqStateFromRootXPrv @n credentials (mkUnboundedAddressPoolGap 10000)
    let (hist, cp) = initWallet block0 bp s
    let addrs = map address . concatMap (view #outputs . fst) $ hist
    let g  = defaultAddressPoolGap
    let s' = SeqState
            (shrinkPool @n (liftPaymentAddress @n) addrs g (internalPool s))
            (shrinkPool @n (liftPaymentAddress @n) addrs g (externalPool s))
            (pendingChangeIxs s)
            (rewardAccountKey s)
    now <- lift getCurrentTime
    let meta = WalletMetadata
            { name = wname
            , creationTime = now
            , passphraseInfo = Nothing
            , delegation = WalletDelegation NotDelegating []
            }
    let pk = PrimaryKey wid
    mapExceptT atomically $
        initializeWallet pk (updateState s' cp) meta hist txp $> wid
  where
    db = ctx ^. dbLayer @s @k
    (block0, GenesisBlockParameters bp txp, _) = ctx ^. genesisData

-- | Check whether a wallet is in good shape when restarting a worker.
checkWalletIntegrity
    :: forall ctx s k. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> BlockchainParameters
    -> ExceptT ErrCheckWalletIntegrity IO ()
checkWalletIntegrity ctx wid bp = db & \DBLayer{..} -> mapExceptT atomically $ do
    cp <- withExceptT ErrCheckWalletIntegrityNoSuchWallet $ withNoSuchWallet wid $
        readCheckpoint (PrimaryKey wid)
    whenDifferentGenesis (blockchainParameters cp) bp $ throwE $
        ErrCheckIntegrityDifferentGenesis
            (getGenesisBlockHash bp)
            (getGenesisBlockHash (blockchainParameters cp))
  where
    db = ctx ^. dbLayer @s @k
    whenDifferentGenesis bp1 bp2 = when $
        (bp1 ^. #getGenesisBlockHash /= bp2 ^. #getGenesisBlockHash) ||
        (bp1 ^. #getGenesisBlockDate /= bp2 ^. #getGenesisBlockDate)

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
    pending <- lift $ readTxHistory pk Descending wholeRange (Just Pending)
    pure (cp, meta, Set.fromList (fromTransactionInfo <$> pending))
  where
    db = ctx ^. dbLayer @s @k

readWalletTxParameters
    :: forall ctx s k. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO TxParameters
readWalletTxParameters ctx wid = db & \DBLayer{..} ->
    mapExceptT atomically $
        withNoSuchWallet wid $
            readTxParameters (PrimaryKey wid)
  where
    db = ctx ^. dbLayer @s @k

walletSyncProgress
    :: forall ctx s.
        ( HasGenesisData ctx
        )
    => ctx
    -> Wallet s
    -> IO SyncProgress
walletSyncProgress ctx w = do
    let bp = blockchainParameters w
    let h = currentTip w
    syncProgressRelativeToTime st (slotParams bp) h <$> getCurrentTime
  where
    (_, _, st) = ctx ^. genesisData

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
    -> (Passphrase "raw", Passphrase "raw")
    -> ExceptT ErrUpdatePassphrase IO ()
updateWalletPassphrase ctx wid (old, new) =
    withRootKey @ctx @s @k ctx wid (coerce old) ErrUpdatePassphraseWithRootKey
        $ \xprv scheme -> withExceptT ErrUpdatePassphraseNoSuchWallet $ do
            -- NOTE
            -- /!\ Important /!\
            -- attachPrivateKeyFromPwd does use 'EncryptWithPBKDF2', so
            -- regardless of the passphrase current scheme, we'll re-encrypt
            -- it using the new scheme, always.
            let oldP = preparePassphrase scheme old
            let newP = preparePassphrase EncryptWithPBKDF2 new
            let xprv' = changePassphrase oldP newP xprv
            attachPrivateKeyFromPwd @ctx @s @k ctx wid (xprv', newP)

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
        ( HasLogger WalletLog ctx
        , HasNetworkLayer t ctx
        , HasDBLayer s k ctx
        , HasGenesisData ctx
        , IsOurs s Address
        , IsOurs s ChimericAccount
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
restoreWallet ctx wid = db & \DBLayer{..} -> do
    cps <- liftIO $ atomically $ listCheckpoints (PrimaryKey wid)
    let forward bs (h, ps) = run $ do
            restoreBlocks @ctx @s @k ctx wid bs h
            saveParams @ctx @s @k ctx wid ps
    liftIO (follow nw tr cps forward (view #header)) >>= \case
        FollowInterrupted -> pure ()
        FollowFailure ->
            restoreWallet @ctx @s @t @k ctx wid
        FollowRollback point -> do
            rollbackBlocks @ctx @s @k ctx wid point
            restoreWallet @ctx @s @t @k ctx wid
  where
    db = ctx ^. dbLayer @s @k
    nw = ctx ^. networkLayer @t
    tr = contramap MsgFollow (ctx ^. logger @WalletLog)

    run :: ExceptT ErrNoSuchWallet IO () -> IO (FollowAction ErrNoSuchWallet)
    run = fmap (either ExitWith (const Continue)) . runExceptT

-- | Rewind the UTxO snapshots, transaction history and other information to a
-- the earliest point in the past that is before or is the point of rollback.
rollbackBlocks
    :: forall ctx s k.
        ( HasLogger WalletLog ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> SlotId
    -> ExceptT ErrNoSuchWallet IO ()
rollbackBlocks ctx wid point = db & \DBLayer{..} -> do
    lift $ traceWith tr $ MsgTryingRollback point
    point' <- mapExceptT atomically $ rollbackTo (PrimaryKey wid) point
    lift $ traceWith tr $ MsgRolledBack point'
  where
    db = ctx ^. dbLayer @s @k
    tr = ctx ^. logger @WalletLog

-- | Apply the given blocks to the wallet and update the wallet state,
-- transaction history and corresponding metadata.
restoreBlocks
    :: forall ctx s k.
        ( HasLogger WalletLog ctx
        , HasDBLayer s k ctx
        , HasGenesisData ctx
        , IsOurs s Address
        , IsOurs s ChimericAccount
        )
    => ctx
    -> WalletId
    -> NonEmpty Block
    -> BlockHeader
    -> ExceptT ErrNoSuchWallet IO ()
restoreBlocks ctx wid blocks nodeTip = db & \DBLayer{..} -> mapExceptT atomically $ do
    cp   <- withNoSuchWallet wid (readCheckpoint $ PrimaryKey wid)
    meta <- withNoSuchWallet wid (readWalletMeta $ PrimaryKey wid)
    let bp = blockchainParameters cp

    unless (cp `isParentOf` NE.head blocks) $ fail $ T.unpack $ T.unwords
        [ "restoreBlocks: given chain isn't a valid continuation."
        , "Wallet is at:", pretty (currentTip cp)
        , "but the given chain continues starting from:"
        , pretty (header (NE.head blocks))
        ]

    let (filteredBlocks, cps) = NE.unzip $ applyBlocks @s blocks cp
    let slotPoolDelegations =
            [ (slotId, cert)
            | let slots = view #slotId . view #header <$> blocks
            , let delegations = view #delegations <$> filteredBlocks
            , (slotId, certs) <- NE.toList $ NE.zip slots delegations
            , cert <- certs
            ]
    let txs = fold $ view #transactions <$> filteredBlocks
    let k = bp ^. #getEpochStability
    let localTip = currentTip $ NE.last cps

    putTxHistory (PrimaryKey wid) txs
    forM_ slotPoolDelegations $ \delegation@(slotId, cert) -> do
        liftIO $ logDelegation delegation
        putDelegationCertificate (PrimaryKey wid) cert slotId

    let unstable = sparseCheckpoints k (nodeTip ^. #blockHeight)

    forM_ (NE.init cps) $ \cp' -> do
        let (Quantity h) = currentTip cp' ^. #blockHeight
        when (fromIntegral h `elem` unstable) $ do
            liftIO $ logCheckpoint cp'
            putCheckpoint (PrimaryKey wid) cp'

    liftIO $ logCheckpoint (NE.last cps)
    putCheckpoint (PrimaryKey wid) (NE.last cps)

    prune (PrimaryKey wid)

    liftIO $ do
        progress <- walletSyncProgress @ctx ctx (NE.last cps)
        traceWith tr $ MsgWalletMetadata meta
        traceWith tr $ MsgSyncProgress progress
        traceWith tr $ MsgDiscoveredTxs txs
        traceWith tr $ MsgTip localTip
        traceWith tr $ MsgBlocks blocks
        traceWith tr $ MsgDiscoveredTxsContent txs
  where
    db = ctx ^. dbLayer @s @k
    tr = ctx ^. logger @WalletLog

    logCheckpoint :: Wallet s -> IO ()
    logCheckpoint cp = traceWith tr $ MsgCheckpoint (currentTip cp)

    logDelegation :: (SlotId, DelegationCertificate) -> IO ()
    logDelegation (slotId, cert) = traceWith tr $ MsgDelegation slotId cert

    isParentOf :: Wallet s -> Block -> Bool
    isParentOf cp = (== parent) . parentHeaderHash . header
      where parent = headerHash $ currentTip cp

-- | Store the node tip params into the wallet database
saveParams
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> TxParameters
    -> ExceptT ErrNoSuchWallet IO ()
saveParams ctx wid params = db & \DBLayer{..} ->
   mapExceptT atomically $ putTxParameters (PrimaryKey wid) params
  where
    db = ctx ^. dbLayer @s @k

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

-- | Fetch the reward balance of a given wallet.
--
-- Rather than force all callers of 'readWallet' to wait for fetching the
-- account balance (via the 'NetworkLayer'), we expose this function for it.
fetchRewardBalance
    :: forall ctx s t k.
        ( HasDBLayer s k ctx
        , HasNetworkLayer t ctx
        , HasRewardAccount s
        , k ~ RewardAccountKey s
        , WalletKey k
        )
    => ctx
    -> WalletId
    -> ExceptT ErrFetchRewards IO (Quantity "lovelace" Word64)
fetchRewardBalance ctx wid = db & \DBLayer{..} -> do
    let pk = PrimaryKey wid
    cp <- withExceptT ErrFetchRewardsNoSuchWallet
        . mapExceptT atomically
        . withNoSuchWallet wid
        $ readCheckpoint pk
    mapExceptT (fmap handleErr)
        . getAccountBalance nw
        . toChimericAccount
        . rewardAccount
        $ getState cp
  where
    db = ctx ^. dbLayer @s @k
    nw = ctx ^. networkLayer @t
    handleErr = \case
        Right x -> Right x
        Left (ErrGetAccountBalanceAccountNotFound _) ->
            Right $ Quantity 0
        Left (ErrGetAccountBalanceNetworkUnreachable e) ->
            Left $ ErrFetchRewardsNetworkUnreachable e

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

-- | List all addresses of a wallet with their metadata. Addresses
-- are ordered from the most-recently-discovered to the oldest known.
listAddresses
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        , IsOurs s Address
        , CompareDiscovery s
        , KnownAddresses s
        )
    => ctx
    -> WalletId
    -> (s -> Address -> Maybe Address)
        -- ^ A function to normalize address, so that delegated addresses
        -- non-delegation addresses found in the transaction history are
        -- shown with their delegation settings.
        -- Use 'Just' for wallet without delegation settings.
    -> ExceptT ErrNoSuchWallet IO [(Address, AddressState)]
listAddresses ctx wid normalize = db & \DBLayer{..} -> do
    (s, txs) <- mapExceptT atomically $ (,)
        <$> (getState <$> withNoSuchWallet wid (readCheckpoint primaryKey))
        <*> lift (readTxHistory primaryKey Descending wholeRange Nothing)
    let maybeIsOurs (TxOut a _) = if fst (isOurs a s)
            then normalize s a
            else Nothing
    let usedAddrs
            = Set.fromList
            $ concatMap
                (mapMaybe maybeIsOurs . W.outputs)
                (fromTransactionInfo <$> txs)
    let knownAddrs =
            L.sortBy (compareDiscovery s) (mapMaybe (normalize s) $ knownAddresses s)
    let withAddressState addr =
            (addr, if addr `Set.member` usedAddrs then Used else Unused)
    return $ withAddressState <$> knownAddrs
  where
    db = ctx ^. dbLayer @s @k
    primaryKey = PrimaryKey wid

createRandomAddress
    :: forall ctx s k n.
        ( HasDBLayer s k ctx
        , PaymentAddress n ByronKey
        , s ~ RndState n
        , k ~ ByronKey
        )
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> Maybe (Index 'Hardened 'AddressK)
    -> ExceptT ErrCreateRandomAddress IO Address
createRandomAddress ctx wid pwd mIx = db & \DBLayer{..} ->
    withRootKey @ctx @s @k ctx wid pwd ErrCreateAddrWithRootKey $ \xprv scheme -> do
        mapExceptT atomically $ do
            cp <- withExceptT ErrCreateAddrNoSuchWallet $
                withNoSuchWallet wid (readCheckpoint (PrimaryKey wid))
            let s = getState cp
            let accIx = Rnd.accountIndex s

            (path, gen') <- case mIx of
                Just addrIx | isKnownIndex accIx addrIx s ->
                    throwE $ ErrIndexAlreadyExists addrIx
                Just addrIx ->
                    pure ((liftIndex accIx, liftIndex addrIx), Rnd.gen s)
                Nothing ->
                    pure $ Rnd.findUnusedPath (Rnd.gen s) accIx (Rnd.unavailablePaths s)

            let prepared = preparePassphrase scheme pwd
            let addr = Rnd.deriveRndStateAddress @n xprv prepared path
            let s' = (Rnd.addDiscoveredAddress addr path s) { Rnd.gen = gen' }
            withExceptT ErrCreateAddrNoSuchWallet $
                putCheckpoint (PrimaryKey wid) (updateState s' cp)
            pure addr
  where
    db = ctx ^. dbLayer @s @k
    isKnownIndex accIx addrIx s =
        (liftIndex accIx, liftIndex addrIx) `Set.member` Rnd.unavailablePaths s

importRandomAddress
    :: forall ctx s k n.
        ( HasDBLayer s k ctx
        , s ~ RndState n
        , k ~ ByronKey
        )
    => ctx
    -> WalletId
    -> Address
    -> ExceptT ErrImportRandomAddress IO ()
importRandomAddress ctx wid addr = db & \DBLayer{..} -> mapExceptT atomically $ do
    cp <- withExceptT ErrImportAddrNoSuchWallet
        $ withNoSuchWallet wid (readCheckpoint (PrimaryKey wid))
    case isOurs addr (getState cp) of
        (False, _) -> throwE ErrImportAddrDoesNotBelong
        (True, s') -> do
            withExceptT ErrImportAddrNoSuchWallet $
                putCheckpoint (PrimaryKey wid) (updateState s' cp)
  where
    db = ctx ^. dbLayer @s @k

-- NOTE
-- Addresses coming from the transaction history might be payment or
-- delegation addresses. So we normalize them all to be delegation addresses
-- to make sure that we compare them correctly.
normalizeDelegationAddress
    :: forall s k n.
        ( DelegationAddress n k
        , HasRewardAccount s
        , k ~ RewardAccountKey s
        )
    => s
    -> Address
    -> Maybe Address
normalizeDelegationAddress s addr = do
    fingerprint <- eitherToMaybe (paymentKeyFingerprint addr)
    pure $ liftDelegationAddress @n fingerprint (rewardAccount s)

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
    -> ( FeePolicy -> Quantity "byte" Int -> Fee )
    -> FeePolicy
    -> FeeOptions
feeOpts tl feeCompute feePolicy = FeeOptions
    { estimateFee = feeCompute feePolicy . estimateSize tl
    , dustThreshold = minBound
    , onDanglingChange = if allowUnbalancedTx tl then SaveMoney else PayAndBalance
    }

-- | Prepare a transaction and automatically select inputs from the
-- wallet to cover the requested outputs. Note that this only runs
-- coin selection for the given outputs. In order to construct (and
-- sign) an actual transaction, use 'signPayment'.
selectCoinsForPayment
    :: forall ctx s t k e.
        ( HasTransactionLayer t k ctx
        , HasLogger WalletLog ctx
        , HasDBLayer s k ctx
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> WalletId
    -> NonEmpty TxOut
    -> ExceptT (ErrSelectForPayment e) IO CoinSelection
selectCoinsForPayment ctx wid recipients = do
    (utxo, txp) <- withExceptT ErrSelectForPaymentNoSuchWallet $
        selectCoinsSetup @ctx @s @k ctx wid
    selectCoinsForPaymentFromUTxO @ctx @t @k @e ctx utxo txp recipients

-- | Retrieve wallet data which is needed for all types of coin selections.
selectCoinsSetup
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (W.UTxO, W.TxParameters)
selectCoinsSetup ctx wid = do
    (wal, _, pending) <- readWallet @ctx @s @k ctx wid
    txp <- readWalletTxParameters @ctx @s @k ctx wid
    let utxo = availableUTxO @s pending wal
    return (utxo, txp)

selectCoinsForPaymentFromUTxO
    :: forall ctx t k e.
        ( HasTransactionLayer t k ctx
        , HasLogger WalletLog ctx
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> W.UTxO
    -> W.TxParameters
    -> NonEmpty TxOut
    -> ExceptT (ErrSelectForPayment e) IO CoinSelection
selectCoinsForPaymentFromUTxO ctx utxo txp recipients = do
    (sel, utxo') <- withExceptT ErrSelectForPaymentCoinSelection $ do
        let opts = coinSelOpts tl (txp ^. #getTxMaxSize)
        CoinSelection.random opts recipients utxo
    lift . traceWith tr $ MsgPaymentCoinSelection sel
    let feePolicy = feeOpts tl computeFee (txp ^. #getFeePolicy)
    withExceptT ErrSelectForPaymentFee $ do
        balancedSel <- adjustForFee feePolicy utxo' sel
        lift . traceWith tr $ MsgPaymentCoinSelectionAdjusted balancedSel
        pure balancedSel
  where
    tl = ctx ^. transactionLayer @t @k
    tr = ctx ^. logger @WalletLog

-- | Select necessary coins to cover for a single delegation request (including
-- one certificate).
selectCoinsForDelegation
    :: forall ctx s t k.
        ( HasTransactionLayer t k ctx
        , HasLogger WalletLog ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrSelectForDelegation IO CoinSelection
selectCoinsForDelegation ctx wid = do
    (utxo, txp) <- withExceptT ErrSelectForDelegationNoSuchWallet $
        selectCoinsSetup @ctx @s @k ctx wid
    selectCoinsForDelegationFromUTxO @_ @t @k ctx utxo txp

selectCoinsForDelegationFromUTxO
    :: forall ctx t k.
        ( HasTransactionLayer t k ctx
        , HasLogger WalletLog ctx
        )
    => ctx
    -> W.UTxO
    -> W.TxParameters
    -> ExceptT ErrSelectForDelegation IO CoinSelection
selectCoinsForDelegationFromUTxO ctx utxo txp = do
    let sel = CoinSelection [] [] []
    let feePolicy = feeOpts tl (computeCertFee 1) (txp ^. #getFeePolicy)
    withExceptT ErrSelectForDelegationFee $ do
        balancedSel <- adjustForFee feePolicy utxo sel
        lift $ traceWith tr $ MsgDelegationCoinSelection balancedSel
        pure balancedSel
  where
    tl = ctx ^. transactionLayer @t @k
    tr = ctx ^. logger @WalletLog

-- | Estimate fee for 'selectCoinsForDelegation'.
estimateFeeForDelegation
    :: forall ctx s t k.
        ( HasTransactionLayer t k ctx
        , HasLogger WalletLog ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrSelectForDelegation IO FeeEstimation
estimateFeeForDelegation ctx wid = do
    (utxo, txp) <- withExceptT ErrSelectForDelegationNoSuchWallet $
        selectCoinsSetup @ctx @s @k ctx wid
    estimateFeeForCoinSelection $
       selectCoinsForDelegationFromUTxO @_ @t @k ctx utxo txp

-- | Constructs a set of coin selections that select all funds from the given
--   source wallet, returning them as change.
--
-- If the coin selections returned by this function are used to create
-- transactions from the given wallet to a target wallet, executing those
-- transactions will have the effect of migrating all funds from the given
-- source wallet to the specified target wallet.
selectCoinsForMigration
    :: forall ctx s t k.
        ( HasTransactionLayer t k ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
       -- ^ The source wallet ID.
    -> ExceptT ErrSelectForMigration IO [CoinSelection]
selectCoinsForMigration ctx wid = do
    (utxo, txp) <- withExceptT ErrSelectForMigrationNoSuchWallet $
        selectCoinsSetup @ctx @s @k ctx wid
    selectCoinsForMigrationFromUTxO @ctx @t @k ctx utxo txp wid

selectCoinsForMigrationFromUTxO
    :: forall ctx t k.
        ( HasTransactionLayer t k ctx
        )
    => ctx
    -> W.UTxO
    -> W.TxParameters
    -> WalletId
       -- ^ The source wallet ID.
    -> ExceptT ErrSelectForMigration IO [CoinSelection]
selectCoinsForMigrationFromUTxO ctx utxo txp wid = do
    let feePolicy@(LinearFee (Quantity a) _ _) = txp ^. #getFeePolicy
    let feeOptions = (feeOpts tl computeFee feePolicy)
            { dustThreshold = Coin $ ceiling a }
    let selOptions = coinSelOpts tl (txp ^. #getTxMaxSize)
    case depleteUTxO feeOptions (idealBatchSize selOptions) utxo of
        cs | not (null cs) -> pure cs
        _ -> throwE (ErrSelectForMigrationEmptyWallet wid)
  where
    tl = ctx ^. transactionLayer @t @k

-- | Estimate fee for 'selectCoinsForPayment'.
estimateFeeForPayment
    :: forall ctx s t k e.
        ( HasTransactionLayer t k ctx
        , HasLogger WalletLog ctx
        , HasDBLayer s k ctx
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> WalletId
    -> NonEmpty TxOut
    -> ExceptT (ErrSelectForPayment e) IO FeeEstimation
estimateFeeForPayment ctx wid recipients = do
    (utxo, txp) <- withExceptT ErrSelectForPaymentNoSuchWallet $
        selectCoinsSetup @ctx @s @k ctx wid
    estimateFeeForCoinSelection $
        selectCoinsForPaymentFromUTxO @ctx @t @k @e ctx utxo txp recipients

-- | Augments the given outputs with new outputs. These new outputs corresponds
-- to change outputs to which new addresses are being assigned to. This updates
-- the wallet state as it needs to keep track of new pending change addresses.
assignChangeAddresses
    :: forall s m.
        ( GenChange s
        , MonadIO m
        )
    => ArgGenChange s
    -> [TxOut]
    -> [Coin]
    -> s
    -> m ([TxOut], s)
assignChangeAddresses argGenChange outs chgs = runStateT $ do
    chgsOuts <- forM chgs $ \c -> do
        addr <- state (genChange argGenChange)
        pure $ TxOut addr c
    liftIO $ shuffle (outs ++ chgsOuts)

-- | Produce witnesses and construct a transaction from a given
-- selection. Requires the encryption passphrase in order to decrypt
-- the root private key. Note that this doesn't broadcast the
-- transaction to the network. In order to do so, use 'submitTx'.
signPayment
    :: forall ctx s t k.
        ( HasTransactionLayer t k ctx
        , HasDBLayer s k ctx
        , HasNetworkLayer t ctx
        , IsOwned s k
        , GenChange s
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> Passphrase "raw"
    -> CoinSelection
    -> ExceptT ErrSignPayment IO (Tx, TxMeta, UTCTime, SealedTx)
signPayment ctx wid argGenChange pwd (CoinSelection ins outs chgs) = db & \DBLayer{..} -> do
    withRootKey @_ @s ctx wid pwd ErrSignPaymentWithRootKey $ \xprv scheme -> do
        nodeTip <- withExceptT ErrSignPaymentNetwork $ currentNodeTip nl
        mapExceptT atomically $ do
            cp <- withExceptT ErrSignPaymentNoSuchWallet $ withNoSuchWallet wid $
                readCheckpoint (PrimaryKey wid)
            (allOuts, s') <-
                assignChangeAddresses argGenChange outs chgs (getState cp)
            withExceptT ErrSignPaymentNoSuchWallet $
                putCheckpoint (PrimaryKey wid) (updateState s' cp)

            let keyFrom = isOwned (getState cp) (xprv, preparePassphrase scheme pwd)
            (tx, sealedTx) <- withExceptT ErrSignPaymentMkTx $ ExceptT $ pure $
                mkStdTx tl keyFrom (nodeTip ^. #slotId) ins allOuts

            let bp = blockchainParameters cp
            let (time, meta) = mkTxMeta bp (currentTip cp) s' ins allOuts
            return (tx, meta, time, sealedTx)
  where
    db = ctx ^. dbLayer @s @k
    tl = ctx ^. transactionLayer @t @k
    nl = ctx ^. networkLayer @t

-- | Very much like 'signPayment', but doesn't not generate change addresses.
signTx
    :: forall ctx s t k.
        ( HasTransactionLayer t k ctx
        , HasDBLayer s k ctx
        , HasNetworkLayer t ctx
        , IsOwned s k
        )
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> UnsignedTx
    -> ExceptT ErrSignPayment IO (Tx, TxMeta, UTCTime, SealedTx)
signTx ctx wid pwd (UnsignedTx inpsNE outsNE) = db & \DBLayer{..} -> do
    withRootKey @_ @s ctx wid pwd ErrSignPaymentWithRootKey $ \xprv scheme -> do
        nodeTip <- withExceptT ErrSignPaymentNetwork $ currentNodeTip nl
        mapExceptT atomically $ do
            cp <- withExceptT ErrSignPaymentNoSuchWallet $ withNoSuchWallet wid $
                readCheckpoint (PrimaryKey wid)

            let keyFrom = isOwned (getState cp) (xprv, preparePassphrase scheme pwd)
            (tx, sealedTx) <- withExceptT ErrSignPaymentMkTx $ ExceptT $ pure $
                mkStdTx tl keyFrom (nodeTip ^. #slotId) inps outs

            let bp = blockchainParameters cp
            let (time, meta) = mkTxMeta bp (currentTip cp) (getState cp) inps outs
            return (tx, meta, time, sealedTx)
  where
    db = ctx ^. dbLayer @s @k
    tl = ctx ^. transactionLayer @t @k
    nl = ctx ^. networkLayer @t
    inps = NE.toList inpsNE
    outs = NE.toList outsNE

-- | Makes a fully-resolved coin selection for the given set of payments.
selectCoinsExternal
    :: forall ctx s t k e.
        ( GenChange s
        , HasDBLayer s k ctx
        , HasLogger WalletLog ctx
        , HasTransactionLayer t k ctx
        , e ~ ErrValidateSelection t
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> NonEmpty TxOut
    -> ExceptT (ErrSelectCoinsExternal e) IO UnsignedTx
selectCoinsExternal ctx wid argGenChange payments = do
    CoinSelection mInputs mPayments mChange <-
        withExceptT ErrSelectCoinsExternalUnableToMakeSelection $
            selectCoinsForPayment @ctx @s @t @k @e ctx wid payments
    mOutputs <- db & \DBLayer{..} ->
        withExceptT ErrSelectCoinsExternalNoSuchWallet $
            mapExceptT atomically $ do
                cp <- withNoSuchWallet wid $ readCheckpoint $ PrimaryKey wid
                (mOutputs, s') <- assignChangeAddresses
                    argGenChange mPayments mChange $ getState cp
                putCheckpoint (PrimaryKey wid) (updateState s' cp)
                pure mOutputs
    UnsignedTx
        <$> ensureNonEmpty mInputs  ErrSelectCoinsExternalUnableToAssignInputs
        <*> ensureNonEmpty mOutputs ErrSelectCoinsExternalUnableToAssignOutputs
  where
    db = ctx ^. dbLayer @s @k
    ensureNonEmpty
        :: forall a. [a]
        -> (WalletId -> ErrSelectCoinsExternal e)
        -> ExceptT (ErrSelectCoinsExternal e) IO (NonEmpty a)
    ensureNonEmpty mxs err = case NE.nonEmpty mxs of
        Nothing -> throwE $ err wid
        Just xs -> pure xs

data ErrSelectCoinsExternal e
    = ErrSelectCoinsExternalNoSuchWallet ErrNoSuchWallet
    | ErrSelectCoinsExternalUnableToMakeSelection (ErrSelectForPayment e)
    | ErrSelectCoinsExternalUnableToAssignInputs WalletId
    | ErrSelectCoinsExternalUnableToAssignOutputs WalletId
    deriving (Eq, Show)

signDelegation
    :: forall ctx s t k.
        ( HasTransactionLayer t k ctx
        , HasDBLayer s k ctx
        , HasNetworkLayer t ctx
        , IsOwned s k
        , GenChange s
        , HardDerivation k
        , AddressIndexDerivationType k ~ 'Soft
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> Passphrase "raw"
    -> CoinSelection
    -> DelegationAction
    -> ExceptT ErrSignDelegation IO (Tx, TxMeta, UTCTime, SealedTx)
signDelegation ctx wid argGenChange pwd coinSel action = db & \DBLayer{..} -> do
    let (CoinSelection ins outs chgs) = coinSel
    nodeTip <- withExceptT ErrSignDelegationNetwork $ currentNodeTip nl
    withRootKey @_ @s ctx wid pwd ErrSignDelegationWithRootKey $ \xprv scheme -> do
        let pwdP = preparePassphrase scheme pwd
        mapExceptT atomically $ do
            cp <- withExceptT ErrSignDelegationNoSuchWallet $ withNoSuchWallet wid $
                readCheckpoint (PrimaryKey wid)
            (allOuts, s') <-
                assignChangeAddresses argGenChange outs chgs (getState cp)
            withExceptT ErrSignDelegationNoSuchWallet $
                putCheckpoint (PrimaryKey wid) (updateState s' cp)

            let rewardAcc = deriveRewardAccount @k pwdP xprv
            let keyFrom = isOwned (getState cp) (xprv, pwdP)
            (tx, sealedTx) <- withExceptT ErrSignDelegationMkTx $ ExceptT $ pure $
                case action of
                    Join poolId ->
                        mkDelegationJoinTx tl poolId (rewardAcc, pwdP) keyFrom (nodeTip ^. #slotId) ins allOuts
                    Quit ->
                        mkDelegationQuitTx tl (rewardAcc, pwdP) keyFrom (nodeTip ^. #slotId) ins allOuts

            let bp = blockchainParameters cp
            let (time, meta) = mkTxMeta bp (currentTip cp) s' ins allOuts
            return (tx, meta, time, sealedTx)
  where
    db = ctx ^. dbLayer @s @k
    tl = ctx ^. transactionLayer @t @k
    nl = ctx ^. networkLayer @t

-- | Construct transaction metadata from a current block header and a list
-- of input and output.
mkTxMeta
    :: IsOurs s Address
    => BlockchainParameters
    -> BlockHeader
    -> s
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> (UTCTime, TxMeta)
mkTxMeta bp blockHeader wState ins outs =
    let
        amtOuts =
            sum (mapMaybe ourCoins outs)

        amtInps = fromIntegral $
            sum (getCoin . coin . snd <$> ins)
    in
        ( slotStartTime (slotParams bp) (blockHeader ^. #slotId)
        , TxMeta
            { status = Pending
            , direction = Outgoing
            , slotId = blockHeader ^. #slotId
            , blockHeight = blockHeader ^. #blockHeight
            , amount = Quantity (amtInps - amtOuts)
            }
        )
  where
    ourCoins :: TxOut -> Maybe Natural
    ourCoins (TxOut addr (Coin val)) =
        if fst (isOurs addr wState)
        then Just (fromIntegral val)
        else Nothing

-- | Broadcast a (signed) transaction to the network.
submitTx
    :: forall ctx s t k.
        ( HasNetworkLayer t ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> (Tx, TxMeta, SealedTx)
    -> ExceptT ErrSubmitTx IO ()
submitTx ctx wid (tx, meta, binary) = db & \DBLayer{..} -> do
    withExceptT ErrSubmitTxNetwork $ postTx nw binary
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
    (tx,binary) <- withExceptT ErrSubmitExternalTxDecode $ except $
        decodeSignedTx tl bytes
    withExceptT ErrSubmitExternalTxNetwork $ postTx nw binary
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

        let sp = slotParams (blockchainParameters cp)

        mapExceptT liftIO (getSlotRange sp) >>= maybe
            (pure [])
            (\r -> lift (readTxHistory pk order r Nothing))
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

{-------------------------------------------------------------------------------
                                  Delegation
-------------------------------------------------------------------------------}

data DelegationAction = Join PoolId | Quit

-- | Helper function to factor necessary logic for joining a stake pool.
joinStakePool
    :: forall ctx s t k.
        ( HasDBLayer s k ctx
        , HasLogger WalletLog ctx
        , HasNetworkLayer t ctx
        , HasTransactionLayer t k ctx
        , IsOwned s k
        , GenChange s
        , HardDerivation k
        , AddressIndexDerivationType k ~ 'Soft
        )
    => ctx
    -> WalletId
    -> (PoolId, [PoolId])
    -> ArgGenChange s
    -> Passphrase "raw"
    -> ExceptT ErrJoinStakePool IO (Tx, TxMeta, UTCTime)
joinStakePool ctx wid (pid, pools) argGenChange pwd = db & \DBLayer{..} -> do
    walMeta <- mapExceptT atomically $ withExceptT ErrJoinStakePoolNoSuchWallet $
        withNoSuchWallet wid $ readWalletMeta (PrimaryKey wid)

    withExceptT ErrJoinStakePoolCannotJoin $ except $
        guardJoin pools (walMeta ^. #delegation) pid

    selection <- withExceptT ErrJoinStakePoolSelectCoin $
        selectCoinsForDelegation @ctx @s @t @k ctx wid

    (tx, txMeta, txTime, sealedTx) <- withExceptT ErrJoinStakePoolSignDelegation $
        signDelegation @ctx @s @t @k ctx wid argGenChange pwd selection (Join pid)

    withExceptT ErrJoinStakePoolSubmitTx $
        submitTx @ctx @s @t @k ctx wid (tx, txMeta, sealedTx)

    pure (tx, txMeta, txTime)
  where
    db = ctx ^. dbLayer @s @k

-- | Helper function to factor necessary logic for quitting a stake pool.
quitStakePool
    :: forall ctx s t k.
        ( HasDBLayer s k ctx
        , HasLogger WalletLog ctx
        , HasNetworkLayer t ctx
        , HasTransactionLayer t k ctx
        , IsOwned s k
        , GenChange s
        , HardDerivation k
        , AddressIndexDerivationType k ~ 'Soft
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> Passphrase "raw"
    -> ExceptT ErrQuitStakePool IO (Tx, TxMeta, UTCTime)
quitStakePool ctx wid argGenChange pwd = db & \DBLayer{..} -> do
    walMeta <- mapExceptT atomically $ withExceptT ErrQuitStakePoolNoSuchWallet $
        withNoSuchWallet wid $ readWalletMeta (PrimaryKey wid)

    withExceptT ErrQuitStakePoolCannotQuit $ except $
        guardQuit (walMeta ^. #delegation)

    selection <- withExceptT ErrQuitStakePoolSelectCoin $
        selectCoinsForDelegation @ctx @s @t @k ctx wid

    (tx, txMeta, txTime, sealedTx) <- withExceptT ErrQuitStakePoolSignDelegation $
        signDelegation @ctx @s @t @k ctx wid argGenChange pwd selection Quit

    withExceptT ErrQuitStakePoolSubmitTx $
        submitTx @ctx @s @t @k ctx wid (tx, txMeta, sealedTx)

    pure (tx, txMeta, txTime)
  where
    db = ctx ^. dbLayer @s @k

{-------------------------------------------------------------------------------
                                 Fee Estimation
-------------------------------------------------------------------------------}

-- | Result of a fee estimation process given a wallet and payment order.
data FeeEstimation = FeeEstimation
    { estMinFee :: Word64
    -- ^ Most coin selections will result in a fee higher than this.
    , estMaxFee :: Word64
    -- ^ Most coin selections will result in a fee lower than this.
    } deriving (Show, Eq)

-- | Estimate the transaction fee for a given coin selection algorithm by
-- repeatedly running it (100 times) and collecting the results. In the returned
-- 'FeeEstimation', the minimum fee is that which 90% of the sampled fees are
-- greater than. The maximum fee is the highest fee observed in the samples.
estimateFeeForCoinSelection
    :: forall m err. Monad m
    => ExceptT err m CoinSelection
    -> ExceptT err m FeeEstimation
estimateFeeForCoinSelection
    = fmap deciles
    . handleErrors
    . replicateM repeats
    . runExceptT
    . fmap feeBalance
  where
    -- Use method R-8 from to get top 90%.
    -- https://en.wikipedia.org/wiki/Quantile#Estimating_quantiles_from_a_sample
    deciles = mkFeeEstimation
        . map round
        . V.toList
        . quantiles medianUnbiased (V.fromList [1, 10]) 10
        . V.fromList
        . map fromIntegral
    mkFeeEstimation [a,b] = FeeEstimation a b
    mkFeeEstimation _ = error "estimateFeeForCoinSelection: impossible"

    -- Remove failed coin selections from samples. Unless they all failed, in
    -- which case pass on the error.
    handleErrors :: m [Either err a] -> ExceptT err m [a]
    handleErrors = ExceptT . fmap skipFailed
      where
        skipFailed samples = case partitionEithers samples of
            ([], []) ->
                error "estimateFeeForCoinSelection: impossible empty list"
            ((e:_), []) ->
                Left e
            (_, samples') ->
                Right samples'

    repeats = 100 -- TODO: modify repeats based on data

{-------------------------------------------------------------------------------
                                  Key Store
-------------------------------------------------------------------------------}
-- | The password here undergoes PBKDF2 encryption using HMAC
-- with the hash algorithm SHA512 which is realized in encryptPassphare
attachPrivateKeyFromPwd
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, Passphrase "encryption")
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKeyFromPwd ctx wid (xprv, pwd) = db & \DBLayer{..} -> do
    hpwd <- liftIO $ encryptPassphrase pwd
    -- NOTE Only new wallets are constructed through this function, so the
    -- passphrase is encrypted with the new scheme (i.e. PBKDF2)
    --
    -- We do an extra sanity check after having encrypted the passphrase: we
    -- tried to avoid some programmer mistakes with the phantom types on
    -- Passphrase, but it's still possible that someone would inadvertently call
    -- this function with a 'Passphrase' that wasn't prepared for
    -- 'EncryptWithPBKDF2', if this happens, this is a programmer error and we
    -- must fail hard for this would have dramatic effects later on.
    case checkPassphrase EncryptWithPBKDF2 (coerce pwd) hpwd of
        Right () -> attachPrivateKey db wid (xprv, hpwd) EncryptWithPBKDF2
        Left{} -> fail
            "Awe crap! The passphrase given to 'attachPrivateKeyFromPwd' wasn't \
            \rightfully constructed. This is a programmer error. Look for calls \
            \to this function and make sure that the given Passphrase wasn't not \
            \prepared using 'EncryptWithScrypt'!"
  where
    db = ctx ^. dbLayer @s @k

-- | The hash here is the output of Scrypt function with the following parameters:
-- - logN = 14
-- - r = 8
-- - p = 1
-- - bytesNumber = 64
attachPrivateKeyFromPwdHash
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, Hash "encryption")
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKeyFromPwdHash ctx wid (xprv, hpwd) = db & \DBLayer{..} ->
    -- NOTE Only legacy wallets are imported through this function, passphrase
    -- were encrypted with the legacy scheme (Scrypt).
    attachPrivateKey db wid (xprv, hpwd) EncryptWithScrypt
  where
    db = ctx ^. dbLayer @s @k

attachPrivateKey
    :: DBLayer IO s k
    -> WalletId
    -> (k 'RootK XPrv, Hash "encryption")
    -> PassphraseScheme
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKey db wid (xprv, hpwd) scheme = db & \DBLayer{..} -> do
    now <- liftIO getCurrentTime
    mapExceptT atomically $ do
        putPrivateKey (PrimaryKey wid) (xprv, hpwd)
        meta <- withNoSuchWallet wid $ readWalletMeta (PrimaryKey wid)
        let modify x = x
                { passphraseInfo = Just $ WalletPassphraseInfo
                    { lastUpdatedAt = now
                    , passphraseScheme = scheme
                    }
                }
        putWalletMeta (PrimaryKey wid) (modify meta)

-- | Execute an action which requires holding a root XPrv.
--
-- 'withRootKey' takes a callback function with two arguments:
--
--  - The encrypted root private key itself
--  - The underlying passphrase scheme (legacy or new)
--
-- Caller are then expected to use 'preparePassphrase' with the given scheme in
-- order to "prepare" the passphrase to be used by other function. This does
-- nothing for the new encryption, but for the legacy encryption with Scrypt,
-- passphrases needed to first be CBOR serialized and blake2b_256 hashed.
--
-- @@@
--     withRootKey @ctx @s @k ctx wid pwd OnError $ \xprv scheme ->
--         changePassphrase (preparePassphrase scheme pwd) newPwd xprv
-- @@@
withRootKey
    :: forall ctx s k e a. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> (ErrWithRootKey -> e)
    -> (k 'RootK XPrv -> PassphraseScheme -> ExceptT e IO a)
    -> ExceptT e IO a
withRootKey ctx wid pwd embed action = db & \DBLayer{..} -> do
    (xprv, scheme) <- withExceptT embed $ mapExceptT atomically $ do
        mScheme <- (>>= (fmap passphraseScheme . passphraseInfo)) <$>
            lift (readWalletMeta $ PrimaryKey wid)
        mXPrv <- lift $ readPrivateKey $ PrimaryKey wid
        case (mXPrv, mScheme) of
            (Just (xprv, hpwd), Just scheme) -> do
                withExceptT (ErrWithRootKeyWrongPassphrase wid) $ ExceptT $
                    return $ checkPassphrase scheme pwd hpwd
                return (xprv, scheme)
            _ ->
                throwE $ ErrWithRootKeyNoRootKey wid
    action xprv scheme
  where
    db = ctx ^. dbLayer @s @k

{-------------------------------------------------------------------------------
                                   Errors
-------------------------------------------------------------------------------}

-- | Errors that can occur when creating an unsigned transaction.
data ErrSelectForPayment e
    = ErrSelectForPaymentNoSuchWallet ErrNoSuchWallet
    | ErrSelectForPaymentCoinSelection (ErrCoinSelection e)
    | ErrSelectForPaymentFee ErrAdjustForFee
    deriving (Show, Eq)

-- | Errors that can occur when listing UTxO statistics.
newtype ErrListUTxOStatistics
    = ErrListUTxOStatisticsNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors that can occur when signing a transaction.
data ErrSignPayment
    = ErrSignPaymentMkTx ErrMkTx
    | ErrSignPaymentNoSuchWallet ErrNoSuchWallet
    | ErrSignPaymentWithRootKey ErrWithRootKey
    | ErrSignPaymentNetwork ErrCurrentNodeTip
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

-- | Errors that can occur when creating unsigned delegation certificate
-- transaction.
data ErrSelectForDelegation
    = ErrSelectForDelegationNoSuchWallet ErrNoSuchWallet
    | ErrSelectForDelegationFee ErrAdjustForFee
    deriving (Show, Eq)

-- | Errors that can occur when signing a delegation certificate.
data ErrSignDelegation
    = ErrSignDelegationNoSuchWallet ErrNoSuchWallet
    | ErrSignDelegationWithRootKey ErrWithRootKey
    | ErrSignDelegationMkTx ErrMkTx
    | ErrSignDelegationNetwork ErrCurrentNodeTip
    deriving (Show, Eq)

data ErrJoinStakePool
    = ErrJoinStakePoolNoSuchWallet ErrNoSuchWallet
    | ErrJoinStakePoolSelectCoin ErrSelectForDelegation
    | ErrJoinStakePoolSignDelegation ErrSignDelegation
    | ErrJoinStakePoolSubmitTx ErrSubmitTx
    | ErrJoinStakePoolCannotJoin ErrCannotJoin
    deriving (Generic, Eq, Show)

data ErrQuitStakePool
    = ErrQuitStakePoolNoSuchWallet ErrNoSuchWallet
    | ErrQuitStakePoolSelectCoin ErrSelectForDelegation
    | ErrQuitStakePoolSignDelegation ErrSignDelegation
    | ErrQuitStakePoolSubmitTx ErrSubmitTx
    | ErrQuitStakePoolCannotQuit ErrCannotQuit
    deriving (Generic, Eq, Show)

-- | Errors that can occur when fetching the reward balance of a wallet
data ErrFetchRewards
    = ErrFetchRewardsNetworkUnreachable ErrNetworkUnavailable
    | ErrFetchRewardsNoSuchWallet ErrNoSuchWallet

data ErrSelectForMigration
    = ErrSelectForMigrationNoSuchWallet ErrNoSuchWallet
    | ErrSelectForMigrationEmptyWallet WalletId
        -- ^ User attempted to migrate an empty wallet
    deriving (Eq, Show)

data ErrCheckWalletIntegrity
    = ErrCheckWalletIntegrityNoSuchWallet ErrNoSuchWallet
    | ErrCheckIntegrityDifferentGenesis (Hash "Genesis") (Hash "Genesis")
    deriving (Eq, Show)

instance Exception ErrCheckWalletIntegrity

data ErrCannotJoin
    = ErrAlreadyDelegating PoolId
    | ErrNoSuchPool PoolId
    deriving (Generic, Eq, Show)

data ErrCannotQuit
    = ErrNotDelegatingOrAboutTo
    deriving (Generic, Eq, Show)

-- | Can't perform given operation because the wallet died.
newtype ErrWalletNotResponding
    = ErrWalletNotResponding WalletId
    deriving (Eq, Show)

data ErrCreateRandomAddress
    = ErrIndexAlreadyExists (Index 'Hardened 'AddressK)
    | ErrCreateAddrNoSuchWallet ErrNoSuchWallet
    | ErrCreateAddrWithRootKey ErrWithRootKey
    | ErrCreateAddressNotAByronWallet
    deriving (Generic, Eq, Show)

data ErrImportRandomAddress
    = ErrImportAddrNoSuchWallet ErrNoSuchWallet
    | ErrImportAddrDoesNotBelong
    | ErrImportAddressNotAByronWallet
    deriving (Generic, Eq, Show)

{-------------------------------------------------------------------------------
                                   Utils
-------------------------------------------------------------------------------}

withNoSuchWallet
    :: Monad m
    => WalletId
    -> m (Maybe a)
    -> ExceptT ErrNoSuchWallet m a
withNoSuchWallet wid =
    maybeToExceptT (ErrNoSuchWallet wid) . MaybeT

guardJoin
    :: [PoolId]
    -> WalletDelegation
    -> PoolId
    -> Either ErrCannotJoin ()
guardJoin knownPools WalletDelegation{active,next} pid = do
    when (pid `notElem` knownPools) $
        Left (ErrNoSuchPool pid)

    when ((null next) && isDelegatingTo (== pid) active) $
        Left (ErrAlreadyDelegating pid)

    when (not (null next) && isDelegatingTo (== pid) (last next)) $
        Left (ErrAlreadyDelegating pid)

guardQuit
    :: WalletDelegation
    -> Either ErrCannotQuit ()
guardQuit WalletDelegation{active,next} = do
    let last_ = maybe active (view #status) $ lastMay next
    unless (isDelegatingTo anyone last_) $
        Left ErrNotDelegatingOrAboutTo
  where
    anyone = const True

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data WalletLog
    = MsgTryingRollback SlotId
    | MsgRolledBack SlotId
    | MsgFollow FollowLog
    | MsgDelegation SlotId DelegationCertificate
    | MsgCheckpoint BlockHeader
    | MsgWalletMetadata WalletMetadata
    | MsgSyncProgress SyncProgress
    | MsgDiscoveredTxs [(Tx, TxMeta)]
    | MsgDiscoveredTxsContent [(Tx, TxMeta)]
    | MsgTip BlockHeader
    | MsgBlocks (NonEmpty Block)
    | MsgDelegationCoinSelection CoinSelection
    | MsgPaymentCoinSelection CoinSelection
    | MsgPaymentCoinSelectionAdjusted CoinSelection
    deriving (Show, Eq)

instance ToText WalletLog where
    toText = \case
        MsgTryingRollback point ->
            "Try rolling back to " <> pretty point
        MsgRolledBack point ->
            "Rolled back to " <> pretty point
        MsgFollow msg ->
            toText msg
        MsgDelegation slotId cert -> case cert of
            CertDelegateNone{} -> mconcat
                [ "Discovered end of delegation within slot "
                , pretty slotId
                ]
            CertDelegateFull{} -> mconcat
                [ "Discovered delegation to pool "
                , pretty (dlgCertPoolId cert)
                , " within slot "
                , pretty slotId
                ]
        MsgCheckpoint checkpointTip ->
            "Creating checkpoint at " <> pretty checkpointTip
        MsgWalletMetadata meta ->
            pretty meta
        MsgSyncProgress progress ->
            "syncProgress: " <> pretty progress
        MsgDiscoveredTxs txs ->
            "discovered " <> pretty (length txs) <> " new transaction(s)"
        MsgDiscoveredTxsContent txs ->
            "transactions: " <> pretty (blockListF (snd <$> txs))
        MsgTip tip ->
            "local tip: " <> pretty tip
        MsgBlocks blocks ->
            "blocks: " <> pretty (NE.toList blocks)
        MsgDelegationCoinSelection sel ->
            "Coins selected for delegation: \n" <> pretty sel
        MsgPaymentCoinSelection sel ->
            "Coins selected for payment: \n" <> pretty sel
        MsgPaymentCoinSelectionAdjusted sel ->
            "Coins after fee adjustment: \n" <> pretty sel

instance HasPrivacyAnnotation WalletLog
instance HasSeverityAnnotation WalletLog where
    getSeverityAnnotation = \case
        MsgTryingRollback _ -> Info
        MsgRolledBack _ -> Info
        MsgFollow msg -> getSeverityAnnotation msg
        MsgDelegation _ _ -> Info
        MsgCheckpoint _ -> Info
        MsgWalletMetadata _ -> Info
        MsgSyncProgress _ -> Info
        MsgDiscoveredTxs _ -> Info
        MsgDiscoveredTxsContent _ -> Debug
        MsgTip _ -> Info
        MsgBlocks _ -> Debug
        MsgDelegationCoinSelection _ -> Debug
        MsgPaymentCoinSelection _ -> Debug
        MsgPaymentCoinSelectionAdjusted _ -> Debug
