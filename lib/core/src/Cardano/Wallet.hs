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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
-- - @k@: A __k__ey derivation scheme intrisically connected to the underlying discovery
--   state @s@. This describes how the hierarchical structure of a wallet is
--   defined as well as the relationship between secret keys and public
--   addresses.

module Cardano.Wallet
    (
    -- * Development
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
    , manageRewardBalance
    , rollbackBlocks
    , checkWalletIntegrity
    , readNextWithdrawal
    , readRewardAccount
    , someRewardAccount
    , queryRewardBalance
    , ErrWalletAlreadyExists (..)
    , ErrNoSuchWallet (..)
    , ErrListUTxOStatistics (..)
    , ErrUpdatePassphrase (..)
    , ErrFetchRewards (..)
    , ErrCheckWalletIntegrity (..)
    , ErrWalletNotResponding (..)
    , ErrReadRewardAccount (..)

    -- ** Address
    , createChangeAddress
    , createRandomAddress
    , importRandomAddresses
    , listAddresses
    , normalizeDelegationAddress
    , ErrCreateRandomAddress(..)
    , ErrImportRandomAddress(..)
    , ErrImportAddress(..)

    -- ** Payment
    , getTxExpiry
    , selectAssets
    , selectAssetsNoOutputs
    , selectionToUnsignedTx
    , signTransaction
    , ErrSelectAssets(..)
    , ErrSignPayment (..)
    , ErrNotASequentialWallet (..)
    , ErrWithdrawalNotWorth (..)

    -- ** Migration

    -- ** Delegation
    , PoolRetirementEpochInfo (..)
    , joinStakePool
    , quitStakePool
    , guardJoin
    , guardQuit
    , ErrJoinStakePool (..)
    , ErrCannotJoin (..)
    , ErrQuitStakePool (..)
    , ErrCannotQuit (..)

    -- ** Fee Estimation
    , FeeEstimation (..)
    , estimateFee
    , calcMinimumDeposit

    -- ** Transaction
    , forgetTx
    , listTransactions
    , getTransaction
    , submitExternalTx
    , submitTx
    , ErrMkTx (..)
    , ErrSubmitTx (..)
    , ErrSubmitExternalTx (..)
    , ErrRemoveTx (..)
    , ErrPostTx (..)
    , ErrDecodeSignedTx (..)
    , ErrListTransactions (..)
    , ErrGetTransaction (..)
    , ErrNoSuchTransaction (..)
    , ErrNetworkUnavailable (..)
    , ErrStartTimeLaterThanEndTime (..)

    -- ** Root Key
    , withRootKey
    , derivePublicKey
    , signMetadataWith
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , ErrSignMetadataWith (..)
    , ErrDerivePublicKey(..)
    , ErrInvalidDerivationIndex(..)

    -- * Logging
    , WalletLog (..)
    ) where

import Prelude hiding
    ( log )

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Api.Typed
    ( serialiseToCBOR )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrRemoveTx (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , SparseCheckpointsConfig (..)
    , defaultSparseCheckpointsConfig
    , sparseCheckpoints
    )
import Cardano.Wallet.Network
    ( ErrGetAccountBalance (..)
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
    , DerivationIndex (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , ErrWrongPassphrase (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , Passphrase
    , PaymentAddress (..)
    , Role (..)
    , SoftDerivation (..)
    , ToRewardAccount (..)
    , WalletKey (..)
    , checkPassphrase
    , encryptPassphrase
    , liftIndex
    , preparePassphrase
    , stakeDerivationPath
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( ErrImportAddress (..), RndStateLike )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState
    , defaultAddressPoolGap
    , derivationPrefix
    , mkSeqStateFromRootXPrv
    , mkUnboundedAddressPoolGap
    , purposeBIP44
    , shrinkPool
    )
import Cardano.Wallet.Primitive.CoinSelection.MA.RoundRobin
    ( SelectionError (..)
    , SelectionResult (..)
    , UnableToConstructChangeError (..)
    , emptySkeleton
    , performSelection
    , selectionDelta
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , applyBlocks
    , availableUTxO
    , currentTip
    , getState
    , initWallet
    , updateState
    )
import Cardano.Wallet.Primitive.Slotting
    ( PastHorizonException (..)
    , TimeInterpreter
    , addRelTime
    , ceilingSlotAt
    , currentRelativeTime
    , interpretQuery
    , neverFails
    , slotRangeFromTimeRange
    , slotRangeFromTimeRange
    , slotToUTCTime
    , unsafeExtendSafeZone
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress, SyncTolerance (..), syncProgress )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DelegationCertificate (..)
    , GenesisParameters (..)
    , IsDelegatingTo (..)
    , NetworkParameters (..)
    , PassphraseScheme (..)
    , PoolId (..)
    , PoolLifeCycleStatus (..)
    , ProtocolParameters (..)
    , Range (..)
    , Signature (..)
    , SlottingParameters (..)
    , SortOrder (..)
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , dlgCertPoolId
    , wholeRange
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), addCoin, coinToInteger, sumCoins )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , SealedTx (..)
    , TransactionInfo (..)
    , Tx
    , TxChange (..)
    , TxIn (..)
    , TxMeta (..)
    , TxMetadata (..)
    , TxOut (..)
    , TxStatus (..)
    , UnsignedTx (..)
    , fromTransactionInfo
    , txOutCoin
    , withdrawals
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxOStatistics, computeUtxoStatistics, log10 )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrDecodeSignedTx (..)
    , ErrMkTx (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, forM_, replicateM, unless, when )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , catchE
    , except
    , mapExceptT
    , runExceptT
    , throwE
    , withExceptT
    )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), maybeToExceptT )
import Control.Monad.Trans.State
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
    ( ($>), (<&>) )
import Data.Generics.Internal.VL.Lens
    ( Lens', view, (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.List
    ( scanl' )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromMaybe, mapMaybe )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime, getCurrentTime )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Data.Word
    ( Word64 )
import Fmt
    ( blockListF, pretty, (+|), (+||), (|+), (||+) )
import GHC.Generics
    ( Generic )
import GHC.Stack
    ( HasCallStack )
import Safe
    ( lastMay )
import Statistics.Quantile
    ( medianUnbiased, quantiles )
import Type.Reflection
    ( Typeable, typeRep )
import UnliftIO.Exception
    ( Exception )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Data.ByteArray as BA
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
-- - @nw = ctx ^. networkLayer@ for the 'NetworkLayer'.
-- - @tl = ctx ^. transactionLayer \\@k@ for the 'TransactionLayer'.
-- - @re = ctx ^. workerRegistry@ for the 'WorkerRegistry'.
--
-- __TroubleShooting__
--
-- @
-- • Overlapping instances for HasType (DBLayer IO s k) ctx
--     arising from a use of ‘myFunction’
--   Matching instances:
-- @
--
-- Occurs when a particular function is missing a top-level constraint
-- (because it uses another function that demands such constraint). Here,
-- `myFunction` needs its surrounding context `ctx` to have a `DBLayer` but
-- the constraint is missing from its host function.
--
-- __Fix__: Add "@HasDBLayer s k@" as a class-constraint to the surrounding function.
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

data WalletLayer s (k :: Depth -> * -> *)
    = WalletLayer
        (Tracer IO WalletLog)
        (Block, NetworkParameters, SyncTolerance)
        (NetworkLayer IO Block)
        (TransactionLayer k)
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
--     :: forall ctx s k.
--         ( HasDBLayer s k ctx
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

type HasGenesisData = HasType (Block, NetworkParameters, SyncTolerance)

type HasLogger msg = HasType (Tracer IO msg)

-- | This module is only interested in one block-, and tx-type. This constraint
-- hides that choice, for some ease of use.
type HasNetworkLayer = HasType (NetworkLayer IO Block)

type HasTransactionLayer k = HasType (TransactionLayer k)

dbLayer
    :: forall s k ctx. HasDBLayer s k ctx
    => Lens' ctx (DBLayer IO s k)
dbLayer =
    typed @(DBLayer IO s k)

genesisData
    :: forall ctx. HasGenesisData ctx
    => Lens' ctx (Block, NetworkParameters, SyncTolerance)
genesisData =
    typed @(Block, NetworkParameters, SyncTolerance)

logger
    :: forall msg ctx. HasLogger msg ctx
    => Lens' ctx (Tracer IO msg)
logger =
    typed @(Tracer IO msg)

networkLayer
    :: forall ctx. (HasNetworkLayer ctx)
    => Lens' ctx (NetworkLayer IO Block)
networkLayer =
    typed @(NetworkLayer IO Block)

transactionLayer
    :: forall k ctx. (HasTransactionLayer k ctx)
    => Lens' ctx (TransactionLayer k)
transactionLayer =
    typed @(TransactionLayer k)

{-------------------------------------------------------------------------------
                                   Wallet
-------------------------------------------------------------------------------}

-- | Initialise and store a new wallet, returning its ID.
createWallet
    :: forall ctx s k.
        ( HasGenesisData ctx
        , HasDBLayer s k ctx
        , IsOurs s Address
        , IsOurs s RewardAccount
        )
    => ctx
    -> WalletId
    -> WalletName
    -> s
    -> ExceptT ErrWalletAlreadyExists IO WalletId
createWallet ctx wid wname s = db & \DBLayer{..} -> do
    let (hist, cp) = initWallet block0 s
    now <- lift getCurrentTime
    let meta = WalletMetadata
            { name = wname
            , creationTime = now
            , passphraseInfo = Nothing
            , delegation = WalletDelegation NotDelegating []
            }
    mapExceptT atomically $
        initializeWallet (PrimaryKey wid) cp meta hist gp pp $> wid
  where
    db = ctx ^. dbLayer @s @k
    (block0, NetworkParameters gp _sp pp, _) = ctx ^. genesisData

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
    let s = mkSeqStateFromRootXPrv @n credentials purposeBIP44 $
            mkUnboundedAddressPoolGap 10000
    let (hist, cp) = initWallet block0 s
    let addrs = map (view #address) . concatMap (view #outputs . fst) $ hist
    let g  = defaultAddressPoolGap
    let s' = Seq.SeqState
            (shrinkPool @n (liftPaymentAddress @n) addrs g (Seq.internalPool s))
            (shrinkPool @n (liftPaymentAddress @n) addrs g (Seq.externalPool s))
            (Seq.pendingChangeIxs s)
            (Seq.rewardAccountKey s)
            (Seq.derivationPrefix s)
            (Seq.scriptPool s)
    now <- lift getCurrentTime
    let meta = WalletMetadata
            { name = wname
            , creationTime = now
            , passphraseInfo = Nothing
            , delegation = WalletDelegation NotDelegating []
            }
    let pk = PrimaryKey wid
    mapExceptT atomically $
        initializeWallet pk (updateState s' cp) meta hist gp pp $> wid
  where
    db = ctx ^. dbLayer @s @k
    (block0, NetworkParameters gp _sp pp, _) = ctx ^. genesisData

-- | Check whether a wallet is in good shape when restarting a worker.
checkWalletIntegrity
    :: forall ctx s k. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> GenesisParameters
    -> ExceptT ErrCheckWalletIntegrity IO ()
checkWalletIntegrity ctx wid gp = db & \DBLayer{..} -> mapExceptT atomically $ do
    gp' <- withExceptT ErrCheckWalletIntegrityNoSuchWallet $ withNoSuchWallet wid $
        readGenesisParameters (PrimaryKey wid)

    whenDifferentGenesis gp gp $ throwE $
        ErrCheckIntegrityDifferentGenesis
            (getGenesisBlockHash gp)
            (getGenesisBlockHash gp')
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
    pending <- lift $ readTxHistory pk Nothing Descending wholeRange (Just Pending)
    pure (cp, meta, Set.fromList (fromTransactionInfo <$> pending))
  where
    db = ctx ^. dbLayer @s @k

readWalletProtocolParameters
    :: forall ctx s k. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ProtocolParameters
readWalletProtocolParameters ctx wid = db & \DBLayer{..} ->
    mapExceptT atomically $
        withNoSuchWallet wid $
            readProtocolParameters (PrimaryKey wid)
  where
    db = ctx ^. dbLayer @s @k

walletSyncProgress
    :: forall ctx s.
        ( HasGenesisData ctx
        , HasNetworkLayer ctx
        , HasCallStack
        )
    => ctx
    -> Wallet s
    -> IO SyncProgress
walletSyncProgress ctx w = do
    let tip = currentTip w
    syncProgress st ti tip =<< currentRelativeTime ti
  where
    (_, _, st) = ctx ^. genesisData

    ti :: TimeInterpreter IO
    ti = neverFails
            "walletSyncProgress only converts times at the tip or before"
            (timeInterpreter $ ctx ^. networkLayer)

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
    :: forall ctx s k.
        ( HasLogger WalletLog ctx
        , HasNetworkLayer ctx
        , HasDBLayer s k ctx
        , HasGenesisData ctx
        , IsOurs s Address
        , IsOurs s RewardAccount
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
        FollowFailure ->
            restoreWallet @ctx @s @k ctx wid
        FollowRollback point -> do
            rollbackBlocks @ctx @s @k ctx wid point
            restoreWallet @ctx @s @k ctx wid
        FollowDone ->
            pure ()

  where
    db = ctx ^. dbLayer @s @k
    nw = ctx ^. networkLayer
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
    -> SlotNo
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
        , HasNetworkLayer ctx
        , HasGenesisData ctx
        , IsOurs s Address
        , IsOurs s RewardAccount
        )
    => ctx
    -> WalletId
    -> NonEmpty Block
    -> BlockHeader
    -> ExceptT ErrNoSuchWallet IO ()
restoreBlocks ctx wid blocks nodeTip = db & \DBLayer{..} -> mapExceptT atomically $ do
    cp   <- withNoSuchWallet wid (readCheckpoint $ PrimaryKey wid)
    meta <- withNoSuchWallet wid (readWalletMeta $ PrimaryKey wid)
    sp   <- liftIO $ currentSlottingParameters nl

    unless (cp `isParentOf` NE.head blocks) $ fail $ T.unpack $ T.unwords
        [ "restoreBlocks: given chain isn't a valid continuation."
        , "Wallet is at:", pretty (currentTip cp)
        , "but the given chain continues starting from:"
        , pretty (header (NE.head blocks))
        ]

    let (filteredBlocks, cps) = NE.unzip $ applyBlocks @s blocks cp
    let slotPoolDelegations =
            [ (slotNo, cert)
            | let slots = view #slotNo . view #header <$> blocks
            , let delegations = view #delegations <$> filteredBlocks
            , (slotNo, certs) <- NE.toList $ NE.zip slots delegations
            , cert <- certs
            ]
    let txs = fold $ view #transactions <$> filteredBlocks
    let epochStability = (3*) <$> getSecurityParameter sp
    let localTip = currentTip $ NE.last cps

    putTxHistory (PrimaryKey wid) txs
    updatePendingTxForExpiry (PrimaryKey wid) (view #slotNo localTip)
    forM_ slotPoolDelegations $ \delegation@(slotNo, cert) -> do
        liftIO $ logDelegation delegation
        putDelegationCertificate (PrimaryKey wid) cert slotNo

    let unstable = sparseCheckpoints cfg (nodeTip ^. #blockHeight)
            where
                -- NOTE
                -- The edge really is an optimization to avoid rolling back too
                -- "far" in the past. Yet, we let the edge construct itself
                -- organically once we reach the tip of the chain and start
                -- processing blocks one by one.
                --
                -- This prevents the wallet from trying to create too many
                -- checkpoints at once during restoration which causes massive
                -- performance degradation on large wallets.
                --
                -- Rollback may still occur during this short period, but
                -- rolling back from a few hundred blocks is relatively fast
                -- anyway.
                cfg = (defaultSparseCheckpointsConfig epochStability) { edgeSize = 0 }

    forM_ (NE.init cps) $ \cp' -> do
        let (Quantity h) = currentTip cp' ^. #blockHeight
        when (fromIntegral h `elem` unstable) $ do
            liftIO $ logCheckpoint cp'
            putCheckpoint (PrimaryKey wid) cp'

    liftIO $ logCheckpoint (NE.last cps)
    putCheckpoint (PrimaryKey wid) (NE.last cps)

    prune (PrimaryKey wid) epochStability

    liftIO $ do
        progress <- walletSyncProgress @ctx @s ctx (NE.last cps)
        traceWith tr $ MsgWalletMetadata meta
        traceWith tr $ MsgSyncProgress progress
        traceWith tr $ MsgDiscoveredTxs txs
        traceWith tr $ MsgTip localTip
        traceWith tr $ MsgBlocks blocks
        traceWith tr $ MsgDiscoveredTxsContent txs
  where
    nl = ctx ^. networkLayer
    db = ctx ^. dbLayer @s @k
    tr = ctx ^. logger @WalletLog

    logCheckpoint :: Wallet s -> IO ()
    logCheckpoint cp = traceWith tr $ MsgCheckpoint (currentTip cp)

    logDelegation :: (SlotNo, DelegationCertificate) -> IO ()
    logDelegation (slotNo, cert) = traceWith tr $ MsgDelegation slotNo cert

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
    -> ProtocolParameters
    -> ExceptT ErrNoSuchWallet IO ()
saveParams ctx wid params = db & \DBLayer{..} ->
   mapExceptT atomically $ putProtocolParameters (PrimaryKey wid) params
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

-- | Fetch the cached reward balance of a given wallet from the database.
fetchRewardBalance
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> IO Coin
fetchRewardBalance ctx wid = db & \DBLayer{..} ->
    atomically $ readDelegationRewardBalance pk
  where
    pk = PrimaryKey wid
    db = ctx ^. dbLayer @s @k

-- | Read the current withdrawal capacity of a wallet. Note that, this simply
-- returns 0 if:
--
-- a) There's no reward account for this type of wallet.
-- b) The current reward value is too small to be considered (adding it would
-- cost more than its value).
readNextWithdrawal
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        , HasTransactionLayer k ctx
        )
    => ctx
    -> WalletId
    -> Coin
    -> IO Coin
readNextWithdrawal ctx wid (Coin withdrawal) = db & \DBLayer{..} -> do
    liftIO (atomically $ readProtocolParameters $ PrimaryKey wid) <&> \case
        -- May happen if done very early, in which case, rewards are probably
        -- not woth considering anyway.
        Nothing -> Coin 0
        Just pp ->
            let
                mkTxCtx txWithdrawal = TransactionCtx
                    { txWithdrawal
                    , txMetadata = Nothing
                    , txTimeToLive = maxBound
                    , txDelegationAction = Nothing
                    }

                costWith =
                    calcMinimumCost tl pp (mkTxCtx $ Coin withdrawal) emptySkeleton

                costWithout =
                    calcMinimumCost tl pp (mkTxCtx $ Coin 0) emptySkeleton

                costOfWithdrawal =
                    coinToInteger costWith - coinToInteger costWithout
            in
                if toInteger withdrawal < 2 * costOfWithdrawal
                then Coin 0
                else Coin withdrawal
  where
    db = ctx ^. dbLayer @s @k
    tl = ctx ^. transactionLayer @k

readRewardAccount
    :: forall ctx s k (n :: NetworkDiscriminant) shelley.
        ( HasDBLayer s k ctx
        , shelley ~ SeqState n ShelleyKey
        , Typeable n
        , Typeable s
        )
    => ctx
    -> WalletId
    -> ExceptT ErrReadRewardAccount IO (RewardAccount, NonEmpty DerivationIndex)
readRewardAccount ctx wid = db & \DBLayer{..} -> do
    cp <- withExceptT ErrReadRewardAccountNoSuchWallet
        $ mapExceptT atomically
        $ withNoSuchWallet wid
        $ readCheckpoint (PrimaryKey wid)
    case testEquality (typeRep @s) (typeRep @shelley) of
        Nothing ->
            throwE ErrReadRewardAccountNotAShelleyWallet
        Just Refl -> do
            let s = getState cp
            let acct = toRewardAccount   $ Seq.rewardAccountKey s
            let path = stakeDerivationPath $ Seq.derivationPrefix s
            pure (acct, path)
  where
    db = ctx ^. dbLayer @s @k

-- | Query the node for the reward balance of a given wallet.
--
-- Rather than force all callers of 'readWallet' to wait for fetching the
-- account balance (via the 'NetworkLayer'), we expose this function for it.
queryRewardBalance
    :: forall ctx.
        ( HasNetworkLayer ctx
        )
    => ctx
    -> RewardAccount
    -> ExceptT ErrFetchRewards IO Coin
queryRewardBalance ctx acct = do
    mapExceptT (fmap handleErr) $ getAccountBalance nw acct
  where
    nw = ctx ^. networkLayer
    handleErr = \case
        Right x -> Right x
        Left (ErrGetAccountBalanceAccountNotFound _) ->
            Right $ Coin 0
        Left (ErrGetAccountBalanceNetworkUnreachable e) ->
            Left $ ErrFetchRewardsNetworkUnreachable e

manageRewardBalance
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( HasLogger WalletLog ctx
        , HasNetworkLayer ctx
        , HasDBLayer s k ctx
        , Typeable s
        , Typeable n
        )
    => Proxy n
    -> ctx
    -> WalletId
    -> IO ()
manageRewardBalance _ ctx wid = db & \DBLayer{..} -> do
    watchNodeTip $ \bh -> do
         traceWith tr $ MsgRewardBalanceQuery bh
         query <- runExceptT $ do
            (acct, _) <- withExceptT ErrFetchRewardsReadRewardAccount $
                readRewardAccount @ctx @s @k @n ctx wid
            queryRewardBalance @ctx ctx acct
         traceWith tr $ MsgRewardBalanceResult query
         case query of
            Right amt -> do
                res <- atomically $ runExceptT $ putDelegationRewardBalance pk amt
                -- It can happen that the wallet doesn't exist _yet_, whereas we
                -- already have a reward balance. If that's the case, we log and
                -- move on.
                case res of
                    Left err -> traceWith tr $ MsgRewardBalanceNoSuchWallet err
                    Right () -> pure ()
            Left _err ->
                -- Occasionaly failing to query is generally not fatal. It will
                -- just update the balance next time the tip changes.
                pure ()
    traceWith tr MsgRewardBalanceExited

  where
    pk = PrimaryKey wid
    db = ctx ^. dbLayer @s @k
    NetworkLayer{watchNodeTip} = ctx ^. networkLayer
    tr = ctx ^. logger @WalletLog

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

-- | List all addresses of a wallet with their metadata. Addresses
-- are ordered from the most-recently-discovered to the oldest known.
listAddresses
    :: forall ctx s k.
        ( HasDBLayer s k ctx
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
    cp <- mapExceptT atomically
        $ withNoSuchWallet wid
        $ readCheckpoint (PrimaryKey wid)
    let s = getState cp

    -- FIXME
    -- Stream this instead of returning it as a single block.
    return
        $ L.sortBy (\(a,_) (b,_) -> compareDiscovery s a b)
        $ mapMaybe (\(addr, st) -> (,st) <$> normalize s addr)
        $ knownAddresses s
  where
    db = ctx ^. dbLayer @s @k

createChangeAddress
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        , GenChange s
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> ExceptT ErrNoSuchWallet IO Address
createChangeAddress ctx wid argGenChange = db & \DBLayer{..} -> do
    mapExceptT atomically $ do
        cp <- withNoSuchWallet wid (readCheckpoint pk)
        let (addr, s') = genChange argGenChange (getState cp)
        putCheckpoint pk (updateState s' cp)
        pure addr
  where
    db = ctx ^. dbLayer @s @k
    pk = PrimaryKey wid

createRandomAddress
    :: forall ctx s k n.
        ( HasDBLayer s k ctx
        , PaymentAddress n k
        , RndStateLike s
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
            let accIx = Rnd.defaultAccountIndex s

            (path, s') <- case mIx of
                Just addrIx | isKnownIndex accIx addrIx s ->
                    throwE $ ErrIndexAlreadyExists addrIx
                Just addrIx ->
                    pure ((liftIndex accIx, liftIndex addrIx), s)
                Nothing ->
                    pure $ Rnd.withRNG s $ \rng ->
                        Rnd.findUnusedPath rng accIx (Rnd.unavailablePaths s)

            let prepared = preparePassphrase scheme pwd
            let addr = Rnd.deriveRndStateAddress @n xprv prepared path
            let cp' = updateState (Rnd.addPendingAddress addr path s') cp
            withExceptT ErrCreateAddrNoSuchWallet $
                putCheckpoint (PrimaryKey wid) cp'
            pure addr
  where
    db = ctx ^. dbLayer @s @k
    isKnownIndex accIx addrIx s =
        (liftIndex accIx, liftIndex addrIx) `Set.member` Rnd.unavailablePaths s

importRandomAddresses
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        , RndStateLike s
        , k ~ ByronKey
        )
    => ctx
    -> WalletId
    -> [Address]
    -> ExceptT ErrImportRandomAddress IO ()
importRandomAddresses ctx wid addrs = db & \DBLayer{..} -> mapExceptT atomically $ do
    cp <- withExceptT ErrImportAddrNoSuchWallet
        $ withNoSuchWallet wid (readCheckpoint (PrimaryKey wid))
    let s0 = getState cp
        ours = scanl' (\s addr -> s >>= Rnd.importAddress addr) (Right s0) addrs
    case last ours of
        Left err ->
            throwE $ ErrImportAddr err
        Right s' ->
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
        , s ~ SeqState n k
        )
    => s
    -> Address
    -> Maybe Address
normalizeDelegationAddress s addr = do
    fingerprint <- eitherToMaybe (paymentKeyFingerprint addr)
    pure $ liftDelegationAddress @n fingerprint $ Seq.rewardAccountKey s

{-------------------------------------------------------------------------------
                                  Transaction
-------------------------------------------------------------------------------}

-- | Augments the given outputs with new outputs. These new outputs corresponds
-- to change outputs to which new addresses are being assigned to. This updates
-- the wallet state as it needs to keep track of new pending change addresses.
assignChangeAddresses
    :: forall s m.
        ( GenChange s
        , MonadIO m
        )
    => ArgGenChange s
    -> SelectionResult TokenBundle
    -> s
    -> m (SelectionResult TxOut, s)
assignChangeAddresses argGenChange sel = runStateT $ do
    changeOuts <- forM (changeGenerated sel) $ \bundle -> do
        addr <- state (genChange argGenChange)
        pure $ TxOut addr bundle
    pure $ sel { changeGenerated = changeOuts }

selectionToUnsignedTx
    :: forall ctx s k input output change.
        ( GenChange s
        , HasDBLayer s k ctx
        , IsOurs s Address
        , input ~ (TxIn, TxOut, NonEmpty DerivationIndex)
        , output ~ TxOut
        , change ~ TxChange (NonEmpty DerivationIndex)
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> SelectionResult TokenBundle
    -> ExceptT ErrNoSuchWallet IO (UnsignedTx input output change)
selectionToUnsignedTx ctx argGenChange wid sel = do
    error "FIXME: selectionToUnsignedTx"
  where
    db = ctx ^. dbLayer @s @k

selectAssetsNoOutputs
    :: forall ctx s k.
        ( HasTransactionLayer k ctx
        , HasLogger WalletLog ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> TransactionCtx
    -> ExceptT ErrSelectAssets IO (Coin, SelectionResult TokenBundle)
selectAssetsNoOutputs ctx wid tx = do
    -- NOTE:
    -- Could be made nicer by allowing 'performSelection' to run with no target
    -- outputs, but to satisfy a minimum Ada target.
    --
    -- To work-around this immediately, I am simply creating a dummy output of
    -- exactly the required deposit amount, only to discard it on the final
    -- result. The resulting selection will therefore have a delta that is at
    -- least the size of the deposit (in practice, slightly bigger because this
    -- extra outputs also increases the apparent minimum fee).
    deposit <- calcMinimumDeposit @_ @s @k ctx wid
    let dummyAddress = Address "-- selectAssetsNoOutputs --"
    let dummyOutput  = TxOut dummyAddress (TokenBundle.fromCoin deposit)
    (actualFee, res) <- selectAssets @ctx @s @k ctx wid tx (dummyOutput :| [])
    pure (actualFee, res { outputsCovered = [] })

-- | Selects assets from the wallet's UTxO to satisfy the requested outputs in
-- the given transaction context. In case of success, returns the selection
-- and its associated cost. That is, the cost is equal to the difference between
-- inputs and outputs.
selectAssets
    :: forall ctx s k.
        ( HasTransactionLayer k ctx
        , HasLogger WalletLog ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> TransactionCtx
    -> NonEmpty TxOut
    -> ExceptT ErrSelectAssets IO (Coin, SelectionResult TokenBundle)
selectAssets ctx wid tx outs = do
    (cp, _, pending) <- withExceptT ErrSelectAssetsNoSuchWallet $
        readWallet @ctx @s @k ctx wid

    guardWithdrawal pending

    pp <- withExceptT ErrSelectAssetsNoSuchWallet $
        readWalletProtocolParameters @ctx @s @k ctx wid

    let utxo :: UTxOIndex
        utxo = UTxOIndex.fromUTxO $ availableUTxO @s pending cp

    liftIO $ traceWith tr $ MsgSelectionStart utxo outs
    sel <- performSelection
        (calcMinimumCoinValue tl pp)
        (calcMinimumCost tl pp tx)
        (initSelectionCriteria tl pp tx utxo outs)
    liftIO $ traceWith tr $ MsgSelectionDone sel
    withExceptT ErrSelectAssetsSelectionError $ except (withFee sel)
  where
    tl = ctx ^. transactionLayer @k
    tr = ctx ^. logger

    withFee
        :: Functor f
        => f (SelectionResult TokenBundle)
        -> f (Coin, SelectionResult TokenBundle)
    withFee = fmap $ \s -> (selectionDelta TokenBundle.getCoin s, s)

    -- Ensure that there's no existing pending withdrawals. Indeed, a withdrawal
    -- is necessarily withdrawing rewards in their totality. So, after a first
    -- withdrawal is executed, the reward pot is empty. So, to prevent two
    -- transactions with withdrawals to go through (which will inevitably cause
    -- one of them to never be inserted), we warn users early on about it.
    guardWithdrawal :: Set Tx -> ExceptT ErrSelectAssets IO ()
    guardWithdrawal pending = do
        case Set.lookupMin $ Set.filter hasWithdrawal pending of
            Just pendingWithdrawal | txWithdrawal tx /= Coin 0 ->
                throwE $ ErrSelectAssetsAlreadyWithdrawing pendingWithdrawal
            _otherwise ->
                pure ()
      where
        hasWithdrawal :: Tx -> Bool
        hasWithdrawal = not . null . withdrawals

-- | Produce witnesses and construct a transaction from a given
-- selection. Requires the encryption passphrase in order to decrypt
-- the root private key. Note that this doesn't broadcast the
-- transaction to the network. In order to do so, use 'submitTx'.
signTransaction
    :: forall ctx s k.
        ( HasTransactionLayer k ctx
        , HasDBLayer s k ctx
        , HasNetworkLayer ctx
        , IsOwned s k
        , GenChange s
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> ((k 'RootK XPrv, Passphrase "encryption") -> (XPrv, Passphrase "encryption"))
       -- ^ Reward account derived from the root key (or somewhere else).
    -> Passphrase "raw"
    -> TransactionCtx
    -> SelectionResult TokenBundle
    -> ExceptT ErrSignPayment IO (Tx, TxMeta, UTCTime, SealedTx)
signTransaction ctx wid argGenChange mkRwdAcct pwd txCtx sel = db & \DBLayer{..} -> do
    era <- liftIO $ currentNodeEra nl
    withRootKey @_ @s ctx wid pwd ErrSignPaymentWithRootKey $ \xprv scheme -> do
        let pwdP = preparePassphrase scheme pwd
        mapExceptT atomically $ do
            cp <- withExceptT ErrSignPaymentNoSuchWallet $ withNoSuchWallet wid $
                readCheckpoint (PrimaryKey wid)
            pp <- withExceptT ErrSignPaymentNoSuchWallet $ withNoSuchWallet wid $
                readProtocolParameters (PrimaryKey wid)
            (sel', s') <- assignChangeAddresses argGenChange sel (getState cp)
            withExceptT ErrSignPaymentNoSuchWallet $
                putCheckpoint (PrimaryKey wid) (updateState s' cp)

            let keyFrom = isOwned (getState cp) (xprv, pwdP)
            let rewardAcnt = mkRwdAcct (xprv, pwdP)

            (tx, sealedTx) <- withExceptT ErrSignPaymentMkTx $ ExceptT $ pure $
                mkTransaction tl era rewardAcnt keyFrom pp txCtx sel'

            (time, meta) <- liftIO $ mkTxMeta ti (currentTip cp) txCtx sel'
            return (tx, meta, time, sealedTx)
  where
    db = ctx ^. dbLayer @s @k
    tl = ctx ^. transactionLayer @k
    nl = ctx ^. networkLayer
    ti = timeInterpreter nl

-- | Calculate the transaction expiry slot, given a 'TimeInterpreter', and an
-- optional TTL in seconds.
--
-- If no TTL is provided, a default of 2 hours is used (note: there is no
-- particular reason why we chose that duration).
getTxExpiry
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -- ^ Context for time to slot calculation.
    -> Maybe NominalDiffTime
    -- ^ Time to live (TTL) in seconds from now.
    -> IO SlotNo
getTxExpiry ti maybeTTL = do
    expTime <- addRelTime ttl <$> currentRelativeTime (unsafeExtendSafeZone ti)
    interpretQuery (unsafeExtendSafeZone ti) $ ceilingSlotAt expTime
  where
    ttl = fromMaybe defaultTTL maybeTTL

    defaultTTL :: NominalDiffTime
    defaultTTL = 7200  -- that's 2 hours

-- | Construct transaction metadata for a pending transaction from the block
-- header of the current tip and a list of input and output.
--
-- FIXME: There's a logic duplication regarding the calculation of the transaction
-- amount between right here, and the Primitive.Model (see prefilterBlocks).
mkTxMeta
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -> BlockHeader
    -> TransactionCtx
    -> SelectionResult TxOut
    -> IO (UTCTime, TxMeta)
mkTxMeta ti' blockHeader txCtx sel =
    let
        amtOuts =
            sumCoins (txOutCoin <$> changeGenerated sel)

        amtInps
            = sumCoins (txOutCoin . snd <$> (inputsSelected sel))
            & addCoin (fromMaybe (Coin 0) (extraCoinSource sel))
    in do
        t <- slotStartTime' (blockHeader ^. #slotNo)
        return
            ( t
            , TxMeta
                { status = Pending
                , direction = if amtInps > amtOuts then Outgoing else Incoming
                , slotNo = blockHeader ^. #slotNo
                , blockHeight = blockHeader ^. #blockHeight
                , amount = Coin.distance amtInps amtOuts
                , expiry = Just (txTimeToLive txCtx)
                }
            )
  where
    slotStartTime' = interpretQuery ti . slotToUTCTime
      where
        ti = neverFails "mkTxMeta slots should never be ahead of the node tip" ti'

-- | Broadcast a (signed) transaction to the network.
submitTx
    :: forall ctx s k.
        ( HasNetworkLayer ctx
        , HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> (Tx, TxMeta, SealedTx)
    -> ExceptT ErrSubmitTx IO ()
submitTx ctx wid (tx, meta, binary) = db & \DBLayer{..} -> do
    withExceptT ErrSubmitTxNetwork  $
        postTx nw binary
    mapExceptT atomically $ withExceptT ErrSubmitTxNoSuchWallet $
        putTxHistory (PrimaryKey wid) [(tx, meta)]
  where
    db = ctx ^. dbLayer @s @k
    nw = ctx ^. networkLayer

-- | Broadcast an externally-signed transaction to the network.
submitExternalTx
    :: forall ctx k.
        ( HasNetworkLayer ctx
        , HasTransactionLayer k ctx
        )
    => ctx
    -> ByteString
    -> ExceptT ErrSubmitExternalTx IO Tx
submitExternalTx ctx bytes = do
    era <- liftIO $ currentNodeEra nw
    (tx,binary) <- withExceptT ErrSubmitExternalTxDecode $ except $
        decodeSignedTx tl era bytes
    withExceptT ErrSubmitExternalTxNetwork $ postTx nw binary
    return tx
  where
    nw = ctx ^. networkLayer
    tl = ctx ^. transactionLayer @k

-- | Remove a pending or expired transaction from the transaction history. This
-- happens at the request of the user. If the transaction is already on chain,
-- or is missing from the transaction history, an error will be returned.
--
-- If a 'Pending' transaction is removed, but later appears in a block, it will
-- be added back to the transaction history.
forgetTx
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> Hash "Tx"
    -> ExceptT ErrRemoveTx IO ()
forgetTx ctx wid tid = db & \DBLayer{..} -> do
    mapExceptT atomically $ removePendingOrExpiredTx (PrimaryKey wid) tid
  where
    db = ctx ^. dbLayer @s @k

-- | List all transactions and metadata from history for a given wallet.
listTransactions
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        , HasNetworkLayer ctx
        )
    => ctx
    -> WalletId
    -> Maybe Coin
        -- Inclusive minimum value of at least one withdrawal in each transaction
    -> Maybe UTCTime
        -- Inclusive minimum time bound.
    -> Maybe UTCTime
        -- Inclusive maximum time bound.
    -> SortOrder
    -> ExceptT ErrListTransactions IO [TransactionInfo]
listTransactions ctx wid mMinWithdrawal mStart mEnd order = db & \DBLayer{..} -> do
    when (Just True == ( (<(Coin 1)) <$> mMinWithdrawal )) $
        throwE ErrListTransactionsMinWithdrawalWrong
    let pk = PrimaryKey wid
    mapExceptT atomically $ do
        mapExceptT liftIO getSlotRange >>= maybe
            (pure [])
            (\r -> lift (readTxHistory pk mMinWithdrawal order r Nothing))
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

    db = ctx ^. dbLayer @s @k

    -- Transforms the user-specified time range into a slot range. If the
    -- user-specified range terminates before the start of the blockchain,
    -- returns 'Nothing'.
    getSlotRange
        :: ExceptT ErrListTransactions IO (Maybe (Range SlotNo))
    getSlotRange = case (mStart, mEnd) of
        (Just start, Just end) | start > end -> do
            let err = ErrStartTimeLaterThanEndTime start end
            throwE (ErrListTransactionsStartTimeLaterThanEndTime err)
        _ -> do
            withExceptT ErrListTransactionsPastHorizonException
                $ interpretQuery ti
                $ slotRangeFromTimeRange
                $ Range mStart mEnd


-- | Get transaction and metadata from history for a given wallet.
getTransaction
    :: forall ctx s k. HasDBLayer s k ctx
    => ctx
    -> WalletId
    -> Hash "Tx"
    -> ExceptT ErrGetTransaction IO TransactionInfo
getTransaction ctx wid tid = db & \DBLayer{..} -> do
    let pk = PrimaryKey wid
    res <- lift $ atomically $ runExceptT $ getTx pk tid
    case res of
        Left err -> do
            throwE (ErrGetTransactionNoSuchWallet err)
        Right Nothing -> do
            let err' = ErrNoSuchTransaction tid
            throwE (ErrGetTransactionNoSuchTransaction err')
        Right (Just tx) ->
            pure tx
  where
    db = ctx ^. dbLayer @s @k

{-------------------------------------------------------------------------------
                                  Delegation
-------------------------------------------------------------------------------}

joinStakePool
    :: forall ctx s k n.
        ( HasDBLayer s k ctx
        , HasLogger WalletLog ctx
        , s ~ SeqState n k
        )
    => ctx
    -> W.EpochNo
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> WalletId
    -> ExceptT ErrJoinStakePool IO (DelegationAction, Maybe Coin)
    -- ^ snd is the deposit
joinStakePool ctx currentEpoch knownPools pid poolStatus wid =
    db & \DBLayer{..} -> do
        (walMeta, isKeyReg) <- mapExceptT atomically $ do
            walMeta <- withExceptT ErrJoinStakePoolNoSuchWallet
                $ withNoSuchWallet wid
                $ readWalletMeta (PrimaryKey wid)
            isKeyReg <- withExceptT ErrJoinStakePoolNoSuchWallet
                $ isStakeKeyRegistered (PrimaryKey wid)
            pure (walMeta, isKeyReg)

        let mRetirementEpoch = view #retirementEpoch <$>
                W.getPoolRetirementCertificate poolStatus
        let retirementInfo =
                PoolRetirementEpochInfo currentEpoch <$> mRetirementEpoch

        withExceptT ErrJoinStakePoolCannotJoin $ except $
            guardJoin knownPools (walMeta ^. #delegation) pid retirementInfo

        liftIO $ traceWith tr $ MsgIsStakeKeyRegistered isKeyReg

        dep <- fmap stakeKeyDeposit $
            withExceptT ErrJoinStakePoolNoSuchWallet
                $ readWalletProtocolParameters @ctx @s @k ctx wid

        return $ if isKeyReg
            then (Join pid, Nothing)
            else (RegisterKeyAndJoin pid, Just dep)
  where
    db = ctx ^. dbLayer @s @k
    tr = ctx ^. logger

-- | Helper function to factor necessary logic for quitting a stake pool.
quitStakePool
    :: forall ctx s k n.
        ( HasDBLayer s k ctx
        , s ~ SeqState n k
        )
    => ctx
    -> WalletId
    -> ExceptT ErrQuitStakePool IO DelegationAction
quitStakePool ctx wid = db & \DBLayer{..} -> do
    walMeta <- mapExceptT atomically
        $ withExceptT ErrQuitStakePoolNoSuchWallet
        $ withNoSuchWallet wid
        $ readWalletMeta (PrimaryKey wid)

    rewards <- liftIO
        $ fetchRewardBalance @ctx @s @k ctx wid

    withExceptT ErrQuitStakePoolCannotQuit $ except $
        guardQuit (walMeta ^. #delegation) rewards

    pure Quit
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
    } deriving (Show, Eq, Generic)

instance NFData FeeEstimation

-- | Calculate the minimum deposit necessary if a given wallet wanted to
-- delegate to a pool. Said differently, this return either 0, or the value of
-- the key deposit protocol parameters if the wallet has no registered stake
-- key.
calcMinimumDeposit
    :: forall ctx s k.
        ( HasDBLayer s k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrSelectAssets IO Coin
calcMinimumDeposit ctx wid = db & \DBLayer{..} ->
    withExceptT ErrSelectAssetsNoSuchWallet $ do
        mapExceptT atomically (isStakeKeyRegistered $ PrimaryKey wid) >>= \case
            True ->
                pure $ Coin 0
            False ->
                stakeKeyDeposit <$> readWalletProtocolParameters @ctx @s @k ctx wid
  where
    db  = ctx ^. dbLayer @s @k

-- | Estimate the transaction fee for a given coin selection algorithm by
-- repeatedly running it (100 times) and collecting the results. In the returned
-- 'FeeEstimation', the minimum fee is that which 90% of the sampled fees are
-- greater than. The maximum fee is the highest fee observed in the samples.
estimateFee
    :: forall m. Monad m
    => ExceptT ErrSelectAssets m Coin
    -> ExceptT ErrSelectAssets m FeeEstimation
estimateFee
    = fmap deciles
    . handleErrors
    . replicateM repeats
    . runExceptT
    . fmap unCoin
    . (`catchE` handleCannotCover)
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
    mkFeeEstimation _ = error "estimateFee: impossible"

    -- Remove failed coin selections from samples. Unless they all failed, in
    -- which case pass on the error.
    handleErrors :: m [Either err a] -> ExceptT err m [a]
    handleErrors = ExceptT . fmap skipFailed
      where
        skipFailed samples = case partitionEithers samples of
            ([], []) ->
                error "estimateFee: impossible empty list"
            ((e:_), []) ->
                Left e
            (_, samples') ->
                Right samples'

    repeats = 100 -- TODO: modify repeats based on data

    -- | When estimating fee, it is rather cumbersome to return "cannot cover fee"
    -- whereas clients are just asking for an estimation. Therefore, we convert
    -- cannot cover errors into the necessary fee amount, even though there isn't
    -- enough in the wallet to cover for these fees.
    handleCannotCover :: ErrSelectAssets -> ExceptT ErrSelectAssets m Coin
    handleCannotCover = \case
        e@(ErrSelectAssetsSelectionError se) -> case se of
            UnableToConstructChange UnableToConstructChangeError{requiredCost} ->
                pure requiredCost
            _ ->
                throwE  e
        e ->
            throwE e

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

-- | Sign an arbitrary transaction metadata object with a private key belonging
-- to the wallet's account.
--
-- This is experimental, and will likely be replaced by a more robust to
-- arbitrary message signing using COSE, or a subset of it.
signMetadataWith
    :: forall ctx s k n.
        ( HasDBLayer s k ctx
        , HardDerivation k
        , AddressIndexDerivationType k ~ 'Soft
        , WalletKey k
        , s ~ SeqState n k
        )
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> (Role, DerivationIndex)
    -> TxMetadata
    -> ExceptT ErrSignMetadataWith IO (Signature TxMetadata)
signMetadataWith ctx wid pwd (role_, ix) metadata = db & \DBLayer{..} -> do
    addrIx <- withExceptT ErrSignMetadataWithInvalidIndex $ guardSoftIndex ix

    cp <- mapExceptT atomically
        $ withExceptT ErrSignMetadataWithNoSuchWallet
        $ withNoSuchWallet wid
        $ readCheckpoint (PrimaryKey wid)

    withRootKey @ctx @s @k ctx wid pwd ErrSignMetadataWithRootKey
        $ \rootK scheme -> do
            let encPwd = preparePassphrase scheme pwd
            let DerivationPrefix (_, _, acctIx) = derivationPrefix (getState cp)
            let acctK = deriveAccountPrivateKey encPwd rootK acctIx
            let addrK = deriveAddressPrivateKey encPwd acctK role_ addrIx
            let msg   = serialiseToCBOR metadata
            pure $ Signature $ BA.convert $ CC.sign encPwd (getRawKey addrK) msg
  where
    db = ctx ^. dbLayer @s @k

-- | Derive public key of a wallet's account.
derivePublicKey
    :: forall ctx s k n.
        ( HasDBLayer s k ctx
        , SoftDerivation k
        , s ~ SeqState n k
        )
    => ctx
    -> WalletId
    -> Role
    -> DerivationIndex
    -> ExceptT ErrDerivePublicKey IO (k 'AddressK XPub)
derivePublicKey ctx wid role_ ix = db & \DBLayer{..} -> do
    addrIx <- withExceptT ErrDerivePublicKeyInvalidIndex $ guardSoftIndex ix

    cp <- mapExceptT atomically
        $ withExceptT ErrDerivePublicKeyNoSuchWallet
        $ withNoSuchWallet wid
        $ readCheckpoint (PrimaryKey wid)

    -- NOTE: Alternatively, we could use 'internalPool', they share the same
    --       account public key.
    let acctK = Seq.accountPubKey $ Seq.externalPool $ getState cp
    let addrK = deriveAddressPublicKey acctK role_ addrIx

    return addrK
  where
    db = ctx ^. dbLayer @s @k

guardSoftIndex
    :: Monad m
    => DerivationIndex
    -> ExceptT ErrInvalidDerivationIndex m (Index 'Soft whatever)
guardSoftIndex ix =
    if ix > DerivationIndex (getIndex @'Soft maxBound)
    then throwE $ ErrIndexTooHigh maxBound ix
    else pure (Index $ getDerivationIndex ix)

{-------------------------------------------------------------------------------
                                   Errors
-------------------------------------------------------------------------------}

data ErrSignMetadataWith
    = ErrSignMetadataWithRootKey ErrWithRootKey
        -- ^ The wallet exists, but there's no root key attached to it
    | ErrSignMetadataWithNoSuchWallet ErrNoSuchWallet
        -- ^ The wallet doesn't exist?
    | ErrSignMetadataWithInvalidIndex ErrInvalidDerivationIndex
        -- ^ User provided a derivation index outside of the 'Soft' domain
    deriving (Eq, Show)

data ErrDerivePublicKey
    = ErrDerivePublicKeyNoSuchWallet ErrNoSuchWallet
        -- ^ The wallet doesn't exist?
    | ErrDerivePublicKeyInvalidIndex ErrInvalidDerivationIndex
        -- ^ User provided a derivation index outside of the 'Soft' domain
    deriving (Eq, Show)

data ErrInvalidDerivationIndex
    = ErrIndexTooHigh (Index 'Soft 'AddressK) DerivationIndex
    deriving (Eq, Show)

-- | Errors that can occur when listing UTxO statistics.
newtype ErrListUTxOStatistics
    = ErrListUTxOStatisticsNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors that can occur when signing a transaction.
data ErrSignPayment
    = ErrSignPaymentMkTx ErrMkTx
    | ErrSignPaymentNoSuchWallet ErrNoSuchWallet
    | ErrSignPaymentWithRootKey ErrWithRootKey
    | ErrSignPaymentIncorrectTTL PastHorizonException
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
    | ErrListTransactionsMinWithdrawalWrong
    | ErrListTransactionsPastHorizonException PastHorizonException
    deriving (Show)

-- | Errors that can occur when trying to get transaction.
data ErrGetTransaction
    = ErrGetTransactionNoSuchWallet ErrNoSuchWallet
    | ErrGetTransactionNoSuchTransaction ErrNoSuchTransaction
    deriving (Show, Eq)

-- | Indicates that the specified transaction hash is not found.
newtype ErrNoSuchTransaction = ErrNoSuchTransaction (Hash "Tx")
    deriving (Show, Eq)

-- | Indicates that the specified start time is later than the specified end
-- time.
data ErrStartTimeLaterThanEndTime = ErrStartTimeLaterThanEndTime
    { errStartTime :: UTCTime
    , errEndTime :: UTCTime
    } deriving (Show, Eq)

data ErrSelectAssets
    = ErrSelectAssetsNoSuchWallet ErrNoSuchWallet
    | ErrSelectAssetsAlreadyWithdrawing Tx
    | ErrSelectAssetsSelectionError SelectionError
    deriving (Generic, Eq, Show)

data ErrJoinStakePool
    = ErrJoinStakePoolNoSuchWallet ErrNoSuchWallet
    | ErrJoinStakePoolCannotJoin ErrCannotJoin
    deriving (Generic, Eq, Show)

data ErrQuitStakePool
    = ErrQuitStakePoolNoSuchWallet ErrNoSuchWallet
    | ErrQuitStakePoolCannotQuit ErrCannotQuit
    deriving (Generic, Eq, Show)

-- | Errors that can occur when fetching the reward balance of a wallet
data ErrFetchRewards
    = ErrFetchRewardsNetworkUnreachable ErrNetworkUnavailable
    | ErrFetchRewardsReadRewardAccount ErrReadRewardAccount
    deriving (Generic, Eq, Show)

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
    | ErrNonNullRewards Coin
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
    | ErrImportAddr ErrImportAddress
    | ErrImportAddressNotAByronWallet
    deriving (Generic, Eq, Show)

data ErrNotASequentialWallet
    = ErrNotASequentialWallet
    deriving (Generic, Eq, Show)

data ErrReadRewardAccount
    = ErrReadRewardAccountNotAShelleyWallet
    | ErrReadRewardAccountNoSuchWallet ErrNoSuchWallet
    deriving (Generic, Eq, Show)

data ErrWithdrawalNotWorth
    = ErrWithdrawalNotWorth
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

data PoolRetirementEpochInfo = PoolRetirementEpochInfo
    { currentEpoch
        :: W.EpochNo
        -- ^ The current epoch.
    , retirementEpoch
        :: W.EpochNo
        -- ^ The retirement epoch of a pool.
    }
    deriving (Eq, Generic, Show)

guardJoin
    :: Set PoolId
    -> WalletDelegation
    -> PoolId
    -> Maybe PoolRetirementEpochInfo
    -> Either ErrCannotJoin ()
guardJoin knownPools delegation pid mRetirementEpochInfo = do
    when (pid `Set.notMember` knownPools) $
        Left (ErrNoSuchPool pid)

    forM_ mRetirementEpochInfo $ \info ->
        when (currentEpoch info >= retirementEpoch info) $
            Left (ErrNoSuchPool pid)

    when ((null next) && isDelegatingTo (== pid) active) $
        Left (ErrAlreadyDelegating pid)

    when (not (null next) && isDelegatingTo (== pid) (last next)) $
        Left (ErrAlreadyDelegating pid)
  where
    WalletDelegation {active, next} = delegation

guardQuit
    :: WalletDelegation
    -> Coin
    -> Either ErrCannotQuit ()
guardQuit WalletDelegation{active,next} rewards = do
    let last_ = maybe active (view #status) $ lastMay next

    unless (isDelegatingTo anyone last_) $
        Left ErrNotDelegatingOrAboutTo

    unless (rewards == Coin 0) $
        Left $ ErrNonNullRewards rewards
  where
    anyone = const True

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data WalletLog
    = MsgTryingRollback SlotNo
    | MsgRolledBack SlotNo
    | MsgFollow FollowLog
    | MsgDelegation SlotNo DelegationCertificate
    | MsgCheckpoint BlockHeader
    | MsgWalletMetadata WalletMetadata
    | MsgSyncProgress SyncProgress
    | MsgDiscoveredTxs [(Tx, TxMeta)]
    | MsgDiscoveredTxsContent [(Tx, TxMeta)]
    | MsgTip BlockHeader
    | MsgBlocks (NonEmpty Block)
    | MsgIsStakeKeyRegistered Bool
    | MsgSelectionStart UTxOIndex (NonEmpty TxOut)
    | MsgSelectionDone (Either SelectionError (SelectionResult TokenBundle))
    | MsgMigrationUTxOBefore UTxOStatistics
    | MsgMigrationUTxOAfter UTxOStatistics
    | MsgRewardBalanceQuery BlockHeader
    | MsgRewardBalanceResult (Either ErrFetchRewards Coin)
    | MsgRewardBalanceNoSuchWallet ErrNoSuchWallet
    | MsgRewardBalanceExited
    deriving (Show, Eq)

instance ToText WalletLog where
    toText = \case
        MsgTryingRollback point ->
            "Try rolling back to " <> pretty point
        MsgRolledBack point ->
            "Rolled back to " <> pretty point
        MsgFollow msg ->
            toText msg
        MsgDelegation slotNo cert -> case cert of
            CertDelegateNone{} -> mconcat
                [ "Discovered end of delegation within slot "
                , pretty slotNo
                ]
            CertDelegateFull{} -> mconcat
                [ "Discovered delegation to pool "
                , pretty (dlgCertPoolId cert)
                , " within slot "
                , pretty slotNo
                ]
            CertRegisterKey {} -> mconcat
                [ "Discovered stake key registration "
                , " within slot "
                , pretty slotNo
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
        MsgIsStakeKeyRegistered True ->
            "Wallet stake key is registered. Will not register it again."
        MsgIsStakeKeyRegistered False ->
            "Wallet stake key is not registered. Will register..."
        MsgSelectionStart utxo recipients ->
            "Starting coin selection " <>
            "|utxo| = "+|UTxOIndex.size utxo|+" " <>
            "#recipients = "+|NE.length recipients|+""
        MsgSelectionDone (Left e) ->
            "Failed to select assets: "+|| e ||+""
        MsgSelectionDone (Right s) ->
            "Assets selected successfully: "+| s |+""
        MsgMigrationUTxOBefore summary ->
            "About to migrate the following distribution: \n" <> pretty summary
        MsgMigrationUTxOAfter summary ->
            "Expected distribution after complete migration: \n" <> pretty summary
        MsgRewardBalanceQuery bh ->
            "Updating the reward balance for block " <> pretty bh
        MsgRewardBalanceResult (Right amt) ->
            "The reward balance is " <> pretty amt
        MsgRewardBalanceNoSuchWallet err ->
            "Trying to store a balance for a wallet that doesn't exist (yet?): " <>
            T.pack (show err)
        MsgRewardBalanceResult (Left err) ->
            "Problem fetching reward balance. Will try again on next chain update. " <>
            T.pack (show err)
        MsgRewardBalanceExited ->
            "Reward balance worker has exited."

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
        MsgSelectionStart{} -> Debug
        MsgSelectionDone{} -> Debug
        MsgMigrationUTxOBefore _ -> Info
        MsgMigrationUTxOAfter _ -> Info
        MsgIsStakeKeyRegistered _ -> Info
        MsgRewardBalanceQuery _ -> Debug
        MsgRewardBalanceResult (Right _) -> Debug
        MsgRewardBalanceResult (Left _) -> Notice
        MsgRewardBalanceNoSuchWallet{} -> Warning
        MsgRewardBalanceExited -> Notice
