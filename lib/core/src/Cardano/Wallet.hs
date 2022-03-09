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
{-# LANGUAGE ViewPatterns #-}

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
-- - @k@: A __k__ey derivation scheme intrinsically connected to the underlying discovery
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
    , getWalletUtxoSnapshot
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

    -- * Shared Wallet
    , updateCosigner
    , ErrAddCosignerKey (..)
    , ErrConstructSharedWallet (..)
    , normalizeSharedAddress

    -- ** Address
    , createRandomAddress
    , importRandomAddresses
    , listAddresses
    , normalizeDelegationAddress
    , lookupTxIns
    , lookupTxOuts
    , ErrCreateRandomAddress(..)
    , ErrImportRandomAddress(..)
    , ErrImportAddress(..)
    , ErrDecodeTx (..)

    -- ** Payment
    , getTxExpiry
    , SelectAssetsParams (..)
    , selectAssets
    , readWalletUTxOIndex
    , assignChangeAddresses
    , assignChangeAddressesAndUpdateDb
    , assignChangeAddressesWithoutDbUpdate
    , selectionToUnsignedTx
    , buildAndSignTransaction
    , signTransaction
    , constructTransaction
    , constructTxMeta
    , ErrSelectAssets(..)
    , ErrSignPayment (..)
    , ErrNotASequentialWallet (..)
    , ErrWithdrawalNotWorth (..)
    , ErrConstructTx (..)
    , ErrMintBurnAssets (..)
    , ErrBalanceTx (..)
    , BalanceTxNotSupportedReason (..)
    , ErrUpdateSealedTx (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrSubmitTransaction (..)

    -- ** Migration
    , createMigrationPlan
    , migrationPlanToSelectionWithdrawals
    , SelectionWithoutChange
    , ErrCreateMigrationPlan (..)

    -- ** Delegation
    , PoolRetirementEpochInfo (..)
    , joinStakePool
    , quitStakePool
    , guardJoin
    , guardQuit
    , ErrStakePoolDelegation (..)

    -- ** Fee Estimation
    , FeeEstimation (..)
    , estimateFee
    , calcMinimumDeposit
    , calcMinimumCoinValues

    -- ** Transaction
    , forgetTx
    , listTransactions
    , listAssets
    , getTransaction
    , submitExternalTx
    , submitTx
    , balanceTransaction
    , PartialTx (..)
    , LocalTxSubmissionConfig (..)
    , defaultLocalTxSubmissionConfig
    , runLocalTxSubmissionPool
    , ErrMkTransaction (..)
    , ErrSubmitTx (..)
    , ErrRemoveTx (..)
    , ErrPostTx (..)
    , ErrListTransactions (..)
    , ErrGetTransaction (..)
    , ErrNoSuchTransaction (..)
    , ErrStartTimeLaterThanEndTime (..)
    , ErrWitnessTx (..)

    -- ** Root Key
    , withRootKey
    , derivePublicKey
    , getAccountPublicKeyAtIndex
    , readAccountPublicKey
    , signMetadataWith
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , ErrSignMetadataWith (..)
    , ErrDerivePublicKey(..)
    , ErrReadAccountPublicKey(..)
    , ErrInvalidDerivationIndex(..)

    -- * Utilities
    , throttle
    , guardHardIndex
    , withNoSuchWallet

    -- * Logging
    , WalletWorkerLog (..)
    , WalletFollowLog (..)
    , WalletLog (..)
    , TxSubmitLog (..)
    ) where

import Prelude hiding
    ( log )

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Address.Script
    ( Cosigner (..) )
import Cardano.Api
    ( serialiseToCBOR )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Crypto.Wallet
    ( toXPub )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Wallet.CoinSelection
    ( Selection
    , SelectionBalanceError (..)
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionOf (..)
    , SelectionOutputError (..)
    , SelectionParams (..)
    , SelectionReportDetailed
    , SelectionReportSummarized
    , SelectionSkeleton (..)
    , SelectionStrategy (..)
    , UnableToConstructChangeError (..)
    , emptySkeleton
    , makeSelectionReportDetailed
    , makeSelectionReportSummarized
    , performSelection
    , selectionDelta
    )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchTransaction (..)
    , ErrNoSuchWallet (..)
    , ErrPutLocalTxSubmission (..)
    , ErrRemoveTx (..)
    , ErrWalletAlreadyExists (..)
    , SparseCheckpointsConfig (..)
    , defaultSparseCheckpointsConfig
    , sparseCheckpoints
    )
import Cardano.Wallet.DB.Checkpoints
    ( DeltaCheckpoints (..) )
import Cardano.Wallet.DB.Sqlite.AddressBook
    ( AddressBookIso, getPrologue )
import Cardano.Wallet.DB.WalletState
    ( DeltaMap (..), DeltaWalletState1 (..), fromWallet, getLatest, getSlot )
import Cardano.Wallet.Logging
    ( BracketLog
    , BracketLog' (..)
    , bracketTracer
    , formatResultMsg
    , resultSeverity
    , traceResult
    , unliftIOTracer
    )
import Cardano.Wallet.Network
    ( ChainFollowLog (..)
    , ChainFollower (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
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
    , deriveRewardAccount
    , encryptPassphrase
    , liftIndex
    , preparePassphrase
    , stakeDerivationPath
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey, deriveAccountPrivateKeyShelley )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , GetAccount (..)
    , GetPurpose (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( ErrImportAddress (..), RndStateLike )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState, defaultAddressPoolGap, mkSeqStateFromRootXPrv, purposeBIP44 )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( CredentialType (..)
    , ErrAddCosigner (..)
    , ErrScriptTemplate (..)
    , SharedState (..)
    , addCosignerAccXPub
    )
import Cardano.Wallet.Primitive.Migration
    ( MigrationPlan (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , applyBlocks
    , availableUTxO
    , currentTip
    , getState
    , initWallet
    , totalUTxO
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
    , slotToUTCTime
    , unsafeExtendSafeZone
    )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncProgress, SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , ChainPoint (..)
    , DelegationCertificate (..)
    , FeePolicy (LinearFee)
    , GenesisParameters (..)
    , IsDelegatingTo (..)
    , NetworkParameters (..)
    , PassphraseScheme (..)
    , PoolId (..)
    , PoolLifeCycleStatus (..)
    , ProtocolParameters (..)
    , Range (..)
    , Signature (..)
    , Slot
    , SlottingParameters (..)
    , SortOrder (..)
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , dlgCertPoolId
    , toSlot
    , wholeRange
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer (..), redeemerData )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , LocalTxSubmissionStatus
    , SealedTx (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxChange (..)
    , TxIn (..)
    , TxMeta (..)
    , TxMetadata (..)
    , TxOut (..)
    , TxStatus (..)
    , UnsignedTx (..)
    , fromTransactionInfo
    , txOutAddCoin
    , txOutCoin
    , withdrawals
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..), UTxOStatistics, computeUtxoStatistics, log10 )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrAssignRedeemers
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrMkTransaction (..)
    , ErrSignTx (..)
    , ErrUpdateSealedTx (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    , TxFeeUpdate (..)
    , TxUpdate (..)
    , Withdrawal (..)
    , defaultTransactionCtx
    , withdrawalToCoin
    )
import Cardano.Wallet.Util
    ( mapFirst )
import Control.Applicative
    ( (<|>) )
import Control.Arrow
    ( left )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, forM_, replicateM, unless, when )
import Control.Monad.Class.MonadTime
    ( DiffTime
    , MonadMonotonicTime (..)
    , MonadTime (..)
    , Time
    , diffTime
    , getCurrentTime
    )
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO )
import Control.Monad.Random.Class
    ( MonadRandom (..) )
import Control.Monad.Random.Extra
    ( StdGenSeed (..), stdGenFromSeed, stdGenSeed )
import Control.Monad.Random.Strict
    ( evalRand )
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
    ( evalState, runState, state )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Crypto.Hash
    ( Blake2b_256, hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.DBVar
    ( modifyDBMaybe )
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
import Data.IntCast
    ( intCast )
import Data.Kind
    ( Type )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( fromMaybe, isJust, mapMaybe )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( NominalDiffTime, UTCTime )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import Data.Void
    ( Void )
import Data.Word
    ( Word16, Word64 )
import Fmt
    ( Buildable
    , Builder
    , blockListF
    , blockMapF
    , build
    , listF'
    , nameF
    , pretty
    , unlinesF
    , (+|)
    , (+||)
    , (|+)
    , (||+)
    )
import GHC.Generics
    ( Generic )
import Safe
    ( lastMay )
import Statistics.Quantile
    ( medianUnbiased, quantiles )
import Text.Pretty.Simple
    ( pShow )
import Type.Reflection
    ( Typeable, typeRep )
import UnliftIO.Exception
    ( Exception, catch, throwIO )
import UnliftIO.MVar
    ( modifyMVar_, newMVar )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Shared as Shared
import qualified Cardano.Wallet.Primitive.Migration as Migration
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
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

data WalletLayer m s (k :: Depth -> Type -> Type)
    = WalletLayer
        (Tracer m WalletWorkerLog)
        (Block, NetworkParameters, SyncTolerance)
        (NetworkLayer m Block)
        (TransactionLayer k SealedTx)
        (DBLayer m s k)
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
type HasDBLayer m s k = HasType (DBLayer m s k)

type HasGenesisData = HasType (Block, NetworkParameters, SyncTolerance)

type HasLogger m msg = HasType (Tracer m msg)

-- | This module is only interested in one block-, and tx-type. This constraint
-- hides that choice, for some ease of use.
type HasNetworkLayer m = HasType (NetworkLayer m Block)

type HasTransactionLayer k = HasType (TransactionLayer k SealedTx)

dbLayer
    :: forall m s k ctx. HasDBLayer m s k ctx
    => Lens' ctx (DBLayer m s k)
dbLayer =
    typed @(DBLayer m s k)

genesisData
    :: forall ctx. HasGenesisData ctx
    => Lens' ctx (Block, NetworkParameters, SyncTolerance)
genesisData =
    typed @(Block, NetworkParameters, SyncTolerance)

logger
    :: forall m msg ctx. HasLogger m msg ctx
    => Lens' ctx (Tracer m msg)
logger =
    typed @(Tracer m msg)

networkLayer
    :: forall m ctx. (HasNetworkLayer m ctx)
    => Lens' ctx (NetworkLayer m Block)
networkLayer =
    typed @(NetworkLayer m Block)

transactionLayer
    :: forall k ctx. (HasTransactionLayer k ctx)
    => Lens' ctx (TransactionLayer k SealedTx)
transactionLayer =
    typed @(TransactionLayer k SealedTx)

{-------------------------------------------------------------------------------
                                   Wallet
-------------------------------------------------------------------------------}

-- | Initialise and store a new wallet, returning its ID.
createWallet
    :: forall ctx m s k.
        ( MonadUnliftIO m
        , MonadTime m
        , HasGenesisData ctx
        , HasDBLayer m s k ctx
        , IsOurs s Address
        , IsOurs s RewardAccount
        )
    => ctx
    -> WalletId
    -> WalletName
    -> s
    -> ExceptT ErrWalletAlreadyExists m WalletId
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
        initializeWallet wid cp meta hist gp $> wid
  where
    db = ctx ^. dbLayer @m @s @k
    (block0, NetworkParameters gp _sp _pp, _) = ctx ^. genesisData

-- | Initialise and store a new legacy Icarus wallet. These wallets are
-- intrinsically sequential, but, in the incentivized testnet, we only have
-- access to the a snapshot of the MainNet.
--
-- To work-around this, we scan the genesis block with an arbitrary big gap and
-- resort to a default gap afterwards.
createIcarusWallet
    :: forall ctx s k n.
        ( HasGenesisData ctx
        , HasDBLayer IO s k ctx
        , PaymentAddress n k
        , k ~ IcarusKey
        , s ~ SeqState n k
        , Typeable n
        )
    => ctx
    -> WalletId
    -> WalletName
    -> (k 'RootK XPrv, Passphrase "encryption")
    -> ExceptT ErrWalletAlreadyExists IO WalletId
createIcarusWallet ctx wid wname credentials = db & \DBLayer{..} -> do
    let g  = defaultAddressPoolGap
    let s = mkSeqStateFromRootXPrv @n credentials purposeBIP44 g
    let (hist, cp) = initWallet block0 s
    now <- lift getCurrentTime
    let meta = WalletMetadata
            { name = wname
            , creationTime = now
            , passphraseInfo = Nothing
            , delegation = WalletDelegation NotDelegating []
            }
    mapExceptT atomically $
        -- FIXME: Why `updateState s cp` and not `cp`?
        -- The genesis block could very well update the address discovery state.
        initializeWallet wid (updateState s cp) meta hist gp $> wid
  where
    db = ctx ^. dbLayer @IO @s @k
    (block0, NetworkParameters gp _sp _pp, _) = ctx ^. genesisData

-- | Check whether a wallet is in good shape when restarting a worker.
checkWalletIntegrity
    :: forall ctx s k. HasDBLayer IO s k ctx
    => ctx
    -> WalletId
    -> GenesisParameters
    -> ExceptT ErrCheckWalletIntegrity IO ()
checkWalletIntegrity ctx wid gp = db & \DBLayer{..} -> mapExceptT atomically $ do
    gp' <- withExceptT ErrCheckWalletIntegrityNoSuchWallet $ withNoSuchWallet wid $
        readGenesisParameters wid

    whenDifferentGenesis gp gp $ throwE $
        ErrCheckIntegrityDifferentGenesis
            (getGenesisBlockHash gp)
            (getGenesisBlockHash gp')
  where
    db = ctx ^. dbLayer @IO @s @k
    whenDifferentGenesis bp1 bp2 = when $
        (bp1 ^. #getGenesisBlockHash /= bp2 ^. #getGenesisBlockHash) ||
        (bp1 ^. #getGenesisBlockDate /= bp2 ^. #getGenesisBlockDate)

-- | Retrieve the wallet state for the wallet with the given ID.
readWallet
    :: forall ctx s k. HasDBLayer IO s k ctx
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (Wallet s, WalletMetadata, Set Tx)
readWallet ctx wid = db & \DBLayer{..} -> mapExceptT atomically $ do
    cp <- withNoSuchWallet wid $ readCheckpoint wid
    meta <- withNoSuchWallet wid $ readWalletMeta wid
    pending <- lift $ readTxHistory wid Nothing Descending wholeRange (Just Pending)
    pure (cp, meta, Set.fromList (fromTransactionInfo <$> pending))
  where
    db = ctx ^. dbLayer @IO @s @k

walletSyncProgress
    :: forall ctx s. HasNetworkLayer IO ctx
    => ctx
    -> Wallet s
    -> IO SyncProgress
walletSyncProgress ctx w = do
    let tip = view #slotNo $ currentTip w
    syncProgress nl tip
  where
    nl = ctx ^. networkLayer

-- | Update a wallet's metadata with the given update function.
updateWallet
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> (WalletMetadata -> WalletMetadata)
    -> ExceptT ErrNoSuchWallet IO ()
updateWallet ctx wid modify = db & \DBLayer{..} -> mapExceptT atomically $ do
    meta <- withNoSuchWallet wid $ readWalletMeta wid
    putWalletMeta wid (modify meta)
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Change a wallet's passphrase to the given passphrase.
updateWalletPassphrase
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
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

getWalletUtxoSnapshot
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , HasTransactionLayer k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO [(TokenBundle, Coin)]
getWalletUtxoSnapshot ctx wid = do
    (wallet, _, pending) <- withExceptT id (readWallet @ctx @s @k ctx wid)
    pp <- liftIO $ currentProtocolParameters nl
    let bundles = availableUTxO @s pending wallet
            & unUTxO
            & F.toList
            & fmap (view #tokens)
    pure $ pairBundleWithMinAdaQuantity pp <$> bundles
  where
    nl = ctx ^. networkLayer
    tl = ctx ^. transactionLayer @k

    pairBundleWithMinAdaQuantity
        :: ProtocolParameters -> TokenBundle -> (TokenBundle, Coin)
    pairBundleWithMinAdaQuantity pp bundle =
        (bundle, computeMinAdaQuantity $ view #tokens bundle)
      where
        computeMinAdaQuantity :: TokenMap -> Coin
        computeMinAdaQuantity =
            view #txOutputMinimumAdaQuantity (view #constraints tl pp)

-- | List the wallet's UTxO statistics.
listUtxoStatistics
    :: forall ctx s k. HasDBLayer IO s k ctx
    => ctx
    -> WalletId
    -> ExceptT ErrListUTxOStatistics IO UTxOStatistics
listUtxoStatistics ctx wid = do
    (wal, _, pending) <- withExceptT
        ErrListUTxOStatisticsNoSuchWallet (readWallet @ctx @s @k ctx wid)
    let utxo = availableUTxO @s pending wal
    pure $ computeUtxoStatistics log10 utxo

-- | Restore a wallet from its current tip.
--
-- After the wallet has been restored,
-- this action will continue to fetch newly created blocks
-- and apply them, or roll back to a previous point whenever
-- the chain switches.
restoreWallet
    :: forall ctx s k.
        ( HasNetworkLayer IO ctx
        , HasDBLayer IO s k ctx
        , HasLogger IO WalletWorkerLog ctx
        , IsOurs s Address
        , IsOurs s RewardAccount
        , AddressBookIso s
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
restoreWallet ctx wid = db & \DBLayer{..} -> do
    catchFromIO $ chainSync nw (contramap MsgChainFollow tr) $ ChainFollower
        { readLocalTip = liftIO $ atomically $ listCheckpoints wid
        , rollForward = \blocks tip -> throwInIO $
            restoreBlocks @ctx @s @k
                ctx (contramap MsgWalletFollow tr) wid blocks tip
        , rollBackward =
            throwInIO . rollbackBlocks @ctx @s @k ctx wid . toSlot
        }
  where
    db = ctx ^. dbLayer @IO @s @k
    nw = ctx ^. networkLayer @IO
    tr = ctx ^. logger @_ @WalletWorkerLog

    -- See Note [CheckedExceptionsAndCallbacks]
    throwInIO :: ExceptT ErrNoSuchWallet IO a -> IO a
    throwInIO x = runExceptT x >>= \case
        Right a -> pure a
        Left  e -> throwIO $ UncheckErrNoSuchWallet e

    catchFromIO :: IO a -> ExceptT ErrNoSuchWallet IO a
    catchFromIO m = ExceptT $
        (Right <$> m) `catch` (\(UncheckErrNoSuchWallet e) -> pure $ Left e)

newtype UncheckErrNoSuchWallet = UncheckErrNoSuchWallet ErrNoSuchWallet
    deriving (Eq, Show)
instance Exception UncheckErrNoSuchWallet

{- NOTE [CheckedExceptionsAndCallbacks]

Callback functions (such as the fields of 'ChainFollower')
may throw exceptions. Such exceptions typically cause the thread
(such as 'chainSync') which calls the callbacks to exit and
to return control to its parent.

Ideally, we would like these exceptions to be \"checked exceptions\",
which means that they are visible on the type level.
In our codebase, we (should) make sure that exceptions which are checked
cannot be instances of the 'Exception' class -- in this way,
it is statically guaranteed that they cannot be thrown in the 'IO' monad.

On the flip side, visibility on the type level does imply that
the calling thread (here 'chainSync') needs to be either polymorphic
in the checked exceptions or aware of them.
Making 'chainSync' aware of the checked exception is currently
not a good idea, because this function is used in different contexts,
which have different checked exceptions.
So, it would need to be polymorphic in the underlying monad,
but at present, 'chainSync' is restricted to 'IO' because some
of its constituents are also restricted to 'IO'.

As a workaround / solution, we wrap the checked exception into a new type
which can be thrown in the 'IO' monad.
When the calling thread exits, we catch the exception again
and present it as a checked exception.

-}

-- | Rewind the UTxO snapshots, transaction history and other information to a
-- the earliest point in the past that is before or is the point of rollback.
rollbackBlocks
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> Slot
    -> ExceptT ErrNoSuchWallet IO ChainPoint
rollbackBlocks ctx wid point = db & \DBLayer{..} -> do
    mapExceptT atomically $ rollbackTo wid point
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Apply the given blocks to the wallet and update the wallet state,
-- transaction history and corresponding metadata.
restoreBlocks
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , IsOurs s Address
        , IsOurs s RewardAccount
        , AddressBookIso s
        )
    => ctx
    -> Tracer IO WalletFollowLog
    -> WalletId
    -> NonEmpty Block
    -> BlockHeader
    -> ExceptT ErrNoSuchWallet IO ()
restoreBlocks ctx tr wid blocks nodeTip = db & \DBLayer{..} -> mapExceptT atomically $ do
    cp0  <- withNoSuchWallet wid (readCheckpoint wid)
    sp   <- liftIO $ currentSlottingParameters nl

    unless (cp0 `isParentOf` NE.head blocks) $ fail $ T.unpack $ T.unwords
        [ "restoreBlocks: given chain isn't a valid continuation."
        , "Wallet is at:", pretty (currentTip cp0)
        , "but the given chain continues starting from:"
        , pretty (header (NE.head blocks))
        ]

    let (filteredBlocks, cps') = NE.unzip $ applyBlocks @s blocks cp0
        cps = NE.map snd cps'
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

    putTxHistory wid txs
    updatePendingTxForExpiry wid (view #slotNo localTip)
    forM_ slotPoolDelegations $ \delegation@(slotNo, cert) -> do
        liftIO $ logDelegation delegation
        putDelegationCertificate wid cert slotNo

    let unstable = Set.fromList $ sparseCheckpoints cfg (nodeTip ^. #blockHeight)
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

        getBlockHeight cp = fromIntegral $
            cp ^. #currentTip . #blockHeight . #getQuantity
        willKeep cp = getBlockHeight cp `Set.member` unstable
        cpsKeep = filter willKeep (NE.init cps) <> [NE.last cps]

        -- NOTE: We have to update the 'Prologue' as well,
        -- as it can contain addresses for pending transactions,
        -- which are removed from the 'Prologue' once the
        -- transactions are accepted onto the chain and discovered.
        --
        -- I'm not so sure that the approach here is correct with
        -- respect to rollbacks, but it is functionally the same
        -- as the code that came before.
        deltaPrologue =
            [ ReplacePrologue $ getPrologue $ getState $ NE.last cps ]
        delta = deltaPrologue ++ reverse
            [ UpdateCheckpoints $ PutCheckpoint (getSlot wcp) wcp
            | wcp <- map (snd . fromWallet) cpsKeep
            ]

    liftIO $ mapM_ logCheckpoint cpsKeep
    ExceptT $ modifyDBMaybe walletsDB $
        adjustNoSuchWallet wid id $ \_ -> Right ( delta, () )

    prune wid epochStability

    liftIO $ do
        traceWith tr $ MsgDiscoveredTxs txs
        traceWith tr $ MsgDiscoveredTxsContent txs
  where
    nl = ctx ^. networkLayer
    db = ctx ^. dbLayer @IO @s @k

    logCheckpoint :: Wallet s -> IO ()
    logCheckpoint cp = traceWith tr $ MsgCheckpoint (currentTip cp)

    logDelegation :: (SlotNo, DelegationCertificate) -> IO ()
    logDelegation = traceWith tr . uncurry MsgDiscoveredDelegationCert

    isParentOf :: Wallet s -> Block -> Bool
    isParentOf cp = (== Just parent) . parentHeaderHash . header
      where parent = headerHash $ currentTip cp

-- | Remove an existing wallet. Note that there's no particular work to
-- be done regarding the restoration worker as it will simply terminate
-- on the next tick when noticing that the corresponding wallet is gone.
deleteWallet
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
deleteWallet ctx wid = db & \DBLayer{..} -> do
    mapExceptT atomically $ removeWallet wid
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Fetch the cached reward balance of a given wallet from the database.
fetchRewardBalance
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> IO Coin
fetchRewardBalance ctx wid = db & \DBLayer{..} ->
    atomically $ readDelegationRewardBalance wid
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Read the current withdrawal capacity of a wallet. Note that, this simply
-- returns 0 if:
--
-- a) There's no reward account for this type of wallet.
-- b) The current reward value is too small to be considered (adding it would
-- cost more than its value).
readNextWithdrawal
    :: forall ctx k.
        ( HasTransactionLayer k ctx
        , HasNetworkLayer IO ctx
        )
    => ctx
    -> Coin
    -> IO Coin
readNextWithdrawal ctx (Coin withdrawal) = do
    pp <- currentProtocolParameters nl

    let costWith =
            calcMinimumCost tl pp (mkTxCtx $ Coin withdrawal) emptySkeleton

    let costWithout =
            calcMinimumCost tl pp (mkTxCtx $ Coin 0) emptySkeleton

    let costOfWithdrawal =
            Coin.toInteger costWith - Coin.toInteger costWithout

    if toInteger withdrawal < 2 * costOfWithdrawal
    then pure (Coin 0)
    else pure (Coin withdrawal)
  where
    tl = ctx ^. transactionLayer @k
    nl = ctx ^. networkLayer

    mkTxCtx wdrl = defaultTransactionCtx
        { txWithdrawal = WithdrawalSelf dummyAcct dummyPath wdrl }
      where
        dummyAcct =
            RewardAccount mempty
        dummyPath =
            DerivationIndex 0 :| []

readRewardAccount
    :: forall ctx s k (n :: NetworkDiscriminant) shelley.
        ( HasDBLayer IO s k ctx
        , shelley ~ SeqState n ShelleyKey
        , Typeable n
        , Typeable s
        )
    => ctx
    -> WalletId
    -> ExceptT ErrReadRewardAccount IO (RewardAccount, XPub, NonEmpty DerivationIndex)
readRewardAccount ctx wid = db & \DBLayer{..} -> do
    cp <- withExceptT ErrReadRewardAccountNoSuchWallet
        $ mapExceptT atomically
        $ withNoSuchWallet wid
        $ readCheckpoint wid
    case testEquality (typeRep @s) (typeRep @shelley) of
        Nothing ->
            throwE ErrReadRewardAccountNotAShelleyWallet
        Just Refl -> do
            let s = getState cp
            let xpub = Seq.rewardAccountKey s
            let acct = toRewardAccount xpub
            let path = stakeDerivationPath $ Seq.derivationPrefix s
            pure (acct, getRawKey xpub, path)
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Query the node for the reward balance of a given wallet.
--
-- Rather than force all callers of 'readWallet' to wait for fetching the
-- account balance (via the 'NetworkLayer'), we expose this function for it.
queryRewardBalance
    :: forall ctx.
        ( HasNetworkLayer IO ctx
        )
    => ctx
    -> RewardAccount
    -> ExceptT ErrFetchRewards IO Coin
queryRewardBalance ctx acct = do
    liftIO $ getCachedRewardAccountBalance nw acct
  where
    nw = ctx ^. networkLayer

manageRewardBalance
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( HasLogger IO WalletWorkerLog ctx
        , HasNetworkLayer IO ctx
        , HasDBLayer IO s k ctx
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
            (acct, _, _) <- withExceptT ErrFetchRewardsReadRewardAccount $
                readRewardAccount @ctx @s @k @n ctx wid
            queryRewardBalance @ctx ctx acct
         traceWith tr $ MsgRewardBalanceResult query
         case query of
            Right amt -> do
                res <- atomically $ runExceptT $
                    putDelegationRewardBalance wid amt
                -- It can happen that the wallet doesn't exist _yet_, whereas we
                -- already have a reward balance. If that's the case, we log and
                -- move on.
                case res of
                    Left err -> traceWith tr $ MsgRewardBalanceNoSuchWallet err
                    Right () -> pure ()
            Left _err ->
                -- Occasionally failing to query is generally not fatal. It will
                -- just update the balance next time the tip changes.
                pure ()
    traceWith tr MsgRewardBalanceExited

  where
    db = ctx ^. dbLayer @IO @s @k
    NetworkLayer{watchNodeTip} = ctx ^. networkLayer
    tr = contramap MsgWallet $ ctx ^. logger @_ @WalletWorkerLog

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

lookupTxIns
    :: forall ctx s k .
        ( HasDBLayer IO s k ctx
        , IsOurs s Address
        )
    => ctx
    -> WalletId
    -> [TxIn]
    -> ExceptT ErrDecodeTx IO [(TxIn, Maybe (TxOut, NonEmpty DerivationIndex))]
lookupTxIns ctx wid txins = db & \DBLayer{..} -> do
    cp <- mapExceptT atomically
          $ withExceptT ErrDecodeTxNoSuchWallet
          $ withNoSuchWallet wid
          $ readCheckpoint wid
    pure $ map (\i -> (i, lookupTxIn cp i)) txins
  where
    db = ctx ^. dbLayer @IO @s @k
    lookupTxIn :: Wallet s -> TxIn -> Maybe (TxOut, NonEmpty DerivationIndex)
    lookupTxIn cp txIn = do
        out@(TxOut addr _) <- UTxO.lookup txIn (totalUTxO mempty cp)
        path <- fst $ isOurs addr (getState cp)
        return (out, path)

lookupTxOuts
    :: forall ctx s k .
        ( HasDBLayer IO s k ctx
        , IsOurs s Address
        )
    => ctx
    -> WalletId
    -> [TxOut]
    -> ExceptT ErrDecodeTx IO [(TxOut, Maybe (NonEmpty DerivationIndex))]
lookupTxOuts ctx wid txouts = db & \DBLayer{..} -> do
    cp <- mapExceptT atomically
          $ withExceptT ErrDecodeTxNoSuchWallet
          $ withNoSuchWallet wid
          $ readCheckpoint wid
    -- NOTE: We evolve the state (in practice an address pool) as we loop
    -- through the outputs, but we don't consider pending transactions.
    -- /Theoretically/ the outputs might only be discoverable after discovering
    -- outputs other pending transactions.
    pure $ flip evalState (getState cp) $ forM txouts $ \out@(TxOut addr _) -> do
        (out,) <$> state (isOurs addr)
  where
    db = ctx ^. dbLayer @IO @s @k

-- | List all addresses of a wallet with their metadata. Addresses
-- are ordered from the most-recently-discovered to the oldest known.
listAddresses
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
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
    -> ExceptT ErrNoSuchWallet IO [(Address, AddressState, NonEmpty DerivationIndex)]
listAddresses ctx wid normalize = db & \DBLayer{..} -> do
    cp <- mapExceptT atomically
        $ withNoSuchWallet wid
        $ readCheckpoint wid
    let s = getState cp

    -- FIXME
    -- Stream this instead of returning it as a single block.
    return
        $ L.sortBy (\(a,_,_) (b,_,_) -> compareDiscovery s a b)
        $ mapMaybe (\(addr, st,path) -> (,st,path) <$> normalize s addr)
        $ knownAddresses s
  where
    db = ctx ^. dbLayer @IO @s @k

createRandomAddress
    :: forall ctx s k n.
        ( HasDBLayer IO s k ctx
        , PaymentAddress n k
        , RndStateLike s
        , k ~ ByronKey
        , AddressBookIso s
        )
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> Maybe (Index 'Hardened 'AddressK)
    -> ExceptT ErrCreateRandomAddress IO (Address, NonEmpty DerivationIndex)
createRandomAddress ctx wid pwd mIx = db & \DBLayer{..} ->
    withRootKey @ctx @s @k ctx wid pwd ErrCreateAddrWithRootKey $ \xprv scheme -> do
        ExceptT $ atomically $ modifyDBMaybe walletsDB $
            adjustNoSuchWallet wid ErrCreateAddrNoSuchWallet $
                createRandomAddress' xprv scheme
  where
    db = ctx ^. dbLayer @IO @s @k

    createRandomAddress' xprv scheme wal = case mIx of
        Just addrIx | isKnownIndex addrIx s0 ->
            Left $ ErrIndexAlreadyExists addrIx
        Just addrIx ->
            Right $ addAddress ((liftIndex accIx, liftIndex addrIx), s0)
        Nothing ->
            Right $ addAddress $ Rnd.withRNG s0 $ \rng ->
                Rnd.findUnusedPath rng accIx (Rnd.unavailablePaths s0)
      where
        s0 = getState $ getLatest wal
        accIx = Rnd.defaultAccountIndex s0
        isKnownIndex addrIx s =
            (liftIndex accIx, liftIndex addrIx) `Set.member` Rnd.unavailablePaths s

        addAddress (path, s1) =
            ( [ ReplacePrologue $ getPrologue $ Rnd.addPendingAddress addr path s1 ]
            , (addr, Rnd.toDerivationIndexes path)
            )
          where
            prepared = preparePassphrase scheme pwd
            addr = Rnd.deriveRndStateAddress @n xprv prepared path

importRandomAddresses
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , RndStateLike s
        , k ~ ByronKey
        , AddressBookIso s
        )
    => ctx
    -> WalletId
    -> [Address]
    -> ExceptT ErrImportRandomAddress IO ()
importRandomAddresses ctx wid addrs = db & \DBLayer{..} ->
    ExceptT $ atomically $ modifyDBMaybe walletsDB $
        adjustNoSuchWallet wid ErrImportAddrNoSuchWallet
            importRandomAddresses'
  where
    db = ctx ^. dbLayer @IO @s @k
    importRandomAddresses' wal = case es1 of
        Left err -> Left $ ErrImportAddr err
        Right s1 -> Right ([ ReplacePrologue $ getPrologue s1 ], ())
      where
        s0  = getState $ getLatest wal
        es1 = foldl' (\s addr -> s >>= Rnd.importAddress addr) (Right s0) addrs

-- | Adjust a specific wallet if it exists or return 'ErrNoSuchWallet'.
adjustNoSuchWallet
    :: WalletId
    -> (ErrNoSuchWallet -> e)
    -> (w -> Either e (dw, b))
    -> (Map WalletId w -> (Maybe (DeltaMap WalletId dw), Either e b))
adjustNoSuchWallet wid err update wallets = case Map.lookup wid wallets of
    Nothing -> (Nothing, Left $ err $ ErrNoSuchWallet wid)
    Just wal -> case update wal of
        Left e -> (Nothing, Left e)
        Right (dw, b) -> (Just $ Adjust wid dw, Right b)

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

-- | A 'PartialTx' is an an unbalanced 'SealedTx' along with the necessary
-- information to balance it.
--
-- The 'inputs' and 'redeemers' must match the binary transaction contained in
-- the 'sealedTx'.
data PartialTx = PartialTx
    { sealedTx :: SealedTx
    , inputs :: [(TxIn, TxOut, Maybe (Hash "Datum"))]
    , redeemers :: [Redeemer]
    } deriving (Show, Generic, Eq)

instance Buildable PartialTx where
    build (PartialTx tx ins redeemers) = nameF "PartialTx" $ mconcat
        [ nameF "inputs" (listF' inF ins)
        , nameF "redeemers" (pretty redeemers)
        , nameF "tx" (cardanoTxF (cardanoTx tx))
        ]
      where
        inF :: (TxIn, TxOut, Maybe (Hash "Datum")) -> Builder
        inF (i,o,d) = ""+|i|+" "+|o|+" "+|d|+""

        cardanoTxF :: Cardano.InAnyCardanoEra Cardano.Tx -> Builder
        cardanoTxF (Cardano.InAnyCardanoEra _ tx') = pretty $ pShow tx'

balanceTransaction
    :: forall m s k ctx.
        ( HasTransactionLayer k ctx
        , HasLogger m WalletWorkerLog ctx
        , GenChange s
        , MonadRandom m
        )
    => ctx
    -> ArgGenChange s
    -> (W.ProtocolParameters, Cardano.ProtocolParameters)
    -> TimeInterpreter (Either PastHorizonException)
    -> (UTxOIndex InputId, Wallet s, Set Tx)
    -> PartialTx
    -> ExceptT ErrBalanceTx m SealedTx
balanceTransaction
    ctx
    generateChange
    (pp, nodePParams)
    ti
    (internalUtxoAvailable, wallet, pendingTxs)
    (PartialTx partialTx@(cardanoTx -> Cardano.InAnyCardanoEra _ (Cardano.Tx (Cardano.TxBody bod) _)) externalInputs redeemers)
    = do
    let (outputs, txWithdrawal, txMetadata, txAssetsToMint, txAssetsToBurn)
            = extractFromTx partialTx

    guardExistingCollateral
    guardZeroAdaOutputs outputs
    guardDeposits
    guardConflictingWithdrawalNetworks

    (delta, extraInputs, extraCollateral, extraOutputs) <- do
        let externalSelectedUtxo = UTxOIndex.fromSequence $
                map (\(i, TxOut a b,_datumHash) -> ((i, a), b)) externalInputs

        let utxoAvailableForInputs = UTxOSelection.fromIndexPair
                (internalUtxoAvailable, externalSelectedUtxo)

        let utxoAvailableForCollateral =
                UTxOIndex.toMap internalUtxoAvailable

        -- NOTE: It is not possible to know the script execution cost in
        -- advance because it actually depends on the final transaction. Inputs
        -- selected as part of the fee balancing might have an influence on the
        -- execution cost.
        -- However, they are bounded so it is possible to balance the
        -- transaction considering only the maximum cost, and only after, try to
        -- adjust the change and ExUnits of each redeemer to something more
        -- sensible than the max execution cost.
        let txPlutusScriptExecutionCost = maxScriptExecutionCost tl pp redeemers
        let txContext = defaultTransactionCtx
                { txPlutusScriptExecutionCost
                , txMetadata
                , txWithdrawal
                , txAssetsToMint
                , txAssetsToBurn
                , txCollateralRequirement =
                    if txPlutusScriptExecutionCost > Coin 0 then
                        SelectionCollateralRequired
                    else
                        SelectionCollateralNotRequired
                } & padFeeEstimation partialTx

        -- FIXME: The coin selection and reported fees will likely be wrong in
        -- the presence of certificates (and deposits / refunds). An immediate
        -- "fix" is to return a proper error from the handler when any key or
        -- pool registration (resp. deregistration) certificate is found in the
        -- transaction. A long-term fix is to handle this case properly during
        -- balancing.
        let transform s sel =
                let (sel', _) = assignChangeAddresses generateChange sel s
                    inputs = F.toList (sel' ^. #inputs)
                 in ( selectionDelta txOutCoin sel'
                    , inputs
                    , fst <$> (sel' ^. #collateral)
                    , sel' ^. #change
                    )
        withExceptT ErrBalanceTxSelectAssets $
            selectAssets @_ @m @s @k ctx pp SelectAssetsParams
                { outputs
                , pendingTxs
                , randomSeed = Nothing
                , txContext
                , utxoAvailableForInputs
                , utxoAvailableForCollateral
                , wallet
                , selectionStrategy = SelectionStrategyOptimal
                }
                transform

    -- NOTE:
    -- Once the coin-selection is done, we need to
    --
    -- (a) Add selected inputs, collateral and change outputs to the transaction
    -- (b) Assign correct execution units to every redeemer
    -- (c) Correctly reference redeemed entities with redeemer pointers
    -- (d) Adjust fees and change output(s) to the new fees.
    --
    -- There's a strong assumption that modifying the fee value AND increasing
    -- the coin values of change outputs does not modify transaction fees; or
    -- more exactly, does not modify the execution units of scripts. This is in
    -- principle a fair assumption because script validators ought to be
    -- unaware of change outputs. If their execution costs increase when change
    -- output values increase, then it becomes impossible to guarantee that fee
    -- balancing will ever converge towards a fixed point. A script validator
    -- doing such a thing is considered bonkers and this is not a behavior we
    -- ought to support.

    candidateTx <- assembleTransaction $ TxUpdate
        { extraInputs
        , extraCollateral
        , extraOutputs
        , feeUpdate = UseNewTxFee delta
        }
    let candidateMinFee = fromMaybe (Coin 0) $
            evaluateMinimumFee tl nodePParams candidateTx


    -- Fee minimization... Ideally we should factor this out and test
    -- separately... Although, we don't want to lose the distinction between
    -- extra outputs and normal outputs

    surplus <- ExceptT . pure $ maybe
        (Left $ ErrBalanceTxNotYetSupported $ UnderestimatedFee
             (candidateMinFee `Coin.difference` delta)
             candidateTx)
        Right
        (delta `Coin.subtract` candidateMinFee)

    guardTxBalanced =<< (assembleTransaction $ TxUpdate
        { extraInputs
        , extraCollateral
        , extraOutputs = mapFirst (txOutAddCoin surplus) extraOutputs
        , feeUpdate = UseNewTxFee candidateMinFee
        })
  where
    tl = ctx ^. transactionLayer @k

    guardTxBalanced :: SealedTx -> ExceptT ErrBalanceTx m SealedTx
    guardTxBalanced tx = do
        bal <- txBalance tx
        if bal == mempty
        then pure tx
        else throwE $ ErrBalanceTxFailedBalancing bal

    txBalance :: SealedTx -> ExceptT ErrBalanceTx m Cardano.Value
    txBalance tx =
        case evaluateTransactionBalance tl tx nodePParams utxo externalInputs of
            Just x -> pure x
            Nothing -> throwE $ ErrBalanceTxUpdateError ErrByronTxNotSupported
      where
        utxo = inputMapToUTxO $ UTxOIndex.toMap internalUtxoAvailable

    assembleTransaction
        :: TxUpdate
        -> ExceptT ErrBalanceTx m SealedTx
    assembleTransaction update = ExceptT . pure $ do
        tx' <- left ErrBalanceTxUpdateError $ updateTx tl partialTx update
        left ErrBalanceTxAssignRedeemers $ assignScriptRedeemers
            tl nodePParams ti resolveInput redeemers tx'
      where
        resolveInput :: TxIn -> Maybe (TxOut, Maybe (Hash "Datum"))
        resolveInput i =
            (\(_,o,d) -> (o,d))
                <$> L.find (\(i',_,_) -> i == i') externalInputs
            <|>
            (\(_,o) -> (o, Nothing))
                <$> L.find (\(i',_) -> i == i') (extraInputs update)

    extractFromTx tx =
        let (Tx _id _fee _coll _inps outs wdrlMap meta _vldt, toMint, toBurn, _)
                = decodeTx tl tx
            -- TODO: Find a better abstraction that can cover this case.
            wdrl = WithdrawalSelf
                (error $ unwords
                    [ "WithdrawalSelf: reward account should never have been used"
                    , "when balancing a transaction, but it was!"
                    ]
                )
                (error $ unwords
                    [ "WithdrawalSelf: derivation path should never have been used"
                    , "when balancing a transaction, but it was!"
                    ]
                )
                (F.fold wdrlMap)
         in (outs, wdrl, meta, toMint, toBurn)

    -- | Wallet coin selection is unaware of many kinds of transaction content
    -- (e.g. datums, redeemers), which could be included in the input to
    -- 'balanceTransaction'. As a workaround we add some padding using
    -- 'evaluateMinimumFee'.
    --
    -- TODO: This logic needs to be consistent with how we call 'selectAssets',
    -- so it would be good to join them into some single helper.
    padFeeEstimation
        :: SealedTx
        -> TransactionCtx
        -> TransactionCtx
    padFeeEstimation sealedTx txCtx =
        let
            (walletTx, _, _, _) = decodeTx tl sealedTx
            worseEstimate = calcMinimumCost tl pp txCtx skeleton
            skeleton = SelectionSkeleton
                { skeletonInputCount = length (view #resolvedInputs walletTx)
                , skeletonOutputs = view #outputs walletTx
                , skeletonChange = mempty
                }
            LinearFee _ (Quantity b) = pp ^. #txParameters . #getFeePolicy
            -- NOTE: Coping with the later additions of script integrity hash and
            -- redeemers ex units increased from 0 to their actual values.
            extraMargin = Coin $ ceiling $ (*) b $ fromIntegral
                $ sizeOfScriptIntegrityHash
                + sum (map sizeOfRedeemer redeemers)
              where
                sizeOfRedeemer
                    = (+ sizeOfRedeemerCommon) . BS.length . redeemerData
                sizeOfScriptIntegrityHash = 35
                sizeOfRedeemerCommon = 17

            txFeePadding = (<> extraMargin) $ fromMaybe (Coin 0) $ do
                betterEstimate <- evaluateMinimumFee tl nodePParams sealedTx
                betterEstimate `Coin.subtract` worseEstimate
        in
            txCtx { txFeePadding }

    guardZeroAdaOutputs outputs = do
        -- We seem to produce imbalanced transactions if zero-ada
        -- outputs are pre-specified. Example from
        -- 'prop_balanceTransactionBalanced':
        --
        -- balanced tx:
        --  2afeed9b
        --  []
        --  inputs 2nd 01f4b788
        --  outputs address: 82d81858...6f57b300
        --          coin: 0.000000
        --          tokens: []
        --  []
        --  metadata:
        --  scriptValidity: valid

        --  Lovelace 1000000 /= Lovelace 0
        --
        --  This is probably due to selectAssets replacing 0 ada outputs with
        --  minUTxOValue in the selection, which doesn't end up in the CBOR tx.
        let zeroAdaOutputs =
                filter (\o -> view (#tokens . #coin) o == Coin 0 ) outputs
        unless (null zeroAdaOutputs) $
            throwE $ ErrBalanceTxNotYetSupported ZeroAdaOutput

    guardDeposits = do
        -- There is currently no way of telling 'selectAssets' about all the
        -- deposits and refunds in the transaction. Not via 'TransactionCtx'.
        --
        -- A promising fix would be to replace the details of 'TransactionCtx' with
        -- a (balance, fee) based on calling the node/ledger
        -- @evaluateTransactionBalance@ on the partial transaction.
        let isReg (Cardano.StakePoolRegistrationCertificate _) = True
            isReg (Cardano.StakeAddressRegistrationCertificate _) = True
            isReg (Cardano.StakeAddressDeregistrationCertificate _) = True
            isReg _ = False
        case Cardano.txCertificates bod of
            Cardano.TxCertificatesNone -> return ()
            Cardano.TxCertificates _ certs _
                | any isReg certs -> throwE $ ErrBalanceTxNotYetSupported Deposits
                | otherwise -> return ()

    guardConflictingWithdrawalNetworks = do
        -- Use of withdrawals with different networks breaks balancing.
        --
        -- For instance the partial tx might contain two withdrawals with the same
        -- key but different networks:
        -- [ (Mainnet, pkh1, coin1)
        -- , (Testnet, pkh1, coin2)
        -- ]
        --
        -- Even though this is absurd, the node/ledger @evaluateTransactionBalance@
        -- will count @coin1+coin2@ towards the total balance. Because the wallet
        -- does not consider the network tag, it will drop one of the two, leading
        -- to a discrepancy.
        let networkOfWdrl ((Cardano.StakeAddress nw _), _, _) = nw
        let conflictingWdrlNetworks = case Cardano.txWithdrawals bod of
                Cardano.TxWithdrawalsNone -> False
                Cardano.TxWithdrawals _ wdrls -> Set.size
                    (Set.fromList $ map networkOfWdrl wdrls) > 1
        when conflictingWdrlNetworks $
            throwE $ ErrBalanceTxNotYetSupported ConflictingNetworks

    guardExistingCollateral = do
        -- Coin selection does not support pre-defining collateral. In Sep 2021
        -- consensus was that we /could/ allow for it with just a day's work or so,
        -- but that the need for it was unclear enough that it was not in any way
        -- a priority.
        case Cardano.txInsCollateral bod of
            Cardano.TxInsCollateralNone -> return ()
            Cardano.TxInsCollateral _ [] -> return ()
            Cardano.TxInsCollateral _ _ ->
                throwE ErrBalanceTxExistingCollateral

-- | Augments the given outputs with new outputs. These new outputs correspond
-- to change outputs to which new addresses have been assigned. This updates
-- the wallet state as it needs to keep track of new pending change addresses.
assignChangeAddresses
    :: forall s. GenChange s
    => ArgGenChange s
    -> Selection
    -> s
    -> (SelectionOf TxOut, s)
assignChangeAddresses argGenChange sel = runState $ do
    changeOuts <- forM (view #change sel) $ \bundle -> do
        addr <- state (genChange argGenChange)
        pure $ TxOut addr bundle
    pure $ sel { change = changeOuts }

assignChangeAddressesAndUpdateDb
    :: forall ctx s k.
        ( GenChange s
        , HasDBLayer IO s k ctx
        , AddressBookIso s
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> Selection
    -> ExceptT ErrSignPayment IO (SelectionOf TxOut)
assignChangeAddressesAndUpdateDb ctx wid generateChange selection =
    db & \DBLayer{..} -> ExceptT $ atomically $ modifyDBMaybe walletsDB $
        adjustNoSuchWallet wid ErrSignPaymentNoSuchWallet
            assignChangeAddressesAndUpdateDb'
  where
    db = ctx ^. dbLayer @IO @s @k
    assignChangeAddressesAndUpdateDb' wallet = Right
        -- Newly generated change addresses only change the Prologue
        ([ReplacePrologue $ getPrologue stateUpdated], selectionUpdated)
      where
        s = getState $ getLatest wallet
        (selectionUpdated, stateUpdated) =
            assignChangeAddresses generateChange selection s

assignChangeAddressesWithoutDbUpdate
    :: forall ctx s k.
        ( GenChange s
        , HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> Selection
    -> ExceptT ErrConstructTx IO (SelectionOf TxOut)
assignChangeAddressesWithoutDbUpdate ctx wid generateChange selection =
    db & \DBLayer{..} -> mapExceptT atomically $ do
        cp <- withExceptT ErrConstructTxNoSuchWallet $
            withNoSuchWallet wid $ readCheckpoint wid
        let (selectionUpdated, _) =
                assignChangeAddresses generateChange selection (getState cp)
        pure selectionUpdated
  where
    db = ctx ^. dbLayer @IO @s @k

selectionToUnsignedTx
    :: forall s input output change withdrawal.
        ( IsOurs s Address
        , input ~ (TxIn, TxOut, NonEmpty DerivationIndex)
        , output ~ TxOut
        , change ~ TxChange (NonEmpty DerivationIndex)
        , withdrawal ~ (RewardAccount, Coin, NonEmpty DerivationIndex)
        )
    => Withdrawal
    -> SelectionOf TxOut
    -> s
    -> (UnsignedTx input output change withdrawal)
selectionToUnsignedTx wdrl sel s =
    UnsignedTx
        { unsignedInputs =
            fullyQualifiedInputs $ NE.toList $ view #inputs sel
        , unsignedOutputs =
            view #outputs sel
        , unsignedChange =
            fullyQualifiedChange $ view #change sel
        , unsignedCollateral =
            fullyQualifiedInputs $ view #collateral sel
        , unsignedWithdrawals =
            fullyQualifiedWithdrawal wdrl
        }
  where
    -- NOTE: External addresses, not known to the wallet, will be filtered out.
    qualifyAddresses
        :: (a -> Address)
        -> [a]
        -> [(a, NonEmpty DerivationIndex)]
    qualifyAddresses getAddress hasAddresses =
        mapMaybe withDerivationPath hasAddresses
      where
        withDerivationPath hasAddress =
            (hasAddress,) <$> fst (isOurs (getAddress hasAddress) s)

    fullyQualifiedInputs :: [(TxIn, TxOut)] -> [input]
    fullyQualifiedInputs =
        fmap mkInput . qualifyAddresses (view #address . snd)
      where
        mkInput ((txin, txout), path) = (txin, txout, path)

    fullyQualifiedChange :: [TxOut] -> [change]
    fullyQualifiedChange =
        fmap mkChange . qualifyAddresses (view #address)
      where
        mkChange (TxOut address bundle, derivationPath) = TxChange {..}
          where
            amount = view #coin   bundle
            assets = view #tokens bundle

    fullyQualifiedWithdrawal :: Withdrawal -> [withdrawal]
    fullyQualifiedWithdrawal = \case
        NoWithdrawal ->
            []
        WithdrawalSelf acct path c ->
            [(acct, c, path)]
        WithdrawalExternal acct path c ->
            [(acct, c, path)]

-- | Read a wallet checkpoint and index its UTxO, for 'selectAssets' and
-- 'selectAssetsNoOutputs'.
readWalletUTxOIndex
    :: forall ctx s k. HasDBLayer IO s k ctx
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (UTxOIndex InputId, Wallet s, Set Tx)
readWalletUTxOIndex ctx wid = do
    (cp, _, pending) <- readWallet @ctx @s @k ctx wid
    let utxo = UTxOIndex.fromMap $ utxoToInputMap $ availableUTxO @s pending cp
    return (utxo, cp, pending)

-- | Calculate the minimum coin values required for a bunch of specified
-- outputs.
calcMinimumCoinValues
    :: forall ctx k f.
        ( HasTransactionLayer k ctx
        , HasNetworkLayer IO ctx
        , Applicative f
        )
    => ctx
    -> f TxOut
    -> IO (f Coin)
calcMinimumCoinValues ctx outs = do
    pp <- currentProtocolParameters nl
    pure
        $ view #txOutputMinimumAdaQuantity (constraints tl pp)
        . view (#tokens . #tokens) <$> outs
  where
    nl = ctx ^. networkLayer
    tl = ctx ^. transactionLayer @k

-- TODO: ADP-1448:
--
-- Replace this type synonym with a type parameter on types that use it.
--
type InputId = (TxIn, Address)

utxoToInputMap :: UTxO -> Map InputId TokenBundle
utxoToInputMap =
    Map.fromList . fmap (\(i, TxOut a b) -> ((i, a), b)) . Map.toList . unUTxO

inputMapToUTxO :: Map InputId TokenBundle -> UTxO
inputMapToUTxO =
    UTxO . Map.fromList . fmap (\((i, a), b) -> (i, TxOut a b)) . Map.toList

-- | Parameters for the 'selectAssets' function.
--
data SelectAssetsParams s result = SelectAssetsParams
    { outputs :: [TxOut]
    , pendingTxs :: Set Tx
    , randomSeed :: Maybe StdGenSeed
    , txContext :: TransactionCtx
    , utxoAvailableForCollateral :: Map InputId TokenBundle
    , utxoAvailableForInputs :: UTxOSelection InputId
    , wallet :: Wallet s
    , selectionStrategy :: SelectionStrategy
        -- ^ Specifies which selection strategy to use. See 'SelectionStrategy'.
    }
    deriving Generic

-- | Selects assets from a wallet.
--
-- This function has the following responsibilities:
--
--  - selecting inputs from the UTxO set to pay for user-specified outputs;
--  - selecting inputs from the UTxO set to pay for collateral;
--  - producing change outputs to return excess value to the wallet;
--  - balancing a selection to pay for the transaction fee.
--
-- When selecting inputs to pay for user-specified outputs, inputs are selected
-- randomly.
--
-- By default, the seed used for random selection is derived automatically,
-- from the given 'MonadRandom' context.
--
-- However, if a concrete value is specified for the optional 'randomSeed'
-- parameter, then that value will be used instead as the seed for random
-- selection.
--
selectAssets
    :: forall ctx m s k result.
        ( HasTransactionLayer k ctx
        , HasLogger m WalletWorkerLog ctx
        , MonadRandom m
        )
    => ctx
    -> ProtocolParameters
    -> SelectAssetsParams s result
    -> (s -> Selection -> result)
    -> ExceptT ErrSelectAssets m result
selectAssets ctx pp params transform = do
    guardPendingWithdrawal
    lift $ traceWith tr $ MsgSelectionStart
        (inputMapToUTxO
            $ UTxOSelection.availableMap
            $ params ^. #utxoAvailableForInputs)
        (params ^. #outputs)
    let selectionConstraints = SelectionConstraints
            { assessTokenBundleSize =
                view #assessTokenBundleSize $
                tokenBundleSizeAssessor tl $
                pp ^. (#txParameters . #getTokenBundleMaxSize)
            , certificateDepositAmount =
                view #stakeKeyDeposit pp
            , computeMinimumAdaQuantity =
                view #txOutputMinimumAdaQuantity $ constraints tl pp
            , computeMinimumCost =
                calcMinimumCost tl pp $ params ^. #txContext
            , computeSelectionLimit =
                view #computeSelectionLimit tl pp $ params ^. #txContext
            , maximumCollateralInputCount =
                intCast @Word16 @Int $ view #maximumCollateralInputCount pp
            , minimumCollateralPercentage =
                view #minimumCollateralPercentage pp
            }
    let selectionParams = SelectionParams
            { assetsToMint =
                params ^. (#txContext . #txAssetsToMint)
            , assetsToBurn =
                params ^. (#txContext . #txAssetsToBurn)
            , extraCoinIn = Coin 0
            , extraCoinOut = Coin 0
            , outputsToCover = params ^. #outputs
            , rewardWithdrawal =
                withdrawalToCoin $ params ^. (#txContext . #txWithdrawal)
            , certificateDepositsReturned =
                case params ^. (#txContext . #txDelegationAction) of
                    Just Quit -> 1
                    _ -> 0
            , certificateDepositsTaken =
                case params ^. (#txContext . #txDelegationAction) of
                    Just (RegisterKeyAndJoin _) -> 1
                    _ -> 0
            , collateralRequirement =
                params ^. (#txContext . #txCollateralRequirement)
            , utxoAvailableForCollateral =
                params ^. #utxoAvailableForCollateral
            , utxoAvailableForInputs =
                params ^. #utxoAvailableForInputs
            , selectionStrategy =
                view #selectionStrategy params
            }
    randomSeed <- maybe stdGenSeed pure (params ^. #randomSeed)
    let mSel = flip evalRand (stdGenFromSeed randomSeed)
            $ runExceptT
            $ performSelection selectionConstraints selectionParams
    case mSel of
        Left e -> lift $
            traceWith tr $ MsgSelectionError e
        Right sel -> lift $ do
            traceWith tr $ MsgSelectionReportSummarized
                $ makeSelectionReportSummarized sel
            traceWith tr $ MsgSelectionReportDetailed
                $ makeSelectionReportDetailed sel
    withExceptT ErrSelectAssetsSelectionError $ except $
        transform (getState $ params ^. #wallet) <$> mSel
  where
    tl = ctx ^. transactionLayer @k
    tr = contramap MsgWallet $ ctx ^. logger @m

    -- Ensure that there's no existing pending withdrawals. Indeed, a withdrawal
    -- is necessarily withdrawing rewards in their totality. So, after a first
    -- withdrawal is executed, the reward pot is empty. So, to prevent two
    -- transactions with withdrawals to go through (which will inevitably cause
    -- one of them to never be inserted), we warn users early on about it.
    guardPendingWithdrawal :: ExceptT ErrSelectAssets m ()
    guardPendingWithdrawal =
        case Set.lookupMin $ Set.filter hasWithdrawal $ params ^. #pendingTxs of
            Just pendingWithdrawal
                | withdrawalToCoin txWithdrawal /= Coin 0 ->
                    throwE $ ErrSelectAssetsAlreadyWithdrawing pendingWithdrawal
            _otherwise ->
                pure ()
      where
        hasWithdrawal :: Tx -> Bool
        hasWithdrawal = not . null . withdrawals

        txWithdrawal :: Withdrawal
        txWithdrawal = params ^. (#txContext . #txWithdrawal)

signTransaction
  :: forall k
   . ( WalletKey k
     , HardDerivation k
     , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
     )
  => TransactionLayer k SealedTx
  -- ^ The way to interact with the wallet backend
  -> Cardano.AnyCardanoEra
  -- ^ The era to operate in
  -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
  -- ^ The wallets address-key lookup function
  -> (k 'RootK XPrv, Passphrase "encryption")
  -- ^ The root key of the wallet
  -> UTxO
  -- ^ The total UTxO set of the wallet (i.e. if pending transactions all
  -- applied).
  -> SealedTx
  -- ^ The transaction to sign
  -> SealedTx
  -- ^ The original transaction, with additional signatures added where
  -- necessary
signTransaction tl era keyLookup (rootKey, rootPwd) utxo =
    let
        rewardAcnt :: (XPrv, Passphrase "encryption")
        rewardAcnt =
            (getRawKey $ deriveRewardAccount @k rootPwd rootKey, rootPwd)

        inputResolver :: TxIn -> Maybe Address
        inputResolver i = do
            TxOut addr _ <- UTxO.lookup i utxo
            pure addr
    in
        addVkWitnesses tl era rewardAcnt keyLookup inputResolver

-- | Produce witnesses and construct a transaction from a given selection.
--
-- Requires the encryption passphrase in order to decrypt the root private key.
-- Note that this doesn't broadcast the transaction to the network. In order to
-- do so, use 'submitTx'.
--
buildAndSignTransaction
    :: forall ctx s k.
        ( HasTransactionLayer k ctx
        , HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , IsOwned s k
        )
    => ctx
    -> WalletId
    -> ( (k 'RootK XPrv, Passphrase "encryption") ->
         (         XPrv, Passphrase "encryption")
       )
       -- ^ Reward account derived from the root key (or somewhere else).
    -> Passphrase "raw"
    -> TransactionCtx
    -> SelectionOf TxOut
    -> ExceptT ErrSignPayment IO (Tx, TxMeta, UTCTime, SealedTx)
buildAndSignTransaction ctx wid mkRwdAcct pwd txCtx sel = db & \DBLayer{..} -> do
    era <- liftIO $ currentNodeEra nl
    withRootKey @_ @s ctx wid pwd ErrSignPaymentWithRootKey $ \xprv scheme -> do
        let pwdP = preparePassphrase scheme pwd
        mapExceptT atomically $ do
            cp <- withExceptT ErrSignPaymentNoSuchWallet
                $ withNoSuchWallet wid
                $ readCheckpoint wid
            pp <- liftIO $ currentProtocolParameters nl
            let keyFrom = isOwned (getState cp) (xprv, pwdP)
            let rewardAcnt = mkRwdAcct (xprv, pwdP)
            (tx, sealedTx) <- withExceptT ErrSignPaymentMkTx $ ExceptT $ pure $
                mkTransaction tl era rewardAcnt keyFrom pp txCtx sel
            (time, meta) <- liftIO $
                mkTxMeta ti (currentTip cp) (getState cp) txCtx sel
            return (tx, meta, time, sealedTx)
  where
    db = ctx ^. dbLayer @IO @s @k
    tl = ctx ^. transactionLayer @k
    nl = ctx ^. networkLayer
    ti = timeInterpreter nl


-- | Construct an unsigned transaction from a given selection.
constructTransaction
    :: forall ctx s k (n :: NetworkDiscriminant).
        ( HasTransactionLayer k ctx
        , HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , Typeable s
        , Typeable n
        )
    => ctx
    -> WalletId
    -> TransactionCtx
    -> SelectionOf TxOut
    -> ExceptT ErrConstructTx IO SealedTx
constructTransaction ctx wid txCtx sel = db & \DBLayer{..} -> do
    era <- liftIO $ currentNodeEra nl
    (_, xpub, _) <- withExceptT ErrConstructTxReadRewardAccount $
        readRewardAccount @ctx @s @k @n ctx wid
    mapExceptT atomically $ do
        pp <- liftIO $ currentProtocolParameters nl
        withExceptT ErrConstructTxBody $ ExceptT $ pure $
            mkUnsignedTransaction tl era xpub pp txCtx sel
  where
    db = ctx ^. dbLayer @IO @s @k
    tl = ctx ^. transactionLayer @k
    nl = ctx ^. networkLayer

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

constructTxMeta
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> TransactionCtx
    -> [(TxIn, Coin)]
    -> [TxOut]
    -> ExceptT ErrSubmitTransaction IO TxMeta
constructTxMeta ctx wid txCtx inps outs = db & \DBLayer{..} -> do
    mapExceptT atomically $ do
        cp <- withExceptT ErrSubmitTransactionNoSuchWallet
              $ withNoSuchWallet wid
              $ readCheckpoint wid
        liftIO $
            mkTxMetaWithoutSel (currentTip cp) txCtx inps outs
  where
    db = ctx ^. dbLayer @IO @s @k

mkTxMetaWithoutSel
    :: BlockHeader
    -> TransactionCtx
    -> [(TxIn, Coin)]
    -> [TxOut]
    -> IO TxMeta
mkTxMetaWithoutSel blockHeader txCtx inps outs =
    let
        amtOuts = F.fold $ map txOutCoin outs

        amtInps
            = F.fold (map snd inps)
            & case txWithdrawal txCtx of
                w@WithdrawalSelf{} -> Coin.add (withdrawalToCoin w)
                WithdrawalExternal{} -> Prelude.id
                NoWithdrawal -> Prelude.id
    in return TxMeta
       { status = Pending
       , direction = if amtInps > amtOuts then Outgoing else Incoming
       , slotNo = blockHeader ^. #slotNo
       , blockHeight = blockHeader ^. #blockHeight
       , amount = Coin.distance amtInps amtOuts
       , expiry = Just (txTimeToLive txCtx)
       }

ourCoin
    :: IsOurs s Address
    => TxOut
    -> s
    -> Maybe Coin
ourCoin (TxOut addr tokens) wState =
    case fst (isOurs addr wState) of
        Just{}  -> Just (TokenBundle.getCoin tokens)
        Nothing -> Nothing

-- | Construct transaction metadata for a pending transaction from the block
-- header of the current tip and a list of input and output.
--
-- FIXME: There's a logic duplication regarding the calculation of the transaction
-- amount between right here, and the Primitive.Model (see prefilterBlocks).
mkTxMeta
    :: IsOurs s Address
    => TimeInterpreter (ExceptT PastHorizonException IO)
    -> BlockHeader
    -> s
    -> TransactionCtx
    -> SelectionOf TxOut
    -> IO (UTCTime, TxMeta)
mkTxMeta ti' blockHeader wState txCtx sel =
    let
        amtOuts = F.fold $
            (txOutCoin <$> view #change sel)
            ++
            mapMaybe (`ourCoin` wState) (view #outputs sel)

        amtInps
            = F.fold (txOutCoin . snd <$> view #inputs sel)
            -- NOTE: In case where rewards were pulled from an external
            -- source, they aren't added to the calculation because the
            -- money is considered to come from outside of the wallet; which
            -- changes the way we look at transactions (in such case, a
            -- transaction is considered 'Incoming' since it brings extra money
            -- to the wallet from elsewhere).
            & case txWithdrawal txCtx of
                w@WithdrawalSelf{} -> Coin.add (withdrawalToCoin w)
                WithdrawalExternal{} -> Prelude.id
                NoWithdrawal -> Prelude.id
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
        ( HasNetworkLayer IO ctx
        , HasDBLayer IO s k ctx
        , HasLogger IO WalletWorkerLog ctx
        )
    => ctx
    -> WalletId
    -> (Tx, TxMeta, SealedTx)
    -> ExceptT ErrSubmitTx IO ()
submitTx ctx wid (tx, meta, binary) = traceResult tr' $ db & \DBLayer{..} -> do
    withExceptT ErrSubmitTxNetwork $
        postTx nw binary
    mapExceptT atomically $ do
        withExceptT ErrSubmitTxNoSuchWallet $
            putTxHistory wid [(tx, meta)]
        withExceptT handleLocalTxSubmissionErr $
            putLocalTxSubmission wid (tx ^. #txId) binary (meta ^. #slotNo)
  where
    db = ctx ^. dbLayer @IO @s @k
    nw = ctx ^. networkLayer

    tr = ctx ^. logger
    tr' = contramap (MsgWallet . MsgTxSubmit . MsgSubmitTx tx meta binary) tr

    handleLocalTxSubmissionErr = \case
        ErrPutLocalTxSubmissionNoSuchWallet e -> ErrSubmitTxNoSuchWallet e
        ErrPutLocalTxSubmissionNoSuchTransaction e -> ErrSubmitTxImpossible e

-- | Broadcast an externally-signed transaction to the network.
--
-- NOTE: external transactions will not be added to the LocalTxSubmission pool,
-- so the user must retry submission themselves.
submitExternalTx
    :: forall ctx k.
        ( HasNetworkLayer IO ctx
        , HasTransactionLayer k ctx
        , HasLogger IO TxSubmitLog ctx
        )
    => ctx
    -> SealedTx
    -> ExceptT ErrPostTx IO Tx
submitExternalTx ctx sealedTx = traceResult trPost $ do
    postTx nw sealedTx
    pure tx
  where
    tl = ctx ^. transactionLayer @k
    nw = ctx ^. networkLayer
    trPost = contramap (MsgSubmitExternalTx (tx ^. #txId)) (ctx ^. logger)
    (tx, _, _, _) = decodeTx tl sealedTx

-- | Remove a pending or expired transaction from the transaction history. This
-- happens at the request of the user. If the transaction is already on chain,
-- or is missing from the transaction history, an error will be returned.
--
-- If a 'Pending' transaction is removed, but later appears in a block, it will
-- be added back to the transaction history.
forgetTx
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> Hash "Tx"
    -> ExceptT ErrRemoveTx IO ()
forgetTx ctx wid tid = db & \DBLayer{..} -> do
    mapExceptT atomically $ removePendingOrExpiredTx wid tid
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Given a LocalTxSubmission record, calculate the slot when it should be
-- retried next.
--
-- The current implementation is really basic. Retry about once _n_ blocks.
scheduleLocalTxSubmission
    :: Word64  -- ^ Resubmission interval in terms of expected blocks.
    -> SlottingParameters
    -> LocalTxSubmissionStatus tx
    -> SlotNo
scheduleLocalTxSubmission numBlocks sp st = (st ^. #latestSubmission) + numSlots
  where
    numSlots = SlotNo (ceiling (fromIntegral numBlocks / f))
    ActiveSlotCoefficient f = getActiveSlotCoefficient sp

-- | Parameters for 'runLocalTxSubmissionPool'
data LocalTxSubmissionConfig = LocalTxSubmissionConfig
    { rateLimit :: DiffTime
        -- ^ Minimum time between checks of pending transactions
    , blockInterval :: Word64
        -- ^ Resubmission interval, in terms of expected blocks.
    } deriving (Generic, Show, Eq)

-- | The current default is to resubmit any pending transaction about once every
-- 10 blocks.
--
-- The default rate limit for checking the pending list is 1000ms.
defaultLocalTxSubmissionConfig :: LocalTxSubmissionConfig
defaultLocalTxSubmissionConfig = LocalTxSubmissionConfig 1 10

-- | Continuous process which monitors the chain tip and retries submission of
-- pending transactions as the chain lengthens.
--
-- Regardless of the frequency of chain updates, this function won't re-query
-- the database faster than the configured 'rateLimit'.
--
-- This only exits if the network layer 'watchNodeTip' function exits.
runLocalTxSubmissionPool
    :: forall ctx s k m.
        ( MonadUnliftIO m
        , MonadMonotonicTime m
        , HasLogger IO WalletWorkerLog ctx
        , HasNetworkLayer m ctx
        , HasDBLayer m s k ctx
        )
    => LocalTxSubmissionConfig
    -> ctx
    -> WalletId
    -> m ()
runLocalTxSubmissionPool cfg ctx wid = db & \DBLayer{..} -> do
    submitPending <- rateLimited $ \bh -> bracketTracer trBracket $ do
        sp <- currentSlottingParameters nw
        pending <- atomically $ readLocalTxSubmissionPending wid
        let sl = bh ^. #slotNo
        -- Re-submit transactions due, ignore errors
        forM_ (filter (isScheduled sp sl) pending) $ \st -> do
            _ <- runExceptT $ traceResult (trRetry (st ^. #txId)) $
                postTx nw (st ^. #submittedTx)
            atomically $ runExceptT $ putLocalTxSubmission
                wid
                (st ^. #txId)
                (st ^. #submittedTx)
                sl
    watchNodeTip nw submitPending
  where
    nw = ctx ^. networkLayer @m
    db = ctx ^. dbLayer @m @s @k

    isScheduled sp now =
        (<= now) . scheduleLocalTxSubmission (blockInterval cfg) sp

    rateLimited = throttle (rateLimit cfg) . const

    tr = unliftIOTracer $ contramap (MsgWallet . MsgTxSubmit) $
        ctx ^. logger @_ @WalletWorkerLog
    trBracket = contramap MsgProcessPendingPool tr
    trRetry i = contramap (MsgRetryPostTx i) tr

-- | Return a function to run an action at most once every _interval_.
throttle
    :: (MonadUnliftIO m, MonadMonotonicTime m)
    => DiffTime
    -> (Time -> a -> m ())
    -> m (a -> m ())
throttle interval action = do
    var <- newMVar Nothing
    pure $ \arg -> modifyMVar_ var $ \prev -> do
        now <- getMonotonicTime
        if (maybe interval (diffTime now) prev >= interval)
           then action now arg $> Just now
           else pure prev

-- | List all transactions and metadata from history for a given wallet.
listTransactions
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
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
    mapExceptT atomically $ do
        mapExceptT liftIO getSlotRange >>= maybe
            (pure [])
            (\r -> lift (readTxHistory wid mMinWithdrawal order r Nothing))
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

    db = ctx ^. dbLayer @IO @s @k

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

-- | Extract assets associated with a given wallet from its transaction history.
listAssets
    :: forall s k ctx. (HasDBLayer IO s k ctx, IsOurs s Address)
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO (Set TokenMap.AssetId)
listAssets ctx wid = db & \DBLayer{..} -> do
    cp <- mapExceptT atomically $ withNoSuchWallet wid $ readCheckpoint wid
    txs <- lift . atomically $
        let noMinWithdrawal = Nothing
            allTxStatuses = Nothing
        in readTxHistory wid noMinWithdrawal Ascending wholeRange allTxStatuses
    let txAssets :: TransactionInfo -> Set TokenMap.AssetId
        txAssets = Set.unions
            . map (TokenBundle.getAssets . view #tokens)
            . filter ourOut
            . txInfoOutputs
        ourOut TxOut{address} = ourAddress address
        ourAddress addr = isJust . fst . isOurs addr $ getState cp
    pure $ Set.unions $ map txAssets txs
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Get transaction and metadata from history for a given wallet.
getTransaction
    :: forall ctx s k. HasDBLayer IO s k ctx
    => ctx
    -> WalletId
    -> Hash "Tx"
    -> ExceptT ErrGetTransaction IO TransactionInfo
getTransaction ctx wid tid = db & \DBLayer{..} -> do
    res <- lift $ atomically $ runExceptT $ getTx wid tid
    case res of
        Left err -> do
            throwE (ErrGetTransactionNoSuchWallet err)
        Right Nothing -> do
            let err' = ErrNoSuchTransaction wid tid
            throwE (ErrGetTransactionNoSuchTransaction err')
        Right (Just tx) ->
            pure tx
  where
    db = ctx ^. dbLayer @IO @s @k

{-------------------------------------------------------------------------------
                                  Migration
-------------------------------------------------------------------------------}

createMigrationPlan
    :: forall ctx k s.
        ( HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , HasTransactionLayer k ctx
        )
    => ctx
    -> WalletId
    -> Withdrawal
    -> ExceptT ErrCreateMigrationPlan IO MigrationPlan
createMigrationPlan ctx wid rewardWithdrawal = do
    (wallet, _, pending) <- withExceptT ErrCreateMigrationPlanNoSuchWallet $
        readWallet @ctx @s @k ctx wid
    pp <- liftIO $ currentProtocolParameters nl
    let txConstraints = view #constraints tl pp
    let utxo = availableUTxO @s pending wallet
    pure
        $ Migration.createPlan txConstraints utxo
        $ Migration.RewardWithdrawal
        $ withdrawalToCoin rewardWithdrawal
  where
    nl = ctx ^. networkLayer
    tl = ctx ^. transactionLayer @k

type SelectionWithoutChange = SelectionOf Void

migrationPlanToSelectionWithdrawals
    :: MigrationPlan
    -> Withdrawal
    -> NonEmpty Address
    -> Maybe (NonEmpty (SelectionWithoutChange, Withdrawal))
migrationPlanToSelectionWithdrawals plan rewardWithdrawal outputAddressesToCycle
    = NE.nonEmpty
    $ L.reverse
    $ fst
    $ L.foldl'
        (flip accumulate)
        ([], NE.toList $ NE.cycle outputAddressesToCycle)
        (view #selections plan)
  where
    accumulate
        :: Migration.Selection (TxIn, TxOut)
        -> ([(SelectionWithoutChange, Withdrawal)], [Address])
        -> ([(SelectionWithoutChange, Withdrawal)], [Address])
    accumulate migrationSelection (selectionWithdrawals, outputAddresses) =
        ( (selection, withdrawal) : selectionWithdrawals
        , outputAddressesRemaining
        )
      where
        selection = Selection
            { inputs = view #inputIds migrationSelection
            , collateral = []
            , outputs
            , extraCoinSource
            , extraCoinSink = Coin 0
            , change = []
            , assetsToMint = TokenMap.empty
            , assetsToBurn = TokenMap.empty
            }

        -- NOTE:
        --
        -- Due to a quirk of history, we need to populate the 'extraCoinSource'
        -- field with the reward withdrawal amount, since the transaction layer
        -- uses the 'selectionDelta' function to calculate the final fee, and
        -- that particular function doesn't know about reward withdrawals.
        --
        -- This is non-ideal, because we're returning the reward withdrawal
        -- amount in two places in the output of this function.
        --
        -- In future, it would be better to return a single record whose fields
        -- more closely resemble exactly what is needed to build a transaction,
        -- and have the transaction layer calculate the actual fee based only
        -- on the contents of that record.
        --
        extraCoinSource = view #rewardWithdrawal migrationSelection

        withdrawal =
            if (view #rewardWithdrawal migrationSelection) > Coin 0
            then rewardWithdrawal
            else NoWithdrawal

        outputs :: [TxOut]
        outputs = zipWith TxOut
            (outputAddresses)
            (NE.toList $ view #outputs migrationSelection)

        outputAddressesRemaining :: [Address]
        outputAddressesRemaining =
            drop (length outputs) outputAddresses

{-------------------------------------------------------------------------------
                                  Delegation
-------------------------------------------------------------------------------}

joinStakePool
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , HasLogger IO WalletWorkerLog ctx
        )
    => ctx
    -> W.EpochNo
    -> Set PoolId
    -> PoolId
    -> PoolLifeCycleStatus
    -> WalletId
    -> ExceptT ErrStakePoolDelegation IO (DelegationAction, Maybe Coin)
    -- ^ snd is the deposit
joinStakePool ctx currentEpoch knownPools pid poolStatus wid =
    db & \DBLayer{..} -> do
        (walMeta, isKeyReg) <- mapExceptT atomically $ do
            walMeta <- withExceptT ErrStakePoolDelegationNoSuchWallet
                $ withNoSuchWallet wid
                $ readWalletMeta wid
            isKeyReg <- withExceptT ErrStakePoolDelegationNoSuchWallet
                $ isStakeKeyRegistered wid
            pure (walMeta, isKeyReg)

        let mRetirementEpoch = view #retirementEpoch <$>
                W.getPoolRetirementCertificate poolStatus
        let retirementInfo =
                PoolRetirementEpochInfo currentEpoch <$> mRetirementEpoch

        withExceptT ErrStakePoolJoin $ except $
            guardJoin knownPools (walMeta ^. #delegation) pid retirementInfo

        liftIO $ traceWith tr $ MsgIsStakeKeyRegistered isKeyReg

        dep <- liftIO $ stakeKeyDeposit <$> currentProtocolParameters nl

        return $ if isKeyReg
            then (Join pid, Nothing)
            else (RegisterKeyAndJoin pid, Just dep)
  where
    db = ctx ^. dbLayer @IO @s @k
    tr = contramap MsgWallet $ ctx ^. logger
    nl = ctx ^. networkLayer

-- | Helper function to factor necessary logic for quitting a stake pool.
quitStakePool
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> Withdrawal
    -> ExceptT ErrStakePoolDelegation IO DelegationAction
quitStakePool ctx wid wdrl = db & \DBLayer{..} -> do
    walMeta <- mapExceptT atomically
        $ withExceptT ErrStakePoolDelegationNoSuchWallet
        $ withNoSuchWallet wid
        $ readWalletMeta wid

    rewards <- liftIO
        $ fetchRewardBalance @ctx @s @k ctx wid

    withExceptT ErrStakePoolQuit $ except $
        guardQuit (walMeta ^. #delegation) wdrl rewards

    pure Quit
  where
    db = ctx ^. dbLayer @IO @s @k

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
        ( HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrSelectAssets IO Coin
calcMinimumDeposit ctx wid = db & \DBLayer{..} ->
    withExceptT ErrSelectAssetsNoSuchWallet $ do
        mapExceptT atomically (isStakeKeyRegistered wid) >>= \case
            True ->
                pure $ Coin 0
            False ->
                liftIO $ stakeKeyDeposit <$> currentProtocolParameters nl
  where
    db = ctx ^. dbLayer @IO @s @k
    nl = ctx ^. networkLayer

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
    -- if clients are just asking for an estimation. Therefore, we convert
    -- "cannot cover" errors into the necessary fee amount, even though there isn't
    -- enough in the wallet to cover for these fees.
    handleCannotCover :: ErrSelectAssets -> ExceptT ErrSelectAssets m Coin
    handleCannotCover = \case
        e@(ErrSelectAssetsSelectionError se) -> case se of
            SelectionBalanceErrorOf (UnableToConstructChange ce) ->
                case ce of
                    UnableToConstructChangeError {requiredCost} ->
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
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, Passphrase "encryption")
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKeyFromPwd ctx wid (xprv, pwd) = db & \_ -> do
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
    db = ctx ^. dbLayer @IO @s @k

-- | The hash here is the output of Scrypt function with the following parameters:
-- - logN = 14
-- - r = 8
-- - p = 1
-- - bytesNumber = 64
attachPrivateKeyFromPwdHash
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, Hash "encryption")
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKeyFromPwdHash ctx wid (xprv, hpwd) = db & \_ ->
    -- NOTE Only legacy wallets are imported through this function, passphrase
    -- were encrypted with the legacy scheme (Scrypt).
    attachPrivateKey db wid (xprv, hpwd) EncryptWithScrypt
  where
    db = ctx ^. dbLayer @IO @s @k

attachPrivateKey
    :: DBLayer IO s k
    -> WalletId
    -> (k 'RootK XPrv, Hash "encryption")
    -> PassphraseScheme
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKey db wid (xprv, hpwd) scheme = db & \DBLayer{..} -> do
    now <- liftIO getCurrentTime
    mapExceptT atomically $ do
        putPrivateKey wid (xprv, hpwd)
        meta <- withNoSuchWallet wid $ readWalletMeta wid
        let modify x = x
                { passphraseInfo = Just $ WalletPassphraseInfo
                    { lastUpdatedAt = now
                    , passphraseScheme = scheme
                    }
                }
        putWalletMeta wid (modify meta)

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
    :: forall ctx s k e a. HasDBLayer IO s k ctx
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> (ErrWithRootKey -> e)
    -> (k 'RootK XPrv -> PassphraseScheme -> ExceptT e IO a)
    -> ExceptT e IO a
withRootKey ctx wid pwd embed action = db & \DBLayer{..} -> do
    (xprv, scheme) <- withExceptT embed $ mapExceptT atomically $ do
        mScheme <- (>>= (fmap passphraseScheme . passphraseInfo)) <$>
            lift (readWalletMeta wid)
        mXPrv <- lift $ readPrivateKey wid
        case (mXPrv, mScheme) of
            (Just (xprv, hpwd), Just scheme) -> do
                withExceptT (ErrWithRootKeyWrongPassphrase wid) $ ExceptT $
                    return $ checkPassphrase scheme pwd hpwd
                return (xprv, scheme)
            _ ->
                throwE $ ErrWithRootKeyNoRootKey wid
    action xprv scheme
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Sign an arbitrary transaction metadata object with a private key belonging
-- to the wallet's account.
--
-- This is experimental, and will likely be replaced by a more robust to
-- arbitrary message signing using COSE, or a subset of it.
signMetadataWith
    :: forall ctx s k n.
        ( HasDBLayer IO s k ctx
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
        $ readCheckpoint wid

    withRootKey @ctx @s @k ctx wid pwd ErrSignMetadataWithRootKey
        $ \rootK scheme -> do
            let encPwd = preparePassphrase scheme pwd
            let DerivationPrefix (_, _, acctIx) = Seq.derivationPrefix (getState cp)
            let acctK = deriveAccountPrivateKey encPwd rootK acctIx
            let addrK = deriveAddressPrivateKey encPwd acctK role_ addrIx
            pure $
                Signature $ BA.convert $
                CC.sign encPwd (getRawKey addrK) $
                hash @ByteString @Blake2b_256 $
                serialiseToCBOR metadata
  where
    db = ctx ^. dbLayer @IO @s @k

derivePublicKey
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , SoftDerivation k
        , GetAccount s k
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
        $ readCheckpoint wid

    let acctK = getAccount $ getState cp
    let addrK = deriveAddressPublicKey acctK role_ addrIx

    return addrK
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Retrieve current public account key of a wallet.
readAccountPublicKey
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , GetAccount s k
        )
    => ctx
    -> WalletId
    -> ExceptT ErrReadAccountPublicKey IO (k 'AccountK XPub)
readAccountPublicKey ctx wid = db & \DBLayer{..} -> do
    cp <- mapExceptT atomically
        $ withExceptT ErrReadAccountPublicKeyNoSuchWallet
        $ withNoSuchWallet wid
        $ readCheckpoint wid
    pure $ getAccount (getState cp)
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Retrieve any public account key of a wallet.
getAccountPublicKeyAtIndex
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , WalletKey k
        , GetPurpose k
        )
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> DerivationIndex
    -> Maybe DerivationIndex
    -> ExceptT ErrReadAccountPublicKey IO (k 'AccountK XPub)
getAccountPublicKeyAtIndex ctx wid pwd ix purposeM = db & \DBLayer{..} -> do
    acctIx <- withExceptT ErrReadAccountPublicKeyInvalidAccountIndex $ guardHardIndex ix

    purpose <- maybe (pure (getPurpose @k))
        (withExceptT ErrReadAccountPublicKeyInvalidPurposeIndex . guardHardIndex)
        purposeM

    _cp <- mapExceptT atomically
        $ withExceptT ErrReadAccountPublicKeyNoSuchWallet
        $ withNoSuchWallet wid
        $ readCheckpoint wid

    withRootKey @ctx @s @k ctx wid pwd ErrReadAccountPublicKeyRootKey
        $ \rootK scheme -> do
            let encPwd = preparePassphrase scheme pwd
            let xprv = deriveAccountPrivateKeyShelley purpose encPwd (getRawKey rootK) acctIx
            pure $ liftRawKey $ toXPub xprv
  where
    db = ctx ^. dbLayer @IO @s @k

guardSoftIndex
    :: Monad m
    => DerivationIndex
    -> ExceptT (ErrInvalidDerivationIndex 'Soft 'AddressK) m (Index 'Soft whatever)
guardSoftIndex ix =
    if ix > DerivationIndex (getIndex @'Soft maxBound) || ix < DerivationIndex (getIndex @'Soft minBound)
    then throwE $ ErrIndexOutOfBound minBound maxBound ix
    else pure (Index $ getDerivationIndex ix)

guardHardIndex
    :: Monad m
    => DerivationIndex
    -> ExceptT (ErrInvalidDerivationIndex 'Hardened level) m (Index 'Hardened whatever)
guardHardIndex ix =
    if ix > DerivationIndex (getIndex @'Hardened maxBound) || ix < DerivationIndex (getIndex @'Hardened minBound)
    then throwE $ ErrIndexOutOfBound minBound maxBound ix
    else pure (Index $ getDerivationIndex ix)

updateCosigner
    :: forall ctx s k n.
        ( s ~ SharedState n k
--        , MkKeyFingerprint k (Proxy n, k 'AddressK CC.XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , Typeable n
        , WalletKey k
        , HasDBLayer IO s k ctx
        , k ~ SharedKey
        )
    => ctx
    -> WalletId
    -> k 'AccountK XPub
    -> Cosigner
    -> CredentialType
    -> ExceptT ErrAddCosignerKey IO ()
updateCosigner ctx wid cosignerXPub cosigner cred = db & \DBLayer{..} ->
    ExceptT $ atomically $ modifyDBMaybe walletsDB $
        adjustNoSuchWallet wid ErrAddCosignerKeyNoSuchWallet
            updateCosigner'
  where
    db = ctx ^. dbLayer @_ @s @k
    updateCosigner' wallet =
        case addCosignerAccXPub (cosigner, cosignerXPub) cred s0 of
            Left err -> Left $ ErrAddCosignerKey err
            Right s1 -> Right ([ReplacePrologue $ getPrologue s1], ())
      where
        s0 = getState $ getLatest wallet

-- NOTE
-- Addresses coming from the transaction history might be base (having payment credential) or
-- base addresses (containing both payment and delegation credentials).
-- So we normalize them all to be base addresses to make sure that we compare them correctly.
normalizeSharedAddress
    :: forall n k. ( Shared.SupportsDiscovery n k, k ~ SharedKey )
    => SharedState n k
    -> Address
    -> Maybe Address
normalizeSharedAddress st addr = case Shared.ready st of
    Shared.Pending -> Nothing
    Shared.Active _ -> do
        let dTM = Shared.delegationTemplate st
        fingerprint <- eitherToMaybe (paymentKeyFingerprint @k addr)
        let (ixM, _) = Shared.isShared addr st
        case (dTM, ixM) of
            (Just dT, Just ix) ->
                pure $ Shared.liftDelegationAddress @n ix dT fingerprint
            _ ->
                pure $ Shared.liftPaymentAddress @n fingerprint

{-------------------------------------------------------------------------------
                                   Errors
-------------------------------------------------------------------------------}

data ErrSignMetadataWith
    = ErrSignMetadataWithRootKey ErrWithRootKey
        -- ^ The wallet exists, but there's no root key attached to it
    | ErrSignMetadataWithNoSuchWallet ErrNoSuchWallet
        -- ^ The wallet doesn't exist?
    | ErrSignMetadataWithInvalidIndex (ErrInvalidDerivationIndex 'Soft 'AddressK)
        -- ^ User provided a derivation index outside of the 'Soft' domain
    deriving (Eq, Show)

data ErrDerivePublicKey
    = ErrDerivePublicKeyNoSuchWallet ErrNoSuchWallet
        -- ^ The wallet doesn't exist?
    | ErrDerivePublicKeyInvalidIndex (ErrInvalidDerivationIndex 'Soft 'AddressK)
        -- ^ User provided a derivation index outside of the 'Soft' domain
    deriving (Eq, Show)

data ErrAddCosignerKey
    = ErrAddCosignerKeyNoSuchWallet ErrNoSuchWallet
        -- ^ The shared wallet doesn't exist?
    | ErrAddCosignerKey ErrAddCosigner
        -- ^ Error adding this co-signer to the shared wallet.
    deriving (Eq, Show)

data ErrConstructSharedWallet
    = ErrConstructSharedWalletWrongScriptTemplate ErrScriptTemplate
        -- ^ The shared wallet' script template doesn't pass validation
    | ErrConstructSharedWalletInvalidIndex (ErrInvalidDerivationIndex 'Hardened 'AccountK)
        -- ^ User provided a derivation index outside of the 'Hard' domain
    deriving (Eq, Show)

data ErrReadAccountPublicKey
    = ErrReadAccountPublicKeyNoSuchWallet ErrNoSuchWallet
        -- ^ The wallet doesn't exist?
    | ErrReadAccountPublicKeyInvalidAccountIndex (ErrInvalidDerivationIndex 'Hardened 'AccountK)
        -- ^ User provided a derivation index for account outside of the 'Hard' domain
    | ErrReadAccountPublicKeyInvalidPurposeIndex (ErrInvalidDerivationIndex 'Hardened 'PurposeK)
        -- ^ User provided a derivation index for purpose outside of the 'Hard' domain
    | ErrReadAccountPublicKeyRootKey ErrWithRootKey
        -- ^ The wallet exists, but there's no root key attached to it
    deriving (Eq, Show)

data ErrInvalidDerivationIndex derivation level
    = ErrIndexOutOfBound (Index derivation level) (Index derivation level) DerivationIndex
    deriving (Eq, Show)

-- | Errors that can occur when listing UTxO statistics.
newtype ErrListUTxOStatistics
    = ErrListUTxOStatisticsNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors that can occur when signing a transaction.
data ErrSignPayment
    = ErrSignPaymentMkTx ErrMkTransaction
    | ErrSignPaymentNoSuchWallet ErrNoSuchWallet
    | ErrSignPaymentWithRootKey ErrWithRootKey
    | ErrSignPaymentIncorrectTTL PastHorizonException
    deriving (Show, Eq)

-- | Errors that can occur when balancing transaction.
data ErrBalanceTx
    = ErrBalanceTxUpdateError ErrUpdateSealedTx
    | ErrBalanceTxSelectAssets ErrSelectAssets
    | ErrBalanceTxExistingCollateral
    | ErrBalanceTxAssignRedeemers ErrAssignRedeemers
    | ErrBalanceTxNotYetSupported BalanceTxNotSupportedReason
    | ErrBalanceTxFailedBalancing Cardano.Value
    deriving (Show, Eq)

-- TODO: Remove once problems are fixed.
data BalanceTxNotSupportedReason
    = UnderestimatedFee Coin SealedTx
    | Deposits
    | ZeroAdaOutput
    | ConflictingNetworks
    deriving (Show, Eq)

-- | Errors that can occur when submitting a transaction.
data ErrSubmitTransaction
    = ErrSubmitTransactionNoSuchWallet ErrNoSuchWallet
    | ErrSubmitTransactionForeignWallet
    | ErrSubmitTransactionPartiallySignedOrNoSignedTx Int Int
    | ErrSubmitTransactionMultidelegationNotSupported
    deriving (Show, Eq)

-- | Errors that can occur when constructing an unsigned transaction.
data ErrConstructTx
    = ErrConstructTxWrongPayload
    | ErrConstructTxBody ErrMkTransaction
    | ErrConstructTxNoSuchWallet ErrNoSuchWallet
    | ErrConstructTxReadRewardAccount ErrReadRewardAccount
    | ErrConstructTxIncorrectTTL PastHorizonException
    | ErrConstructTxMultidelegationNotSupported
    | ErrConstructTxMultiaccountNotSupported
    | ErrConstructTxNotImplemented String
    -- ^ Temporary error constructor.
    deriving (Show, Eq)

newtype ErrMintBurnAssets
    = ErrMintBurnNotImplemented T.Text
      -- ^ Temporary error constructor.
    deriving (Show, Eq)

-- | Errors that can occur when signing a transaction.
data ErrWitnessTx
    = ErrWitnessTxSignTx ErrSignTx
    | ErrWitnessTxNoSuchWallet ErrNoSuchWallet
    | ErrWitnessTxWithRootKey ErrWithRootKey
    | ErrWitnessTxIncorrectTTL PastHorizonException
    deriving (Show, Eq)

-- | Errors that can occur when decoding a transaction.
newtype ErrDecodeTx
    = ErrDecodeTxNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors that can occur when submitting a signed transaction to the network.
data ErrSubmitTx
    = ErrSubmitTxNetwork ErrPostTx
    | ErrSubmitTxNoSuchWallet ErrNoSuchWallet
    | ErrSubmitTxImpossible ErrNoSuchTransaction
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

-- | Indicates that the specified start time is later than the specified end
-- time.
data ErrStartTimeLaterThanEndTime = ErrStartTimeLaterThanEndTime
    { errStartTime :: UTCTime
    , errEndTime :: UTCTime
    } deriving (Show, Eq)

data ErrCreateMigrationPlan
    = ErrCreateMigrationPlanEmpty
    | ErrCreateMigrationPlanNoSuchWallet ErrNoSuchWallet
    deriving (Generic, Eq, Show)

data ErrSelectAssets
    = ErrSelectAssetsPrepareOutputsError (SelectionOutputError Address)
    | ErrSelectAssetsNoSuchWallet ErrNoSuchWallet
    | ErrSelectAssetsAlreadyWithdrawing Tx
    | ErrSelectAssetsSelectionError (SelectionError Address InputId)
    deriving (Generic, Eq, Show)

data ErrStakePoolDelegation
    = ErrStakePoolDelegationNoSuchWallet ErrNoSuchWallet
    | ErrStakePoolJoin ErrCannotJoin
    | ErrStakePoolQuit ErrCannotQuit

-- | Errors that can occur when fetching the reward balance of a wallet
newtype ErrFetchRewards
    = ErrFetchRewardsReadRewardAccount ErrReadRewardAccount
    deriving (Generic, Eq, Show)

data ErrCheckWalletIntegrity
    = ErrCheckWalletIntegrityNoSuchWallet ErrNoSuchWallet
    | ErrCheckIntegrityDifferentGenesis (Hash "Genesis") (Hash "Genesis")
    deriving (Eq, Show)

instance Exception ErrCheckWalletIntegrity

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
    -> Withdrawal
    -> Coin
    -> Either ErrCannotQuit ()
guardQuit WalletDelegation{active,next} wdrl rewards = do
    let last_ = maybe active (view #status) $ lastMay next

    unless (isDelegatingTo anyone last_) $
        Left ErrNotDelegatingOrAboutTo

    case wdrl of
        WithdrawalSelf {} -> return ()
        _
            | rewards == Coin 0  -> return ()
            | otherwise          -> Left $ ErrNonNullRewards rewards
  where
    anyone = const True

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

-- | Log messages for actions running within a wallet worker context.
data WalletWorkerLog
    = MsgWallet WalletLog
    | MsgWalletFollow WalletFollowLog
    | MsgChainFollow ChainFollowLog
    deriving (Show, Eq)

instance ToText WalletWorkerLog where
    toText = \case
        MsgWallet msg -> toText msg
        MsgWalletFollow msg -> toText msg
        MsgChainFollow msg -> toText msg

instance HasPrivacyAnnotation WalletWorkerLog

instance HasSeverityAnnotation WalletWorkerLog where
    getSeverityAnnotation = \case
        MsgWallet msg -> getSeverityAnnotation msg
        MsgWalletFollow msg -> getSeverityAnnotation msg
        MsgChainFollow msg -> getSeverityAnnotation msg

-- | Log messages arising from the restore and follow process.
data WalletFollowLog
    = MsgDiscoveredDelegationCert SlotNo DelegationCertificate
    | MsgCheckpoint BlockHeader
    | MsgDiscoveredTxs [(Tx, TxMeta)]
    | MsgDiscoveredTxsContent [(Tx, TxMeta)]
    deriving (Show, Eq)

-- | Log messages from API server actions running in a wallet worker context.
data WalletLog
    = MsgSelectionStart UTxO [TxOut]
    | MsgSelectionError (SelectionError Address InputId)
    | MsgSelectionReportSummarized SelectionReportSummarized
    | MsgSelectionReportDetailed SelectionReportDetailed
    | MsgMigrationUTxOBefore UTxOStatistics
    | MsgMigrationUTxOAfter UTxOStatistics
    | MsgRewardBalanceQuery BlockHeader
    | MsgRewardBalanceResult (Either ErrFetchRewards Coin)
    | MsgRewardBalanceNoSuchWallet ErrNoSuchWallet
    | MsgRewardBalanceExited
    | MsgTxSubmit TxSubmitLog
    | MsgIsStakeKeyRegistered Bool
    deriving (Show, Eq)

instance ToText WalletFollowLog where
    toText = \case
        MsgDiscoveredDelegationCert slotNo cert -> case cert of
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
        MsgDiscoveredTxs txs ->
            "discovered " <> pretty (length txs) <> " new transaction(s)"
        MsgDiscoveredTxsContent txs ->
            "transactions: " <> pretty (blockListF (snd <$> txs))

instance ToText WalletLog where
    toText = \case
        MsgSelectionStart utxo recipients ->
            "Starting coin selection " <>
            "|utxo| = "+|UTxO.size utxo|+" " <>
            "#recipients = "+|length recipients|+""
        MsgSelectionError e ->
            "Failed to select assets:\n"+|| e ||+""
        MsgSelectionReportSummarized s ->
            "Selection report (summarized):\n"+| s |+""
        MsgSelectionReportDetailed s ->
            "Selection report (detailed):\n"+| s |+""
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
        MsgTxSubmit msg ->
            toText msg
        MsgIsStakeKeyRegistered True ->
            "Wallet stake key is registered. Will not register it again."
        MsgIsStakeKeyRegistered False ->
            "Wallet stake key is not registered. Will register..."

instance HasPrivacyAnnotation WalletFollowLog
instance HasSeverityAnnotation WalletFollowLog where
    getSeverityAnnotation = \case
        MsgDiscoveredDelegationCert _ _ -> Info
        MsgCheckpoint _ -> Debug
        MsgDiscoveredTxs [] -> Debug
        MsgDiscoveredTxs _ -> Info
        MsgDiscoveredTxsContent _ -> Debug

instance HasPrivacyAnnotation WalletLog
instance HasSeverityAnnotation WalletLog where
    getSeverityAnnotation = \case
        MsgSelectionStart{} -> Debug
        MsgSelectionError{} -> Debug
        MsgSelectionReportSummarized{} -> Info
        MsgSelectionReportDetailed{} -> Debug
        MsgMigrationUTxOBefore _ -> Info
        MsgMigrationUTxOAfter _ -> Info
        MsgRewardBalanceQuery _ -> Debug
        MsgRewardBalanceResult (Right _) -> Debug
        MsgRewardBalanceResult (Left _) -> Notice
        MsgRewardBalanceNoSuchWallet{} -> Warning
        MsgRewardBalanceExited -> Notice
        MsgTxSubmit msg -> getSeverityAnnotation msg
        MsgIsStakeKeyRegistered _ -> Info

data TxSubmitLog
    = MsgSubmitTx Tx TxMeta SealedTx (BracketLog' (Either ErrSubmitTx ()))
    | MsgSubmitExternalTx (Hash "Tx") (BracketLog' (Either ErrPostTx Tx))
    | MsgRetryPostTx (Hash "Tx") (BracketLog' (Either ErrPostTx ()))
    | MsgProcessPendingPool BracketLog
    deriving (Show, Eq)

instance ToText TxSubmitLog

instance Buildable TxSubmitLog where
    build = \case
        MsgSubmitTx tx meta sealed msg -> case msg of
            BracketStart -> unlinesF
                [ "Submitting transaction "+|tx ^. #txId|+" to local node"
                , blockMapF
                    [ ("Tx" :: Text, build tx)
                    , ("SealedTx", build sealed)
                    , ("TxMeta", build meta) ]
                ]
            BracketFinish res ->
                "Transaction "+|tx ^. #txId|+" "+|case res of
                    Right _ -> "accepted by local node"
                    Left err -> "failed: "+||err||+""
            _ -> formatResultMsg "submitTx" [("txid", tx ^. #txId)] msg

        MsgSubmitExternalTx txid msg -> case msg of
            BracketStart -> "Submitting external transaction "+|txid|+
                " to local node..."
            BracketFinish res ->
                "Transaction "+|txid|+" "+|case res of
                    Right tx -> unlinesF
                        [ "accepted by local node"
                        , nameF "tx" (build tx)
                        ]
                    Left err -> "failed: "+|toText err|+""
            _ -> formatResultMsg "submitExternalTx" [("txid", txid)] msg

        MsgRetryPostTx txid msg -> case msg of
            BracketStart -> "Retrying submission of transaction "+|txid|+
                " to local node..."
            BracketFinish res ->
                "Transaction "+|txid|+" resubmitted to local node and " <>
                case res of
                    Right _ -> "accepted again"
                    Left _ -> "not accepted (this is expected)"
            _ -> formatResultMsg "runLocalTxSubmissionPool(postTx)"
                [("txid", txid)] msg

        MsgProcessPendingPool msg ->
            "Processing the pending local tx submission pool: "+|msg|+""

instance HasPrivacyAnnotation TxSubmitLog
instance HasSeverityAnnotation TxSubmitLog where
    getSeverityAnnotation = \case
        MsgSubmitTx _ _ _ b -> resultSeverity Info b
        MsgSubmitExternalTx _ b -> resultSeverity Info b
        MsgRetryPostTx _ b -> case b of
            BracketFinish (Right _) -> Info
            BracketException _ -> Error
            _ -> Debug
        MsgProcessPendingPool msg -> getSeverityAnnotation msg
