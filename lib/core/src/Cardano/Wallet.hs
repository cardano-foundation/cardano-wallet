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
    , ErrSelectAssets(..)
    , ErrSignPayment (..)
    , ErrNotASequentialWallet (..)
    , ErrWithdrawalNotWorth (..)
    , ErrConstructTx (..)
    , ErrMintBurnAssets (..)
    , ErrBalanceTx (..)
    , ErrUpdateSealedTx (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)

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
    ( ErrPostTx (..)
    , FollowAction (..)
    , FollowExceptionRecovery (..)
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
    ( ParentContext (..)
    , SeqState
    , defaultAddressPoolGap
    , mkSeqStateFromRootXPrv
    , purposeBIP44
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( CredentialType (..)
    , ErrAddCosigner (..)
    , ErrScriptTemplate (..)
    , SharedState (..)
    , SharedStateFields (..)
    , addCosignerAccXPub
    , isShared
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( Selection
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionOf (..)
    , SelectionOutputInvalidError (..)
    , SelectionParams (..)
    , SelectionReportDetailed
    , SelectionReportSummarized
    , makeSelectionReportDetailed
    , makeSelectionReportSummarized
    , performSelection
    , selectionDelta
    )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( SelectionSkeleton (..), emptySkeleton )
import Cardano.Wallet.Primitive.Collateral
    ( asCollateral )
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
import Control.Monad.Random
    ( MonadRandom )
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
    , blockListF
    , blockMapF
    , build
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
import Type.Reflection
    ( Typeable, typeRep )
import UnliftIO.Exception
    ( Exception )
import UnliftIO.MVar
    ( modifyMVar_, newMVar )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Shared as Shared
import qualified Cardano.Wallet.Primitive.CoinSelection.Balance as Balance
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

-- | Restore a wallet from its current tip up to the network tip.
--
-- This function returns immediately, starting a worker thread in the
-- background that will fetch and apply remaining blocks until the
-- network tip is reached or until failure.
restoreWallet
    :: forall ctx s k.
        ( HasNetworkLayer IO ctx
        , HasDBLayer IO s k ctx
        , HasLogger IO WalletWorkerLog ctx
        , IsOurs s Address
        , IsOurs s RewardAccount
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
restoreWallet ctx wid = db & \DBLayer{..} -> do
    let readCps = liftIO $ atomically $ listCheckpoints wid
    let forward bs h innerTr = run $ do
            restoreBlocks @ctx @s @k ctx innerTr wid bs h
    let backward = runExceptT . rollbackBlocks @ctx @s @k ctx wid
    liftIO $ follow nw tr readCps forward backward RetryOnExceptions (view #header)
  where
    db = ctx ^. dbLayer @IO @s @k
    nw = ctx ^. networkLayer
    tr = contramap MsgFollow (ctx ^. logger @_ @WalletWorkerLog)

    run :: ExceptT ErrNoSuchWallet IO () -> IO (FollowAction ErrNoSuchWallet)
    run = fmap (either ExitWith (const Continue)) . runExceptT

-- | Rewind the UTxO snapshots, transaction history and other information to a
-- the earliest point in the past that is before or is the point of rollback.
rollbackBlocks
    :: forall ctx s k. (HasDBLayer IO s k ctx)
    => ctx
    -> WalletId
    -> SlotNo
    -> ExceptT ErrNoSuchWallet IO SlotNo
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
        )
    => ctx
    -> Tracer IO WalletFollowLog
    -> WalletId
    -> NonEmpty Block
    -> BlockHeader
    -> ExceptT ErrNoSuchWallet IO ()
restoreBlocks ctx tr wid blocks nodeTip = db & \DBLayer{..} -> mapExceptT atomically $ do
    cp   <- withNoSuchWallet wid (readCheckpoint wid)
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

    putTxHistory wid txs
    updatePendingTxForExpiry wid (view #slotNo localTip)
    forM_ slotPoolDelegations $ \delegation@(slotNo, cert) -> do
        liftIO $ logDelegation delegation
        putDelegationCertificate wid cert slotNo

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
            putCheckpoint wid cp'

    liftIO $ logCheckpoint (NE.last cps)
    putCheckpoint wid (NE.last cps)

    prune wid epochStability

    liftIO $ do
        traceWith tr $ MsgDiscoveredTxs txs
        traceWith tr $ MsgBlocks blocks
        traceWith tr $ MsgDiscoveredTxsContent txs
  where
    nl = ctx ^. networkLayer
    db = ctx ^. dbLayer @IO @s @k

    logCheckpoint :: Wallet s -> IO ()
    logCheckpoint cp = traceWith tr $ MsgCheckpoint (currentTip cp)

    logDelegation :: (SlotNo, DelegationCertificate) -> IO ()
    logDelegation = traceWith tr . uncurry MsgDiscoveredDelegationCert

    isParentOf :: Wallet s -> Block -> Bool
    isParentOf cp = (== parent) . parentHeaderHash . header
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
            coinToInteger costWith - coinToInteger costWithout

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
                -- Occasionaly failing to query is generally not fatal. It will
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
        )
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> Maybe (Index 'Hardened 'AddressK)
    -> ExceptT ErrCreateRandomAddress IO (Address, NonEmpty DerivationIndex)
createRandomAddress ctx wid pwd mIx = db & \DBLayer{..} ->
    withRootKey @ctx @s @k ctx wid pwd ErrCreateAddrWithRootKey $ \xprv scheme -> do
        mapExceptT atomically $ do
            cp <- withExceptT ErrCreateAddrNoSuchWallet $
                withNoSuchWallet wid (readCheckpoint wid)
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
                putCheckpoint wid cp'
            pure (addr, Rnd.toDerivationIndexes path)
  where
    db = ctx ^. dbLayer @IO @s @k
    isKnownIndex accIx addrIx s =
        (liftIndex accIx, liftIndex addrIx) `Set.member` Rnd.unavailablePaths s

importRandomAddresses
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , RndStateLike s
        , k ~ ByronKey
        )
    => ctx
    -> WalletId
    -> [Address]
    -> ExceptT ErrImportRandomAddress IO ()
importRandomAddresses ctx wid addrs = db & \DBLayer{..} -> mapExceptT atomically $ do
    cp <- withExceptT ErrImportAddrNoSuchWallet
        $ withNoSuchWallet wid (readCheckpoint wid)
    let s0 = getState cp
        ours = scanl' (\s addr -> s >>= Rnd.importAddress addr) (Right s0) addrs
    case last ours of
        Left err ->
            throwE $ ErrImportAddr err
        Right s' ->
            withExceptT ErrImportAddrNoSuchWallet $
                putCheckpoint wid (updateState s' cp)
  where
    db = ctx ^. dbLayer @IO @s @k

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
    -> (UTxOIndex, Wallet s, Set Tx)
    -> PartialTx
    -> ExceptT ErrBalanceTx m SealedTx
balanceTransaction
    ctx
    generateChange
    (pp, nodePParams)
    ti
    (internalUtxoAvailable, wallet, pendingTxs)
    (PartialTx partialTx externalInputs redeemers)
    = do
    let (outputs, txWithdrawal, txMetadata, txAssetsToMint, txAssetsToBurn)
            = extractFromTx partialTx

    (delta, extraInputs, extraCollateral, extraOutputs) <- do
        let externalSelectedUtxo = UTxOIndex.fromSequence $
                map (\(i,o,_datumHash) -> (i, o)) externalInputs

        let utxoAvailableForInputs = UTxOSelection.fromIndexPair
                (internalUtxoAvailable, externalSelectedUtxo)

        let utxoAvailableForCollateral =
                UTxOIndex.toUTxO internalUtxoAvailable

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
                , txContext
                , utxoAvailableForInputs
                , utxoAvailableForCollateral
                , wallet
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
        , newFee = const delta
        }
    let candidateMinFee = fromMaybe (Coin 0) $
            evaluateMinimumFee tl nodePParams candidateTx

    let surplus = delta `Coin.difference` candidateMinFee
    assembleTransaction $ TxUpdate
        { extraInputs
        , extraCollateral
        , extraOutputs = mapFirst (txOutAddCoin surplus) extraOutputs
        , newFee = const candidateMinFee
        }
  where
    tl = ctx ^. transactionLayer @k

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
        let (Tx _id _fee _coll _inps outs wdrlMap meta _vldt, toMint, toBurn)
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
                (sumCoins wdrlMap)
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
            (walletTx, _, _) = decodeTx tl sealedTx
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
                betterEstimate `Coin.subtractCoin` worseEstimate
        in
            txCtx { txFeePadding }

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
        )
    => ctx
    -> WalletId
    -> ArgGenChange s
    -> Selection
    -> ExceptT ErrSignPayment IO (SelectionOf TxOut)
assignChangeAddressesAndUpdateDb ctx wid generateChange selection =
    db & \DBLayer{..} -> mapExceptT atomically $ do
        cp <- withExceptT ErrSignPaymentNoSuchWallet $
            withNoSuchWallet wid $ readCheckpoint wid
        let (selectionUpdated, stateUpdated) =
                assignChangeAddresses generateChange selection (getState cp)
        withExceptT ErrSignPaymentNoSuchWallet $
            putCheckpoint wid (updateState stateUpdated cp)
        pure selectionUpdated
  where
    db = ctx ^. dbLayer @IO @s @k

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
    -> ExceptT ErrNoSuchWallet IO (UTxOIndex, Wallet s, Set Tx)
readWalletUTxOIndex ctx wid = do
    (cp, _, pending) <- readWallet @ctx @s @k ctx wid
    let utxo = UTxOIndex.fromUTxO $ availableUTxO @s pending cp
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

-- | Parameters for the 'selectAssets' function.
--
data SelectAssetsParams s result = SelectAssetsParams
    { outputs :: [TxOut]
    , pendingTxs :: Set Tx
    , txContext :: TransactionCtx
    , utxoAvailableForCollateral :: UTxO
    , utxoAvailableForInputs :: UTxOSelection
    , wallet :: Wallet s
    }
    deriving Generic

-- | Selects assets from the wallet's UTxO to satisfy the requested outputs in
-- the given transaction context. In case of success, returns the selection
-- and its associated cost. That is, the cost is equal to the difference between
-- inputs and outputs.
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
        (UTxOSelection.availableUTxO $ params ^. #utxoAvailableForInputs)
        (params ^. #outputs)
    mSel <- runExceptT $ performSelection
        SelectionConstraints
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
            , utxoSuitableForCollateral =
                asCollateral . snd
            }
        SelectionParams
            { assetsToMint =
                params ^. (#txContext . #txAssetsToMint)
            , assetsToBurn =
                params ^. (#txContext . #txAssetsToBurn)
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
            }
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
    :: forall ctx s k.
        ( HasTransactionLayer k ctx
        , HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , HardDerivation k
        , WalletKey k
        , IsOwned s k
        , Bounded (Index (AddressIndexDerivationType k) 'AddressK)
        )
    => ctx
    -> WalletId
    -> Passphrase "raw"
    -> SealedTx
    -> ExceptT ErrWitnessTx IO SealedTx
signTransaction ctx wid pwd tx = db & \DBLayer{..} -> do
    era <- liftIO $ currentNodeEra nl
    withRootKey @_ @s ctx wid pwd ErrWitnessTxWithRootKey $ \rootK scheme -> do
        cp <- mapExceptT atomically
            $ withExceptT ErrWitnessTxNoSuchWallet
            $ withNoSuchWallet wid
            $ readCheckpoint wid
        let pwdP = preparePassphrase scheme pwd
        let rewardAcnt = (getRawKey $ deriveRewardAccount @k pwdP rootK, pwdP)
        let addressResolver = mkAddressResolver cp (rootK, pwdP)
        let inputResolver = mkInputResolver cp
        pure $ addVkWitnesses tl era rewardAcnt addressResolver inputResolver tx
  where
    db = ctx ^. dbLayer @IO @s @k
    tl = ctx ^. transactionLayer @k
    nl = ctx ^. networkLayer

    mkInputResolver cp i = do
        TxOut addr _ <- UTxO.lookup i (totalUTxO mempty cp)
        pure addr

    mkAddressResolver cp (rootK, pwdP) =
        isOwned (getState cp) (rootK, pwdP)

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
        amtOuts = sumCoins $
            (txOutCoin <$> view #change sel)
            ++
            mapMaybe ourCoin (view #outputs sel)

        amtInps
            = sumCoins (txOutCoin . snd <$> view #inputs sel)
            -- NOTE: In case where rewards were pulled from an external
            -- source, they aren't added to the calculation because the
            -- money is considered to come from outside of the wallet; which
            -- changes the way we look at transactions (in such case, a
            -- transaction is considered 'Incoming' since it brings extra money
            -- to the wallet from elsewhere).
            & case txWithdrawal txCtx of
                w@WithdrawalSelf{} -> addCoin (withdrawalToCoin w)
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

    ourCoin :: TxOut -> Maybe Coin
    ourCoin (TxOut addr tokens) =
        case fst (isOurs addr wState) of
            Just{}  -> Just (TokenBundle.getCoin tokens)
            Nothing -> Nothing

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
    (tx, _, _) = decodeTx tl sealedTx

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
    :: forall ctx s k n.
        ( HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , HasLogger IO WalletWorkerLog ctx
        , s ~ SeqState n k
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
    :: forall ctx s k n.
        ( HasDBLayer IO s k ctx
        , s ~ SeqState n k
        )
    => ctx
    -> WalletId
    -> ExceptT ErrStakePoolDelegation IO DelegationAction
quitStakePool ctx wid = db & \DBLayer{..} -> do
    walMeta <- mapExceptT atomically
        $ withExceptT ErrStakePoolDelegationNoSuchWallet
        $ withNoSuchWallet wid
        $ readWalletMeta wid

    rewards <- liftIO
        $ fetchRewardBalance @ctx @s @k ctx wid

    withExceptT ErrStakePoolQuit $ except $
        guardQuit (walMeta ^. #delegation) rewards

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
            SelectionBalanceError (Balance.UnableToConstructChange ce) ->
                case ce of
                    Balance.UnableToConstructChangeError {requiredCost} ->
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
        , MkKeyFingerprint k (Proxy n, k 'AddressK CC.XPub)
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
updateCosigner ctx wid accXPub cosigner cred = db & \DBLayer{..} -> do
    mapExceptT atomically $ do
        cp <- withExceptT ErrAddCosignerKeyNoSuchWallet $ withNoSuchWallet wid $
              readCheckpoint wid
        case addCosignerAccXPub accXPub cosigner cred (getState cp) of
            Left err -> throwE (ErrAddCosignerKey err)
            Right st' -> withExceptT ErrAddCosignerKeyNoSuchWallet $
                putCheckpoint wid (updateState st' cp)
  where
    db = ctx ^. dbLayer @_ @s @k

-- NOTE
-- Addresses coming from the transaction history might be base (having payment credential) or
-- base addresses (containing both payment and delegation credentials).
-- So we normalize them all to be base addresses to make sure that we compare them correctly.
normalizeSharedAddress
    :: forall s k n.
        ( MkKeyFingerprint k Address
        , MkKeyFingerprint k (Proxy n, k 'AddressK XPub)
        , s ~ SharedState n k
        , k ~ SharedKey
        , SoftDerivation k
        , Typeable n
        )
    => s
    -> Address
    -> Maybe Address
normalizeSharedAddress s@(SharedState _ state') addr = case state' of
    PendingFields _ -> Nothing
    ReadyFields pool -> do
        let (ParentContextShared _ _ dTM) = Seq.context pool
        fingerprint <- eitherToMaybe (paymentKeyFingerprint @k addr)
        let (ixM, _) = isShared addr s
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
    | ErrBalanceTxAssignRedeemers ErrAssignRedeemers
    deriving (Show, Eq)

-- | Errors that can occur when constructing an unsigned transaction.
data ErrConstructTx
    = ErrConstructTxWrongPayload
    | ErrConstructTxBody ErrMkTransaction
    | ErrConstructTxNoSuchWallet ErrNoSuchWallet
    | ErrConstructTxReadRewardAccount ErrReadRewardAccount
    | ErrConstructTxIncorrectTTL PastHorizonException
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
    = ErrSelectAssetsPrepareOutputsError SelectionOutputInvalidError
    | ErrSelectAssetsNoSuchWallet ErrNoSuchWallet
    | ErrSelectAssetsAlreadyWithdrawing Tx
    | ErrSelectAssetsSelectionError SelectionError
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

-- | Log messages for actions running within a wallet worker context.
data WalletWorkerLog
    = MsgWallet WalletLog
    | MsgFollow (FollowLog WalletFollowLog)
    deriving (Show, Eq)

instance ToText WalletWorkerLog where
    toText = \case
        MsgWallet msg -> toText msg
        MsgFollow msg -> toText msg

instance HasPrivacyAnnotation WalletWorkerLog

instance HasSeverityAnnotation WalletWorkerLog where
    getSeverityAnnotation = \case
        MsgWallet msg -> getSeverityAnnotation msg
        MsgFollow msg -> getSeverityAnnotation msg

-- | Log messages arising from the restore and follow process.
data WalletFollowLog
    = MsgDiscoveredDelegationCert SlotNo DelegationCertificate
    | MsgCheckpoint BlockHeader
    | MsgDiscoveredTxs [(Tx, TxMeta)]
    | MsgDiscoveredTxsContent [(Tx, TxMeta)]
    | MsgBlocks (NonEmpty Block)
    deriving (Show, Eq)

-- | Log messages from API server actions running in a wallet worker context.
data WalletLog
    = MsgSelectionStart UTxO [TxOut]
    | MsgSelectionError SelectionError
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
        MsgBlocks blocks ->
            "blocks: " <> pretty (NE.toList blocks)

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
        MsgBlocks _ -> Debug -- Ideally move to FollowLog or remove

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
