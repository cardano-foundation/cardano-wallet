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
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- HLINT ignore "Use ||" -}

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
--   values for this parameter are described in
--   'Cardano.Wallet.AddressDiscovery' sub-modules.
--   For instance @SeqState@ or @Rnd State@.
--
-- - @k@: A __k__ey derivation scheme intrinsically connected to the underlying
--   discovery state @s@. This describes how the hierarchical structure of a
--   wallet is defined as well as the relationship between secret keys and
--   public addresses.

module Cardano.Wallet
    ( WalletException (..)

    -- * WalletLayer
    , WalletLayer (..)

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
    , attachPrivateKeyFromPwdHashByron
    , attachPrivateKeyFromPwdHashShelley
    , getWalletUtxoSnapshot
    , listUtxoStatistics
    , readWallet
    , deleteWallet
    , restoreWallet
    , updateWallet
    , updateWalletPassphraseWithOldPassphrase
    , updateWalletPassphraseWithMnemonic
    , walletSyncProgress
    , fetchRewardBalance
    , manageRewardBalance
    , rollbackBlocks
    , checkWalletIntegrity
    , mkExternalWithdrawal
    , mkSelfWithdrawal
    , shelleyOnlyMkSelfWithdrawal
    , readRewardAccount
    , shelleyOnlyReadRewardAccount
    , someRewardAccount
    , readPolicyPublicKey
    , writePolicyPublicKey
    , ErrWalletAlreadyExists (..)
    , ErrNoSuchWallet (..)
    , ErrListUTxOStatistics (..)
    , ErrUpdatePassphrase (..)
    , ErrFetchRewards (..)
    , ErrCheckWalletIntegrity (..)
    , ErrWalletNotResponding (..)
    , ErrReadRewardAccount (..)
    , ErrReadPolicyPublicKey (..)
    , ErrWritePolicyPublicKey (..)
    , ErrGetPolicyId (..)

    -- * Shared Wallet
    , updateCosigner
    , ErrAddCosignerKey (..)
    , ErrConstructSharedWallet (..)
    , normalizeSharedAddress
    , constructSharedTransaction

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
    , transactionExpirySlot
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
    , ErrWithdrawalNotBeneficial (..)
    , ErrConstructTx (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxInternalError (..)
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
    , posAndNegFromCardanoValue

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
    ( Cosigner (..), KeyHash )
import Cardano.Address.Style.Shared
    ( deriveDelegationPublicKey )
import Cardano.Api
    ( AnyCardanoEra, serialiseToCBOR )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Crypto.Wallet
    ( toXPub )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Tx.Balance.Internal.CoinSelection
    ( Selection
    , SelectionBalanceError (..)
    , SelectionCollateralRequirement (..)
    , SelectionConstraints (..)
    , SelectionError (..)
    , SelectionLimitOf (NoLimit)
    , SelectionOf (..)
    , SelectionOutputError (..)
    , SelectionParams (..)
    , SelectionReportDetailed
    , SelectionReportSummarized
    , SelectionSkeleton (..)
    , SelectionStrategy (..)
    , UnableToConstructChangeError (..)
    , WalletSelectionContext
    , WalletUTxO (..)
    , emptySkeleton
    , makeSelectionReportDetailed
    , makeSelectionReportSummarized
    , performSelection
    )
import Cardano.Wallet.Address.Book
    ( AddressBookIso, Prologue (..), getDiscoveries, getPrologue )
import Cardano.Wallet.Checkpoints
    ( DeltaCheckpoints (..)
    , SparseCheckpointsConfig (..)
    , defaultSparseCheckpointsConfig
    , sparseCheckpoints
    )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchTransaction (..)
    , ErrPutLocalTxSubmission (..)
    , ErrRemoveTx (..)
    , ErrWalletAlreadyExists (..)
    )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState1 (..)
    , ErrNoSuchWallet (..)
    , adjustNoSuchWallet
    , fromWallet
    , getLatest
    , getSlot
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
    ( ChainFollowLog (..)
    , ChainFollower (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( BoundedAddressLength (..)
    , DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , Role (..)
    , SoftDerivation (..)
    , ToRewardAccount (..)
    , WalletKey (..)
    , deriveRewardAccount
    , hashVerificationKey
    , liftIndex
    , stakeDerivationPath
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.MintBurn
    ( derivePolicyPrivateKey, policyDerivationPath )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey (..), replaceCosignersWithVerKeys )
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
    , MaybeLight (..)
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
    , isShared
    )
import Cardano.Wallet.Primitive.BlockSummary
    ( ChainEvents )
import Cardano.Wallet.Primitive.Migration
    ( MigrationPlan (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockData (..)
    , Wallet
    , applyBlocks
    , availableUTxO
    , currentTip
    , firstHeader
    , getState
    , initWallet
    , totalUTxO
    )
import Cardano.Wallet.Primitive.Passphrase
    ( ErrWrongPassphrase (..)
    , Passphrase
    , PassphraseHash
    , PassphraseScheme (..)
    , WalletPassphraseInfo (..)
    , checkPassphrase
    , currentPassphraseScheme
    , encryptPassphrase'
    , preparePassphrase
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
    ( SyncProgress )
import Cardano.Wallet.Primitive.Types
    ( ActiveSlotCoefficient (..)
    , Block (..)
    , BlockHeader (..)
    , ChainPoint (..)
    , DelegationCertificate (..)
    , FeePolicy (LinearFee)
    , GenesisParameters (..)
    , LinearFunction (LinearFunction)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , Range (..)
    , Signature (..)
    , Slot
    , SlottingParameters (..)
    , SortOrder (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WithOrigin (..)
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
    ( Redeemer (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (UnsafeTokenName), TokenPolicyId (UnsafeTokenPolicyId) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (TokenQuantity) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , LocalTxSubmissionStatus
    , SealedTx (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxChange (..)
    , TxMeta (..)
    , TxMetadata (..)
    , TxStatus (..)
    , UnsignedTx (..)
    , fromTransactionInfo
    , sealedTxFromCardano
    , withdrawals
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxOIndex
    ( UTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOSelection
    ( UTxOSelection )
import Cardano.Wallet.Primitive.Types.UTxOStatistics
    ( UTxOStatistics )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Cardano.Wallet.Shelley.Compatibility
    ( fromCardanoTxIn, fromCardanoTxOut, toCardanoUTxO )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrAssignRedeemers (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrMkTransaction (..)
    , ErrMoreSurplusNeeded (ErrMoreSurplusNeeded)
    , ErrSignTx (..)
    , ErrUpdateSealedTx (..)
    , PreSelection
    , TransactionCtx (..)
    , TransactionLayer (..)
    , TxFeeAndChange (TxFeeAndChange)
    , TxFeeUpdate (..)
    , TxUpdate (..)
    , Withdrawal (..)
    , WitnessCountCtx (..)
    , defaultTransactionCtx
    , withdrawalToCoin
    )
import Control.Arrow
    ( first, left )
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
import Data.DBVar
    ( modifyDBMaybe )
import Data.Either
    ( lefts, partitionEithers )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( Lens', over, view, (.~), (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.IntCast
    ( intCast, intCastMaybe )
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
    ( Proxy (..) )
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
    , blockListF'
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
import Numeric.Natural
    ( Natural )
import Statistics.Quantile
    ( medianUnbiased, quantiles )
import System.Random.StdGenSeed
    ( StdGenSeed (..), stdGenFromSeed, stdGenSeed )
import Text.Pretty.Simple
    ( pShow )
import Type.Reflection
    ( Typeable, typeRep )
import UnliftIO.Exception
    ( Exception, catch, throwIO )
import UnliftIO.MVar
    ( modifyMVar_, newMVar )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shared as CA
import qualified Cardano.Address.Style.Shelley as CAShelley
import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Tx.Balance.Internal.CoinSelection as CS
import qualified Cardano.Wallet.Checkpoints.Policy as CP
import qualified Cardano.Wallet.DB.WalletState as WS
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Shared as Shared
import qualified Cardano.Wallet.Primitive.Migration as Migration
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOIndex as UTxOIndex
import qualified Cardano.Wallet.Primitive.Types.UTxOSelection as UTxOSelection
import qualified Cardano.Wallet.Primitive.Types.UTxOStatistics as UTxOStatistics
import qualified Cardano.Wallet.Write.Tx as WriteTx
import qualified Cardano.Wallet.Write.Tx as Write.Tx
import qualified Data.ByteArray as BA
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

data WalletLayer m s (k :: Depth -> Type -> Type) ktype
    = WalletLayer
        (Tracer m WalletWorkerLog)
        (Block, NetworkParameters)
        (NetworkLayer m Block)
        (TransactionLayer k ktype SealedTx)
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

type HasGenesisData = HasType (Block, NetworkParameters)

type HasLogger m msg = HasType (Tracer m msg)

-- | This module is only interested in one block-, and tx-type. This constraint
-- hides that choice, for some ease of use.
type HasNetworkLayer m = HasType (NetworkLayer m Block)

type HasTransactionLayer k ktype = HasType (TransactionLayer k ktype SealedTx)

dbLayer :: forall m s k ctx. HasDBLayer m s k ctx => Lens' ctx (DBLayer m s k)
dbLayer = typed @(DBLayer m s k)

genesisData ::
    forall ctx. HasGenesisData ctx => Lens' ctx (Block, NetworkParameters)
genesisData = typed @(Block, NetworkParameters)

logger :: forall m msg ctx. HasLogger m msg ctx => Lens' ctx (Tracer m msg)
logger = typed @(Tracer m msg)

networkLayer ::
    forall m ctx. (HasNetworkLayer m ctx) => Lens' ctx (NetworkLayer m Block)
networkLayer = typed @(NetworkLayer m Block)

transactionLayer ::
    forall k ktype ctx. (HasTransactionLayer k ktype ctx)
    => Lens' ctx (TransactionLayer k ktype SealedTx)
transactionLayer = typed @(TransactionLayer k ktype SealedTx)

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
            }
    mapExceptT atomically $
        initializeWallet wid cp meta hist gp $> wid
  where
    db = ctx ^. dbLayer @m @s @k
    (block0, NetworkParameters gp _sp _pp) = ctx ^. genesisData

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
        , PaymentAddress n k 'CredFromKeyK
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
            }
    mapExceptT atomically $
        initializeWallet wid cp meta hist gp $> wid
  where
    db = ctx ^. dbLayer @IO @s @k
    (block0, NetworkParameters gp _sp _pp) = ctx ^. genesisData

-- | Check whether a wallet is in good shape when restarting a worker.
checkWalletIntegrity :: DBLayer IO s k -> WalletId -> GenesisParameters -> IO ()
checkWalletIntegrity db walletId gp = db & \DBLayer{..} -> do
    gp' <- atomically (readGenesisParameters walletId) >>= do
        let noSuchWallet = ErrNoSuchWallet walletId
        maybe (throwIO $ ErrCheckWalletIntegrityNoSuchWallet noSuchWallet) pure
    when ( (gp ^. #getGenesisBlockHash /= gp' ^. #getGenesisBlockHash) ||
           (gp ^. #getGenesisBlockDate /= gp' ^. #getGenesisBlockDate) )
        (throwIO $ ErrCheckIntegrityDifferentGenesis
            (getGenesisBlockHash gp) (getGenesisBlockHash gp'))

-- | Retrieve the wallet state for the wallet with the given ID.
readWallet
    :: forall ctx s k. HasDBLayer IO s k ctx
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO
        (Wallet s, (WalletMetadata, WalletDelegation), Set Tx)
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
    meta <- fmap fst . withNoSuchWallet wid $ readWalletMeta wid
    putWalletMeta wid (modify meta)
  where
    db = ctx ^. dbLayer @IO @s @k

-- | Change a wallet's passphrase to the given passphrase.
updateWalletPassphraseWithOldPassphrase
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , WalletKey k
        )
    => ctx
    -> WalletId
    -> (Passphrase "user", Passphrase "user")
    -> ExceptT ErrUpdatePassphrase IO ()
updateWalletPassphraseWithOldPassphrase ctx wid (old, new) =
    withRootKey @ctx @s @k ctx wid old ErrUpdatePassphraseWithRootKey
        $ \xprv scheme -> withExceptT ErrUpdatePassphraseNoSuchWallet $ do
            -- IMPORTANT NOTE:
            -- This use 'EncryptWithPBKDF2', regardless of the passphrase
            -- current scheme, we'll re-encrypt it using the current scheme,
            -- always.
            let new' = (currentPassphraseScheme, new)
            let xprv' = changePassphrase (scheme, old) new' xprv
            attachPrivateKeyFromPwdScheme @ctx @s @k ctx wid (xprv', new')

updateWalletPassphraseWithMnemonic
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, Passphrase "user")
    -> ExceptT ErrUpdatePassphrase IO ()
updateWalletPassphraseWithMnemonic ctx wid (xprv, new) =
    withExceptT ErrUpdatePassphraseNoSuchWallet $ do
        attachPrivateKeyFromPwdScheme @ctx @s @k ctx wid
            (xprv, (currentPassphraseScheme , new))

getWalletUtxoSnapshot
    :: forall ctx s k ktype.
        ( HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , HasTransactionLayer k ktype ctx
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO [(TokenBundle, Coin)]
getWalletUtxoSnapshot ctx wid = do
    (wallet, _, pending) <- withExceptT id (readWallet @ctx @s @k ctx wid)
    pp <- liftIO $ currentProtocolParameters nl
    era <- liftIO $ currentNodeEra nl
    let txOuts = availableUTxO @s pending wallet
            & unUTxO
            & F.toList
    pure $ first (view #tokens) . pairTxOutWithMinAdaQuantity era pp <$> txOuts
  where
    nl = ctx ^. networkLayer
    tl = ctx ^. transactionLayer @k @ktype

    pairTxOutWithMinAdaQuantity
        :: Cardano.AnyCardanoEra
        -> ProtocolParameters
        -> TxOut
        -> (TxOut, Coin)
    pairTxOutWithMinAdaQuantity era pp out =
        (out, computeMinAdaQuantity out)
      where
        computeMinAdaQuantity :: TxOut -> Coin
        computeMinAdaQuantity (TxOut addr bundle) =
            view #txOutputMinimumAdaQuantity
                (constraints tl era pp)
                (addr)
                (view #tokens bundle)

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
    pure $ UTxOStatistics.compute utxo

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
        , MaybeLight s
        )
    => ctx
    -> WalletId
    -> ExceptT ErrNoSuchWallet IO ()
restoreWallet ctx wid = db & \DBLayer{..} ->
    let checkpointPolicy = CP.defaultPolicy
        readChainPoints = atomically $ listCheckpoints wid
        rollBackward =
            throwInIO . rollbackBlocks @_ @s @k ctx wid . toSlot
        rollForward' blockdata tip = throwInIO $
            restoreBlocks @_ @s @k
                ctx (contramap MsgWalletFollow tr) wid blockdata tip
    in
      catchFromIO $ case (maybeDiscover, lightSync nw) of
        (Just discover, Just sync) ->
            sync $ ChainFollower
                { checkpointPolicy
                , readChainPoints
                , rollForward = rollForward' . either List (Summary discover)
                , rollBackward
                }
        (_,_) -> -- light-mode not available
            chainSync nw (contramap MsgChainFollow tr) $ ChainFollower
                { checkpointPolicy
                , readChainPoints
                , rollForward = rollForward' . List
                , rollBackward
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
--
-- Concurrency: `restoreBlocks` is atomic.
-- However, in the future, we may assume that
-- it is called in a sequential fashion for each wallet.
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
    -> BlockData IO (Either Address RewardAccount) ChainEvents s
    -> BlockHeader
    -> ExceptT ErrNoSuchWallet IO ()
restoreBlocks ctx tr wid blocks nodeTip = db & \DBLayer{..} ->
  mapExceptT atomically $ do
    sp  <- liftIO $ currentSlottingParameters nl
    cp0 <- withNoSuchWallet wid (readCheckpoint wid)
    unless (cp0 `isParentOf` firstHeader blocks) $ fail $ T.unpack $ T.unwords
        [ "restoreBlocks: given chain isn't a valid continuation."
        , "Wallet is at:", pretty (currentTip cp0)
        , "but the given chain continues starting from:"
        , pretty (firstHeader blocks)
        ]

    -- TODO on concurrency:
    -- In light-mode, 'applyBlocks' may take some time to retrieve
    -- transaction data. We want avoid blocking the database by
    -- not wrapping this into a call to 'atomically'.
    -- However, this only works if the latest database checkpoint, `cp0`,
    -- does not change in the meantime.
    (filteredBlocks', cps') <- liftIO $ NE.unzip <$> applyBlocks @s blocks cp0
    let cps = NE.map snd cps'
        filteredBlocks = concat filteredBlocks'
    let slotPoolDelegations =
            [ (pseudoSlotNo (fblock ^. #slot), cert)
            | fblock <- filteredBlocks
            , cert <- view #delegations fblock
            ]
        pseudoSlotNo Origin = 0
        pseudoSlotNo (At sl) = sl
    let txs = foldMap (view #transactions) filteredBlocks
    let epochStability = (3*) <$> getSecurityParameter sp
    let localTip = currentTip $ NE.last cps

    -- FIXME LATER during ADP-1403
    -- We need to rethink checkpoint creation and consider the case
    -- where the blocks are given as a 'Summary' and not a full 'List'
    -- of blocks. In this case, it could happen that the current
    -- scheme fails to create sufficiently many checkpoint as
    -- it was never able to touch the corresponding block.
    -- For now, we avoid this situation by being always supplied a 'List'
    -- in the unstable region close to the tip.
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
            [ UpdateCheckpoints [ PutCheckpoint (getSlot wcp) wcp ]
            | wcp <- map (snd . fromWallet) cpsKeep
            ]

    liftIO $ forM_ txs $ \(Tx {txCBOR=mcbor},_) ->
        forM_ mcbor $ \cbor -> do
            traceWith tr $ MsgStoringCBOR cbor

    putTxHistory wid txs

    updatePendingTxForExpiry wid (view #slotNo localTip)
    forM_ slotPoolDelegations $ \delegation@(slotNo, cert) -> do
        liftIO $ logDelegation delegation
        putDelegationCertificate wid cert slotNo

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

    isParentOf :: Wallet s -> BlockHeader -> Bool
    isParentOf cp = (== Just parent) . parentHeaderHash
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
fetchRewardBalance :: forall s k. DBLayer IO s k -> WalletId -> IO Coin
fetchRewardBalance DBLayer{..} = atomically . readDelegationRewardBalance

mkExternalWithdrawal
    :: forall k ktype tx
     . NetworkLayer IO Block
    -> TransactionLayer k ktype tx
    -> AnyCardanoEra
    -> SomeMnemonic
    -> IO (Either ErrWithdrawalNotBeneficial Withdrawal)
mkExternalWithdrawal netLayer txLayer era mnemonic = do
    let (_, rewardAccount, derivationPath) =
            someRewardAccount @ShelleyKey mnemonic
    balance <- getCachedRewardAccountBalance netLayer rewardAccount
    pp <- currentProtocolParameters netLayer
    pure $ checkRewardIsWorthTxCost txLayer pp era balance $>
        WithdrawalExternal rewardAccount derivationPath balance

mkSelfWithdrawal
    :: forall ktype tx (n :: NetworkDiscriminant)
     . NetworkLayer IO Block
    -> TransactionLayer ShelleyKey ktype tx
    -> AnyCardanoEra
    -> DBLayer IO (SeqState n ShelleyKey) ShelleyKey
    -> WalletId
    -> IO Withdrawal
mkSelfWithdrawal netLayer txLayer era db wallet = do
    (rewardAccount, _, derivationPath) <-
        runExceptT (readRewardAccount db wallet)
            >>= either (throwIO . ExceptionReadRewardAccount) pure
    balance <- getCachedRewardAccountBalance netLayer rewardAccount
    pp <- currentProtocolParameters netLayer
    pure $ WithdrawalSelf rewardAccount derivationPath
        $ either (\_notWorth -> Coin 0) (\_worth -> balance)
        $ checkRewardIsWorthTxCost txLayer pp era balance

-- | Unsafe version of the `mkSelfWithdrawal` function that throws an exception
-- when applied to a non-shelley or a non-sequential wallet.
shelleyOnlyMkSelfWithdrawal
    :: forall s k ktype tx (n :: NetworkDiscriminant)
     . (Typeable s, Typeable k, Typeable n)
    => NetworkLayer IO Block
    -> TransactionLayer k ktype tx
    -> AnyCardanoEra
    -> DBLayer IO s k
    -> WalletId
    -> IO Withdrawal
shelleyOnlyMkSelfWithdrawal netLayer txLayer era db wallet =
    case testEquality (typeRep @s) (typeRep @(SeqState n k)) of
        Nothing -> notShelleyWallet
        Just Refl -> case testEquality (typeRep @k) (typeRep @ShelleyKey) of
            Nothing -> notShelleyWallet
            Just Refl -> mkSelfWithdrawal netLayer txLayer era db wallet
  where
    notShelleyWallet = throwIO
        $ ExceptionReadRewardAccount ErrReadRewardAccountNotAShelleyWallet

checkRewardIsWorthTxCost
    :: forall k ktype tx
     . TransactionLayer k ktype tx
    -> ProtocolParameters
    -> AnyCardanoEra
    -> Coin
    -> Either ErrWithdrawalNotBeneficial ()
checkRewardIsWorthTxCost txLayer pp era balance = do
    when (balance == Coin 0)
        $ Left ErrWithdrawalNotBeneficial
    let minimumCost txCtx = calcMinimumCost txLayer era pp txCtx emptySkeleton
        costWith = minimumCost $ mkTxCtx balance
        costWithout = minimumCost $ mkTxCtx $ Coin 0
        worthOfWithdrawal = Coin.toInteger costWith - Coin.toInteger costWithout
    when (Coin.toInteger balance < 2 * worthOfWithdrawal)
        $ Left ErrWithdrawalNotBeneficial
  where
    mkTxCtx wdrl = defaultTransactionCtx
        { txWithdrawal = WithdrawalSelf dummyAcct dummyPath wdrl }
      where
        dummyAcct = RewardAccount mempty
        dummyPath = DerivationIndex 0 :| []

readRewardAccount
    :: forall (n :: NetworkDiscriminant)
     . DBLayer IO (SeqState n ShelleyKey) ShelleyKey
    -> WalletId
    -> ExceptT ErrReadRewardAccount IO
        (RewardAccount, XPub, NonEmpty DerivationIndex)
readRewardAccount db wid = do
    walletState <- getState <$>
        withExceptT ErrReadRewardAccountNoSuchWallet
            (readWalletCheckpoint db wid)
    let xpub = Seq.rewardAccountKey walletState
    let path = stakeDerivationPath $ Seq.derivationPrefix walletState
    pure (toRewardAccount xpub, getRawKey xpub, path)
  where
    readWalletCheckpoint ::
        DBLayer IO s k -> WalletId -> ExceptT ErrNoSuchWallet IO (Wallet s)
    readWalletCheckpoint DBLayer{..} wallet =
        liftIO (atomically (readCheckpoint wallet)) >>=
            maybe (throwE (ErrNoSuchWallet wallet)) pure

-- | Unsafe version of the `readRewardAccount` function
-- that throws error when applied to a non-sequential
-- or a non-shelley wallet state.
shelleyOnlyReadRewardAccount
    :: forall s k (n :: NetworkDiscriminant)
     . (Typeable s, Typeable n, Typeable k)
    => DBLayer IO s k
    -> WalletId
    -> ExceptT ErrReadRewardAccount IO
        (RewardAccount, XPub, NonEmpty DerivationIndex)
shelleyOnlyReadRewardAccount db wid =
    case testEquality (typeRep @s) (typeRep @(SeqState n k)) of
        Nothing -> throwE ErrReadRewardAccountNotAShelleyWallet
        Just Refl ->
            case testEquality (typeRep @k) (typeRep @ShelleyKey) of
                Nothing -> throwE ErrReadRewardAccountNotAShelleyWallet
                Just Refl -> readRewardAccount db wid

readPolicyPublicKey
    :: forall ctx s k (n :: NetworkDiscriminant) shelley.
        ( HasDBLayer IO s k ctx
        , shelley ~ SeqState n ShelleyKey
        , Typeable n
        , Typeable s
        )
    => ctx
    -> WalletId
    -> ExceptT ErrReadPolicyPublicKey IO (XPub, NonEmpty DerivationIndex)
readPolicyPublicKey ctx wid = db & \DBLayer{..} -> do
    cp <- withExceptT ErrReadPolicyPublicKeyNoSuchWallet
        $ mapExceptT atomically
        $ withNoSuchWallet wid
        $ readCheckpoint wid
    case testEquality (typeRep @s) (typeRep @shelley) of
        Nothing ->
            throwE ErrReadPolicyPublicKeyNotAShelleyWallet
        Just Refl -> do
            let s = getState cp
            case Seq.policyXPub s of
                Nothing -> throwE ErrReadPolicyPublicKeyAbsent
                Just xpub -> pure (getRawKey xpub, policyDerivationPath)
  where
    db = ctx ^. dbLayer @IO @s @k

manageRewardBalance
    :: forall (n :: NetworkDiscriminant)
     . Tracer IO WalletWorkerLog
    -> NetworkLayer IO Block
    -> DBLayer IO (SeqState n ShelleyKey) ShelleyKey
    -> WalletId
    -> IO ()
manageRewardBalance tr' netLayer db@DBLayer{..} wid = do
    watchNodeTip netLayer $ \bh -> do
         traceWith tr $ MsgRewardBalanceQuery bh
         query <- runExceptT $ do
            (acct, _, _) <- withExceptT ErrFetchRewardsReadRewardAccount
                $ readRewardAccount db wid
            liftIO $ getCachedRewardAccountBalance netLayer acct
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
    tr = contramap MsgWallet tr'

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
        , PaymentAddress n k 'CredFromKeyK
        , RndStateLike s
        , k ~ ByronKey
        , AddressBookIso s
        )
    => ctx
    -> WalletId
    -> Passphrase "user"
    -> Maybe (Index 'Hardened 'CredFromKeyK)
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

-- NOTE
-- Addresses coming from the transaction history might be payment or
-- delegation addresses. So we normalize them all to be delegation addresses
-- to make sure that we compare them correctly.
normalizeDelegationAddress
    :: forall s k n.
        ( DelegationAddress n k 'CredFromKeyK
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
-- The 'TxIn's of the 'inputs' must exactly match the inputs contained in the
-- 'tx'. If not, the behaviour is undefined. This will be fixed by ADP-1662.
--
-- The provided 'redeemers' will overwrite any redeemers inside the 'tx'. This
-- is done as the internal redeemers in the 'tx' use an index referring to a
-- 'TxIn', rather than an actual 'TxIn'. When we are adding extra inputs as part
-- of balancing, these indexes become incorrect.
--
-- TODO: With some extra care, we could probably remove the 'redeemers' field
-- and instead adjust the existing redeemer indexes ourselves when balancing,
-- even though they are in an "unordered" set.
data PartialTx era = PartialTx
    { tx :: Cardano.Tx era
    , inputs :: Cardano.UTxO era
      -- ^ NOTE: Can we rename this to something better? Perhaps 'extraUTxO'?
    , redeemers :: [Redeemer]
    } deriving (Show, Generic, Eq)

instance Buildable (PartialTx era) where
    build (PartialTx tx (Cardano.UTxO ins) redeemers)
        = nameF "PartialTx" $ mconcat
            [ nameF "inputs" (blockListF' "-" inF (Map.toList ins))
            , nameF "redeemers" (pretty redeemers)
            , nameF "tx" (cardanoTxF tx)
            ]
      where
        inF = build . show

        cardanoTxF :: Cardano.Tx era -> Builder
        cardanoTxF tx' = pretty $ pShow tx'

balanceTransaction
    :: forall era m s k ktype.
        ( GenChange s
        , MonadRandom m
        , WriteTx.IsRecentEra era
        , BoundedAddressLength k
        )
    => Tracer m WalletLog
    -> TransactionLayer k ktype SealedTx
    -> ArgGenChange s
    -> (W.ProtocolParameters, Cardano.ProtocolParameters)
    -- ^ 'Cardano.ProtocolParameters' can be retrieved via a Local State Query
    -- to a local node.
    --
    -- If passed an incorrect value, a phase 1 script integrity hash mismatch
    -- will protect against collateral being forfeited.
    --
    -- TODO: Remove the 'W.ProtocolParameters' argument.
    -> TimeInterpreter (Either PastHorizonException)
    -- ^ Needed to convert convert validity intervals from 'UTCTime' to 'SlotNo'
    -- when executing Plutus scripts.
    --
    -- Can be hard-coded for a given network configuration. Just be
    -- cautious of the fact that the value will occasionally
    -- change as new eras are introduced to Cardano.
    --
    -- It is unclear whether an incorrect value could cause collateral to be
    -- forfeited. We should ideally investigate and clarify as part of ADP-1544
    -- or similar ticket. Relevant ledger code: https://github.com/input-output-hk/cardano-ledger/blob/fdec04e8c071060a003263cdcb37e7319fb4dbf3/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/TxInfo.hs#L428-L440
    -> (UTxOIndex WalletUTxO, Wallet s, Set Tx)
    -- ^ TODO [ADP-1789] Replace with @Cardano.UTxO@ and something simpler than
    -- @Wallet s@ for change address generation.
    -> PartialTx era
    -> ExceptT ErrBalanceTx m (Cardano.Tx era)
balanceTransaction tr txLayer change pp ti wallet unadjustedPtx = do
    -- TODO [ADP-1490] Take 'Ledger.PParams era' directly as argument, and avoid
    -- converting to/from Cardano.ProtocolParameters. This may affect
    -- performance. The addition of this one specific conversion seems to have
    -- made the --match "balanceTransaction" unit tests 11% slower in CPU time.
    let ledgerPP = Cardano.toLedgerPParams shelleyEra $ snd pp
    let adjustedPtx = over (#tx)
            (increaseZeroAdaOutputs recentEra ledgerPP)
            unadjustedPtx

    let balanceWith strategy =
            balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
                @era @m @s @k @ktype
                tr txLayer change pp ti wallet strategy adjustedPtx
    balanceWith SelectionStrategyOptimal
        `catchE` \e ->
            if minimalStrategyIsWorthTrying e
            then balanceWith SelectionStrategyMinimal
            else throwE e
  where
    shelleyEra = Cardano.shelleyBasedEra @era
    recentEra = WriteTx.recentEra @era

    -- Determines whether or not the minimal selection strategy is worth trying.
    -- This depends upon the way in which the optimal selection strategy failed.
    minimalStrategyIsWorthTrying :: ErrBalanceTx -> Bool
    minimalStrategyIsWorthTrying e = or
        [ maxSizeLimitExceeded
        , unableToConstructChange
        , selectionCollateralError
        ]
      where
        -- The size of a transaction can be reduced by selecting fewer inputs,
        -- or by generating less change. Since the minimal selection strategy
        -- selects as few inputs and generates as little change as possible,
        -- using this strategy might allow us to generate a transaction within
        -- the size limit.
        maxSizeLimitExceeded = case e of
            ErrBalanceTxMaxSizeLimitExceeded ->
                True
            _someOtherError ->
                False

        -- In situations where the available supply of ada is constrained, or
        -- where all available ada is bundled up with other tokens, this can
        -- prevent us from generating change. In this case, trying again with
        -- the minimal selection strategy might allow us to select fewer
        -- inputs, generate less change, lower the amount of ada required to
        -- pay for the change, and therefore increase the chance that we can
        -- generate change successfully.
        unableToConstructChange = case e of
            ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionBalanceErrorOf
                (UnableToConstructChange {}))) ->
                True
            _someOtherError ->
                False

        -- The minimum required amount of collateral depends on the transaction
        -- fee, which in turn depends on the space occupied by ordinary inputs
        -- and generated change. If we select fewer inputs and generate less
        -- change, we can lower the transaction fee, lower the minimum required
        -- amount of collateral, and increase the chance of being able to
        -- satisfy the minimum.
        selectionCollateralError = case e of
            ErrBalanceTxSelectAssets
                (ErrSelectAssetsSelectionError
                (SelectionCollateralErrorOf {})) ->
                True
            _someOtherError ->
                False

-- | Increases the ada value of any 0-ada outputs in the transaction to the
-- minimum according to 'computeMinimumCoinForTxOut'.
increaseZeroAdaOutputs
    :: forall era. WriteTx.RecentEra era
    -> WriteTx.PParams (Cardano.ShelleyLedgerEra era)
    -> Cardano.Tx era
    -> Cardano.Tx era
increaseZeroAdaOutputs era pp = WriteTx.modifyLedgerBody $
    WriteTx.modifyTxOutputs era $ \out ->
        flip (WriteTx.modifyTxOutCoin era) out $ \c ->
            if c == mempty
            then WriteTx.computeMinimumCoinForTxOut era pp out
            else c

-- | Internal helper to 'balanceTransaction'
balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
    :: forall era m s k ktype.
        ( GenChange s
        , BoundedAddressLength k
        , MonadRandom m
        , WriteTx.IsRecentEra era
        )
    => Tracer m WalletLog
    -> TransactionLayer k ktype SealedTx
    -> ArgGenChange s
    -> (W.ProtocolParameters, Cardano.ProtocolParameters)
    -> TimeInterpreter (Either PastHorizonException)
    -> (UTxOIndex WalletUTxO, Wallet s, Set Tx)
    -> SelectionStrategy
    -> PartialTx era
    -> ExceptT ErrBalanceTx m (Cardano.Tx era)
balanceTransactionWithSelectionStrategyAndNoZeroAdaAdjustment
    tr
    txLayer
    generateChange
    (pp, nodePParams)
    ti
    (internalUtxoAvailable, wallet, _pendingTxs)
    selectionStrategy
    ptx@(PartialTx partialTx inputUTxO redeemers)
    = do
    guardExistingCollateral partialTx
    guardExistingTotalCollateral partialTx
    guardExistingReturnCollateral partialTx
    guardConflictingWithdrawalNetworks partialTx
    guardWalletUTxOConsistencyWith inputUTxO

    let era = Cardano.anyCardanoEra $ Cardano.cardanoEra @era

    (balance0, minfee0) <- balanceAfterSettingMinFee partialTx

    (extraInputs, extraCollateral, extraOutputs) <- do

        -- NOTE: It is not possible to know the script execution cost in
        -- advance because it actually depends on the final transaction. Inputs
        -- selected as part of the fee balancing might have an influence on the
        -- execution cost.
        -- However, they are bounded so it is possible to balance the
        -- transaction considering only the maximum cost, and only after, try to
        -- adjust the change and ExUnits of each redeemer to something more
        -- sensible than the max execution cost.

        randomSeed <- stdGenSeed
        let
            transform :: s -> Selection -> ([(TxIn, TxOut)], [TxIn], [TxOut])
            transform s sel =
                let (sel', _) = assignChangeAddresses generateChange sel s
                    inputs = F.toList (sel' ^. #inputs)
                in  ( inputs
                    , fst <$> (sel' ^. #collateral)
                    , sel' ^. #change
                    )

        lift $ traceWith tr $ MsgSelectionForBalancingStart
            (UTxOIndex.size internalUtxoAvailable)
            (BuildableInAnyEra Cardano.cardanoEra ptx)

        externalSelectedUtxo <- extractExternallySelectedUTxO ptx
        let mSel = selectAssets'
                era
                (extractOutputsFromTx $ toSealed partialTx)
                (UTxOSelection.fromIndexPair
                    (internalUtxoAvailable, externalSelectedUtxo))
                balance0
                minfee0
                randomSeed

        case mSel of
            Left e -> lift $
                traceWith tr $ MsgSelectionError e
            Right sel -> lift $ do
                traceWith tr $ MsgSelectionReportSummarized
                    $ makeSelectionReportSummarized sel
                traceWith tr $ MsgSelectionReportDetailed
                    $ makeSelectionReportDetailed sel

        withExceptT (ErrBalanceTxSelectAssets . ErrSelectAssetsSelectionError)
            . ExceptT . pure $
                transform (getState wallet) <$> mSel

    -- NOTE:
    -- Once the coin selection is done, we need to
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

    let unsafeFromLovelace (Cardano.Lovelace l) = Coin.unsafeFromIntegral l
    candidateTx <- assembleTransaction $ TxUpdate
        { extraInputs
        , extraCollateral
        , extraOutputs
        , feeUpdate = UseNewTxFee $ unsafeFromLovelace minfee0
        }

    (balance, candidateMinFee) <- balanceAfterSettingMinFee candidateTx
    surplus <- case Cardano.selectLovelace balance of
        (Cardano.Lovelace c)
            | c >= 0 ->
                pure $ Coin.unsafeFromIntegral c
            | otherwise ->
                throwE . ErrBalanceTxInternalError $
                ErrUnderestimatedFee
                    (Coin.unsafeFromIntegral (-c))
                    (toSealed candidateTx)

    let feeAndChange = TxFeeAndChange
            (unsafeFromLovelace candidateMinFee)
            (extraOutputs)
    let feePolicy = view (#txParameters . #getFeePolicy) pp

    -- @distributeSurplus@ should never fail becase we have provided enough
    -- padding in @selectAssets'@.
    TxFeeAndChange updatedFee updatedChange <- withExceptT
        (\(ErrMoreSurplusNeeded c) ->
            ErrBalanceTxInternalError
                $ ErrUnderestimatedFee c (toSealed candidateTx))
        (ExceptT . pure $ distributeSurplus txLayer feePolicy surplus feeAndChange)

    guardTxSize =<< guardTxBalanced =<< (assembleTransaction $ TxUpdate
        { extraInputs
        , extraCollateral
        , extraOutputs = updatedChange
        , feeUpdate = UseNewTxFee updatedFee
        })
  where
    toSealed :: Cardano.Tx era -> SealedTx
    toSealed = sealedTxFromCardano . Cardano.InAnyCardanoEra Cardano.cardanoEra

    -- | Extract the inputs from the raw 'tx' of the 'Partialtx', with the
    -- corresponding 'TxOut' according to @combinedUTxO@.
    --
    -- === Examples using pseudo-code
    --
    -- >>> let extraUTxO = {inA -> outA, inB -> outB }
    -- >>> let tx = addInputs [inA] emptyTx
    -- >>> let ptx = PartialTx tx extraUTxO []
    -- >>> extractExternallySelectedUTxO ptx
    -- Right (UTxOIndex.fromMap {inA -> outA})
    --
    -- >>> let extraUTxO = {inB -> outB }
    -- >>> let tx = addInputs [inA, inC] emptyTx
    -- >>> let ptx = PartialTx tx extraUTxO []
    -- >>> extractExternallySelectedUTxO ptx
    -- Left (ErrBalanceTxUnresolvedInputs [inA, inC])
    extractExternallySelectedUTxO
        :: PartialTx era
        -> ExceptT ErrBalanceTx m (UTxOIndex WalletUTxO)
    extractExternallySelectedUTxO (PartialTx tx _ _rdms) = do
        let res = flip map txIns $ \(i, _) -> do
                case Map.lookup i utxo of
                    Nothing ->
                       Left i
                    Just o -> do
                        let i' = fromCardanoTxIn i
                        let TxOut addr bundle = fromCardanoTxOut o
                        pure (WalletUTxO i' addr, bundle)

        case partitionEithers res of
            ([], resolved) ->
                pure $ UTxOIndex.fromSequence resolved
            (unresolvedInsHead:unresolvedInsTail, _) ->
                throwE
                . ErrBalanceTxUnresolvedInputs
                . fmap fromCardanoTxIn
                $ (unresolvedInsHead :| unresolvedInsTail)
      where
        Cardano.UTxO utxo = combinedUTxO
        Cardano.Tx (Cardano.TxBody (Cardano.TxBodyContent { Cardano.txIns })) _
            = tx

    guardTxSize :: Cardano.Tx era -> ExceptT ErrBalanceTx m (Cardano.Tx era)
    guardTxSize tx = do
        let size = estimateSignedTxSize txLayer nodePParams tx
        let maxSize = TxSize
                . intCast
                . getQuantity
                $ view (#txParameters . #getTxMaxSize) pp
        when (size > maxSize) $
            throwE ErrBalanceTxMaxSizeLimitExceeded
        return tx

    guardTxBalanced :: Cardano.Tx era -> ExceptT ErrBalanceTx m (Cardano.Tx era)
    guardTxBalanced tx = do
        let bal = txBalance tx
        if bal == mempty
            then pure tx
            else throwE $ ErrBalanceTxInternalError $ ErrFailedBalancing bal

    txBalance :: Cardano.Tx era -> Cardano.Value
    txBalance tx =
        evaluateTransactionBalance txLayer tx nodePParams combinedUTxO

    balanceAfterSettingMinFee
        :: Cardano.Tx era
        -> ExceptT ErrBalanceTx m (Cardano.Value, Cardano.Lovelace)
    balanceAfterSettingMinFee tx = ExceptT . pure $ do
        -- NOTE: evaluateMinimumFee relies on correctly estimating the required
        -- number of witnesses.
        let minfee = evaluateMinimumFee txLayer nodePParams tx
        let update = TxUpdate [] [] [] (UseNewTxFee minfee)
        tx' <- left ErrBalanceTxUpdateError $ updateTx txLayer tx update
        let balance = evaluateTransactionBalance txLayer tx' nodePParams combinedUTxO
        let minfee' = Cardano.Lovelace $ fromIntegral $ unCoin minfee
        return (balance, minfee')

    -- | Ensure the wallet UTxO is consistent with a provided @Cardano.UTxO@.
    --
    -- They are not consistent iff an input can be looked up in both UTxO sets
    -- with different @Address@, or @TokenBundle@ values.
    --
    -- The @Cardano.UTxO era@ is allowed to contain additional information, like
    -- datum hashes, which the wallet UTxO cannot represent.
    --
    -- NOTE: Representing the wallet utxo as a @Cardano.UTxO@ will not make this
    -- check easier, even if it may be useful in other regards.
    guardWalletUTxOConsistencyWith
        :: Cardano.UTxO era
        -> ExceptT ErrBalanceTx m ()
    guardWalletUTxOConsistencyWith u' = do
        let u = Map.mapKeys fromCardanoTxIn
                . Map.map fromCardanoTxOut
                $ (unUTxO u')
        let conflicts = lefts $ flip map (Map.toList u) $ \(i, o) ->
                case i `UTxO.lookup` walletUTxO of
                    Just o' -> unless (o == o') $ Left (o, o')
                    Nothing -> pure ()

        case conflicts of
            [] -> return ()
            (c:cs) -> throwE $ ErrBalanceTxInputResolutionConflicts (c :| cs)
      where
         unUTxO (Cardano.UTxO u) = u

    walletUTxO :: UTxO
    walletUTxO = CS.toExternalUTxOMap $ UTxOIndex.toMap internalUtxoAvailable

    combinedUTxO :: Cardano.UTxO era
    combinedUTxO = Cardano.UTxO $ mconcat
         -- The @Cardano.UTxO@ can contain strictly more information than
         -- @W.UTxO@. Therefore we make the user-specified @inputUTxO@ to take
         -- precedence. This matters if a user is trying to balance a tx making
         -- use of a datum hash in a UTxO which is also present in the wallet
         -- UTxO set. (Whether or not this is a sane thing for the user to do,
         -- is another question.)
         [ unUTxO inputUTxO
         , unUTxO $ toCardanoUTxO Cardano.shelleyBasedEra walletUTxO
         ]

      where
         unUTxO (Cardano.UTxO u) = u

    assembleTransaction
        :: TxUpdate
        -> ExceptT ErrBalanceTx m (Cardano.Tx era)
    assembleTransaction update = ExceptT . pure $ do
        tx' <- left ErrBalanceTxUpdateError $ updateTx txLayer partialTx update
        left ErrBalanceTxAssignRedeemers $ assignScriptRedeemers
            txLayer nodePParams ti combinedUTxO redeemers tx'

    extractOutputsFromTx tx =
        let
            era = Cardano.AnyCardanoEra $ Cardano.cardanoEra @era
            (Tx {outputs}, _, _, _, _, _) = decodeTx txLayer era AnyWitnessCountCtx tx
        in outputs

    guardConflictingWithdrawalNetworks
        (Cardano.Tx (Cardano.TxBody body) _) = do
        -- Use of withdrawals with different networks breaks balancing.
        --
        -- For instance the partial tx might contain two withdrawals with the
        -- same key but different networks:
        -- [ (Mainnet, pkh1, coin1)
        -- , (Testnet, pkh1, coin2)
        -- ]
        --
        -- Even though this is absurd, the node/ledger
        -- @evaluateTransactionBalance@ will count @coin1+coin2@ towards the
        -- total balance. Because the wallet does not consider the network tag,
        -- it will drop one of the two, leading to a discrepancy.
        let networkOfWdrl ((Cardano.StakeAddress nw _), _, _) = nw
        let conflictingWdrlNetworks = case Cardano.txWithdrawals body of
                Cardano.TxWithdrawalsNone -> False
                Cardano.TxWithdrawals _ wdrls -> Set.size
                    (Set.fromList $ map networkOfWdrl wdrls) > 1
        when conflictingWdrlNetworks $
            throwE ErrBalanceTxConflictingNetworks

    guardExistingCollateral (Cardano.Tx (Cardano.TxBody body) _) = do
        -- Coin selection does not support pre-defining collateral. In Sep 2021
        -- consensus was that we /could/ allow for it with just a day's work or
        -- so, but that the need for it was unclear enough that it was not in
        -- any way a priority.
        case Cardano.txInsCollateral body of
            Cardano.TxInsCollateralNone -> return ()
            Cardano.TxInsCollateral _ [] -> return ()
            Cardano.TxInsCollateral _ _ ->
                throwE ErrBalanceTxExistingCollateral

    guardExistingTotalCollateral (Cardano.Tx (Cardano.TxBody body) _) =
        case Cardano.txTotalCollateral body of
            Cardano.TxTotalCollateralNone -> return ()
            Cardano.TxTotalCollateral _ _ ->
               throwE ErrBalanceTxExistingTotalCollateral

    guardExistingReturnCollateral (Cardano.Tx (Cardano.TxBody body) _) =
        case Cardano.txReturnCollateral body of
            Cardano.TxReturnCollateralNone -> return ()
            Cardano.TxReturnCollateral _ _ ->
               throwE ErrBalanceTxExistingReturnCollateral

    -- | Select assets to cover the specified balance and fee.
    --
    -- If the transaction contains redeemers, the function will also ensure the
    -- selection covers the fees for the maximum allowed execution units of a
    -- transaction. For this, and other reasons, the selection may include too
    -- much ada.
    selectAssets'
        :: Cardano.AnyCardanoEra
        -> [TxOut]
        -> UTxOSelection WalletUTxO
        -- ^ Describes which utxos are pre-selected, and which can be used as
        -- inputs or collateral.
        -> Cardano.Value -- Balance to cover
        -> Cardano.Lovelace -- Current minfee (before selecting assets)
        -> StdGenSeed
        -> Either (SelectionError WalletSelectionContext) Selection
    selectAssets' era outs utxoSelection balance fee0 seed =
        let
            txPlutusScriptExecutionCost = maxScriptExecutionCost txLayer pp redeemers
            colReq =
                if txPlutusScriptExecutionCost > Coin 0 then
                    SelectionCollateralRequired
                else
                    SelectionCollateralNotRequired

            (positiveBundle, negativeBundle) = posAndNegFromCardanoValue balance
            (TokenBundle positiveAda positiveTokens) = positiveBundle
            (TokenBundle negativeAda negativeTokens) = negativeBundle

            adaInOutputs = F.foldMap (TokenBundle.getCoin . view #tokens) outs
            tokensInOutputs = F.foldMap (TokenBundle.tokens . view #tokens) outs
            tokensInInputs = TokenBundle.tokens
                $ UTxOSelection.selectedBalance utxoSelection
            adaInInputs = TokenBundle.getCoin
                $ UTxOSelection.selectedBalance utxoSelection

            boringFee =
                let
                    boringSkeleton = SelectionSkeleton
                        { skeletonInputCount =
                            UTxOSelection.selectedSize utxoSelection
                        , skeletonOutputs = outs
                        , skeletonChange = []
                        }
                in calcMinimumCost
                        txLayer
                        era
                        pp
                        defaultTransactionCtx
                        boringSkeleton

            feePadding =
                let LinearFee LinearFunction {slope = perByte} =
                        view (#txParameters . #getFeePolicy) pp
                    scriptIntegrityHashBytes = 32 + 2

                    -- Add padding to allow the fee value to increase.
                    -- Out of caution, assume it can increase by the theoretical
                    -- maximum of 8 bytes ('maximumCostOfIncreasingCoin').
                    --
                    -- NOTE: It's not convenient to import the constant at the
                    -- moment because of the package split.
                    --
                    -- Any overestimation will be reduced by 'distributeSurplus'
                    -- in the final stage of 'balanceTransaction'.
                    extraBytes = 8
                in
                Coin $ (round perByte) * (extraBytes + scriptIntegrityHashBytes)

            fromCardanoLovelace (Cardano.Lovelace l) = Coin.unsafeFromIntegral l

            selectionConstraints = SelectionConstraints
                { assessTokenBundleSize =
                    view #assessTokenBundleSize $
                    tokenBundleSizeAssessor txLayer $
                    pp ^. (#txParameters . #getTokenBundleMaxSize)
                , certificateDepositAmount =
                    view #stakeKeyDeposit pp
                , computeMinimumAdaQuantity =
                    view #txOutputMinimumAdaQuantity
                        (constraints txLayer era pp)
                , isBelowMinimumAdaQuantity =
                    view #txOutputBelowMinimumAdaQuantity
                        (constraints txLayer era pp)
                , computeMinimumCost = \skeleton -> mconcat
                    [ feePadding
                    , fromCardanoLovelace fee0
                    , calcMinimumCost txLayer era pp
                        (defaultTransactionCtx
                            { txPlutusScriptExecutionCost =
                                txPlutusScriptExecutionCost })
                        skeleton
                    ] `Coin.difference` boringFee
                , computeSelectionLimit = \_ -> NoLimit
                , maximumCollateralInputCount =
                    intCast @Word16 @Int $ view #maximumCollateralInputCount pp
                , minimumCollateralPercentage =
                    view #minimumCollateralPercentage pp
                , maximumLengthChangeAddress =
                    maxLengthAddressFor $ Proxy @k
                }

            selectionParams = SelectionParams
                -- The following fields are essensially adjusting the coin
                -- selections notion of balance by @balance0 - sum inputs + sum
                -- outputs + fee0@ where @balance0@ is the balance of the
                -- partial tx.
                { assetsToMint = positiveTokens <> tokensInOutputs
                , assetsToBurn = negativeTokens <> tokensInInputs
                , extraCoinIn =
                    positiveAda
                    <> adaInOutputs
                    <> fromCardanoLovelace fee0
                , extraCoinOut = negativeAda <> adaInInputs

                -- We don't use the following 3 fields because certs and
                -- withdrawals are already included in the balance (passed in
                -- above).
                , rewardWithdrawal = Coin 0
                , certificateDepositsReturned = 0
                , certificateDepositsTaken = 0

                -- NOTE: It is important that coin selection has the correct
                -- notion of fees, because it will be used to tell how much
                -- collateral is needed.
                , collateralRequirement = colReq
                , outputsToCover = outs
                , utxoAvailableForCollateral =
                      UTxOSelection.availableMap utxoSelection
                , utxoAvailableForInputs = utxoSelection
                , selectionStrategy = selectionStrategy
                }
            in
                flip evalRand (stdGenFromSeed seed)
                    $ runExceptT
                    $ performSelection selectionConstraints selectionParams

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
    qualifyAddresses getAddress =
        mapMaybe withDerivationPath
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
    -> ExceptT ErrNoSuchWallet IO (UTxOIndex WalletUTxO, Wallet s, Set Tx)
readWalletUTxOIndex ctx wid = do
    (cp, _, pending) <- readWallet @ctx @s @k ctx wid
    let utxo = UTxOIndex.fromMap $
            CS.toInternalUTxOMap $ availableUTxO @s pending cp
    return (utxo, cp, pending)

-- | Calculate the minimum coin values required for a bunch of specified
-- outputs.
calcMinimumCoinValues
    :: forall ctx k ktype f.
        ( HasTransactionLayer k ktype ctx
        , HasNetworkLayer IO ctx
        , Applicative f
        )
    => ctx
    -> Cardano.AnyCardanoEra
    -> f TxOut
    -> IO (f Coin)
calcMinimumCoinValues ctx era outs = do
    pp <- currentProtocolParameters nl
    pure
        $ uncurry (view #txOutputMinimumAdaQuantity (constraints tl era pp))
        . (\o -> (view #address o, view (#tokens . #tokens) o)) <$> outs
  where
    nl = ctx ^. networkLayer
    tl = ctx ^. transactionLayer @k @ktype

-- | Parameters for the 'selectAssets' function.
--
data SelectAssetsParams s result = SelectAssetsParams
    { outputs :: [TxOut]
    , pendingTxs :: Set Tx
    , randomSeed :: Maybe StdGenSeed
    , txContext :: TransactionCtx
    , utxoAvailableForCollateral :: Map WalletUTxO TokenBundle
    , utxoAvailableForInputs :: UTxOSelection WalletUTxO
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
    :: forall ctx m s k ktype result.
        ( BoundedAddressLength k
        , HasTransactionLayer k ktype ctx
        , HasLogger m WalletWorkerLog ctx
        , MonadRandom m
        )
    => ctx
    -> Cardano.AnyCardanoEra
    -> ProtocolParameters
    -> SelectAssetsParams s result
    -> (s -> Selection -> result)
    -> ExceptT ErrSelectAssets m result
selectAssets ctx era pp params transform = do
    guardPendingWithdrawal
    lift $ traceWith tr $ MsgSelectionStart
        (UTxOSelection.availableSize $ params ^. #utxoAvailableForInputs)
        (params ^. #outputs)
    let selectionConstraints = SelectionConstraints
            { assessTokenBundleSize =
                view #assessTokenBundleSize $
                tokenBundleSizeAssessor tl $
                pp ^. (#txParameters . #getTokenBundleMaxSize)
            , certificateDepositAmount =
                view #stakeKeyDeposit pp
            , computeMinimumAdaQuantity =
                view #txOutputMinimumAdaQuantity (constraints tl era pp)
            , isBelowMinimumAdaQuantity =
                view #txOutputBelowMinimumAdaQuantity (constraints tl era pp)
            , computeMinimumCost =
                calcMinimumCost tl era pp $ params ^. #txContext
            , computeSelectionLimit =
                Cardano.Wallet.Transaction.computeSelectionLimit
                    tl era pp $ params ^. #txContext
            , maximumCollateralInputCount =
                intCast @Word16 @Int $ view #maximumCollateralInputCount pp
            , minimumCollateralPercentage =
                view #minimumCollateralPercentage pp
            , maximumLengthChangeAddress =
                maxLengthAddressFor $ Proxy @k
            }
    let selectionParams = SelectionParams
            { assetsToMint =
                fst $ params ^. #txContext . #txAssetsToMint
            , assetsToBurn =
                fst $ params ^. #txContext . #txAssetsToBurn
            , extraCoinIn = Coin 0
            , extraCoinOut = Coin 0
            , outputsToCover = params ^. #outputs
            , rewardWithdrawal =
                withdrawalToCoin $ params ^. #txContext . #txWithdrawal
            , certificateDepositsReturned =
                case params ^. #txContext . #txDelegationAction of
                    Just Quit -> 1
                    _ -> 0
            , certificateDepositsTaken =
                case params ^. #txContext . #txDelegationAction of
                    Just (JoinRegisteringKey _) -> 1
                    _ -> 0
            , collateralRequirement =
                params ^. #txContext . #txCollateralRequirement
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
    tl = ctx ^. transactionLayer @k @ktype
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
  :: forall k ktype
   . ( WalletKey k
     , HardDerivation k
     , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
     )
  => TransactionLayer k ktype SealedTx
  -- ^ The way to interact with the wallet backend
  -> Cardano.AnyCardanoEra
  -- ^ Preferred latest era
  -> (Address -> Maybe (k ktype XPrv, Passphrase "encryption"))
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
signTransaction tl preferredLatestEra keyLookup (rootKey, rootPwd) utxo =
    let
        rewardAcnt :: (XPrv, Passphrase "encryption")
        rewardAcnt =
            (getRawKey $ deriveRewardAccount @k rootPwd rootKey, rootPwd)

        policyKey :: (KeyHash, XPrv, Passphrase "encryption")
        policyKey =
            ( hashVerificationKey @k CA.Policy $ liftRawKey $ toXPub xprv
            , xprv
            , rootPwd
            )
          where
            xprv = derivePolicyPrivateKey rootPwd (getRawKey rootKey) minBound

        inputResolver :: TxIn -> Maybe Address
        inputResolver i = do
            TxOut addr _ <- UTxO.lookup i utxo
            pure addr
    in
        addVkWitnesses
            tl
            preferredLatestEra
            rewardAcnt
            policyKey
            keyLookup
            inputResolver

-- | Produce witnesses and construct a transaction from a given selection.
--
-- Requires the encryption passphrase in order to decrypt the root private key.
-- Note that this doesn't broadcast the transaction to the network. In order to
-- do so, use 'submitTx'.
--
buildAndSignTransaction
    :: forall ctx s k.
        ( HasTransactionLayer k 'CredFromKeyK ctx
        , HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , IsOwned s k 'CredFromKeyK
        )
    => ctx
    -> WalletId
    -> Cardano.AnyCardanoEra
    -> ( (k 'RootK XPrv, Passphrase "encryption") ->
         (         XPrv, Passphrase "encryption")
       )
       -- ^ Reward account derived from the root key (or somewhere else).
    -> Passphrase "user"
    -> TransactionCtx
    -> SelectionOf TxOut
    -> ExceptT ErrSignPayment IO (Tx, TxMeta, UTCTime, SealedTx)
buildAndSignTransaction ctx wid era mkRwdAcct pwd txCtx sel = db & \DBLayer{..} ->
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
    tl = ctx ^. transactionLayer @k @'CredFromKeyK
    nl = ctx ^. networkLayer
    ti = timeInterpreter nl

-- | Construct an unsigned transaction from a given selection.
constructTransaction
    :: forall (n :: NetworkDiscriminant) ktype era
     . Write.Tx.IsRecentEra era
    => TransactionLayer ShelleyKey ktype SealedTx
    -> NetworkLayer IO Block
    -> DBLayer IO (SeqState n ShelleyKey) ShelleyKey
    -> WalletId
    -> TransactionCtx
    -> PreSelection
    -> ExceptT ErrConstructTx IO (Cardano.TxBody era)
constructTransaction txLayer netLayer db wid txCtx preSel = do
    (_, xpub, _) <- readRewardAccount db wid
        & withExceptT ErrConstructTxReadRewardAccount
    pp <- liftIO $ currentProtocolParameters netLayer
    mkUnsignedTransaction txLayer xpub pp txCtx (Left preSel)
        & withExceptT ErrConstructTxBody . except

-- | Construct an unsigned transaction from a given selection
-- for a shared wallet.
constructSharedTransaction
    :: forall ctx s k (n :: NetworkDiscriminant) era.
        ( HasTransactionLayer k 'CredFromScriptK ctx
        , HasDBLayer IO s k ctx
        , HasNetworkLayer IO ctx
        , k ~ SharedKey
        , s ~ SharedState n k
        , Typeable n
        , Write.Tx.IsRecentEra era
        )
    => ctx
    -> WalletId
    -> TransactionCtx
    -> SelectionOf TxOut
    -> ExceptT ErrConstructTx IO (Cardano.TxBody era)
constructSharedTransaction ctx wid txCtx sel = db & \DBLayer{..} -> do
    cp <- withExceptT ErrConstructTxNoSuchWallet
        $ mapExceptT atomically
        $ withNoSuchWallet wid
        $ readCheckpoint wid
    let s = getState cp
    let accXPub = getRawKey $ Shared.accountXPub s
    let xpub = CA.getKey $
            deriveDelegationPublicKey (CA.liftXPub accXPub) minBound
    let allInps =
            (view #collateral sel) ++
            NE.toList (view #inputs sel)
    let getScript (_, TxOut addr _) = case fst (isShared addr s) of
            Nothing ->
                error $ "Some inputs selected by coin selection do not belong "
                <> "to multi-signature wallet"
            Just (ix,role) ->
                let template = paymentTemplate s
                    role' = case role of
                        UtxoExternal -> CAShelley.UTxOExternal
                        UtxoInternal -> CAShelley.UTxOInternal
                        MutableAccount ->
                            error "role is specified only for payment credential"
                in replaceCosignersWithVerKeys role' template ix
    let scriptInps =
            foldr (\inp@(txin,_) -> Map.insert txin (getScript inp))
            Map.empty allInps
    let txCtx' = txCtx {txNativeScriptInputs = scriptInps}
    mapExceptT atomically $ do
        pp <- liftIO $ currentProtocolParameters nl
        withExceptT ErrConstructTxBody $ ExceptT $ pure
            $ mkUnsignedTransaction tl @era xpub pp txCtx' (Right sel)
  where
    db = ctx ^. dbLayer @IO @s @k
    tl = ctx ^. transactionLayer @k @'CredFromScriptK
    nl = ctx ^. networkLayer

-- | Calculate the transaction expiry slot, given a 'TimeInterpreter', and an
-- optional TTL in seconds.
--
-- If no TTL is provided, a default of 2 hours is used (note: there is no
-- particular reason why we chose that duration).
transactionExpirySlot
    :: TimeInterpreter (ExceptT PastHorizonException IO)
    -- ^ Context for time to slot calculation.
    -> Maybe NominalDiffTime
    -- ^ Time to live (TTL) in seconds from now.
    -> IO SlotNo
transactionExpirySlot safeTimeInterpreter maybeTTL =
    interpretQuery timeInterpreter . ceilingSlotAt . addRelTime ttl
        =<< currentRelativeTime timeInterpreter
  where
    timeInterpreter = unsafeExtendSafeZone safeTimeInterpreter
    ttl :: NominalDiffTime = fromMaybe defaultTTL maybeTTL
    defaultTTL :: NominalDiffTime = 7200  -- that's 2 hours

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
        amtOuts = F.fold $ map TxOut.coin outs

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
       , expiry = Just (snd $ txValidityInterval txCtx)
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
            (TxOut.coin <$> view #change sel)
            ++
            mapMaybe (`ourCoin` wState) (view #outputs sel)

        amtInps
            = F.fold (TxOut.coin . snd <$> view #inputs sel)
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
                , expiry = Just (snd $ txValidityInterval txCtx)
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
    :: forall ctx k ktype.
        ( HasNetworkLayer IO ctx
        , HasTransactionLayer k ktype ctx
        , HasLogger IO TxSubmitLog ctx
        )
    => ctx
    -> SealedTx
    -> ExceptT ErrPostTx IO Tx
submitExternalTx ctx sealedTx = do
    -- FIXME: We read the current era to constrain the @sealedTx@ **twice**:
    -- once here for decodeTx, and once in postTx before submitting.
    era <- liftIO $ currentNodeEra nw
    let (tx, _, _, _, _, _) = decodeTx tl era AnyWitnessCountCtx sealedTx
    let trPost = contramap (MsgSubmitExternalTx (tx ^. #txId)) (ctx ^. logger)
    traceResult trPost $ do
        postTx nw sealedTx
        pure tx
  where
    tl = ctx ^. transactionLayer @k @ktype
    nw = ctx ^. networkLayer

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
        , HasTransactionLayer k 'CredFromKeyK ctx
        )
    => ctx
    -> Cardano.AnyCardanoEra
    -> WalletId
    -> Withdrawal
    -> ExceptT ErrCreateMigrationPlan IO MigrationPlan
createMigrationPlan ctx era wid rewardWithdrawal = do
    (wallet, _, pending) <- withExceptT ErrCreateMigrationPlanNoSuchWallet $
        readWallet @ctx @s @k ctx wid
    pp <- liftIO $ currentProtocolParameters nl
    let txConstraints = constraints tl era pp
    let utxo = availableUTxO @s pending wallet
    pure
        $ Migration.createPlan txConstraints utxo
        $ Migration.RewardWithdrawal
        $ withdrawalToCoin rewardWithdrawal
  where
    nl = ctx ^. networkLayer
    tl = ctx ^. transactionLayer @k @'CredFromKeyK

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
-- with the hash algorithm SHA512 which is realized in encryptPassphrase
attachPrivateKeyFromPwdScheme
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, (PassphraseScheme, Passphrase "user"))
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKeyFromPwdScheme ctx wid (xprv, (scheme, pwd)) = db & \_ -> do
    hpwd <- liftIO $ encryptPassphrase' scheme pwd
    -- NOTE Only new wallets are constructed through this function, so the
    -- passphrase is encrypted with the new scheme (i.e. PBKDF2)
    --
    -- We do an extra sanity check after having encrypted the passphrase: we
    -- tried to avoid some programmer mistakes with the phantom types on
    -- Passphrase, but it's still possible that someone would inadvertently call
    -- this function with a 'Passphrase' that wasn't prepared for
    -- 'EncryptWithPBKDF2', if this happens, this is a programmer error and we
    -- must fail hard for this would have dramatic effects later on.
    case checkPassphrase scheme pwd hpwd of
        Right () -> attachPrivateKey db wid (xprv, hpwd) scheme
        Left{} -> fail
            "Awe crap! The passphrase given to 'attachPrivateKeyFromPwd' wasn't \
            \rightfully constructed. This is a programmer error. Look for calls \
            \to this function and make sure that the given Passphrase wasn't not \
            \prepared using 'EncryptWithScrypt'!"
  where
    db = ctx ^. dbLayer @IO @s @k

attachPrivateKeyFromPwd
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, Passphrase "user")
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKeyFromPwd ctx wid (xprv, pwd) =
    attachPrivateKeyFromPwdScheme @ctx @s @k ctx wid
       (xprv, (currentPassphraseScheme, pwd))

-- | The hash here is the output of Scrypt function with the following parameters:
-- - logN = 14
-- - r = 8
-- - p = 1
-- - bytesNumber = 64
attachPrivateKeyFromPwdHashByron
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, PassphraseHash)
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKeyFromPwdHashByron ctx wid (xprv, hpwd) = db & \_ ->
    -- NOTE Only legacy wallets are imported through this function, passphrase
    -- were encrypted with the legacy scheme (Scrypt).
    attachPrivateKey db wid (xprv, hpwd) EncryptWithScrypt
  where
    db = ctx ^. dbLayer @IO @s @k

attachPrivateKeyFromPwdHashShelley
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> (k 'RootK XPrv, PassphraseHash)
    -> ExceptT ErrNoSuchWallet IO ()
attachPrivateKeyFromPwdHashShelley ctx wid (xprv, hpwd) = db & \_ ->
    attachPrivateKey db wid (xprv, hpwd) currentPassphraseScheme
  where
    db = ctx ^. dbLayer @IO @s @k

attachPrivateKey
    :: DBLayer IO s k
    -> WalletId
    -> (k 'RootK XPrv, PassphraseHash)
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
        putWalletMeta wid (modify $ fst meta)

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
    -> Passphrase "user"
    -> (ErrWithRootKey -> e)
    -> (k 'RootK XPrv -> PassphraseScheme -> ExceptT e IO a)
    -> ExceptT e IO a
withRootKey ctx wid pwd embed action = db & \DBLayer{..} -> do
    (xprv, scheme) <- withExceptT embed $ mapExceptT atomically $ do
        mScheme <- (>>= (fmap passphraseScheme . passphraseInfo)) <$>
            lift (fmap fst <$> readWalletMeta wid)
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
    -> Passphrase "user"
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
    -> ExceptT ErrDerivePublicKey IO (k (AddressCredential k) XPub)
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

writePolicyPublicKey
    :: forall ctx s (n :: NetworkDiscriminant).
        ( HasDBLayer IO s ShelleyKey ctx
        , s ~ SeqState n ShelleyKey
        )
    => ctx
    -> WalletId
    -> Passphrase "user"
    -> ExceptT ErrWritePolicyPublicKey IO (ShelleyKey 'PolicyK XPub)
writePolicyPublicKey ctx wid pwd = db & \DBLayer{..} -> do
    cp <- mapExceptT atomically
        $ withExceptT ErrWritePolicyPublicKeyNoSuchWallet
        $ withNoSuchWallet wid
        $ readCheckpoint wid

    let (SeqPrologue seqState) = getPrologue $ getState cp

    policyXPub <- withRootKey
        @ctx @s @ShelleyKey ctx wid pwd ErrWritePolicyPublicKeyWithRootKey $
        \rootK scheme -> do
            let encPwd = preparePassphrase scheme pwd
            let xprv = derivePolicyPrivateKey encPwd (getRawKey rootK) minBound
            pure $ liftRawKey $ toXPub xprv

    let seqState' = seqState & #policyXPub .~ Just policyXPub
    ExceptT $ atomically $ modifyDBMaybe walletsDB $
        adjustNoSuchWallet wid ErrWritePolicyPublicKeyNoSuchWallet $
        \_ -> Right ( [ReplacePrologue $ SeqPrologue seqState'], () )

    pure policyXPub
  where
    db = ctx ^. dbLayer @IO @s @ShelleyKey

-- | Retrieve any public account key of a wallet.
getAccountPublicKeyAtIndex
    :: forall ctx s k.
        ( HasDBLayer IO s k ctx
        , WalletKey k
        , GetPurpose k
        )
    => ctx
    -> WalletId
    -> Passphrase "user"
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
    -> ExceptT (ErrInvalidDerivationIndex 'Soft 'CredFromKeyK) m (Index 'Soft whatever)
guardSoftIndex ix =
    if ix > DerivationIndex (getIndex @'Soft maxBound) ||
       ix < DerivationIndex (getIndex @'Soft minBound)
    then throwE $ ErrIndexOutOfBound minBound maxBound ix
    else pure (Index $ getDerivationIndex ix)

guardHardIndex
    :: Monad m
    => DerivationIndex
    -> ExceptT (ErrInvalidDerivationIndex 'Hardened level) m (Index 'Hardened whatever)
guardHardIndex ix =
    if ix > DerivationIndex (getIndex @'Hardened maxBound) ||
       ix < DerivationIndex (getIndex @'Hardened minBound)
    then throwE $ ErrIndexOutOfBound minBound maxBound ix
    else pure (Index $ getDerivationIndex ix)

updateCosigner
    :: forall ctx s k n.
        ( s ~ SharedState n k
        , k ~ SharedKey
        , Shared.SupportsDiscovery n k
        , WalletKey k
        , HasDBLayer IO s k ctx
        )
    => ctx
    -> WalletId
    -> k 'AccountK XPub
    -> Cosigner
    -> CredentialType
    -> ExceptT ErrAddCosignerKey IO ()
updateCosigner ctx wid cosignerXPub cosigner cred = db & \DBLayer{..} -> do
    cp <- withExceptT ErrAddCosignerKeyNoSuchWallet
        $ mapExceptT atomically
        $ withNoSuchWallet wid
        $ readCheckpoint wid
    ExceptT $ atomically $ modifyDBMaybe walletsDB $
        adjustNoSuchWallet wid ErrAddCosignerKeyNoSuchWallet
            (updateCosigner' cp)
  where
    db = ctx ^. dbLayer @_ @s @k
    updateCosigner' cp wallet =
        case addCosignerAccXPub (cosigner, cosignerXPub) cred s0 of
            Left err -> Left $ ErrAddCosignerKey err
            Right s1 -> case ready s1 of
                Shared.Pending ->
                    Right (prologueUpdate s1, ())
                Shared.Active _ ->
                  Right (prologueUpdate s1 ++ discoveriesUpdate s1, ())
      where
        s0 = getState $ getLatest wallet
        prologueUpdate s =
            [ReplacePrologue $ getPrologue s]
        wc@(WS.WalletCheckpoint bh utxo' _) = snd $ fromWallet cp
        discoveriesUpdate s =
            [ UpdateCheckpoints
              [ PutCheckpoint (getSlot wc)
                (WS.WalletCheckpoint bh utxo' (getDiscoveries s))
              ]
            ]

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
            (Just dT, Just (ix,_)) ->
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
    | ErrSignMetadataWithInvalidIndex (ErrInvalidDerivationIndex 'Soft 'CredFromKeyK)
        -- ^ User provided a derivation index outside of the 'Soft' domain
    deriving (Eq, Show)

data ErrDerivePublicKey
    = ErrDerivePublicKeyNoSuchWallet ErrNoSuchWallet
        -- ^ The wallet doesn't exist?
    | ErrDerivePublicKeyInvalidIndex (ErrInvalidDerivationIndex 'Soft 'CredFromKeyK)
        -- ^ User provided a derivation index outside of the 'Soft' domain
    deriving (Eq, Show)

data ErrAddCosignerKey
    = ErrAddCosignerKeyNoSuchWallet ErrNoSuchWallet
        -- ^ The shared wallet doesn't exist?
    | ErrAddCosignerKey ErrAddCosigner
        -- ^ Error adding this co-signer to the shared wallet.
    | ErrAddCosignerKeyWalletMetadataNotFound
        -- ^ No meta was found.
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
    | ErrBalanceTxMaxSizeLimitExceeded
    | ErrBalanceTxExistingCollateral
    | ErrBalanceTxExistingTotalCollateral
    | ErrBalanceTxExistingReturnCollateral
    | ErrBalanceTxConflictingNetworks
    | ErrBalanceTxAssignRedeemers ErrAssignRedeemers
    | ErrBalanceTxInternalError ErrBalanceTxInternalError
    | ErrBalanceTxInputResolutionConflicts (NonEmpty (TxOut, TxOut))
    | ErrBalanceTxUnresolvedInputs (NonEmpty TxIn)
    | ErrOldEraNotSupported Cardano.AnyCardanoEra
    deriving (Show, Eq)

data ErrBalanceTxInternalError
    = ErrUnderestimatedFee Coin SealedTx
    | ErrFailedBalancing Cardano.Value
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
    | ErrConstructTxWrongMintingBurningTemplate
    | ErrConstructTxAssetNameTooLong
    | ErrConstructTxMintOrBurnAssetQuantityOutOfBounds
    | ErrConstructTxWrongValidityBounds
    | ErrConstructTxValidityIntervalNotWithinScriptTimelock
    | ErrConstructTxSharedWalletIncomplete
    deriving (Show, Eq)

-- | Errors that can occur when getting policy id.
data ErrGetPolicyId
    = ErrGetPolicyIdReadPolicyPubliKey ErrReadPolicyPublicKey
    | ErrGetPolicyIdWrongMintingBurningTemplate
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
    | ErrWithRootKeyWrongMnemonic WalletId
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
    = ErrSelectAssetsPrepareOutputsError
        (SelectionOutputError WalletSelectionContext)
    | ErrSelectAssetsNoSuchWallet ErrNoSuchWallet
    | ErrSelectAssetsAlreadyWithdrawing Tx
    | ErrSelectAssetsSelectionError (SelectionError WalletSelectionContext)
    deriving (Generic, Eq, Show)

data ErrStakePoolDelegation
    = ErrStakePoolDelegationNoSuchWallet ErrNoSuchWallet
    | ErrStakePoolJoin ErrCannotJoin
    | ErrStakePoolQuit ErrCannotQuit
    deriving (Show)

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
    = ErrIndexAlreadyExists (Index 'Hardened 'CredFromKeyK)
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

data ErrWithdrawalNotBeneficial
    = ErrWithdrawalNotBeneficial
    deriving (Generic, Eq, Show)

data ErrReadPolicyPublicKey
    = ErrReadPolicyPublicKeyNotAShelleyWallet
    | ErrReadPolicyPublicKeyNoSuchWallet ErrNoSuchWallet
    | ErrReadPolicyPublicKeyAbsent
    deriving (Generic, Eq, Show)

data ErrWritePolicyPublicKey
    = ErrWritePolicyPublicKeyNoSuchWallet ErrNoSuchWallet
    | ErrWritePolicyPublicKeyWithRootKey ErrWithRootKey
    deriving (Generic, Eq, Show)

-- | This exception type should gradually replace all cases of `ExceptT Err*`
-- as there is no point in tracking errors at the type level
-- which represent exceptional cases and are always propagated to clients.
data WalletException
    = ExceptionSignMetadataWith ErrSignMetadataWith
    | ExceptionDerivePublicKey ErrDerivePublicKey
    | ExceptionAddCosignerKey ErrAddCosignerKey
    | ExceptionConstructSharedWallet ErrConstructSharedWallet
    | ExceptionReadAccountPublicKey ErrReadAccountPublicKey
    | ExceptionListUTxOStatistics ErrListUTxOStatistics
    | ExceptionSignPayment ErrSignPayment
    | ExceptionBalanceTx ErrBalanceTx
    | ExceptionBalanceTxInternalError ErrBalanceTxInternalError
    | ExceptionSubmitTransaction ErrSubmitTransaction
    | ExceptionConstructTx ErrConstructTx
    | ExceptionGetPolicyId ErrGetPolicyId
    | ExceptionWitnessTx ErrWitnessTx
    | ExceptionDecodeTx ErrDecodeTx
    | ExceptionSubmitTx ErrSubmitTx
    | ExceptionUpdatePassphrase ErrUpdatePassphrase
    | ExceptionWithRootKey ErrWithRootKey
    | ExceptionListTransactions ErrListTransactions
    | ExceptionGetTransaction ErrGetTransaction
    | ExceptionStartTimeLaterThanEndTime ErrStartTimeLaterThanEndTime
    | ExceptionCreateMigrationPlan ErrCreateMigrationPlan
    | ExceptionSelectAssets ErrSelectAssets
    | ExceptionStakePoolDelegation ErrStakePoolDelegation
    | ExceptionFetchRewards ErrFetchRewards
    | ExceptionWalletNotResponding ErrWalletNotResponding
    | ExceptionCreateRandomAddress ErrCreateRandomAddress
    | ExceptionImportRandomAddress ErrImportRandomAddress
    | ExceptionNotASequentialWallet ErrNotASequentialWallet
    | ExceptionReadRewardAccount ErrReadRewardAccount
    | ExceptionWithdrawalNotBeneficial ErrWithdrawalNotBeneficial
    | ExceptionReadPolicyPublicKey ErrReadPolicyPublicKey
    | ExceptionWritePolicyPublicKey ErrWritePolicyPublicKey
    | forall level. ExceptionSoftDerivationIndex
        (ErrInvalidDerivationIndex 'Soft level)
    | forall level. ExceptionHardenedDerivationIndex
        (ErrInvalidDerivationIndex 'Hardened level)

deriving instance (Show WalletException)

instance Exception WalletException

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
    | MsgStoringCBOR TxCBOR
    deriving (Show, Eq)

-- | Helper wrapper type for the sake of logging.
data BuildableInAnyEra tx = forall era.
    ( Eq (tx era)
    , Show (tx era)
    , Buildable (tx era)
    ) => BuildableInAnyEra (Cardano.CardanoEra era) (tx era)

instance Show (BuildableInAnyEra a) where
    show (BuildableInAnyEra _ a) = show a

instance Eq (BuildableInAnyEra a) where
    BuildableInAnyEra era1 thing1 == BuildableInAnyEra era2 thing2 =
        case testEquality era1 era2 of
            Just Refl -> thing1 == thing2
            Nothing -> False

instance Buildable (BuildableInAnyEra a) where
    build (BuildableInAnyEra _ x) = build x

-- | Log messages from API server actions running in a wallet worker context.
data WalletLog
    = MsgSelectionStart Int [TxOut]
    | MsgSelectionForBalancingStart Int (BuildableInAnyEra PartialTx)
    | MsgSelectionError (SelectionError WalletSelectionContext)
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
        MsgStoringCBOR txCBOR ->
            "store new cbor for "
                <> toText txCBOR

instance ToText WalletLog where
    toText = \case
        MsgSelectionStart utxoSize recipients ->
            "Starting coin selection " <>
            "|utxo| = "+|utxoSize|+" " <>
            "#recipients = "+|length recipients|+""
        MsgSelectionForBalancingStart utxoSize partialTx ->
            "Starting coin selection for balancing " <>
            "|utxo| = "+|utxoSize|+" " <>
            "partialTx = "+|partialTx|+""
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
        MsgStoringCBOR {} -> Debug

instance HasPrivacyAnnotation WalletLog
instance HasSeverityAnnotation WalletLog where
    getSeverityAnnotation = \case
        MsgSelectionStart{} -> Debug
        MsgSelectionForBalancingStart{} -> Debug
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

-- | Convert a 'Cardano.Value' into a positive and negative component. Useful
-- to convert the potentially negative balance of a partial tx into
-- TokenBundles.
posAndNegFromCardanoValue
    :: Cardano.Value
    -> (TokenBundle.TokenBundle, TokenBundle.TokenBundle)
posAndNegFromCardanoValue = foldMap go . Cardano.valueToList
  where
    go :: (Cardano.AssetId, Cardano.Quantity)
       -> (TokenBundle.TokenBundle, TokenBundle.TokenBundle)
    go (Cardano.AdaAssetId, q) = partition q $
        TokenBundle.fromCoin . Coin.fromNatural
    go (Cardano.AssetId policy name, q) = partition q $ \n ->
        TokenBundle.fromFlatList (Coin 0)
            [ ( TokenBundle.AssetId (mkPolicyId policy) (mkTokenName name)
              , TokenQuantity n
              )
            ]

    -- | Convert a 'Cardano.Quantity' to a 'TokenBundle' using the supplied
    -- function. The result is stored in 'fst' for positive quantities, and
    -- 'snd' for negative quantities.
    partition
        :: Cardano.Quantity
        -> (Natural -> TokenBundle.TokenBundle)
        -> (TokenBundle.TokenBundle, TokenBundle.TokenBundle)
    partition (Cardano.Quantity i) f
        | Just n <- maybeIntegerToNatural      i  = (f n, mempty)
        | Just n <- maybeIntegerToNatural (abs i) = (mempty, f n)
        | otherwise = (mempty, mempty)

    maybeIntegerToNatural = intCastMaybe @Integer @Natural

    mkPolicyId = UnsafeTokenPolicyId . Hash . Cardano.serialiseToRawBytes
    mkTokenName = UnsafeTokenName . Cardano.serialiseToRawBytes
