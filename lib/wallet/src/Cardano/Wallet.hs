{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- suppress false warning
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

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
    , readPrivateKey
    , restoreWallet
    , updateWallet
    , updateWalletPassphraseWithOldPassphrase
    , updateWalletPassphraseWithMnemonic
    , walletSyncProgress
    , fetchRewardBalance
    , manageRewardBalance
    , manageSharedRewardBalance
    , rollbackBlocks
    , checkWalletIntegrity
    , mkExternalWithdrawal
    , mkSelfWithdrawal
    , mkSelfWithdrawalShared
    , shelleyOnlyMkSelfWithdrawal
    , sharedOnlyReadRewardAccount
    , readRewardAccount
    , shelleyOnlyReadRewardAccount
    , someRewardAccount
    , readPolicyPublicKey
    , writePolicyPublicKey
    , ErrWalletAlreadyExists (..)
    , ErrNoSuchWallet (..)
    , ErrWalletNotInitialized (..)
    , ErrUpdatePassphrase (..)
    , ErrFetchRewards (..)
    , ErrCheckWalletIntegrity (..)
    , ErrWalletNotResponding (..)
    , ErrReadRewardAccount (..)
    , ErrReadPolicyPublicKey (..)
    , ErrWritePolicyPublicKey (..)
    , ErrGetPolicyId (..)
    , readWalletMeta
    , isStakeKeyRegistered
    , putDelegationCertificate
    , readDelegation
    , getCurrentEpochSlotting

    -- * Shared Wallet
    , updateCosigner
    , ErrAddCosignerKey (..)
    , ErrConstructSharedWallet (..)
    , normalizeSharedAddress
    , constructUnbalancedSharedTransaction

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

    -- ** Payment
    , transactionExpirySlot
    , buildCoinSelectionForTransaction
    , CoinSelection (..)
    , readWalletUTxO
    , defaultChangeAddressGen
    , dummyChangeAddressGen
    , assignChangeAddressesAndUpdateDb
    , assignChangeAddressesWithoutDbUpdate
    , selectionToUnsignedTx
    , readNodeTipStateForTxWrite
    , buildSignSubmitTransaction
    , buildTransaction
    , buildTransactionPure
    , buildAndSignTransactionPure
    , buildAndSignTransaction
    , BuiltTx (..)
    , signTransaction
    , constructTransaction
    , constructTxMeta
    , ErrSelectAssets(..)
    , ErrSignPayment (..)
    , ErrNotASequentialWallet (..)
    , ErrWithdrawalNotBeneficial (..)
    , ErrConstructTx (..)
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
    , Fee (..)
    , Percentile (..)
    , DelegationFee (..)
    , delegationFee
    , getStakeKeyDeposit
    , transactionFee
    , calculateFeePercentiles
    , padFeePercentiles
    , calcMinimumCoinValues

    -- ** Transaction
    , forgetTx
    , listTransactions
    , listAssets
    , getTransaction
    , submitExternalTx
    , submitTx
    , readLocalTxSubmissionPending
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
    , ErrWriteTxEra (..)

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
    , utxoAssumptionsForWallet
    , txWitnessTagForKey

    -- * Logging
    , WalletWorkerLog (..)
    , WalletFollowLog (..)
    , WalletLog (..)
    , TxSubmitLog (..)
    , putPrivateKey

    -- * Internal
    , putWalletCheckpoints
    ) where

import Prelude hiding
    ( log )

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Address.Script
    ( Cosigner (..), KeyHash )
import Cardano.Api
    ( serialiseToCBOR )
import Cardano.Api.Extra
    ( inAnyCardanoEra )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..), nullTracer )
import Cardano.Crypto.Wallet
    ( toXPub )
import Cardano.Mnemonic
    ( SomeMnemonic )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Tx.Balance.Internal.CoinSelection
    ( Selection
    , SelectionBalanceError (..)
    , SelectionError (..)
    , SelectionOf (..)
    , UnableToConstructChangeError (..)
    )
import Cardano.Wallet.Address.Book
    ( AddressBookIso, Prologue (..), getDiscoveries, getPrologue )
import Cardano.Wallet.Address.Derivation
    ( DelegationAddress (..)
    , Depth (..)
    , DerivationIndex (..)
    , DerivationPrefix (..)
    , DerivationType (..)
    , HardDerivation (..)
    , Index (..)
    , MkKeyFingerprint (..)
    , PaymentAddress (..)
    , Role (..)
    , SoftDerivation (..)
    , ToRewardAccount (..)
    , deriveRewardAccount
    , liftDelegationAddressS
    , liftIndex
    , stakeDerivationPath
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Address.Derivation.MintBurn
    ( derivePolicyPrivateKey, policyDerivationPath )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..), replaceCosignersWithVerKeys )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..), deriveAccountPrivateKeyShelley )
import Cardano.Wallet.Address.Discovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , GetAccount (..)
    , GetPurpose (..)
    , IsOurs (..)
    , KnownAddresses (..)
    , MaybeLight (..)
    )
import Cardano.Wallet.Address.Discovery.Random
    ( ErrImportAddress (..), RndStateLike )
import Cardano.Wallet.Address.Discovery.Sequential
    ( SeqState (..), defaultAddressPoolGap, purposeBIP44 )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType (..)
    , ErrAddCosigner (..)
    , ErrScriptTemplate (..)
    , SharedState (..)
    , isShared
    )
import Cardano.Wallet.Address.Keys.BoundedAddressLength
    ( maxLengthAddressFor )
import Cardano.Wallet.Address.Keys.SequentialAny
    ( mkSeqStateFromRootXPrv )
import Cardano.Wallet.Address.Keys.Shared
    ( addCosignerAccXPub )
import Cardano.Wallet.Address.Keys.WalletKey
    ( AfterByron
    , afterByron
    , changePassphraseNew
    , getRawKey
    , hashVerificationKey
    , liftRawKey
    )
import Cardano.Wallet.Address.States.IsOwned
    ( isOwned )
import Cardano.Wallet.Checkpoints
    ( DeltaCheckpoints (..), extendCheckpoints, pruneCheckpoints )
import Cardano.Wallet.DB
    ( DBFresh (..)
    , DBLayer (..)
    , DBLayerParams (..)
    , ErrNoSuchTransaction (..)
    , ErrRemoveTx (..)
    , ErrWalletAlreadyExists (..)
    , ErrWalletNotInitialized (..)
    )
import Cardano.Wallet.DB.Errors
    ( ErrNoSuchWallet (..) )
import Cardano.Wallet.DB.Store.Delegations.Layer
    ( CurrentEpochSlotting, mkCurrentEpochSlotting )
import Cardano.Wallet.DB.Store.Info.Store
    ( DeltaWalletInfo (..), WalletInfo (..) )
import Cardano.Wallet.DB.Store.Submissions.Layer
    ( mkLocalTxSubmission )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( TxSubmissionsStatus )
import Cardano.Wallet.DB.WalletState
    ( DeltaWalletState
    , DeltaWalletState1 (..)
    , WalletState (..)
    , fromWallet
    , getLatest
    , getSlot
    )
import Cardano.Wallet.Flavor
    ( AllFlavors
    , CredFromOf
    , Excluding
    , FlavorOf
    , Including
    , KeyFlavorS (..)
    , KeyOf
    , NetworkOf
    , WalletFlavor (..)
    , WalletFlavorS (..)
    , WalletFlavors (..)
    , keyFlavorFromState
    , keyOfWallet
    )
import Cardano.Wallet.Logging
    ( BracketLog
    , BracketLog' (..)
    , bracketTracer
    , formatResultMsg
    , resultSeverity
    , traceResult
    )
import Cardano.Wallet.Network
    ( ChainFollowLog (..)
    , ChainFollower (..)
    , ErrPostTx (..)
    , NetworkLayer (..)
    )
import Cardano.Wallet.Primitive.BlockSummary
    ( ChainEvents )
import Cardano.Wallet.Primitive.Migration
    ( MigrationPlan (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockData (..)
    , Wallet
    , applyBlocks
    , applyOurTxToUTxO
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
    , toTimeTranslation
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
    , GenesisParameters (..)
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
    , stabilityWindowShelley
    , toSlot
    , wholeRange
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Credentials
    ( ClearCredentials, RootCredentials (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
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
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Primitive.Types.UTxOStatistics
    ( UTxOStatistics )
import Cardano.Wallet.Read.NetworkId
    ( HasSNetworkId (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Cardano.Wallet.Shelley.Compatibility
    ( fromCardanoBlock
    , fromCardanoLovelace
    , fromCardanoTxIn
    , fromCardanoTxOut
    , fromCardanoWdrls
    )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWallet )
import Cardano.Wallet.Shelley.Transaction
    ( txWitnessTagForKey )
import Cardano.Wallet.Transaction
    ( DelegationAction (..)
    , ErrCannotJoin (..)
    , ErrCannotQuit (..)
    , ErrMkTransaction (..)
    , ErrSignTx (..)
    , PreSelection (..)
    , TransactionCtx (..)
    , TransactionLayer (..)
    , TxValidityInterval
    , Withdrawal (..)
    , WitnessCountCtx (..)
    , defaultTransactionCtx
    , withdrawalToCoin
    )
import Cardano.Wallet.Transaction.Built
    ( BuiltTx (..) )
import Cardano.Wallet.TxWitnessTag
    ( TxWitnessTag (..) )
import Cardano.Wallet.Write.Tx
    ( recentEra )
import Cardano.Wallet.Write.Tx.Balance
    ( BalanceTxLog (..)
    , ChangeAddressGen (..)
    , ErrBalanceTx (..)
    , ErrBalanceTxInternalError (..)
    , ErrSelectAssets (..)
    , PartialTx (..)
    , UTxOAssumptions (..)
    , assignChangeAddresses
    , balanceTransaction
    , constructUTxOIndex
    )
import Cardano.Wallet.Write.Tx.SizeEstimation
    ( getFeePerByteFromWalletPParams, _txRewardWithdrawalCost )
import Cardano.Wallet.Write.Tx.TimeTranslation
    ( TimeTranslation )
import Control.Arrow
    ( first, (>>>) )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, forM_, join, replicateM, unless, when, (<=<) )
import Control.Monad.Class.MonadTime
    ( MonadTime (..) )
import Control.Monad.IO.Unlift
    ( MonadIO (..), MonadUnliftIO )
import Control.Monad.Random.Strict
    ( Rand, StdGen, evalRand, initStdGen )
import Control.Monad.State.Class
    ( MonadState (get, put) )
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
import Control.Monad.Trans.State
    ( StateT, evalState, runStateT, state )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Crypto.Hash
    ( Blake2b_256, hash )
import Data.ByteString
    ( ByteString )
import Data.DBVar
    ( DBVar, readDBVar )
import Data.Delta
    ( Replace (..) )
import Data.Delta.Update
    ( onDBVar, update )
import Data.Either
    ( partitionEithers )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Function
    ( (&) )
import Data.Functor
    ( ($>), (<&>) )
import Data.Functor.Contravariant
    ( (>$<) )
import Data.Generics.Internal.VL.Lens
    ( Lens', view, (.~), (^.) )
import Data.Generics.Labels
    ()
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.IntCast
    ( intCast )
import Data.List
    ( foldl' )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Maybe
    ( fromJust, fromMaybe, isJust, mapMaybe, maybeToList )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock
    ( DiffTime, NominalDiffTime, UTCTime )
import Data.Void
    ( Void )
import Data.Word
    ( Word64 )
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
import GHC.Num
    ( Natural )
import GHC.TypeNats
    ( Nat )
import Ouroboros.Consensus.Util.IOLike
    ( MonadMonotonicTime, Time, diffTime, getMonotonicTime )
import Statistics.Quantile
    ( medianUnbiased, quantiles )
import UnliftIO.Exception
    ( Exception, catch, evaluate, throwIO, try )
import UnliftIO.MVar
    ( modifyMVar_, newMVar )

import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shelley as CAShelley
import qualified Cardano.Api as Cardano
import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Slotting.Slot as Slot
import qualified Cardano.Wallet.Address.Discovery.Random as Rnd
import qualified Cardano.Wallet.Address.Discovery.Sequential as Seq
import qualified Cardano.Wallet.Address.Discovery.Shared as Shared
import qualified Cardano.Wallet.Checkpoints.Policy as CP
import qualified Cardano.Wallet.DB.Store.Delegations.Layer as Dlgs
import qualified Cardano.Wallet.DB.Store.Submissions.Layer as Submissions
import qualified Cardano.Wallet.DB.WalletState as WalletState
import qualified Cardano.Wallet.DB.WalletState as WS
import qualified Cardano.Wallet.Primitive.Migration as Migration
import qualified Cardano.Wallet.Primitive.Slotting as Slotting
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Cardano.Wallet.Primitive.Types.UTxOStatistics as UTxOStatistics
import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Write.ProtocolParameters as Write
import qualified Cardano.Wallet.Write.Tx as Write
import qualified Cardano.Wallet.Write.Tx.SizeEstimation as Write
import qualified Data.ByteArray as BA
import qualified Data.Delta.Update as Delta
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
-- - @db = ctx ^. dbLayer for the 'DBLayer'.
-- - @tr = ctx ^. logger@ for the Logger.
-- - @nw = ctx ^. networkLayer@ for the 'NetworkLayer'.
-- - @tl = ctx ^. transactionLayer \\@k@ for the 'TransactionLayer'.
-- - @re = ctx ^. workerRegistry@ for the 'WorkerRegistry'.
--
-- __TroubleShooting__
--
-- @
-- • Overlapping instances for HasType (DBLayer IO s) ctx
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

data WalletLayer m s =
    WalletLayer
        { logger_ :: Tracer m WalletWorkerLog
        , genesisData_ :: (Block, NetworkParameters)
        , networkLayer_ :: NetworkLayer m Read.Block
        , transactionLayer_ :: TransactionLayer (KeyOf s) (CredFromOf s) SealedTx
        , dbLayer_ :: DBLayer m s
        }
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

type HasGenesisData = HasType (Block, NetworkParameters)

type HasLogger m msg = HasType (Tracer m msg)

-- | This module is only interested in one block-, and tx-type. This constraint
-- hides that choice, for some ease of use.
type HasNetworkLayer m = HasType (NetworkLayer m Read.Block)

type HasTransactionLayer k ktype = HasType (TransactionLayer k ktype SealedTx)

dbLayer :: Lens' (WalletLayer m s) (DBLayer m s)
dbLayer = #dbLayer_

genesisData ::
    forall ctx. HasGenesisData ctx => Lens' ctx (Block, NetworkParameters)
genesisData = typed @(Block, NetworkParameters)

logger :: forall m msg ctx. HasLogger m msg ctx => Lens' ctx (Tracer m msg)
logger = typed @(Tracer m msg)

networkLayer ::
    forall m ctx. (HasNetworkLayer m ctx) => Lens' ctx (NetworkLayer m Read.Block)
networkLayer = typed @(NetworkLayer m Read.Block)

transactionLayer ::
    forall k ktype ctx. (HasTransactionLayer k ktype ctx)
    => Lens' ctx (TransactionLayer k ktype SealedTx)
transactionLayer = typed @(TransactionLayer k ktype SealedTx)

-- | Convenience to apply an 'Update' to the 'WalletState' via the 'DBLayer'.
onWalletState
    :: WalletLayer m s
    -> Delta.Update (WalletState.DeltaWalletState s) r
    -> m r
onWalletState ctx update' = db & \DBLayer{..} ->
    atomically $ Delta.onDBVar walletState update'
  where
    db = ctx ^. dbLayer

{-------------------------------------------------------------------------------
                                   Wallet
-------------------------------------------------------------------------------}

-- | Initialise and store a new wallet, returning its ID.
createWallet
    :: forall m s
     . ( MonadUnliftIO m
       , MonadTime m
       , IsOurs s Address
       , IsOurs s RewardAccount
       )
    => (Block, NetworkParameters)
    -> DBFresh m s
    -> WalletId
    -> WalletName
    -> s
    -> ExceptT ErrWalletAlreadyExists m (DBLayer m s)
createWallet
    (block0, NetworkParameters gp _sp _pp)
    DBFresh{bootDBLayer}
    wid
    wname
    s =
        do
            let (hist, cp) = initWallet block0 s
            now <- lift getCurrentTime
            let meta =
                    WalletMetadata
                        { name = wname
                        , creationTime = now
                        , passphraseInfo = Nothing
                        }
            withExceptT (const $ ErrWalletAlreadyExists wid)
                $ bootDBLayer
                $ DBLayerParams cp meta hist gp

-- | Initialise and store a new legacy Icarus wallet. These wallets are
-- intrinsically sequential, but, in the incentivized testnet, we only have
-- access to the a snapshot of the MainNet.
--
-- To work-around this, we scan the genesis block with an arbitrary big gap and
-- resort to a default gap afterwards.
createIcarusWallet
    :: forall s k n
     . ( PaymentAddress k 'CredFromKeyK
       , k ~ IcarusKey
       , s ~ SeqState n k
       , HasSNetworkId n
       )
    => (Block, NetworkParameters)
    -> DBFresh IO s
    -> WalletId
    -> WalletName
    -> ClearCredentials k
    -> ExceptT ErrWalletAlreadyExists IO (DBLayer IO s)
createIcarusWallet
    (block0, NetworkParameters gp _sp _pp)
    DBFresh{bootDBLayer}
    wid
    wname
    credentials = do
        let g = defaultAddressPoolGap
            s = mkSeqStateFromRootXPrv @n IcarusKeyS credentials purposeBIP44 g
            (hist, cp) = initWallet block0 s
        now <- lift getCurrentTime
        let meta =
                WalletMetadata
                    { name = wname
                    , creationTime = now
                    , passphraseInfo = Nothing
                    }
        withExceptT (const $ ErrWalletAlreadyExists wid)
            $ bootDBLayer
            $ DBLayerParams cp meta hist gp


-- | Check whether a wallet is in good shape when restarting a worker.
checkWalletIntegrity :: DBLayer IO s -> GenesisParameters -> IO ()
checkWalletIntegrity db gp = db & \DBLayer{..} -> do
    gp' <- atomically readGenesisParameters >>= do
        maybe (throwIO ErrCheckWalletIntegrityNoGenesisParameters) pure
    when ( (gp ^. #getGenesisBlockHash /= gp' ^. #getGenesisBlockHash) ||
           (gp ^. #getGenesisBlockDate /= gp' ^. #getGenesisBlockDate) )
        (throwIO $ ErrCheckIntegrityDifferentGenesis
            (getGenesisBlockHash gp) (getGenesisBlockHash gp'))

readWalletMeta :: Functor f => DBVar f (DeltaWalletState s) -> f WalletMetadata
readWalletMeta walletState = walletMeta . info <$> readDBVar walletState

readPrivateKey
    :: Functor stm
    => DBVar stm (DeltaWalletState s)
    -> stm (Maybe (KeyOf s 'RootK XPrv, PassphraseHash))
readPrivateKey walletState =
    readDBVar walletState <&> \mc -> do
        (RootCredentials pk h) <- credentials mc
        pure (pk, h)

putPrivateKey
    :: Monad m
    => DBVar m (DeltaWalletState s)
    -> (KeyOf s 'RootK XPrv, PassphraseHash)
    -> m ()
putPrivateKey walletState (pk, hpw) = onDBVar walletState $ update $ \_ ->
    [UpdateCredentials $ Replace $ Just $ RootCredentials pk hpw]

readDelegation
    :: Monad stm
    => DBVar stm (DeltaWalletState s)
    -> stm (CurrentEpochSlotting -> WalletDelegation)
readDelegation walletState = do
    dels <- view #delegations <$> readDBVar walletState
    pure $ \dsarg -> Dlgs.readDelegation dsarg dels

-- | Return information about the current epoch.
--
-- In the event that wall clock time is too far ahead of the node,
-- we return the epoch of the node tip.
getCurrentEpochSlotting
    :: NetworkLayer IO block
    -> IO CurrentEpochSlotting
getCurrentEpochSlotting nl = do
    epoch <- getCurrentEpoch
    mkCurrentEpochSlotting ti epoch
  where
    ti = Slotting.expectAndThrowFailures $ timeInterpreter nl

    getCurrentEpoch =
        currentEpochFromWallClock >>= \case
            Right a -> pure a
            Left _ -> currentEpochFromNodeTip

    currentEpochFromNodeTip :: IO W.EpochNo
    currentEpochFromNodeTip = do
        tip <- currentNodeTip nl
        interpretQuery ti $ Slotting.epochOf $ tip ^. #slotNo

    currentEpochFromWallClock :: IO (Either PastHorizonException W.EpochNo)
    currentEpochFromWallClock =
        try $ Slotting.currentEpoch ti

-- | Retrieve the wallet state for the wallet with the given ID.
readWallet
    :: WalletLayer IO s
    -> IO (Wallet s, (WalletMetadata, WalletDelegation), Set Tx)
readWallet ctx = do
    currentEpochSlotting <- getCurrentEpochSlotting nl
    db & \DBLayer{..} -> atomically $ do
        cp <- readCheckpoint
        meta <- readWalletMeta walletState
        dele <- readDelegation walletState
        pending <-
            readTransactions
                Nothing
                Descending
                wholeRange
                (Just Pending)
                Nothing
                Nothing
        pure (cp, (meta, dele currentEpochSlotting), Set.fromList (fromTransactionInfo <$> pending))
  where
    db = ctx ^. dbLayer
    nl = ctx ^. networkLayer

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

putWalletMeta
    :: Monad stm
    => DBVar stm (DeltaWalletState s)
    -> WalletMetadata
    -> stm ()
putWalletMeta walletState wm = onDBVar walletState $ update $ \_ ->
    [UpdateInfo $ UpdateWalletMetadata wm]

-- | Update a wallet's metadata with the given update function.
updateWallet
    :: WalletLayer IO s
    -> (WalletMetadata -> WalletMetadata)
    -> IO ()
updateWallet ctx f = onWalletState ctx $ update $ \s ->
    [ UpdateInfo
        $ UpdateWalletMetadata
        $ f
        $ s ^. #info . #walletMeta
    ]

-- | Change a wallet's passphrase to the given passphrase.
updateWalletPassphraseWithOldPassphrase
    :: forall s
     . WalletFlavorS s
    -> WalletLayer IO s
    -> WalletId
    -> (Passphrase "user", Passphrase "user")
    -> ExceptT ErrUpdatePassphrase IO ()
updateWalletPassphraseWithOldPassphrase wF ctx wid (old, new) =
    withRootKey db wid old ErrUpdatePassphraseWithRootKey
        $ \xprv scheme -> do
            -- IMPORTANT NOTE:
            -- This use 'EncryptWithPBKDF2', regardless of the passphrase
            -- current scheme, we'll re-encrypt it using the current scheme,
            -- always.
            let new' = (currentPassphraseScheme, new)
                xprv' = changePassphraseNew (keyOfWallet wF)
                    (scheme, old) new' xprv
            lift $ attachPrivateKeyFromPwdScheme ctx (xprv', new')
  where
    db = ctx ^. typed

updateWalletPassphraseWithMnemonic
    :: WalletLayer IO s
    -> (KeyOf s 'RootK XPrv, Passphrase "user")
    -> IO ()
updateWalletPassphraseWithMnemonic ctx (xprv, new) =
    attachPrivateKeyFromPwdScheme ctx
        (xprv, (currentPassphraseScheme , new))

getWalletUtxoSnapshot
    :: WalletLayer IO s
    -> IO [(TokenBundle, Coin)]
getWalletUtxoSnapshot ctx = do
    (wallet, _, pending) <- readWallet ctx
    pp <- currentProtocolParameters nl
    let txOuts = availableUTxO pending wallet
            & unUTxO
            & F.toList
    pure $ first (view #tokens) . pairTxOutWithMinAdaQuantity pp <$> txOuts
  where
    nl = ctx ^. networkLayer
    tl = transactionLayer_ ctx

    pairTxOutWithMinAdaQuantity
        :: ProtocolParameters
        -> TxOut
        -> (TxOut, Coin)
    pairTxOutWithMinAdaQuantity pp out =
        (out, computeMinAdaQuantity out)
      where
        computeMinAdaQuantity :: TxOut -> Coin
        computeMinAdaQuantity (TxOut addr bundle) =
            view #txOutputMinimumAdaQuantity
                (Write.txConstraints pp (transactionWitnessTag tl))
                (addr)
                (view #tokens bundle)

-- | List the wallet's UTxO statistics.
listUtxoStatistics
    :: WalletLayer IO s
    -> IO UTxOStatistics
listUtxoStatistics ctx = do
    (wal, _, pending) <- readWallet ctx
    let utxo = availableUTxO pending wal
    pure $ UTxOStatistics.compute utxo

-- | Restore a wallet from its current tip.
--
-- After the wallet has been restored,
-- this action will continue to fetch newly created blocks
-- and apply them, or roll back to a previous point whenever
-- the chain switches.
restoreWallet
    :: forall s.
        ( IsOurs s Address
        , IsOurs s RewardAccount
        , AddressBookIso s
        , MaybeLight s
        )
    => WalletLayer IO s
    -> ExceptT ErrNoSuchWallet IO ()
restoreWallet ctx = db & \DBLayer{..} ->
    let checkpointPolicy = CP.defaultPolicy
        readChainPoints = atomically listCheckpoints
        rollBackward = rollbackBlocks ctx . toSlot
        rollForward' = restoreBlocks ctx (contramap MsgWalletFollow tr)
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
                , rollForward = \blocks tip ->
                    rollForward' (List $ fromCardanoBlock gp <$> blocks) tip
                , rollBackward
                }
  where
    db = ctx ^. dbLayer
    nw = ctx ^. networkLayer
    tr = ctx ^. logger
    (_block0, NetworkParameters{genesisParameters=gp}) = ctx ^. genesisData

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
    :: WalletLayer IO s
    -> Slot
    -> IO ChainPoint
rollbackBlocks ctx point = db & \DBLayer{..} ->
    atomically $ rollbackTo point
  where
    db = ctx ^. dbLayer

-- | Apply the given blocks to the wallet and update the wallet state,
-- transaction history and corresponding metadata.
--
-- Concurrency: `restoreBlocks` is atomic.
-- However, in the future, we may assume that
-- it is called in a sequential fashion for each wallet.
restoreBlocks
    ::  ( IsOurs s Address
        , IsOurs s RewardAccount
        , AddressBookIso s
        )
    => WalletLayer IO s
    -> Tracer IO WalletFollowLog
    -> BlockData IO (Either Address RewardAccount) ChainEvents s
    -> BlockHeader
    -> IO ()
restoreBlocks ctx tr blocks nodeTip = db & \DBLayer{..} -> atomically $ do
    slottingParams  <- liftIO $ currentSlottingParameters nl
    cp0 <- readCheckpoint
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
    (filteredBlocks', cps') <- liftIO $ NE.unzip <$> applyBlocks blocks cp0
    let cps = NE.map snd cps'
        filteredBlocks = concat filteredBlocks'
        slotPoolDelegations =
            [ (pseudoSlotNo (fblock ^. #slot), cert)
            | fblock <- filteredBlocks
            , cert <- view #delegations fblock
            ]
        pseudoSlotNo Origin = 0
        pseudoSlotNo (At sl) = sl
        txs = foldMap (view #transactions) filteredBlocks
        epochStability = (3*) <$> getSecurityParameter slottingParams
        localTip = currentTip $ NE.last cps

        finalitySlot = nodeTip ^. #slotNo
            - stabilityWindowShelley slottingParams

        -- Checkpoint deltas
        wcps = snd . fromWallet <$> cps
        deltaPutCheckpoints =
            extendCheckpoints
                getSlot
                (view $ #currentTip . #blockHeight)
                epochStability
                (nodeTip ^. #blockHeight)
                wcps

        deltaPruneCheckpoints wallet =
            pruneCheckpoints
                (view $ #currentTip . #blockHeight)
                epochStability
                (localTip ^. #blockHeight)
                (wallet ^. #checkpoints)

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

    liftIO $ forM_ txs $ \(Tx {txCBOR=mcbor},_) ->
        forM_ mcbor $ \cbor -> do
            traceWith tr $ MsgStoringCBOR cbor

    putTxHistory txs

    rollForwardTxSubmissions (localTip ^. #slotNo)
        $ fmap (\(tx,meta) -> (meta ^. #slotNo, txId tx)) txs
    let deltaPruneSubmissions =
            [ UpdateSubmissions $ Submissions.pruneByFinality finalitySlot ]

    forM_ slotPoolDelegations $ \delegation@(slotNo, cert) -> do
            liftIO $ logDelegation delegation
            putDelegationCertificate walletState cert slotNo

    Delta.onDBVar walletState $ Delta.update $ \_wallet ->
        deltaPrologue
        <> [ UpdateCheckpoints deltaPutCheckpoints ]
        <> deltaPruneSubmissions

    Delta.onDBVar walletState $ Delta.update $ \wallet ->
        [ UpdateCheckpoints $ deltaPruneCheckpoints wallet ]

    liftIO $ do
        traceWith tr $ MsgDiscoveredTxs txs
        traceWith tr $ MsgDiscoveredTxsContent txs
  where
    nl = ctx ^. networkLayer
    db = ctx ^. dbLayer

    logDelegation :: (SlotNo, DelegationCertificate) -> IO ()
    logDelegation = traceWith tr . uncurry MsgDiscoveredDelegationCert

    isParentOf :: Wallet s -> BlockHeader -> Bool
    isParentOf cp = (== Just parent) . parentHeaderHash
      where parent = headerHash $ currentTip cp

putDelegationCertificate
    :: Monad stm
    => DBVar stm (DeltaWalletState s)
    -> DelegationCertificate
    -> SlotNo
    -> stm ()
putDelegationCertificate walletState cert slot
    = onDBVar walletState $ update $ \_ ->
        [UpdateDelegations $ Dlgs.putDelegationCertificate cert slot]

readDelegationRewardBalance
    :: Monad stm
    => DBVar stm (DeltaWalletState s)
    -> stm Coin
readDelegationRewardBalance walletState =
    view #rewards <$> readDBVar walletState

-- | Fetch the cached reward balance of a given wallet from the database.
fetchRewardBalance :: forall s . DBLayer IO s -> IO Coin
fetchRewardBalance DBLayer{..} = atomically
    $ readDelegationRewardBalance walletState

mkExternalWithdrawal
    :: NetworkLayer IO block
    -> TxWitnessTag
    -> SomeMnemonic
    -> IO (Either ErrWithdrawalNotBeneficial Withdrawal)
mkExternalWithdrawal netLayer txWitnessTag mnemonic = do
    let (_, rewardAccount, derivationPath) =
            someRewardAccount @ShelleyKey mnemonic
    balance <- getCachedRewardAccountBalance netLayer rewardAccount
    pp <- currentProtocolParameters netLayer
    let (xprv, _acct , _path) = someRewardAccount @ShelleyKey mnemonic
    pure $ checkRewardIsWorthTxCost txWitnessTag pp balance Nothing $>
        WithdrawalExternal rewardAccount derivationPath balance xprv

mkSelfWithdrawal
    :: NetworkLayer IO block
    -> TxWitnessTag
    -> DBLayer IO (SeqState n ShelleyKey)
    -> IO Withdrawal
mkSelfWithdrawal netLayer txWitnessTag db = do
    (rewardAccount, _, derivationPath) <- readRewardAccount db
    balance <- getCachedRewardAccountBalance netLayer rewardAccount
    pp <- currentProtocolParameters netLayer
    pure $ case checkRewardIsWorthTxCost txWitnessTag pp balance Nothing of
        Left ErrWithdrawalNotBeneficial -> NoWithdrawal
        Right () -> WithdrawalSelf rewardAccount derivationPath balance

-- | Unsafe version of the `mkSelfWithdrawal` function that throws an exception
-- when applied to a non-shelley or a non-sequential wallet.
shelleyOnlyMkSelfWithdrawal
    :: forall s block
     . WalletFlavor s
    => NetworkLayer IO block
    -> TxWitnessTag
    -> DBLayer IO s
    -> IO Withdrawal
shelleyOnlyMkSelfWithdrawal netLayer txWitnessTag db =
    case walletFlavor @s  of
        ShelleyWallet -> mkSelfWithdrawal netLayer txWitnessTag db
        _ -> notShelleyWallet
  where
    notShelleyWallet = throwIO
        $ ExceptionReadRewardAccount ErrReadRewardAccountNotAShelleyWallet

mkSelfWithdrawalShared
    :: forall n block
     . NetworkLayer IO block
    -> TxWitnessTag
    -> Maybe CA.ScriptTemplate
    -> DBLayer IO (SharedState n SharedKey)
    -> IO Withdrawal
mkSelfWithdrawalShared netLayer txWitnessTag delegationTemplateM db = do
    (rewardAccount, _, derivationPath) <-
        readRewardAccount @(SharedState n SharedKey) db
    balance <- getCachedRewardAccountBalance netLayer rewardAccount
    pp <- currentProtocolParameters netLayer
    return $ case checkRewardIsWorthTxCost txWitnessTag pp balance delegationTemplateM of
        Left ErrWithdrawalNotBeneficial -> NoWithdrawal
        Right () -> WithdrawalSelf rewardAccount derivationPath balance

checkRewardIsWorthTxCost
    :: TxWitnessTag
    -> ProtocolParameters
    -> Coin
    -> Maybe CA.ScriptTemplate
    -> Either ErrWithdrawalNotBeneficial ()
checkRewardIsWorthTxCost txWitnessTag pp balance delegationTemplateM = do
    when (balance == Coin 0)
        $ Left ErrWithdrawalNotBeneficial
    let costOfWithdrawal = _txRewardWithdrawalCost feePerByte witType balance
    when (Coin.toInteger balance < 2 * Coin.toInteger costOfWithdrawal)
        $ Left ErrWithdrawalNotBeneficial
  where
    feePerByte = getFeePerByteFromWalletPParams pp
    witType = case delegationTemplateM of
        Just t -> Left t
        Nothing -> Right txWitnessTag

readRewardAccount
    :: forall s.
    ( WalletFlavor s
    , Including AllFlavors '[ 'ShelleyF, 'SharedF] (FlavorOf s)
    )
    => DBLayer IO s
    -> IO (RewardAccount, Maybe XPub, NonEmpty DerivationIndex)
readRewardAccount db = do
    walletState <- getState <$> readWalletCheckpoint db
    case walletFlavor @s of
        ShelleyWallet -> do
            let xpub = Seq.rewardAccountKey walletState
                path = stakeDerivationPath $ Seq.derivationPrefix walletState
            pure (toRewardAccount xpub, Just $ getRawKey ShelleyKeyS xpub, path)
        SharedWallet -> do
            let path = stakeDerivationPath $ Shared.derivationPrefix walletState
            case Shared.rewardAccountKey walletState of
                Just rewardAcct -> pure (rewardAcct, Nothing, path)
                Nothing -> throwIO $ ExceptionReadRewardAccount ErrReadRewardAccountMissing

readWalletCheckpoint
    :: DBLayer IO s -> IO (Wallet s)
readWalletCheckpoint DBLayer{..} = liftIO $ atomically readCheckpoint

-- | Put a sequence of checkpoints into the database.
--
-- WARNING: This function is broken. It is only used in the `db` benchmark.
-- To be removed.
putWalletCheckpoints
    :: AddressBookIso s
    => DBLayer IO s -> [Wallet s] -> IO ()
putWalletCheckpoints DBLayer{..} = atomically . mapM_ putCheckpoint
  where
    putCheckpoint cp =
        Delta.onDBVar walletState
        $ Delta.update $ \_ ->
            let (prologue, wcp) = WalletState.fromWallet cp
                slot = WalletState.getSlot wcp
            in  [ WalletState.UpdateCheckpoints
                    [ PutCheckpoint slot wcp ]
                    , WalletState.ReplacePrologue prologue
                ]


-- | Unsafe version of the `readRewardAccount` function
-- that throws error when applied to a non-shared
-- or a non-shared wallet state.
sharedOnlyReadRewardAccount
    :: forall s
     . WalletFlavor s
    => DBLayer IO s
    -> ExceptT ErrReadRewardAccount IO
        (Maybe (RewardAccount, NonEmpty DerivationIndex))
sharedOnlyReadRewardAccount db = do
    case walletFlavor @s of
        SharedWallet -> do
            walletState <- lift $ getState <$> readWalletCheckpoint db
            let path = stakeDerivationPath $ Shared.derivationPrefix walletState
            case Shared.rewardAccountKey walletState of
                Just rewardAcct -> pure $ Just (rewardAcct, path)
                Nothing -> pure Nothing
        _ -> throwE ErrReadRewardAccountNotASharedWallet

-- | Unsafe version of the `readRewardAccount` function
-- that throws error when applied to a non-sequential
-- or a non-shelley wallet state.
shelleyOnlyReadRewardAccount
    :: forall s
     . WalletFlavor s
    => DBLayer IO s
    -> ExceptT ErrReadRewardAccount IO
        (RewardAccount, Maybe XPub, NonEmpty DerivationIndex)
shelleyOnlyReadRewardAccount db = do
    case walletFlavor @s of
        ShelleyWallet -> lift $ readRewardAccount db
        _ -> throwE ErrReadRewardAccountNotAShelleyWallet

readPolicyPublicKey
    :: forall s
     . WalletFlavor s
    => WalletLayer IO s
    -> ExceptT ErrReadPolicyPublicKey IO (XPub, NonEmpty DerivationIndex)
readPolicyPublicKey ctx = db & \DBLayer{..} -> do
    cp <- lift $ atomically readCheckpoint
    case walletFlavor @s of
        ShelleyWallet -> do
            let s = getState cp
            case Seq.policyXPub s of
                Nothing -> throwE ErrReadPolicyPublicKeyAbsent
                Just xpub -> pure
                    ( getRawKey (keyFlavorFromState @s) xpub
                    , policyDerivationPath
                    )
        _ ->
            throwE ErrReadPolicyPublicKeyNotAShelleyWallet
  where
    db = ctx ^. dbLayer

manageRewardBalance
    :: forall n block
     . Tracer IO WalletWorkerLog
    -> NetworkLayer IO block
    -> DBLayer IO (SeqState n ShelleyKey)
    -> IO ()
manageRewardBalance tr' netLayer db = do
    watchNodeTip netLayer $ \bh -> do
         traceWith tr $ MsgRewardBalanceQuery bh
         query <- do
            (acct, _, _) <- readRewardAccount db
            liftIO $ getCachedRewardAccountBalance netLayer acct
         handleRewardAccountQuery tr db $ Right query
    traceWith tr MsgRewardBalanceExited
  where
    tr = contramap MsgWallet tr'


handleRewardAccountQuery
    :: Monad m
    => Tracer m WalletLog
    -> DBLayer m s
    -> Either ErrFetchRewards Coin
    -> m ()
handleRewardAccountQuery tr DBLayer{..} query = do
    traceWith tr $ MsgRewardBalanceResult query
    case query of
       Right amt -> atomically $ putDelegationRewardBalance walletState amt
           -- It can happen that the wallet doesn't exist _yet_, whereas we
           -- already have a reward balance. If that's the case, we log and
           -- move on.
       Left _err ->
           -- Occasionally failing to query is generally not fatal. It will
           -- just update the balance next time the tip changes.
           pure ()

putDelegationRewardBalance
    :: (Monad stm)
    => DBVar stm (DeltaWalletState s)
    -> Coin
    -> stm ()
putDelegationRewardBalance walletState amt =
    onDBVar walletState $ update $ \_ ->
        [UpdateRewards $ Replace amt]

manageSharedRewardBalance
    :: forall n block
     . Tracer IO WalletWorkerLog
    -> NetworkLayer IO block
    -> DBLayer IO (SharedState n SharedKey)
    -> IO ()
manageSharedRewardBalance tr' netLayer db = do
    watchNodeTip netLayer $ \bh -> do
         traceWith tr $ MsgRewardBalanceQuery bh
         query <- runExceptT $ do
            (acct, _, _) <-
                lift $ readRewardAccount @(SharedState n SharedKey) db
            liftIO $ getCachedRewardAccountBalance netLayer acct
         handleRewardAccountQuery tr db query
    traceWith tr MsgRewardBalanceExited
  where
    tr = contramap MsgWallet tr'

{-------------------------------------------------------------------------------
                                    Address
-------------------------------------------------------------------------------}

lookupTxIns
    :: IsOurs s Address
    => WalletLayer IO s
    -> [TxIn]
    -> IO [(TxIn, Maybe (TxOut, NonEmpty DerivationIndex))]
lookupTxIns ctx txins = db & \DBLayer{..} -> do
    cp <- atomically readCheckpoint
    pure $ map (\i -> (i, lookupTxIn cp i)) txins
  where
    db = ctx ^. dbLayer

lookupTxIn
    :: IsOurs s Address
    => Wallet s
    -> TxIn
    -> Maybe (TxOut, NonEmpty DerivationIndex)
lookupTxIn wallet txIn = do
    out@(TxOut addr _) <- UTxO.lookup txIn (totalUTxO mempty wallet)
    (out,) <$> fst (isOurs addr (getState wallet))

lookupTxOuts
    :: IsOurs s Address
    => WalletLayer IO s
    -> [TxOut]
    -> IO [(TxOut, Maybe (NonEmpty DerivationIndex))]
lookupTxOuts ctx txouts = db & \DBLayer{..} -> do
    cp <- atomically readCheckpoint
    -- NOTE: We evolve the state (in practice an address pool) as we loop
    -- through the outputs, but we don't consider pending transactions.
    -- /Theoretically/ the outputs might only be discoverable after discovering
    -- outputs other pending transactions.
    pure $ flip evalState (getState cp) $ forM txouts $ \out@(TxOut addr _) ->
        (out,) <$> state (isOurs addr)
  where
    db = ctx ^. dbLayer

-- | List all addresses of a wallet with their metadata. Addresses
-- are ordered from the most-recently-discovered to the oldest known.
listAddresses
    :: forall s .
        ( CompareDiscovery s
        , KnownAddresses s
        )
    => WalletLayer IO s
    -> (s -> Address -> Maybe Address)
        -- ^ A function to normalize address, so that delegated addresses
        -- non-delegation addresses found in the transaction history are
        -- shown with their delegation settings.
        -- Use 'Just' for wallet without delegation settings.
    -> IO [(Address, AddressState, NonEmpty DerivationIndex)]
listAddresses ctx normalize = db & \DBLayer{..} -> do
    cp <- atomically readCheckpoint
    let s = getState cp

    -- FIXME
    -- Stream this instead of returning it as a single block.
    return
        $ L.sortBy (\(a,_,_) (b,_,_) -> compareDiscovery s a b)
        $ mapMaybe (\(addr, st,path) -> (,st,path) <$> normalize s addr)
        $ knownAddresses s
  where
    db = ctx ^. dbLayer

createRandomAddress
    :: forall s n .
        ( RndStateLike s
        , ByronKey ~ KeyOf s
        , AddressBookIso s
        , HasSNetworkId n
        )
    => WalletLayer IO s
    -> WalletId
    -> Passphrase "user"
    -> Maybe (Index 'Hardened 'CredFromKeyK)
    -> ExceptT ErrCreateRandomAddress IO (Address, NonEmpty DerivationIndex)
createRandomAddress ctx wid pwd mIx = db & \DBLayer{..} ->
    withRootKey db wid pwd ErrCreateAddrWithRootKey $ \xprv scheme ->
        ExceptT
            . atomically
            . Delta.onDBVar walletState
            . Delta.updateWithResultAndError
            $ createRandomAddress' xprv scheme
  where
    db = ctx ^. dbLayer

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
    :: forall s
     . ( RndStateLike s
       , AddressBookIso s
       )
    => WalletLayer IO s
    -> [Address]
    -> ExceptT ErrImportRandomAddress IO ()
importRandomAddresses ctx addrs =
    ExceptT . onWalletState ctx . Delta.updateWithError
        $ importRandomAddresses'
  where
    importRandomAddresses' wal = case es1 of
        Left err -> Left $ ErrImportAddr err
        Right s1 -> Right [ReplacePrologue $ getPrologue s1]
      where
        s0 = getState $ getLatest wal
        es1 = foldl' (\s addr -> s >>= Rnd.importAddress addr) (Right s0) addrs

-- NOTE
-- Addresses coming from the transaction history might be payment or
-- delegation addresses. So we normalize them all to be delegation addresses
-- to make sure that we compare them correctly.
normalizeDelegationAddress
    :: forall s k n.
        ( DelegationAddress k 'CredFromKeyK
        , s ~ SeqState n k
        , HasSNetworkId n
        )
    => s
    -> Address
    -> Maybe Address
normalizeDelegationAddress s addr = do
    fingerprint <- eitherToMaybe (paymentKeyFingerprint addr)
    pure
        $ liftDelegationAddressS @n fingerprint
        $ Seq.rewardAccountKey s

assignChangeAddressesAndUpdateDb
    :: ( GenChange s
       , AddressBookIso s
       , WalletFlavor s
       )
    => WalletLayer IO s
    -> ArgGenChange s
    -> Selection
    -> IO (SelectionOf TxOut)
assignChangeAddressesAndUpdateDb ctx argGenChange selection =
    onWalletState ctx . Delta.updateWithResult
        $ assignChangeAddressesAndUpdateDb'
  where
    assignChangeAddressesAndUpdateDb' wallet =
        -- Newly generated change addresses only change the Prologue
        ([ReplacePrologue $ getPrologue stateUpdated], selectionUpdated)
      where
        s = getState $ getLatest wallet
        (selectionUpdated, stateUpdated) =
            assignChangeAddresses
                (defaultChangeAddressGen argGenChange )
                selection
                s

assignChangeAddressesWithoutDbUpdate
    :: forall s
     . ( GenChange s
       , WalletFlavor s
       )
    => WalletLayer IO s
    -> ArgGenChange s
    -> Selection
    -> IO (SelectionOf TxOut)
assignChangeAddressesWithoutDbUpdate ctx argGenChange selection =
    db & \DBLayer{..} -> atomically $ do
        cp <- readCheckpoint
        let (selectionUpdated, _) =
                assignChangeAddresses
                    (defaultChangeAddressGen argGenChange)
                    selection
                    (getState cp)
        pure selectionUpdated
  where
    db = ctx ^. dbLayer

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
        WithdrawalExternal acct path c _ ->
            [(acct, c, path)]

data CoinSelection = CoinSelection
    { inputs :: [(TxIn, TxOut, NonEmpty DerivationIndex)]
    , outputs :: [TxOut]
    , change :: [TxChange (NonEmpty DerivationIndex)]
    , collateral :: [(TxIn, TxOut, NonEmpty DerivationIndex)]
    , withdrawals :: [(RewardAccount, Coin, NonEmpty DerivationIndex)]
    , delegationAction :: Maybe (DelegationAction, NonEmpty DerivationIndex)
    , deposit :: Maybe Coin
    , refund :: Maybe Coin
    }
    deriving (Generic)

buildCoinSelectionForTransaction
    :: forall s n k era
     . ( Write.IsRecentEra era
       , IsOurs s Address
       , s ~ SeqState n k
       )
    => Wallet s
    -> [TxOut] -- ^ payment outputs to exclude from change outputs
    -> Coin -- ^ protocol parameter deposit amount
    -> Maybe DelegationAction
    -> Cardano.Tx era
    -> CoinSelection
buildCoinSelectionForTransaction
    wallet paymentOutputs depositRefund delegationAction cardanoTx =
    CoinSelection
    { inputs = resolveInput . fromCardanoTxIn . fst =<< txIns
    , outputs = paymentOutputs
    , change = do
        out <-
            -- NOTE: We assume that the change outputs are always
            -- at the end of the list. This is true for the current
            -- 'balanceTransaction' implementation, but may not
            -- be true for other implementations.
            drop (length  paymentOutputs) $ fromCardanoTxOut <$> txOuts
        let address = out ^. #address
        derivationPath <-
            maybeToList $ fst $ isOurs address (getState wallet)
        pure TxChange
            { address
            , amount = out ^. #tokens . #coin
            , assets = out ^. #tokens . #tokens
            , derivationPath
            }
    , collateral = resolveInput . fromCardanoTxIn =<<
        case txInsCollateral of
            Cardano.TxInsCollateralNone -> []
            Cardano.TxInsCollateral _supported is -> is
    , withdrawals = fromCardanoWdrls txWithdrawals <&>
        \(acct, coin) -> (acct, coin, rewardAcctPath)
    , delegationAction = (,rewardAcctPath) <$> delegationAction
    , deposit =
        case delegationAction of
            Just (JoinRegisteringKey _poolId) -> Just depositRefund
            _ -> Nothing
    , refund =
        case delegationAction of
            Just Quit -> Just depositRefund
            _ -> Nothing
    }
  where
    Cardano.TxBody Cardano.TxBodyContent
        { txIns, txOuts, txInsCollateral, txWithdrawals } =
            Cardano.getTxBody cardanoTx

    resolveInput txIn = do
        (txOut, derivationPath) <- maybeToList (lookupTxIn wallet txIn)
        pure (txIn, txOut, derivationPath)

    rewardAcctPath =
        stakeDerivationPath (Seq.derivationPrefix (getState wallet))

-- | Read a wallet checkpoint and its UTxO, for 'selectAssets' and
-- 'selectAssetsNoOutputs'.
readWalletUTxO
    :: WalletLayer IO s
    -> IO (UTxO, Wallet s, Set Tx)
readWalletUTxO ctx = do
    (cp, _, pending) <- readWallet ctx
    return (availableUTxO pending cp, cp, pending)

-- | Calculate the minimum coin values required for a bunch of specified
-- outputs.
calcMinimumCoinValues
    :: ProtocolParameters
    -> TransactionLayer k ktype tx
    -> TxOut
    -> Coin
calcMinimumCoinValues pp txLayer =
    uncurry (constraints ^. #txOutputMinimumAdaQuantity)
     . (\o -> (o ^. #address, o ^. #tokens . #tokens))
  where
    constraints = Write.txConstraints pp $ transactionWitnessTag txLayer

signTransaction
  :: forall k ktype
   . ( HardDerivation k
     , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
     )
  => KeyFlavorS k
  -> TransactionLayer k ktype SealedTx
  -- ^ The way to interact with the wallet backend
  -> Cardano.AnyCardanoEra
  -- ^ Preferred latest era
  -> WitnessCountCtx
  -> (Address -> Maybe (k ktype XPrv, Passphrase "encryption"))
  -- ^ The wallets address-key lookup function
  -> (Maybe (XPrv, Passphrase "encryption"))
  -- ^ Optional external reward account
  -> ClearCredentials k
  -- ^ The root key of the wallet
  -> UTxO
  -- ^ The total UTxO set of the wallet (i.e. if pending transactions all
  -- applied).
  -> Maybe (Index 'Hardened 'AccountK)
  -- ^ account ix used only for delegation scripts
  -> SealedTx
  -- ^ The transaction to sign
  -> SealedTx
  -- ^ The original transaction, with additional signatures added where
  -- necessary
signTransaction key tl preferredLatestEra witCountCtx keyLookup mextraRewardAcc
    (RootCredentials rootKey rootPwd) utxo accIxForStakingM =
    let
        rewardAcnts :: [(XPrv, Passphrase "encryption")]
        rewardAcnts = ourRewardAcc : maybeToList mextraRewardAcc
          where
            ourRewardAcc =
                (getRawKey key $
                    deriveRewardAccount @k rootPwd rootKey minBound
                , rootPwd
                )

        policyKey :: Maybe (KeyHash, XPrv, Passphrase "encryption")
        policyKey = afterByron key $ \key' ->
                ( hashVerificationKey key CA.Policy $ liftRawKey key'
                    $ toXPub xprv
                , xprv
                , rootPwd
                )
          where
            xprv = derivePolicyPrivateKey rootPwd (getRawKey key rootKey)
                minBound

        stakingKeyM :: Maybe (KeyHash, XPrv, Passphrase "encryption")
        stakingKeyM = join $ afterByron key $ \key' ->
            case xprvM of
                Just xprv ->
                    Just
                        ( hashVerificationKey key CA.Delegation
                            $ liftRawKey key'
                            $ toXPub xprv
                        , xprv
                        , rootPwd
                        )
                Nothing -> Nothing
          where
            xprvM =
                getRawKey key
                    . deriveRewardAccount @k rootPwd rootKey
                    <$> accIxForStakingM

        inputResolver :: TxIn -> Maybe Address
        inputResolver i = do
            TxOut addr _ <- UTxO.lookup i utxo
            pure addr
    in
        addVkWitnesses
            tl
            preferredLatestEra
            witCountCtx
            rewardAcnts
            policyKey
            stakingKeyM
            keyLookup
            inputResolver

type MakeRewardAccountBuilder k =
    ClearCredentials k -> (XPrv, Passphrase "encryption")

data ErrWriteTxEra
    = ErrNodeNotYetInRecentEra Cardano.AnyCardanoEra
    -- ^ Node is not synced enough or on an unsupported testnet in an older era.
    | ErrPartialTxNotInNodeEra
        Write.AnyRecentEra -- node era
    -- ^ The provided partial tx is not deserialisable as a tx in the era of the
    -- node.
    --
    -- NOTE: In general we don't have /one/ known tx era. The tx could in theory
    -- be deserialisable in all other eras than the one node era we need.
    -- Exposing a 'Set AnyCardanoEra' of the candidate tx eras /could/ be done,
    -- but would require some work.
    deriving (Show, Eq)

readNodeTipStateForTxWrite
    :: NetworkLayer IO Read.Block
    -> IO (Write.InAnyRecentEra Write.ProtocolParameters, TimeTranslation)
readNodeTipStateForTxWrite netLayer = do
    timeTranslation <- toTimeTranslation (timeInterpreter netLayer)

    res <- currentLedgerProtocolParameters
        <$> currentProtocolParameters netLayer

    case Write.toRecentEraGADT res of
        Right pp -> pure (pp, timeTranslation)
        Left era -> throwIO $ invalidEra era
  where
    invalidEra =
        ExceptionWriteTxEra . ErrNodeNotYetInRecentEra

-- | Build, Sign, Submit transaction.
--
-- Requires the encryption passphrase in order to decrypt the root private key.
buildSignSubmitTransaction
    :: forall s k
     . ( HardDerivation k
       , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
       , IsOurs s RewardAccount
       , AddressBookIso s
       , IsOurs s Address
       , WalletFlavor s
       , CredFromOf s ~ 'CredFromKeyK
       , k ~ KeyOf s
       , CredFromOf s ~ 'CredFromKeyK
       , HasSNetworkId (NetworkOf s)
       , Excluding '[SharedKey] k
       )
    => DBLayer IO s
    -> NetworkLayer IO Read.Block
    -> TransactionLayer k 'CredFromKeyK SealedTx
    -> Passphrase "user"
    -> WalletId
    -> ChangeAddressGen s
    -> PreSelection
    -> TransactionCtx
    -> IO (BuiltTx, UTCTime)
buildSignSubmitTransaction db@DBLayer{..} netLayer txLayer
    pwd walletId changeAddrGen preSelection txCtx = do
    --
    stdGen <- initStdGen
    (Write.InAnyRecentEra _era protocolParams, timeTranslation)
        <- readNodeTipStateForTxWrite netLayer
    let ti = timeInterpreter netLayer
    throwOnErr <=< runExceptT $ withRootKey db walletId pwd wrapRootKeyError $
        \rootKey scheme -> lift $ do
        (BuiltTx{..}, slot) <- atomically $ do
            pendingTxs <- fmap fromTransactionInfo <$>
                readTransactions
                    Nothing
                    Descending
                    wholeRange
                    (Just Pending)
                    Nothing
                    Nothing
            txWithSlot@(builtTx, slot) <- (throwOnErr <=<
                (Delta.onDBVar walletState . Delta.updateWithResultAndError)) $
                \s -> do
                    let wallet = WalletState.getLatest s
                        utxo = availableUTxO (Set.fromList pendingTxs) wallet
                    buildAndSignTransactionPure @k @s
                        timeTranslation
                        utxo
                        rootKey
                        scheme
                        pwd
                        protocolParams
                        txLayer
                        changeAddrGen
                        preSelection
                        txCtx
                        & (`runStateT` wallet)
                        & runExceptT . withExceptT wrapBalanceConstructError
                        & (`evalRand` stdGen)
                        & fmap (\(builtTx, wallet') ->
                            -- Newly generated change addresses
                            -- only change the Prologue
                            ( [ReplacePrologue $ getPrologue $ getState wallet']
                            , (builtTx, currentTip wallet' ^. #slotNo)
                            )
                        )

            Delta.onDBVar walletState
                . WalletState.updateSubmissions
                . Delta.update
                $ \_ -> Submissions.addTxSubmission builtTx slot

            pure txWithSlot

        postTx netLayer builtSealedTx
            & throwWrappedErr wrapNetworkError
            & liftIO

        slotToUTCTime slot
            & interpretQuery (neverFails "slot is ahead of the node tip" ti)
            & fmap (BuiltTx{..},)
            & liftIO
  where
    throwOnErr :: (MonadIO m, Exception e) => Either e a -> m a
    throwOnErr = either (liftIO . throwIO) pure

    throwWrappedErr f e = runExceptT (withExceptT f e) >>= throwOnErr

    wrapRootKeyError = ExceptionWitnessTx . ErrWitnessTxWithRootKey
    wrapNetworkError = ExceptionSubmitTx . ErrSubmitTxNetwork
    wrapBalanceConstructError = either ExceptionBalanceTx ExceptionConstructTx

buildAndSignTransactionPure
    :: forall k s era
     . ( HardDerivation k
       , Bounded (Index (AddressIndexDerivationType k) (AddressCredential k))
       , IsOurs s RewardAccount
       , IsOurs s Address
       , WalletFlavor s
       , Write.IsRecentEra era
       , k ~ KeyOf s
       , CredFromOf s ~ 'CredFromKeyK
       , Excluding '[SharedKey] k
       , HasSNetworkId (NetworkOf s)
       )
    => TimeTranslation
    -> UTxO
    -> k 'RootK XPrv
    -> PassphraseScheme
    -> Passphrase "user"
    -> Write.ProtocolParameters era
    -> TransactionLayer k 'CredFromKeyK SealedTx
    -> ChangeAddressGen s
    -> PreSelection
    -> TransactionCtx
    -> StateT
        (Wallet s)
        (ExceptT (Either ErrBalanceTx ErrConstructTx) (Rand StdGen))
        BuiltTx
buildAndSignTransactionPure
    timeTranslation utxoIndex rootKey passphraseScheme userPassphrase
    pp txLayer changeAddrGen preSelection txCtx = do
    wallet <- get
    (unsignedBalancedTx, updatedWalletState) <- lift $
        buildTransactionPure @s @era
            wallet
            timeTranslation
            utxoIndex
            txLayer
            changeAddrGen
            pp
            preSelection
            txCtx
    put wallet { getState = updatedWalletState }

    let mExternalRewardAccount = case view #txWithdrawal txCtx of
            WithdrawalExternal _ _ _ externalXPrv
                -> Just (externalXPrv, mempty) -- no passphrase
            _
                -> Nothing

        passphrase = preparePassphrase passphraseScheme userPassphrase
        signedTx = signTransaction @k @'CredFromKeyK
            (keyFlavorFromState @s)
            txLayer
            anyCardanoEra
            AnyWitnessCountCtx
            (isOwned wF (getState wallet) (rootKey, passphrase))
            mExternalRewardAccount
            (RootCredentials rootKey passphrase)
            (wallet ^. #utxo)
            Nothing
            (sealedTxFromCardano $ inAnyCardanoEra unsignedBalancedTx)

        ( tx
            , _tokenMapWithScripts1
            , _tokenMapWithScripts2
            , _certificates
            , _validityIntervalExplicit
            , _witnessCount
            ) = decodeTx txLayer anyCardanoEra AnyWitnessCountCtx signedTx

        utxo' = applyOurTxToUTxO
            (Slot.at $ currentTip wallet ^. #slotNo)
            (currentTip wallet ^. #blockHeight)
            (getState wallet)
            tx
            (wallet ^. #utxo)
        meta = case utxo' of
            Nothing -> error $ unwords
                [ "buildAndSignTransactionPure:"
                , "Can't apply constructed transaction."
                ]
            Just ((_tx, appliedMeta), _deltaUtxo, _nextUtxo) ->
                appliedMeta
                    { status = Pending
                    , expiry = Just (snd (txValidityInterval txCtx))
                    }
        -- tx coming from `decodeTx` doesn't contain previous tx outputs that
        -- correspond to this tx inputs, so its inputs aren't "resolved".
        -- We restore corresponding outputs by searching them in the UTxO again.
        txResolved = tx
            { resolvedInputs =
                resolveInputs  (resolvedInputs tx)
            , resolvedCollateralInputs =
                resolveInputs  (resolvedCollateralInputs tx)
            }
        resolveInputs = fmap (\(txIn, _) ->
            (txIn, UTxO.lookup txIn (wallet ^. #utxo)))

    pure BuiltTx
        { builtTx = txResolved
        , builtTxMeta = meta
        , builtSealedTx = signedTx
        }
  where
    wF = walletFlavor @s
    anyCardanoEra = Cardano.AnyCardanoEra $ Write.cardanoEra @era

buildTransaction
    :: forall s era.
        ( WalletFlavor s
        , Write.IsRecentEra era
        , AddressBookIso s
        , Excluding '[SharedKey] (KeyOf s)
        )
    => DBLayer IO s
    -> TransactionLayer (KeyOf s) 'CredFromKeyK SealedTx
    -> TimeTranslation
    -> ChangeAddressGen s
    -> Write.ProtocolParameters era
    -> TransactionCtx
    -> [TxOut] -- ^ payment outputs
    -> IO (Cardano.Tx era, Wallet s)
buildTransaction DBLayer{..} txLayer timeTranslation changeAddrGen
    protocolParameters txCtx paymentOuts = do
    stdGen <- initStdGen
    atomically $ do
        wallet <- readDBVar walletState <&> WalletState.getLatest

        pendingTxs <- Set.fromList . fmap fromTransactionInfo <$>
            readTransactions
                Nothing Descending wholeRange (Just Pending) Nothing Nothing

        let utxo = availableUTxO @s pendingTxs wallet

        fmap (\s' -> wallet { getState = s' }) <$>
            buildTransactionPure @s @era
                wallet
                timeTranslation
                utxo
                txLayer
                changeAddrGen
                protocolParameters
                PreSelection { outputs = paymentOuts }
                txCtx
                & runExceptT . withExceptT
                    (either ExceptionBalanceTx ExceptionConstructTx)
                & (`evalRand` stdGen)
                & either (liftIO . throwIO) pure

buildTransactionPure
    :: forall s era.
        ( Write.IsRecentEra era
        , WalletFlavor s
        , Excluding '[SharedKey] (KeyOf s)
        )
    => Wallet s
    -> TimeTranslation
    -> UTxO
    -> TransactionLayer (KeyOf s) 'CredFromKeyK SealedTx
    -> ChangeAddressGen s
    -> Write.ProtocolParameters era
    -> PreSelection
    -> TransactionCtx
    -> ExceptT
        (Either ErrBalanceTx ErrConstructTx)
        (Rand StdGen)
        (Cardano.Tx era, s)
buildTransactionPure
    wallet timeTranslation utxo txLayer changeAddrGen pparams preSelection txCtx
    = do
    unsignedTxBody <-
        withExceptT (Right . ErrConstructTxBody) . except $
            mkUnsignedTransaction txLayer @era
                (Left $ unsafeShelleyOnlyGetRewardXPub @s (getState wallet))
                txCtx
                (Left preSelection)

    withExceptT Left $
        balanceTransaction @_ @_ @s
            nullTracer
            (utxoAssumptionsForWallet (walletFlavor @s))
            pparams
            timeTranslation
            (constructUTxOIndex utxo)
            changeAddrGen
            (getState wallet)
            PartialTx
                { tx = Cardano.Tx unsignedTxBody []
                , inputs = Cardano.UTxO mempty
                , redeemers = []
                }

-- HACK: 'mkUnsignedTransaction' takes a reward account 'XPub' even when the
-- wallet is a Byron wallet, and doesn't actually have a reward account.
--
-- 'buildAndSignTransaction' achieves this by deriving the 'XPub' regardless
-- of wallet type, from the root key. To avoid requiring another
-- 'withRootKey' call, and to make the sketchy behaviour more explicit, we
-- make 'buildAndSignTransactionPure' partial instead.
--
-- https://input-output.atlassian.net/browse/ADP-2933

unsafeShelleyOnlyGetRewardXPub
    :: forall s
     . WalletFlavor s
    => s -> XPub
unsafeShelleyOnlyGetRewardXPub walletState =
    case walletFlavor @s of
        ShelleyWallet -> getRawKey (keyFlavorFromState @s)
                $ Seq.rewardAccountKey walletState
        _  -> error $ unwords
            [ "buildAndSignTransactionPure:"
            , "can't delegate using non-shelley wallet"
            ]

-- | Produce witnesses and construct a transaction from a given selection.
--
-- Requires the encryption passphrase in order to decrypt the root private key.
-- Note that this doesn't broadcast the transaction to the network. In order to
-- do so, use 'submitTx'.
--
buildAndSignTransaction
    :: forall s k.
        ( IsOurs s Address
        , k ~ KeyOf s
        , HasSNetworkId (NetworkOf s)
        , CredFromOf s ~ 'CredFromKeyK
        , WalletFlavor s
        )
    => WalletLayer IO s
    -> WalletId
    -> Cardano.AnyCardanoEra
    -> MakeRewardAccountBuilder k
    -> Passphrase "user"
    -> TransactionCtx
    -> SelectionOf TxOut
    -> ExceptT ErrSignPayment IO (Tx, TxMeta, UTCTime, SealedTx)
buildAndSignTransaction ctx wid era mkRwdAcct pwd txCtx sel = db & \DBLayer{..} ->
    withRootKey db wid pwd ErrSignPaymentWithRootKey $ \xprv scheme -> do
        let pwdP = preparePassphrase scheme pwd
        mapExceptT atomically $ do
            cp <- lift readCheckpoint
            pp <- liftIO $ currentProtocolParameters nl
            let keyFrom = isOwned wF (getState cp) (xprv, pwdP)
                rewardAcnt = mkRwdAcct $ RootCredentials xprv pwdP
            (tx, sealedTx) <- withExceptT ErrSignPaymentMkTx $ ExceptT $ pure $
                mkTransaction tl era rewardAcnt keyFrom pp txCtx sel
            let amountOut :: Coin =
                    F.fold $
                        fmap TxOut.coin (sel ^. #change) <>
                        mapMaybe (`ourCoin` getState cp) (sel ^. #outputs)
                amountIn :: Coin =
                    F.fold (NE.toList (TxOut.coin . snd <$> sel ^. #inputs))
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
            time <- liftIO $ tipSlotStartTime $ currentTip cp
            let meta = mkTxMeta
                    (currentTip cp) (txValidityInterval txCtx)
                    amountIn amountOut
            pure (tx, meta, time, sealedTx)
  where
    wF = walletFlavor @s
    db = ctx ^. dbLayer
    tl = transactionLayer_ ctx
    nl = ctx ^. networkLayer
    ti = timeInterpreter nl
    tipSlotStartTime tipHeader = interpretQuery
        (neverFails "buildAndSignTransaction slot is ahead of the node tip" ti)
        (slotToUTCTime (tipHeader ^. #slotNo))

-- | Construct an unsigned transaction from a given selection.
constructTransaction
    :: forall n ktype era
     . Write.IsRecentEra era
    => TransactionLayer ShelleyKey ktype SealedTx
    -> DBLayer IO (SeqState n ShelleyKey)
    -> TransactionCtx
    -> PreSelection
    -> ExceptT ErrConstructTx IO (Cardano.TxBody era)
constructTransaction txLayer db txCtx preSel = do
    (_, xpub, _) <- lift $ readRewardAccount db
    mkUnsignedTransaction txLayer (Left $ fromJust xpub) txCtx (Left preSel)
        & withExceptT ErrConstructTxBody . except

constructUnbalancedSharedTransaction
    :: forall n ktype era
     . ( Write.IsRecentEra era
       , HasSNetworkId n)
    => TransactionLayer SharedKey ktype SealedTx
    -> DBLayer IO (SharedState n SharedKey)
    -> TransactionCtx
    -> PreSelection
    -> ExceptT ErrConstructTx IO
        (Cardano.TxBody era, (Address -> CA.Script KeyHash))
constructUnbalancedSharedTransaction txLayer db txCtx sel = db & \DBLayer{..} -> do
    cp <- lift $ atomically readCheckpoint
    let s = getState cp
        scriptM =
            flip (replaceCosignersWithVerKeys CAShelley.Stake) minBound <$>
            delegationTemplate s
        getScript addr = case fst (isShared addr s) of
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
    sealedTx <- mapExceptT atomically $ do
        withExceptT ErrConstructTxBody $ ExceptT $ pure $
            mkUnsignedTransaction txLayer (Right scriptM) txCtx (Left sel)
    pure (sealedTx, getScript)

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
    :: DBLayer IO s
    -> TransactionCtx
    -> [(TxIn, Coin)]
    -> [TxOut]
    -> IO TxMeta
constructTxMeta DBLayer{..} txCtx inps outs = atomically $ do
        checkpoint <- readCheckpoint
        let latestBlockHeader = currentTip checkpoint
            amountOut = F.fold $ map TxOut.coin outs
            amountIn = F.fold (map snd inps)
                & case txWithdrawal txCtx of
                    w@WithdrawalSelf{} -> Coin.add (withdrawalToCoin w)
                    WithdrawalExternal{} -> Prelude.id
                    NoWithdrawal -> Prelude.id
            validity = txValidityInterval txCtx
        pure $ mkTxMeta latestBlockHeader validity amountIn amountOut

ourCoin :: IsOurs s Address => TxOut -> s -> Maybe Coin
ourCoin (TxOut addr tokens) wState =
    fst (isOurs addr wState) $> TokenBundle.getCoin tokens

-- | Construct transaction metadata for a pending transaction
mkTxMeta
    :: BlockHeader
    -> TxValidityInterval
    -> Coin -- Our inputs amount
    -> Coin -- Outputs amount
    -> TxMeta
mkTxMeta latestBlockHeader txValidity amountIn amountOut =
    TxMeta
        { status = Pending
        , direction = if amountIn > amountOut then Outgoing else Incoming
        , slotNo = latestBlockHeader ^. #slotNo
        , blockHeight = latestBlockHeader ^. #blockHeight
        , amount = Coin.distance amountIn amountOut
        , expiry = Just (snd txValidity)
        }

-- | Broadcast a (signed) transaction to the network.
submitTx
    :: MonadUnliftIO m
    => Tracer m WalletWorkerLog
    -> DBLayer m s
    -> NetworkLayer m block
    -> BuiltTx
    -> ExceptT ErrSubmitTx m ()
submitTx tr DBLayer{walletState, atomically} nw tx@BuiltTx{..} =
    traceResult (MsgWallet . MsgTxSubmit . MsgSubmitTx tx >$< tr) $ do
        withExceptT ErrSubmitTxNetwork $ postTx nw builtSealedTx
        lift
            . atomically
            . Delta.onDBVar walletState
            . WalletState.updateSubmissions
            . Delta.update
            $ \_ -> Submissions.addTxSubmission tx (builtTxMeta ^. #slotNo)

-- | Broadcast an externally-signed transaction to the network.
--
-- NOTE: external transactions will not be added to the LocalTxSubmission pool,
-- so the user must retry submission themselves.
submitExternalTx
    :: Tracer IO TxSubmitLog
    -> NetworkLayer IO block
    -> TransactionLayer k ktype SealedTx
    -> SealedTx
    -> ExceptT ErrPostTx IO Tx
submitExternalTx tr nw tl sealedTx = do
    -- FIXME: We read the current era to constrain the @sealedTx@ **twice**:
    -- once here for decodeTx, and once in postTx before submitting.
    era <- liftIO $ currentNodeEra nw
    let (tx, _, _, _, _, _) = decodeTx tl era AnyWitnessCountCtx sealedTx
    traceResult (MsgSubmitExternalTx (tx ^. #txId) >$< tr) $ do
        postTx nw sealedTx
        pure tx

-- | Remove a pending or expired transaction from the transaction history. This
-- happens at the request of the user. If the transaction is already on chain,
-- or is missing from the transaction history, an error will be returned.
--
-- If a 'Pending' transaction is removed, but later appears in a block, it will
-- be added back to the transaction history.
forgetTx
    :: WalletLayer m s
    -> Hash "Tx"
    -> ExceptT ErrRemoveTx m ()
forgetTx ctx txid =
    ExceptT
        . onWalletState ctx
        . WalletState.updateSubmissions
        . Delta.updateWithError
        $ Submissions.removePendingOrExpiredTx txid

-- | List all transactions from the local submission pool which are
-- still pending as of the latest checkpoint of the given wallet. The
-- slot numbers for first submission and most recent submission are
-- included.
readLocalTxSubmissionPending
    :: WalletLayer m s
    -> m [TxSubmissionsStatus]
readLocalTxSubmissionPending ctx = db & \DBLayer{..} ->
    atomically
        $ readLocalTxSubmissionPending' <$> readDBVar walletState
  where
    db = ctx ^. dbLayer

    readLocalTxSubmissionPending' =
          filter Submissions.isInSubmission
        . Submissions.getInSubmissionTransactions
        . WalletState.submissions

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
    :: forall s m.
        ( MonadUnliftIO m
        , MonadMonotonicTime m
        )
    => LocalTxSubmissionConfig
    -> WalletLayer m s
    -> m ()
runLocalTxSubmissionPool cfg ctx = db & \DBLayer{..} -> do
    submitPending <- rateLimited $ \bh -> bracketTracer trBracket $ do
        sp <- currentSlottingParameters nw
        pending <- readLocalTxSubmissionPending ctx
        let sl = bh ^. #slotNo
            pendingOldStyle = pending >>= mkLocalTxSubmission
        -- Re-submit transactions due, ignore errors
        forM_ (filter (isScheduled sp sl) pendingOldStyle) $ \st -> do
            _ <- runExceptT $ traceResult (trRetry (st ^. #txId)) $
                postTx nw (st ^. #submittedTx )
            atomically $ resubmitTx (st ^. #txId) (st ^. #submittedTx) sl
    watchNodeTip nw submitPending
  where
    nw = networkLayer_ ctx
    db = dbLayer_ ctx

    isScheduled sp now =
        (<= now) . scheduleLocalTxSubmission (blockInterval cfg) sp

    rateLimited = throttle (rateLimit cfg) . const

    tr = contramap (MsgWallet . MsgTxSubmit) $ logger_ ctx
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
    :: WalletLayer IO s
    -> Maybe Coin
    -- Inclusive minimum value of at least one withdrawal in each transaction
    -> Maybe UTCTime
    -- Inclusive minimum time bound.
    -> Maybe UTCTime
    -- Inclusive maximum time bound.
    -> SortOrder
    -> Maybe Natural
    -- ^ Maximum number of transactions to return.
    -> Maybe Address
    -> ExceptT ErrListTransactions IO [TransactionInfo]
listTransactions ctx mMinWithdrawal mStart mEnd order mLimit mAddress
    = db & \DBLayer{..} -> do
        when (Just True == ( (<(Coin 1)) <$> mMinWithdrawal )) $
            throwE ErrListTransactionsMinWithdrawalWrong
        mapExceptT atomically $ do
            mapExceptT liftIO getSlotRange >>= maybe
                (pure [])
                (\r -> lift
                $ readTransactions mMinWithdrawal order r Nothing mLimit mAddress)
  where
    ti :: TimeInterpreter (ExceptT PastHorizonException IO)
    ti = timeInterpreter (ctx ^. networkLayer)

    db = ctx ^. dbLayer

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
    :: IsOurs s Address
    => WalletLayer IO s
    -> IO (Set TokenMap.AssetId)
listAssets ctx = db & \DBLayer{..} -> do
    cp <- atomically readCheckpoint
    txs <- atomically $
        let noMinWithdrawal = Nothing
            allTxStatuses = Nothing
        in readTransactions noMinWithdrawal Ascending wholeRange
            allTxStatuses Nothing Nothing
    let txAssets :: TransactionInfo -> Set TokenMap.AssetId
        txAssets = Set.unions
            . map (TokenBundle.getAssets . view #tokens)
            . filter ourOut
            . txInfoOutputs
        ourOut TxOut{address} = ourAddress address
        ourAddress addr = isJust . fst . isOurs addr $ getState cp
    pure $ Set.unions $ map txAssets txs
  where
    db = ctx ^. dbLayer

-- | Get transaction and metadata from history for a given wallet.
getTransaction
    :: WalletLayer IO s
    -> Hash "Tx"
    -> ExceptT ErrGetTransaction IO TransactionInfo
getTransaction ctx tid =
    db & \DBLayer {..} -> do
        res <- lift $ atomically $ getTx tid
        case res of
            Nothing -> throwE
                $ ErrGetTransactionNoSuchTransaction
                $ ErrNoSuchTransaction tid
            Just tx -> pure tx
  where
    db = ctx ^. dbLayer

{-------------------------------------------------------------------------------
                                  Migration
-------------------------------------------------------------------------------}

createMigrationPlan
    :: WalletLayer IO s
    -> Withdrawal
    -> IO MigrationPlan
createMigrationPlan ctx rewardWithdrawal = do
    (wallet, _, pending) <- readWallet ctx
    pp <- currentProtocolParameters nl
    let txConstraints = Write.txConstraints pp (transactionWitnessTag tl)
        utxo = availableUTxO pending wallet
    pure
        $ Migration.createPlan txConstraints utxo
        $ Migration.RewardWithdrawal
        $ withdrawalToCoin rewardWithdrawal
  where
    nl = ctx ^. networkLayer
    tl = transactionLayer_ ctx

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

newtype Fee = Fee { feeToCoin :: Coin }
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, NFData)

newtype Percentile (n :: Nat) a = Percentile a
    deriving stock (Show, Generic)
    deriving newtype (Eq, Ord, NFData)

data DelegationFee = DelegationFee
    { feePercentiles :: (Percentile 10 Fee, Percentile 90 Fee)
    , deposit :: Coin
    } deriving (Generic)

instance NFData DelegationFee

getStakeKeyDeposit
    :: forall era. Write.IsRecentEra era
    => Write.ProtocolParameters era
    -> Coin
getStakeKeyDeposit = toWallet
    . Write.stakeKeyDeposit (recentEra @era)
    . Write.pparamsLedger

isStakeKeyRegistered
    :: Functor stm
    => DBVar stm (DeltaWalletState s)
    -> stm Bool
isStakeKeyRegistered walletState =
    Dlgs.isStakeKeyRegistered . view #delegations
        <$> readDBVar walletState

delegationFee
    :: forall s
     . ( AddressBookIso s
       , WalletFlavor s
       , Excluding '[SharedKey] (KeyOf s)
       )
    => DBLayer IO s
    -> NetworkLayer IO Read.Block
    -> TransactionLayer (KeyOf s) 'CredFromKeyK SealedTx
    -> ChangeAddressGen s
    -> IO DelegationFee
delegationFee db@DBLayer{..} netLayer txLayer changeAddressGen = do
    (Write.InAnyRecentEra era protocolParams, timeTranslation)
        <- readNodeTipStateForTxWrite netLayer
    feePercentiles <- transactionFee
        db protocolParams txLayer timeTranslation changeAddressGen
        defaultTransactionCtx
        -- It would seem that we should add a delegation action
        -- to the partial tx we construct, this was not done
        -- previously, and the difference should be negligible.
        (PreSelection [])
    deposit <- liftIO
        $ atomically (isStakeKeyRegistered walletState) <&> \case
            False -> toWallet
                $ Write.stakeKeyDeposit era
                $ Write.pparamsLedger protocolParams
            True -> Coin 0
    pure DelegationFee { feePercentiles, deposit }

transactionFee
    :: forall s era
     . ( AddressBookIso s
       , Write.IsRecentEra era
       , WalletFlavor s
       , Excluding '[SharedKey] (KeyOf s)
       )
    => DBLayer IO s
    -> Write.ProtocolParameters era
    -> TransactionLayer (KeyOf s) 'CredFromKeyK SealedTx
    -> TimeTranslation
    -> ChangeAddressGen s
    -> TransactionCtx
    -> PreSelection
    -> IO (Percentile 10 Fee, Percentile 90 Fee)
transactionFee DBLayer{atomically, walletState} protocolParams txLayer
    timeTranslation changeAddressGen txCtx preSelection = do
        wallet <- liftIO . atomically
            $ readDBVar walletState <&> WalletState.getLatest
        utxoIndex <-
            -- Important:
            --
            -- Since it's potentially expensive to construct a UTxO index, we
            -- really want to avoid constructing the index more than once.
            --
            -- In order to avoid accidentally passing an unevaluated thunk to
            -- the 'calculateFeePercentiles' function (which might lead to
            -- repeatedly evaluating the index on every iteration of the fee
            -- calculation loop), we first evaluate the index to WHNF.
            --
            -- Evaluating to WHNF should be enough to ensure that the index is
            -- fully evaluated, as all fields of the 'UTxOIndex' type are
            -- strict, and each field is defined in terms of 'Data.Map.Strict'.
            --
            evaluate $ constructUTxOIndex $ availableUTxO mempty wallet
        unsignedTxBody <- wrapErrMkTransaction $
            mkUnsignedTransaction txLayer @era
                (Left $ unsafeShelleyOnlyGetRewardXPub @s (getState wallet))
                txCtx
                (Left preSelection)

        let ptx = PartialTx
                { tx = Cardano.Tx unsignedTxBody []
                , inputs = Cardano.UTxO mempty
                , redeemers = []
                }

        wrapErrSelectAssets $ calculateFeePercentiles $ do
            res <- runExceptT $
                    balanceTransaction @_ @_ @s
                        nullTracer
                        (utxoAssumptionsForWallet (walletFlavor @s))
                        protocolParams
                        timeTranslation
                        utxoIndex
                        changeAddressGen
                        (getState wallet)
                        ptx
            case res of
                Right (Cardano.Tx (Cardano.TxBody bodyContent) _, _updatedWallet)
                    -> pure $ case Cardano.txFee bodyContent of
                        Cardano.TxFeeExplicit _ coin
                            -> Fee (fromCardanoLovelace coin)
                        Cardano.TxFeeImplicit Cardano.TxFeesImplicitInByronEra
                            -> case Write.recentEra @era of {}
                Left (ErrBalanceTxSelectAssets errSelectAssets)
                    -> throwE errSelectAssets
                Left otherErr -> throwIO $ ExceptionBalanceTx otherErr
  where
    wrapErrSelectAssets
        = throwWrappedErr ExceptionSelectAssets

    wrapErrMkTransaction
        = throwWrappedErr (ExceptionConstructTx . ErrConstructTxBody)
        . ExceptT
        . pure

    throwWrappedErr
        :: (Exception e, MonadIO m)
        => (e' -> e)
        -> ExceptT e' m a ->
        m a
    throwWrappedErr f a = either (throwIO . f) pure =<< runExceptT a

-- | Repeatedly (100 times) runs given transaction fee estimation calculation
-- returning 1st and 9nth decile (10nth and 90nth percentile) values of a
-- recoded distribution.
calculateFeePercentiles
    :: forall m
     . Monad m
    => ExceptT ErrSelectAssets m Fee
    -> ExceptT ErrSelectAssets m (Percentile 10 Fee, Percentile 90 Fee)
calculateFeePercentiles
    = fmap deciles
    . handleErrors
    . replicateM repeats
    . runExceptT
    . fmap (unCoin . feeToCoin)
    . (`catchE` handleCannotCover)
  where
    -- Use method R-8 from to get top 90%.
    -- https://en.wikipedia.org/wiki/Quantile#Estimating_quantiles_from_a_sample
    deciles
        = mkFeePercentiles
        . map round
        . V.toList
        . quantiles medianUnbiased (V.fromList [1, 10]) 10
        . V.fromList
        . map fromIntegral

    mkFeePercentiles = \case
        [a, b] -> let pf = Percentile . Fee . Coin.fromNatural in (pf a, pf b)
        _ -> error "calculateFeePercentiles: impossible"

    -- Remove failed coin selections from samples. Unless they all failed, in
    -- which case pass on the error.
    handleErrors :: m [Either err a] -> ExceptT err m [a]
    handleErrors = ExceptT . fmap skipFailed
      where
        skipFailed samples = case partitionEithers samples of
            ([], []) -> error "calculateFeePercentiles: impossible empty list"
            ((e:_), []) -> Left e
            (_, samples') -> Right samples'

    repeats = 100 -- TODO: modify repeats based on data

    -- | When estimating fee, it is rather cumbersome to return
    -- "cannot cover fee" if clients are just asking for an estimation.
    -- Therefore, we convert "cannot cover" errors into the necessary
    -- fee amount, even though there isn't enough in the wallet
    -- to cover for these fees.
    handleCannotCover :: ErrSelectAssets -> ExceptT ErrSelectAssets m Fee
    handleCannotCover = \case
        ErrSelectAssetsSelectionError
            ( SelectionBalanceErrorOf
                ( UnableToConstructChange
                    UnableToConstructChangeError{requiredCost}
                )
            ) -> pure $ Fee requiredCost
        e -> throwE e

-- | Make a pair of fee estimation percentiles more imprecise.
--
-- For a given number of bytes `n` and interval `(p, q)`, this function
-- computes the superinterval `(p - x, q + x)`, where `x` is the cost of
-- encoding `n` bytes according to the given protocol parameters.
padFeePercentiles
    :: Write.FeePerByte
    -> Quantity "byte" Word
    -- ^ Number of bytes by which to extend the interval in both directions.
    -> (Percentile 10 Fee, Percentile 90 Fee)
    -> (Percentile 10 Fee, Percentile 90 Fee)
padFeePercentiles
    feePerByte
    (Quantity byteDelta)
    (Percentile (Fee a), Percentile (Fee b)) =
        ( Percentile $ Fee $ a `Coin.difference` coinDelta
        , Percentile $ Fee $ b `Coin.add` coinDelta
        )
  where
    coinDelta :: Coin
    coinDelta =
        toWallet $ Write.feeOfBytes feePerByte (intCast byteDelta)

{-------------------------------------------------------------------------------
                                  Key Store
-------------------------------------------------------------------------------}
-- | The password here undergoes PBKDF2 encryption using HMAC
-- with the hash algorithm SHA512 which is realized in encryptPassphrase
attachPrivateKeyFromPwdScheme
    :: WalletLayer IO s
    -> (KeyOf s 'RootK XPrv, (PassphraseScheme, Passphrase "user"))
    -> IO ()
attachPrivateKeyFromPwdScheme ctx (xprv, (scheme, pwd)) = db & \_ -> do
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
        Right () -> attachPrivateKey db (xprv, hpwd) scheme
        Left{} -> fail
            "Awe crap! The passphrase given to 'attachPrivateKeyFromPwd' wasn't \
            \rightfully constructed. This is a programmer error. Look for calls \
            \to this function and make sure that the given Passphrase wasn't not \
            \prepared using 'EncryptWithScrypt'!"
  where
    db = ctx ^. dbLayer

attachPrivateKeyFromPwd
    :: WalletLayer IO s
    -> (KeyOf s 'RootK XPrv, Passphrase "user")
    -> IO ()
attachPrivateKeyFromPwd ctx (xprv, pwd) =
    attachPrivateKeyFromPwdScheme ctx
       (xprv, (currentPassphraseScheme, pwd))

-- | The hash here is the output of Scrypt function with the following parameters:
-- - logN = 14
-- - r = 8
-- - p = 1
-- - bytesNumber = 64
attachPrivateKeyFromPwdHashByron
    :: WalletLayer IO s
    -> (KeyOf s 'RootK XPrv, PassphraseHash)
    -> IO ()
attachPrivateKeyFromPwdHashByron ctx (xprv, hpwd) = db & \_ ->
    -- NOTE Only legacy wallets are imported through this function, passphrase
    -- were encrypted with the legacy scheme (Scrypt).
    attachPrivateKey db (xprv, hpwd) EncryptWithScrypt
  where
    db = ctx ^. dbLayer

attachPrivateKeyFromPwdHashShelley
    :: WalletLayer IO s
    -> (KeyOf s 'RootK XPrv, PassphraseHash)
    -> IO ()
attachPrivateKeyFromPwdHashShelley ctx (xprv, hpwd) = db & \_ ->
    attachPrivateKey db (xprv, hpwd) currentPassphraseScheme
  where
    db = ctx ^. dbLayer

attachPrivateKey
    :: DBLayer IO s
    -> (KeyOf s 'RootK XPrv, PassphraseHash)
    -> PassphraseScheme
    -> IO ()
attachPrivateKey db pk  scheme = db & \DBLayer{..} -> do
    now <- liftIO getCurrentTime
    atomically $ do
        putPrivateKey walletState pk
        meta <- readWalletMeta walletState
        let modify x = x
                { passphraseInfo = Just $ WalletPassphraseInfo
                    { lastUpdatedAt = now
                    , passphraseScheme = scheme
                    }
                }
        putWalletMeta walletState (modify meta)

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
    :: forall s e a
     . DBLayer IO s
    -> WalletId
    -> Passphrase "user"
    -> (ErrWithRootKey -> e)
    -> (KeyOf s 'RootK XPrv -> PassphraseScheme -> ExceptT e IO a)
    -> ExceptT e IO a
withRootKey DBLayer{..} wid pwd embed action = do
    (xprv, scheme) <- withExceptT embed . ExceptT . atomically $ do
        wMetadata <- readWalletMeta walletState
        let mScheme = passphraseScheme <$> passphraseInfo wMetadata
        mXPrv <- readPrivateKey walletState
        pure $ case (mXPrv, mScheme) of
            (Just (xprv, hpwd), Just scheme) ->
                case checkPassphrase scheme pwd hpwd of
                    Left err -> Left $ ErrWithRootKeyWrongPassphrase wid err
                    Right _ -> Right (xprv, scheme)
            _ -> Left $ ErrWithRootKeyNoRootKey wid
    action xprv scheme

-- | Sign an arbitrary transaction metadata object with a private key belonging
-- to the wallet's account.
--
-- This is experimental, and will likely be replaced by a more robust to
-- arbitrary message signing using COSE, or a subset of it.
signMetadataWith
    :: forall s n k.
        ( HardDerivation k
        , AddressIndexDerivationType k ~ 'Soft
        , WalletFlavor s
        , s ~ SeqState n k
        )
    => WalletLayer IO s
    -> WalletId
    -> Passphrase "user"
    -> (Role, DerivationIndex)
    -> TxMetadata
    -> ExceptT ErrSignMetadataWith IO (Signature TxMetadata)
signMetadataWith ctx wid pwd (role_, ix) metadata = db & \DBLayer{..} -> do
    addrIx <- withExceptT ErrSignMetadataWithInvalidIndex $ guardSoftIndex ix

    cp <- lift $ atomically readCheckpoint

    withRootKey db wid pwd ErrSignMetadataWithRootKey $ \rootK scheme -> do
        let encPwd = preparePassphrase scheme pwd
            DerivationPrefix (_, _, acctIx) = Seq.derivationPrefix (getState cp)
            acctK = deriveAccountPrivateKey encPwd rootK acctIx
            addrK = deriveAddressPrivateKey encPwd acctK role_ addrIx
        pure $
            Signature $ BA.convert $
            CC.sign encPwd (getRawKey (keyFlavorFromState @s) addrK) $
            hash @ByteString @Blake2b_256 $
            serialiseToCBOR metadata
  where
    db = ctx ^. dbLayer

derivePublicKey
    :: forall s k.
        ( k ~ KeyOf s
        , SoftDerivation k
        , GetAccount s k
        )
    => WalletLayer IO s
    -> Role
    -> DerivationIndex
    -> ExceptT ErrDerivePublicKey IO (k (AddressCredential k) XPub)
derivePublicKey ctx role_ ix = db & \DBLayer{..} -> do
    addrIx <- withExceptT ErrDerivePublicKeyInvalidIndex $ guardSoftIndex ix

    cp <- lift $ atomically readCheckpoint

    let acctK = getAccount $ getState cp
        addrK = deriveAddressPublicKey acctK role_ addrIx

    return addrK
  where
    db = ctx ^. dbLayer

-- | Retrieve current public account key of a wallet.
readAccountPublicKey
    :: forall s k.
        ( k ~ KeyOf s
        , GetAccount s k
        )
    => WalletLayer IO s
    -> IO (k 'AccountK XPub)
readAccountPublicKey ctx = db & \DBLayer{..} -> do
    cp <- atomically readCheckpoint
    pure $ getAccount (getState cp)
  where
    db = ctx ^. dbLayer

writePolicyPublicKey
    :: forall s n
     . s ~ SeqState n ShelleyKey
    => WalletLayer IO s
    -> WalletId
    -> Passphrase "user"
    -> ExceptT ErrWritePolicyPublicKey IO (ShelleyKey 'PolicyK XPub)
writePolicyPublicKey ctx wid pwd = db & \DBLayer{..} -> do
    cp <- lift $ atomically readCheckpoint

    let (SeqPrologue seqState) = getPrologue $ getState cp

    policyXPub <- withRootKey
        db wid pwd ErrWritePolicyPublicKeyWithRootKey $
        \rootK scheme -> do
            let encPwd = preparePassphrase scheme pwd
                xprv = derivePolicyPrivateKey encPwd
                    (getRawKey ShelleyKeyS rootK)
                    minBound
            pure $ ShelleyKey $ toXPub xprv

    let seqState' = seqState & #policyXPub .~ Just policyXPub
    lift $ atomically $ Delta.onDBVar walletState $ Delta.update
        $ \_ -> [ReplacePrologue $ SeqPrologue seqState']

    pure policyXPub
  where
    db = ctx ^. dbLayer

-- | Retrieve any public account key of a wallet.
getAccountPublicKeyAtIndex
    :: forall s k
     . ( GetPurpose k
       , WalletFlavor s
       , k ~ KeyOf s
       , AfterByron k
       )
    => WalletLayer IO s
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

    _cp <- lift $ atomically readCheckpoint
    let kf = keyFlavorFromState @s
    withRootKey db wid pwd ErrReadAccountPublicKeyRootKey
        $ \rootK scheme -> do
            let encPwd = preparePassphrase scheme pwd
                xprv = deriveAccountPrivateKeyShelley purpose encPwd
                    (getRawKey kf rootK) acctIx
            pure $ liftRawKey kf $ toXPub xprv
  where
    db = ctx ^. dbLayer

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
    :: forall s k n
     . ( s ~ SharedState n k
       , k ~ SharedKey
       , Shared.SupportsDiscovery n k
       )
    => WalletLayer IO s
    -> k 'AccountK XPub
    -> Cosigner
    -> CredentialType
    -> ExceptT ErrAddCosignerKey IO ()
updateCosigner ctx cosignerXPub cosigner cred =
    ExceptT . onWalletState ctx . Delta.updateWithError
        $ updateCosigner'
  where
    kF = keyFlavorFromState @s
    updateCosigner' wallet =
        case addCosignerAccXPub kF (cosigner, cosignerXPub) cred s0 of
            Left err -> Left $ ErrAddCosignerKey err
            Right s1 -> Right $ case ready s1 of
                Shared.Pending -> prologueUpdate s1
                Shared.Active _ -> prologueUpdate s1 ++ discoveriesUpdate s1
      where
        cp = getLatest wallet
        s0 = getState cp
        prologueUpdate s =
            [ReplacePrologue $ getPrologue s]
        wc@(WS.WalletCheckpoint bh utxo' _) = snd $ fromWallet cp
        discoveriesUpdate s =
            [ UpdateCheckpoints
                [ PutCheckpoint
                    (getSlot wc)
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
    | ErrSignMetadataWithInvalidIndex (ErrInvalidDerivationIndex 'Soft 'CredFromKeyK)
        -- ^ User provided a derivation index outside of the 'Soft' domain
    deriving (Eq, Show)

newtype ErrDerivePublicKey
    = ErrDerivePublicKeyInvalidIndex (ErrInvalidDerivationIndex 'Soft 'CredFromKeyK)
        -- ^ User provided a derivation index outside of the 'Soft' domain
    deriving (Eq, Show)

data ErrAddCosignerKey
    = ErrAddCosignerKey ErrAddCosigner
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
    = ErrReadAccountPublicKeyInvalidAccountIndex (ErrInvalidDerivationIndex 'Hardened 'AccountK)
        -- ^ User provided a derivation index for account outside of the 'Hard' domain
    | ErrReadAccountPublicKeyInvalidPurposeIndex (ErrInvalidDerivationIndex 'Hardened 'PurposeK)
        -- ^ User provided a derivation index for purpose outside of the 'Hard' domain
    | ErrReadAccountPublicKeyRootKey ErrWithRootKey
        -- ^ The wallet exists, but there's no root key attached to it
    deriving (Eq, Show)

data ErrInvalidDerivationIndex derivation level
    = ErrIndexOutOfBound (Index derivation level) (Index derivation level) DerivationIndex
    deriving (Eq, Show)


-- | Errors that can occur when signing a transaction.
data ErrSignPayment
    = ErrSignPaymentMkTx ErrMkTransaction
    | ErrSignPaymentWithRootKey ErrWithRootKey
    | ErrSignPaymentIncorrectTTL PastHorizonException
    deriving (Show, Eq)

-- | Errors that can occur when submitting a transaction.
data ErrSubmitTransaction
    = ErrSubmitTransactionForeignWallet
    | ErrSubmitTransactionPartiallySignedOrNoSignedTx Int Int
    | ErrSubmitTransactionMultidelegationNotSupported
    deriving (Show, Eq)

-- | Errors that can occur when constructing an unsigned transaction.
data ErrConstructTx
    = ErrConstructTxWrongPayload
    | ErrConstructTxBody ErrMkTransaction
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
    | ErrConstructTxDelegationInvalid
    deriving (Show, Eq)

-- | Errors that can occur when getting policy id.
data ErrGetPolicyId
    = ErrGetPolicyIdReadPolicyPubliKey ErrReadPolicyPublicKey
    | ErrGetPolicyIdWrongMintingBurningTemplate
    deriving (Show, Eq)

-- | Errors that can occur when signing a transaction.
data ErrWitnessTx
    = ErrWitnessTxSignTx ErrSignTx
    | ErrWitnessTxWithRootKey ErrWithRootKey
    | ErrWitnessTxIncorrectTTL PastHorizonException
    deriving (Show, Eq)

-- | Errors that can occur when submitting a signed transaction to the network.
data ErrSubmitTx
    = ErrSubmitTxNetwork ErrPostTx
    | ErrSubmitTxImpossible ErrNoSuchTransaction
    deriving (Show, Eq)

-- | Errors that can occur when trying to change a wallet's passphrase.
newtype ErrUpdatePassphrase
    = ErrUpdatePassphraseWithRootKey ErrWithRootKey
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
    = ErrListTransactionsStartTimeLaterThanEndTime ErrStartTimeLaterThanEndTime
    | ErrListTransactionsMinWithdrawalWrong
    | ErrListTransactionsPastHorizonException PastHorizonException
    deriving (Show)

-- | Errors that can occur when trying to get transaction.
newtype ErrGetTransaction
    = ErrGetTransactionNoSuchTransaction ErrNoSuchTransaction
    deriving (Show, Eq)

-- | Indicates that the specified start time is later than the specified end
-- time.
data ErrStartTimeLaterThanEndTime = ErrStartTimeLaterThanEndTime
    { errStartTime :: UTCTime
    , errEndTime :: UTCTime
    } deriving (Show, Eq)

data ErrCreateMigrationPlan
    = ErrCreateMigrationPlanEmpty
    deriving (Generic, Eq, Show)

data ErrStakePoolDelegation
    = ErrStakePoolJoin ErrCannotJoin
    | ErrStakePoolQuit ErrCannotQuit
    deriving (Show)

-- | Errors that can occur when fetching the reward balance of a wallet
data ErrFetchRewards
    = ErrFetchRewardsReadRewardAccount ErrReadRewardAccount
    | ErrFetchRewardsMissingRewardAccount
    deriving (Generic, Eq, Show)

data ErrCheckWalletIntegrity
    = ErrCheckWalletIntegrityNoGenesisParameters
    | ErrCheckIntegrityDifferentGenesis (Hash "Genesis") (Hash "Genesis")
    deriving (Eq, Show)

instance Exception ErrCheckWalletIntegrity

-- | Can't perform given operation because the wallet died.
newtype ErrWalletNotResponding
    = ErrWalletNotResponding WalletId
    deriving (Eq, Show)

data ErrCreateRandomAddress
    = ErrIndexAlreadyExists (Index 'Hardened 'CredFromKeyK)
    | ErrCreateAddrWithRootKey ErrWithRootKey
    | ErrCreateAddressNotAByronWallet
    deriving (Generic, Eq, Show)

data ErrImportRandomAddress
    = ErrImportAddr ErrImportAddress
    | ErrImportAddressNotAByronWallet
    deriving (Generic, Eq, Show)

data ErrNotASequentialWallet
    = ErrNotASequentialWallet
    deriving (Generic, Eq, Show)

data ErrReadRewardAccount
    = ErrReadRewardAccountNotAShelleyWallet
    | ErrReadRewardAccountNotASharedWallet
    | ErrReadRewardAccountMissing
    deriving (Generic, Eq, Show)

data ErrWithdrawalNotBeneficial
    = ErrWithdrawalNotBeneficial
    deriving (Generic, Eq, Show)

data ErrReadPolicyPublicKey
    = ErrReadPolicyPublicKeyNotAShelleyWallet
    | ErrReadPolicyPublicKeyAbsent
    deriving (Generic, Eq, Show)

newtype ErrWritePolicyPublicKey
    = ErrWritePolicyPublicKeyWithRootKey ErrWithRootKey
    deriving (Generic, Eq, Show)

-- | This exception type should gradually replace all cases of `ExceptT Err*`
-- as there is no point in tracking errors at the type level
-- which represent exceptional cases and are always propagated to clients.
data WalletException
    = ExceptionNoSuchWallet ErrNoSuchWallet
    | ExceptionSignMetadataWith ErrSignMetadataWith
    | ExceptionDerivePublicKey ErrDerivePublicKey
    | ExceptionAddCosignerKey ErrAddCosignerKey
    | ExceptionConstructSharedWallet ErrConstructSharedWallet
    | ExceptionReadAccountPublicKey ErrReadAccountPublicKey
    | ExceptionSignPayment ErrSignPayment
    | ExceptionBalanceTx ErrBalanceTx
    | ExceptionWriteTxEra ErrWriteTxEra
    | ExceptionBalanceTxInternalError ErrBalanceTxInternalError
    | ExceptionSubmitTransaction ErrSubmitTransaction
    | ExceptionConstructTx ErrConstructTx
    | ExceptionGetPolicyId ErrGetPolicyId
    | ExceptionWitnessTx ErrWitnessTx
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


-- | Log messages from API server actions running in a wallet worker context.
data WalletLog
    = MsgBalanceTx BalanceTxLog
    | MsgMigrationUTxOBefore UTxOStatistics
    | MsgMigrationUTxOAfter UTxOStatistics
    | MsgRewardBalanceQuery BlockHeader
    | MsgRewardBalanceResult (Either ErrFetchRewards Coin)
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
        MsgBalanceTx msg -> toText msg
        MsgMigrationUTxOBefore summary ->
            "About to migrate the following distribution: \n" <> pretty summary
        MsgMigrationUTxOAfter summary ->
            "Expected distribution after complete migration: \n" <> pretty summary
        MsgRewardBalanceQuery bh ->
            "Updating the reward balance for block " <> pretty bh
        MsgRewardBalanceResult (Right amt) ->
            "The reward balance is " <> pretty amt
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
        MsgBalanceTx msg -> getSeverityAnnotation msg
        MsgMigrationUTxOBefore _ -> Info
        MsgMigrationUTxOAfter _ -> Info
        MsgRewardBalanceQuery _ -> Debug
        MsgRewardBalanceResult (Right _) -> Debug
        MsgRewardBalanceResult (Left _) -> Notice
        MsgRewardBalanceExited -> Notice
        MsgTxSubmit msg -> getSeverityAnnotation msg
        MsgIsStakeKeyRegistered _ -> Info

data TxSubmitLog
    = MsgSubmitTx BuiltTx (BracketLog' (Either ErrSubmitTx ()))
    | MsgSubmitExternalTx (Hash "Tx") (BracketLog' (Either ErrPostTx Tx))
    | MsgRetryPostTx (Hash "Tx") (BracketLog' (Either ErrPostTx ()))
    | MsgProcessPendingPool BracketLog
    deriving (Show, Eq)

instance ToText TxSubmitLog

instance Buildable TxSubmitLog where
    build = \case
        MsgSubmitTx BuiltTx{..} msg -> case msg of
            BracketStart -> unlinesF
                [ "Submitting transaction "+|builtTx ^. #txId|+" to local node"
                , blockMapF
                    [ ("Tx" :: Text, build builtTx)
                    , ("SealedTx", build builtSealedTx)
                    , ("TxMeta", build builtTxMeta) ]
                ]
            BracketFinish res ->
                "Transaction "+|builtTx ^. #txId|+" "+|case res of
                    Right _ -> "accepted by local node"
                    Left err -> "failed: "+||err||+""
            _ -> formatResultMsg "submitTx" [("txid", builtTx ^. #txId)] msg

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
        MsgSubmitTx _ b -> resultSeverity Info b
        MsgSubmitExternalTx _ b -> resultSeverity Info b
        MsgRetryPostTx _ b -> case b of
            BracketFinish (Right _) -> Info
            BracketException _ -> Error
            _ -> Debug
        MsgProcessPendingPool msg -> getSeverityAnnotation msg

-- | Construct the default 'ChangeAddressGen s' for a given 's'.
defaultChangeAddressGen
    :: forall s.
        ( GenChange s
        , WalletFlavor s
        )
    => ArgGenChange s
    -> ChangeAddressGen s
defaultChangeAddressGen arg =
    ChangeAddressGen
        (genChange arg)
        (maxLengthAddressFor (keyFlavorFromState @s))

-- WARNING: Must never be used to create real transactions for submission to the
-- blockchain as funds sent to a dummy change address would be irrecoverable.
dummyChangeAddressGen :: forall s. WalletFlavor s => ChangeAddressGen s
dummyChangeAddressGen =
    ChangeAddressGen
        (maxLengthAddressFor (keyFlavorFromState @s),)
        (maxLengthAddressFor (keyFlavorFromState @s))

utxoAssumptionsForWallet
    :: forall s
     . (Excluding '[SharedKey] (KeyOf s))
    => WalletFlavorS s
    -> UTxOAssumptions
utxoAssumptionsForWallet = keyOfWallet >>> \case
    ByronKeyS -> AllByronKeyPaymentCredentials
    IcarusKeyS -> AllByronKeyPaymentCredentials
    ShelleyKeyS -> AllKeyPaymentCredentials
