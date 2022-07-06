{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- These are state machine model tests for the 'DBLayer' implementations.
--
-- The basic principle is to define the simplest possible model, without real
-- types or crypto. Then generate a test case, which is a sequence of
-- actions. Run the actions on both the model and the actual implementation, and
-- check that they are "equivalent" at every step.
--
-- There is an excellent article about this testing method available at:
--   https://iohk.io/blog/an-in-depth-look-at-quickcheck-state-machine/
--
-- The example code for the article is here:
--   https://github.com/well-typed/qsm-in-depth
--
-- This module follows the article and example code (Version1.hs), pretty much
-- exactly.

module Cardano.Wallet.DB.StateMachine
    ( prop_sequential
    , prop_parallel
    , validateGenerators
    , showLabelledExamples
    , TestConstraints
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Address.Script
    ( ScriptTemplate (..) )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchTransaction (..)
    , ErrPutLocalTxSubmission (..)
    , ErrRemoveTx (..)
    , ErrWalletAlreadyExists (..)
    , cleanDB
    )
import Cardano.Wallet.DB.Arbitrary
    ( GenState, GenTxHistory (..), InitialCheckpoint (..) )
import Cardano.Wallet.DB.Pure.Implementation
    ( Database (..)
    , Err (..)
    , TxHistory
    , WalletDatabase (..)
    , emptyDatabase
    , mCleanDB
    , mInitializeWallet
    , mIsStakeKeyRegistered
    , mListCheckpoints
    , mListWallets
    , mPutCheckpoint
    , mPutDelegationCertificate
    , mPutDelegationRewardBalance
    , mPutLocalTxSubmission
    , mPutPrivateKey
    , mPutTxHistory
    , mPutWalletMeta
    , mReadCheckpoint
    , mReadDelegationRewardBalance
    , mReadGenesisParameters
    , mReadLocalTxSubmissionPending
    , mReadPrivateKey
    , mReadTxHistory
    , mReadWalletMeta
    , mRemovePendingOrExpiredTx
    , mRemoveWallet
    , mRollbackTo
    , mUpdatePendingTxForExpiry
    )
import Cardano.Wallet.DB.WalletState
    ( ErrNoSuchWallet (..) )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters, dummyTimeInterpreter )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationPrefix
    , Index
    , KeyFingerprint
    , NetworkDiscriminant (..)
    , PersistPrivateKey (..)
    , Role (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shared
    ()
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap, PendingIxs (..), SeqState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( Readiness
    , SharedAddressPool (..)
    , SharedAddressPools (..)
    , SharedState (..)
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( PassphraseHash (..) )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , ChainPoint
    , DecentralizationLevel
    , DelegationCertificate
    , EpochNo (..)
    , ExecutionUnits (..)
    , FeePolicy
    , GenesisParameters (..)
    , LinearFunction
    , PoolId (..)
    , Range (..)
    , Slot
    , SlotNo (..)
    , SortOrder (..)
    , StakeKeyCertificate
    , TokenBundleMaxSize
    , TxParameters (..)
    , WalletId (..)
    , WalletMetadata (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address, AddressState )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..)
    , LocalTxSubmissionStatus (..)
    , SealedTx (..)
    , TransactionInfo (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta
    , TxMetadata
    , TxOut (..)
    , TxScriptValidity
    , TxSize (..)
    , TxStatus
    , inputs
    , mockSealedTx
    )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Control.Foldl
    ( Fold (..) )
import Control.Monad
    ( forM_, replicateM, void, when )
import Control.Monad.IO.Unlift
    ( MonadIO )
import Control.Monad.Trans.Except
    ( mapExceptT, runExceptT )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString, hash )
import Data.Bifunctor
    ( bimap, first )
import Data.ByteString
    ( ByteString )
import Data.Foldable
    ( foldl', toList )
import Data.Functor.Classes
    ( Eq1, Show1 )
import Data.List.Extra
    ( enumerate )
import Data.Map
    ( Map )
import Data.Map.Strict.NonEmptyMap
    ( NonEmptyMap )
import Data.Maybe
    ( catMaybes, fromJust, isJust, isNothing, mapMaybe )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Set
    ( Set )
import Data.Time.Clock
    ( NominalDiffTime, diffUTCTime, getCurrentTime )
import Data.TreeDiff
    ( ToExpr (..), defaultExprViaShow, genericToExpr )
import GHC.Generics
    ( Generic, Generic1 )
import System.Random
    ( getStdRandom, randomR )
import Test.Hspec
    ( HasCallStack, SpecWith, describe, expectationFailure, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Args (..)
    , Gen
    , Property
    , applyArbitrary2
    , arbitraryBoundedEnum
    , collect
    , elements
    , frequency
    , generate
    , labelledExamplesWith
    , property
    , resize
    , (===)
    )
import Test.QuickCheck.Monadic
    ( monadicIO, run )
import Test.QuickCheck.Random
    ( mkQCGen )
import Test.StateMachine
    ( CommandNames (..)
    , Concrete
    , GenSym
    , Logic (..)
    , Reason (..)
    , Reference
    , StateMachine
    , Symbolic
    , forAllCommands
    , forall
    , member
    , prettyCommands
    , prettyParallelCommands
    , runCommands
    , runParallelCommands
    , (.==)
    )
import Test.StateMachine.Parallel
    ( forAllParallelCommands )
import Test.StateMachine.Types
    ( Command (..), Commands (..), ParallelCommands, ParallelCommandsF (..) )
import UnliftIO.Async
    ( race_ )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( evaluate )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Control.Foldl as Foldl
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.TreeDiff as Expr
import qualified Data.TreeDiff.OMap as Expr
import qualified Test.QuickCheck as QC
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2

{- HLINT ignore "Unused LANGUAGE pragma" -}

{-------------------------------------------------------------------------------
  Mock implementation
-------------------------------------------------------------------------------}

-- | The mock state type uses the model database with mock wallet ID and key
-- types.
type Mock s = Database MWid s MPrivKey

-- | Mock wallet ID -- simple and easy to read.
newtype MWid = MWid String
    deriving (Show, Eq, Ord, Generic)

-- | Convert a mock wallet ID to a real one by hashing it, then splicing the
-- mock ID in front so that both ID types are sorted in the same order.
unMockWid :: MWid -> WalletId
unMockWid (MWid wid) = WalletId m
  where
    Just m = digestFromByteString spliced
    spliced = wid' <> B8.drop (B8.length wid') hashed
    hashed = BA.convert (hash wid' :: Digest Blake2b_160)
    wid' = B8.pack wid

-- | Represent (XPrv, Hash) as a string.
type MPrivKey = String

class PersistPrivateKey k => MockPrivKey k where
    -- | Stuff a mock private key into the type used by 'DBLayer'.
    fromMockPrivKey
        :: MPrivKey
        -> (k XPrv, PassphraseHash)

    -- | Unstuff the DBLayer private key into the mock type.
    toMockPrivKey :: (k XPrv, PassphraseHash) -> MPrivKey
    toMockPrivKey (_, h) = B8.unpack (BA.convert h)

zeroes :: ByteString
zeroes = B8.replicate 256 '0'

instance MockPrivKey (ShelleyKey 'RootK) where
    fromMockPrivKey s = (k, unMockPrivKeyHash s)
      where (k, _) = unsafeDeserializeXPrv (zeroes, mempty)

instance MockPrivKey (SharedKey 'RootK) where
    fromMockPrivKey s = (k, unMockPrivKeyHash s)
      where (k, _) = unsafeDeserializeXPrv (zeroes, mempty)

instance MockPrivKey (ByronKey 'RootK) where
    fromMockPrivKey s = (k, unMockPrivKeyHash s)
      where (k, _) = unsafeDeserializeXPrv (zeroes <> ":", mempty)

unMockPrivKeyHash :: MPrivKey -> PassphraseHash
unMockPrivKeyHash = PassphraseHash .  BA.convert . B8.pack

unMockTxId :: HasCallStack => Hash "Tx" -> SealedTx
unMockTxId = mockSealedTx . getHash

reMockTxId :: SealedTx -> Hash "Tx"
reMockTxId = Hash . serialisedTx

{-------------------------------------------------------------------------------
  Language
-------------------------------------------------------------------------------}

data Cmd s wid
    = CleanDB
    | CreateWallet MWid (Wallet s) WalletMetadata TxHistory GenesisParameters
    | RemoveWallet wid
    | ListWallets
    | PutCheckpoint wid (Wallet s)
    | ReadCheckpoint wid
    | ListCheckpoints wid
    | PutWalletMeta wid WalletMetadata
    | ReadWalletMeta wid
    | PutTxHistory wid TxHistory
    | ReadTxHistory wid
        (Maybe Coin)
        SortOrder
        (Range SlotNo)
        (Maybe TxStatus)
    | GetTx wid (Hash "Tx")
    | PutLocalTxSubmission wid (Hash "Tx") SlotNo
    | ReadLocalTxSubmissionPending wid
    | UpdatePendingTxForExpiry wid SlotNo
    | RemovePendingOrExpiredTx wid (Hash "Tx")
    | PutPrivateKey wid MPrivKey
    | ReadPrivateKey wid
    | ReadGenesisParameters wid
    | RollbackTo wid Slot
    | PutDelegationCertificate wid DelegationCertificate SlotNo
    | IsStakeKeyRegistered wid
    | PutDelegationRewardBalance wid Coin
    | ReadDelegationRewardBalance wid
    deriving stock (Show, Generic1, Eq, Functor, Foldable, Traversable)
    deriving anyclass (CommandNames)

data Success s wid
    = Unit ()
    | NewWallet wid
    | WalletIds [wid]
    | Checkpoint (Maybe (Wallet s))
    | Metadata (Maybe WalletMetadata)
    | TxHistory [TransactionInfo]
    | LocalTxSubmission [LocalTxSubmissionStatus (Hash "Tx")]
    | PrivateKey (Maybe MPrivKey)
    | GenesisParams (Maybe GenesisParameters)
    | ChainPoints [ChainPoint]
    | Point ChainPoint
    | DelegationRewardBalance Coin
    | StakeKeyStatus Bool
    deriving stock (Show, Generic1, Eq, Functor, Foldable, Traversable)

newtype Resp s wid
    = Resp (Either (Err wid) (Success s wid))
    deriving (Show, Eq)

instance Functor (Resp s) where
    fmap f (Resp r) = Resp (bimap (fmap f) (fmap f) r)

instance Foldable (Resp s) where
    foldMap f (Resp r) = either (foldMap f) (foldMap f) r

instance Traversable (Resp s) where
    traverse f (Resp (Right r)) = Resp . Right <$> traverse f r
    traverse f (Resp (Left e)) = Resp . Left <$> traverse f e

{-------------------------------------------------------------------------------
  Interpreter: mock implementation
-------------------------------------------------------------------------------}

runMock :: HasCallStack => Cmd s MWid -> Mock s -> (Resp s MWid, Mock s)
runMock = \case
    CleanDB ->
        first (Resp . fmap Unit) . mCleanDB
    CreateWallet wid wal meta txs gp ->
        first (Resp . fmap (const (NewWallet wid)))
            . mInitializeWallet wid wal meta txs gp
    RemoveWallet wid ->
        first (Resp . fmap Unit) . mRemoveWallet wid
    ListWallets ->
        first (Resp . fmap WalletIds) . mListWallets
    PutCheckpoint wid wal ->
        first (Resp . fmap Unit) . mPutCheckpoint wid wal
    ListCheckpoints wid ->
        first (Resp . fmap ChainPoints) . mListCheckpoints wid
    ReadCheckpoint wid ->
        first (Resp . fmap Checkpoint) . mReadCheckpoint wid
    PutWalletMeta wid meta ->
        first (Resp . fmap Unit) . mPutWalletMeta wid meta
    ReadWalletMeta wid ->
        first (Resp . fmap Metadata) . mReadWalletMeta timeInterpreter wid
    PutDelegationCertificate wid cert sl ->
        first (Resp . fmap Unit) . mPutDelegationCertificate wid cert sl
    IsStakeKeyRegistered wid ->
        first (Resp . fmap StakeKeyStatus) . mIsStakeKeyRegistered wid
    PutTxHistory wid txs ->
        first (Resp . fmap Unit) . mPutTxHistory wid txs
    ReadTxHistory wid minW order range status ->
        first (Resp . fmap TxHistory)
        . mReadTxHistory timeInterpreter wid minW order range status
    GetTx _wid _tid ->
        first (Resp . fmap (TxHistory . maybe [] pure))
        -- TODO: Implement mGetTx
        -- . mGetTx wid tid
        . (Right Nothing,)
    PutLocalTxSubmission wid tid sl ->
        first (Resp . fmap Unit)
        . mPutLocalTxSubmission wid tid (unMockTxId tid) sl
    ReadLocalTxSubmissionPending wid ->
        first (Resp . fmap (LocalTxSubmission . map (fmap reMockTxId)))
        . mReadLocalTxSubmissionPending wid
    UpdatePendingTxForExpiry wid sl ->
        first (Resp . fmap Unit) . mUpdatePendingTxForExpiry wid sl
    RemovePendingOrExpiredTx wid tid ->
        first (Resp . fmap Unit) . mRemovePendingOrExpiredTx wid tid
    PutPrivateKey wid pk ->
        first (Resp . fmap Unit) . mPutPrivateKey wid pk
    ReadPrivateKey wid ->
        first (Resp . fmap PrivateKey) . mReadPrivateKey wid
    ReadGenesisParameters wid ->
        first (Resp . fmap GenesisParams) . mReadGenesisParameters wid
    PutDelegationRewardBalance wid amt ->
        first (Resp . fmap Unit) . mPutDelegationRewardBalance wid amt
    ReadDelegationRewardBalance wid ->
        first (Resp . fmap DelegationRewardBalance)
        . mReadDelegationRewardBalance wid
    RollbackTo wid point ->
        first (Resp . fmap Point) . mRollbackTo wid point
  where
    timeInterpreter = dummyTimeInterpreter

{-------------------------------------------------------------------------------
  Interpreter: real I/O
-------------------------------------------------------------------------------}

runIO
    :: forall m s k. (MonadIO m, MockPrivKey (k 'RootK))
    => DBLayer m s k
    -> Cmd s WalletId
    -> m (Resp s WalletId)
runIO db@DBLayer{..} = fmap Resp . go
  where
    go
        :: Cmd s WalletId
        -> m (Either (Err WalletId) (Success s WalletId))
    go = \case
        CleanDB -> do
            Right . Unit <$> cleanDB db
        CreateWallet wid wal meta txs gp ->
            catchWalletAlreadyExists (const (NewWallet (unMockWid wid))) $
            mapExceptT atomically $
            initializeWallet (unMockWid wid) wal meta txs gp
        RemoveWallet wid -> catchNoSuchWallet Unit $
            mapExceptT atomically $ removeWallet wid
        ListWallets -> Right . WalletIds <$>
            atomically listWallets
        PutCheckpoint wid wal -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putCheckpoint wid wal
        ReadCheckpoint wid -> Right . Checkpoint <$>
            atomically (readCheckpoint wid)
        ListCheckpoints wid -> Right . ChainPoints <$>
            atomically (listCheckpoints wid)
        PutWalletMeta wid meta -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putWalletMeta wid meta
        ReadWalletMeta wid -> fmap (Right . Metadata) $
            atomically $ readWalletMeta wid
        PutDelegationCertificate wid pool sl -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putDelegationCertificate wid pool sl
        IsStakeKeyRegistered wid -> catchNoSuchWallet StakeKeyStatus $
            mapExceptT atomically $ isStakeKeyRegistered wid
        PutTxHistory wid txs -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putTxHistory wid txs
        ReadTxHistory wid minWith order range status ->
            fmap (Right . TxHistory) $
            atomically $
            readTxHistory wid minWith order range status
        GetTx wid tid ->
            catchNoSuchWallet (TxHistory . maybe [] pure) $
            mapExceptT atomically $ getTx wid tid
        PutLocalTxSubmission wid tid sl ->
            catchPutLocalTxSubmission Unit $
            mapExceptT atomically $
            putLocalTxSubmission wid tid (unMockTxId tid) sl
        ReadLocalTxSubmissionPending wid ->
            Right . LocalTxSubmission . map (fmap reMockTxId) <$>
            atomically (readLocalTxSubmissionPending wid)
        UpdatePendingTxForExpiry wid sl -> catchNoSuchWallet Unit $
            mapExceptT atomically $ updatePendingTxForExpiry wid sl
        RemovePendingOrExpiredTx wid tid ->
            (catchCannotRemovePendingTx wid) Unit $
            mapExceptT atomically $
            removePendingOrExpiredTx wid tid
        PutPrivateKey wid pk -> catchNoSuchWallet Unit $
            mapExceptT atomically $
            putPrivateKey wid (fromMockPrivKey pk)
        ReadPrivateKey wid -> Right . PrivateKey . fmap toMockPrivKey <$>
            atomically (readPrivateKey wid)
        ReadGenesisParameters wid -> Right . GenesisParams <$>
            atomically (readGenesisParameters wid)
        PutDelegationRewardBalance wid amt -> catchNoSuchWallet Unit $
            mapExceptT atomically $
            putDelegationRewardBalance wid amt
        ReadDelegationRewardBalance wid -> Right . DelegationRewardBalance <$>
            atomically (readDelegationRewardBalance wid)
        RollbackTo wid sl -> catchNoSuchWallet Point $
            mapExceptT atomically $ rollbackTo wid sl

    catchWalletAlreadyExists f =
        fmap (bimap errWalletAlreadyExists f) . runExceptT
    catchNoSuchWallet f =
        fmap (bimap errNoSuchWallet f) . runExceptT
    catchCannotRemovePendingTx wid f =
        fmap (bimap (errCannotRemovePendingTx wid) f) . runExceptT
    catchPutLocalTxSubmission f =
        fmap (bimap errPutLocalTxSubmission f) . runExceptT

    errNoSuchWallet :: ErrNoSuchWallet -> Err WalletId
    errNoSuchWallet (ErrNoSuchWallet wid) = NoSuchWallet wid

    errWalletAlreadyExists :: ErrWalletAlreadyExists -> Err WalletId
    errWalletAlreadyExists (ErrWalletAlreadyExists wid) = WalletAlreadyExists wid

    errNoSuchTransaction :: ErrNoSuchTransaction -> Err WalletId
    errNoSuchTransaction (ErrNoSuchTransaction wid tid) = NoSuchTx wid tid

    errCannotRemovePendingTx :: WalletId -> ErrRemoveTx -> Err WalletId
    errCannotRemovePendingTx _ (ErrRemoveTxNoSuchWallet e) =
        errNoSuchWallet e
    errCannotRemovePendingTx _ (ErrRemoveTxNoSuchTransaction e) =
        errNoSuchTransaction e
    errCannotRemovePendingTx wid (ErrRemoveTxAlreadyInLedger tid) =
        CantRemoveTxInLedger wid tid

    errPutLocalTxSubmission :: ErrPutLocalTxSubmission -> Err WalletId
    errPutLocalTxSubmission (ErrPutLocalTxSubmissionNoSuchWallet e) =
        errNoSuchWallet e
    errPutLocalTxSubmission (ErrPutLocalTxSubmissionNoSuchTransaction e) =
        errNoSuchTransaction e

{-------------------------------------------------------------------------------
  Working with references
-------------------------------------------------------------------------------}

-- | Shortcut for instantiating a Cmd/Resp with references to return values
-- which are either Concrete or Symbolic. Concrete references are actual
-- values. Symbolic references correspond to the return value of a command.
-- The Functor f will be Cmd/Resp and reference type r will be
-- Concrete/Symbolic.
newtype At f r
    = At (f (Reference WalletId r))
    deriving (Generic)

deriving instance
    Show (f (Reference WalletId r)) => Show (At f r)

type f :@ r = At f r

type RefEnv k a r = [(Reference k r, a)]

(!) :: (Eq1 r, Eq k) => RefEnv k a r -> Reference k r -> a
env ! r = fromJust (lookup r env)

{-------------------------------------------------------------------------------
  Relating the mock model to the real implementation
-------------------------------------------------------------------------------}

type WidRefs r =
    RefEnv WalletId MWid r

data Model s r
    = Model (Mock s) (WidRefs r)
    deriving (Generic)

deriving instance (Show1 r, Show s) => Show (Model s r)

initModel :: Model s r
initModel = Model emptyDatabase []

toMock :: (Functor (f s), Eq1 r) => Model s r -> f s :@ r -> f s MWid
toMock (Model _ wids) (At fr) = fmap (wids !) fr

step :: Eq1 r => Model s r -> Cmd s :@ r -> (Resp s MWid, Mock s)
step m@(Model mock _) c = runMock (toMock m c) mock

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event s r = Event
    { before :: Model s r
    , cmd :: Cmd s :@ r
    , after :: Model s r
    , mockResp :: Resp s MWid
    }

deriving instance (Show1 r, Show s) => Show (Event s r)

lockstep
    :: forall s r. Eq1 r
    => Model s   r
    -> Cmd s  :@ r
    -> Resp s :@ r
    -> Event s   r
lockstep m@(Model _ ws) c (At resp) = Event
    { before = m
    , cmd = c
    , after = Model mock' (ws <> ws')
    , mockResp = resp'
    }
  where
    (resp', mock') = step m c
    ws' :: WidRefs r
    ws' = zip (toList resp) (toList resp')

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

{- HLINT ignore generator "Use ++" -}
generator
    :: forall s. (Arbitrary (Wallet s), GenState s)
    => Model s Symbolic
    -> Maybe (Gen (Cmd s :@ Symbolic))
generator m@(Model _ wids) = Just $ frequency $ fmap (fmap At) . snd <$> concat
    [ generatorWithoutId
    , if null wids then [] else generatorWithWid (fst <$> wids)
    , if null tids then [] else generatorWithTids tids
    ]
  where
    tids = filter (not . null . snd) (txIdsFromModel m)

declareGenerator
    :: String -- ^ A readable name
    -> Int -- ^ Frequency
    -> Gen cmd -- ^ Generator
    -> (String, (Int, Gen cmd))
declareGenerator name f gen = (name, (f, gen))

generatorWithoutId
    :: forall s r. (Arbitrary (Wallet s), GenState s)
    => [(String, (Int, Gen (Cmd s (Reference WalletId r))))]
generatorWithoutId =
    [ declareGenerator "CreateWallet" 5
        $ CreateWallet
            <$> genId
            <*> (getInitialCheckpoint <$> arbitrary)
            <*> arbitrary
            <*> fmap unGenTxHistory arbitrary
            <*> pure dummyGenesisParameters
    ]
  where
    genId :: Gen MWid
    genId = MWid <$> elements ["a", "b", "c"]

generatorWithWid
    :: forall s r. (Arbitrary (Wallet s), GenState s)
    => [Reference WalletId r]
    -> [(String, (Int, Gen (Cmd s (Reference WalletId r))))]
generatorWithWid wids =
    [ declareGenerator "RemoveWallet" 3
        $ RemoveWallet <$> genId
    , declareGenerator "ListWallets" 5
        $ pure ListWallets
    , declareGenerator "PutCheckpoints" 5
        $ PutCheckpoint <$> genId <*> arbitrary
    , declareGenerator "ReadCheckpoint" 5
        $ ReadCheckpoint <$> genId
    , declareGenerator "ListCheckpoints" 5
        $ ListCheckpoints <$> genId
    , declareGenerator "PutWalletMeta" 5
        $ PutWalletMeta <$> genId <*> arbitrary
    , declareGenerator "ReadWalletMeta" 5
        $ ReadWalletMeta <$> genId
    , declareGenerator "PutDelegationCertificate" 5
        $ PutDelegationCertificate <$> genId <*> arbitrary <*> arbitrary
    , declareGenerator "IsStakeKeyRegistered" 1
        $ IsStakeKeyRegistered <$> genId
    , declareGenerator "PutTxHistory" 5
        $ PutTxHistory <$> genId <*> fmap unGenTxHistory arbitrary
    , declareGenerator "ReadTxHistory" 5
        $ ReadTxHistory
            <$> genId
            <*> genMinWithdrawal
            <*> genSortOrder
            <*> genRange
            <*> arbitrary
    , declareGenerator "ReadLocalTxSubmissionPending" 3
        $ ReadLocalTxSubmissionPending <$> genId
    , declareGenerator "UpdatePendingTxForExpiry" 4
        $ UpdatePendingTxForExpiry <$> genId <*> arbitrary
    , declareGenerator "RemovePendingOrExpiredTx" 4
        $ RemovePendingOrExpiredTx <$> genId <*> arbitrary
    , declareGenerator "PutPrivateKey" 3
        $ PutPrivateKey <$> genId <*> genPrivKey
    , declareGenerator "ReadPrivateKey" 3
        $ ReadPrivateKey <$> genId
    , declareGenerator "RollbackTo" 1
        $ RollbackTo <$> genId <*> arbitrary
    -- TODO: Implement mPrune
    -- , declareGenerator "Prune" 1
    --     $ Prune <$> genId <*> arbitrary
    , declareGenerator "ReadGenesisParameters" 1
        $ ReadGenesisParameters <$> genId
    ]
  where
    genId :: Gen (Reference WalletId r)
    genId = QC.elements wids

    genPrivKey :: Gen MPrivKey
    genPrivKey = elements ["pk1", "pk2", "pk3"]

    genSortOrder :: Gen SortOrder
    genSortOrder = arbitraryBoundedEnum

    genRange :: Gen (Range SlotNo)
    genRange = applyArbitrary2 Range

    genMinWithdrawal :: Gen (Maybe Coin)
    genMinWithdrawal = frequency
        [ (10, pure Nothing)
        , (1, Just <$> arbitrary)
        ]

generatorWithTids
    :: forall s r. (Arbitrary (Wallet s), GenState s, Eq (Reference WalletId r))
    => [(Reference WalletId r, [Hash "Tx"])]
    -> [(String, (Int, Gen (Cmd s (Reference WalletId r))))]
generatorWithTids tids =
    [ declareGenerator "PutLocalTxSubmission" 3 genValidPutLocalTxSubmission
    -- TODO: Implement mGetTx
    -- , declareGenerator "GetTx" 3
    --     $ GetTx <$> genId <*> arbitrary
    ]
  where
    -- A valid LocalTxSubmission entry references a TxMeta of the wallet.
    genValidPutLocalTxSubmission = do
        wid <- elements (fst <$> tids)
        tid <- maybe arbitrary elements $ lookup wid tids
        PutLocalTxSubmission wid tid <$> arbitrary

txIdsFromModel :: Model s r -> [(Reference WalletId r, [Hash "Tx"])]
txIdsFromModel (Model db widRefs) = mapMaybe getTids widRefs
  where
    getTids (widRef, wid) = (widRef,) . Map.keys . txHistory <$>
        Map.lookup wid (wallets db)

isUnordered :: Ord x => [x] -> Bool
isUnordered xs = xs /= L.sort xs

shrinker
    :: Arbitrary (Wallet s)
    => Cmd s :@ r
    -> [Cmd s :@ r]
shrinker (At cmd) = case cmd of
    PutCheckpoint wid wal ->
        [ At $ PutCheckpoint wid wal'
        | wal' <- shrink wal ]
    PutTxHistory wid h ->
        [ At $ PutTxHistory wid h'
        | h' <- map unGenTxHistory . shrink . GenTxHistory $ h
        ]
    CreateWallet wid wal met txs gp ->
        [ At $ CreateWallet wid wal' met' (unGenTxHistory txs') gp
        | (txs', wal', met') <- shrink (GenTxHistory txs, wal, met)
        ]
    PutWalletMeta wid met ->
        [ At $ PutWalletMeta wid met'
        | met' <- shrink met
        ]
    RollbackTo wid sid ->
        [ At $ RollbackTo wid sid'
        | sid' <- shrink sid
        ]
    ReadTxHistory wid minW so range status ->
        [ At $ ReadTxHistory wid minW so range' status
        | range' <- shrink range
        ]
    _ -> []

{-------------------------------------------------------------------------------
  The state machine proper
-------------------------------------------------------------------------------}

transition :: Eq1 r => Model s r -> Cmd s :@ r -> Resp s :@ r -> Model s r
transition m c = after . lockstep m c

precondition :: Model s Symbolic -> Cmd s :@ Symbolic -> Logic
precondition (Model _ wids) (At c) =
    forall (toList c) (`member` map fst wids)

postcondition
    :: (Eq s, Show s)
    => Model s Concrete -> Cmd s :@ Concrete -> Resp s :@ Concrete -> Logic
postcondition m c r =
    toMock (after e) r .== mockResp e
  where
    e = lockstep m c r

semantics
    :: (MonadIO m, MockPrivKey (k 'RootK))
    => DBLayer m s k
    -> Cmd s :@ Concrete
    -> m (Resp s :@ Concrete)
semantics db (At c) =
    (At . fmap QSM.reference) <$>
        runIO db (fmap QSM.concrete c)

symbolicResp :: Model s Symbolic -> Cmd s :@ Symbolic -> GenSym (Resp s :@ Symbolic)
symbolicResp m c =
    At <$> traverse (const QSM.genSym) resp
  where
    (resp, _mock') = step m c

type TestConstraints s k =
    ( MockPrivKey (k 'RootK)
    , Eq s
    , GenState s
    , Arbitrary (Wallet s)
    , ToExpr s
    )

sm
    :: (MonadIO m, TestConstraints s k)
    => DBLayer m s k
    -> StateMachine (Model s) (At (Cmd s)) m (At (Resp s))
sm db = QSM.StateMachine
    { initModel = initModel
    , transition = transition
    , precondition = precondition
    , postcondition = postcondition
    , invariant = Nothing
    , generator = generator
    , shrinker = const shrinker
    , semantics = semantics db
    , mock = symbolicResp
    , cleanup = const $ pure ()
    }

{-------------------------------------------------------------------------------
  Additional type class instances required to run the QSM tests
-------------------------------------------------------------------------------}

instance Functor f => Rank2.Functor (At f) where
    fmap = \f (At x) -> At $ fmap (lift f) x
      where
        lift :: (r x -> r' x) -> QSM.Reference x r -> QSM.Reference x r'
        lift f (QSM.Reference x) = QSM.Reference (f x)

instance Foldable f => Rank2.Foldable (At f) where
    foldMap = \f (At x) -> foldMap (lift f) x
      where
        lift :: (r x -> m) -> QSM.Reference x r -> m
        lift f (QSM.Reference x) = f x

instance Traversable t => Rank2.Traversable (At t) where
    traverse = \f (At x) -> At <$> traverse (lift f) x
      where
        lift
          :: Functor f
          => (r x -> f (r' x))
          -> QSM.Reference x r
          -> f (QSM.Reference x r')
        lift f (QSM.Reference x) = QSM.Reference <$> f x

deriving instance ToExpr s => ToExpr (Model s Concrete)

instance ToExpr s => ToExpr (Mock s) where
    toExpr = genericToExpr

instance (ToExpr k, ToExpr v) => ToExpr (NonEmptyMap k v) where
    toExpr = genericToExpr

instance ToExpr WalletId where
    toExpr = defaultExprViaShow

instance ToExpr s => ToExpr (Wallet s) where
    toExpr = genericToExpr

instance ToExpr BlockHeader where
    toExpr = genericToExpr

instance ToExpr (Hash purpose) where
    toExpr = genericToExpr

instance ToExpr b => ToExpr (Quantity a b) where
    toExpr = genericToExpr

instance ToExpr GenesisParameters where
    toExpr = defaultExprViaShow

instance ToExpr Slot where
    toExpr = genericToExpr

instance ToExpr SlotNo where
    toExpr = genericToExpr

instance ToExpr EpochNo where
    toExpr = defaultExprViaShow

instance ToExpr TxStatus where
    toExpr = genericToExpr

instance ToExpr PoolId where
    toExpr = defaultExprViaShow

instance ToExpr AddressState where
    toExpr = genericToExpr

instance (ToExpr addr, ToExpr ix) => ToExpr (AddressPool.Pool addr ix) where
    toExpr pool = Expr.Rec "Pool" $ Expr.fromList
        [ ("gap", toExpr $ AddressPool.gap pool)
        , ("addresses", toExpr $ AddressPool.addresses pool)
        ]

instance ToExpr DerivationPrefix where
    toExpr = defaultExprViaShow

instance ToExpr (Index ix typ) where
    toExpr = defaultExprViaShow

instance ToExpr (SeqState 'Mainnet ShelleyKey) where
    toExpr = defaultExprViaShow

instance ToExpr (RndState 'Mainnet) where
    toExpr = defaultExprViaShow

instance ToExpr a => ToExpr (Readiness a) where
    toExpr = genericToExpr

instance ToExpr AddressPoolGap where
    toExpr = genericToExpr

instance ToExpr ScriptTemplate where
    toExpr = defaultExprViaShow

instance ToExpr (SharedKey 'AccountK CC.XPub) where
    toExpr = defaultExprViaShow

instance ToExpr (KeyFingerprint "payment" SharedKey) where
    toExpr = defaultExprViaShow

instance ToExpr PendingIxs where
    toExpr = genericToExpr

instance ToExpr (SharedAddressPool 'UtxoExternal SharedKey) where
    toExpr = genericToExpr

instance ToExpr (SharedAddressPool 'UtxoInternal SharedKey) where
    toExpr = genericToExpr

instance ToExpr (SharedAddressPools SharedKey) where
    toExpr = genericToExpr

instance ToExpr (SharedState 'Mainnet SharedKey) where
    toExpr = genericToExpr

instance (ToExpr s, ToExpr xprv) => ToExpr (WalletDatabase s xprv) where
    toExpr = genericToExpr

instance ToExpr UTxO where
    toExpr = genericToExpr

instance ToExpr WalletMetadata where
    toExpr = defaultExprViaShow

instance ToExpr Tx where
    toExpr = genericToExpr

instance ToExpr TxScriptValidity where
    toExpr = genericToExpr

instance ToExpr TxIn where
    toExpr = genericToExpr

instance ToExpr TxMetadata where
    toExpr = defaultExprViaShow

instance ToExpr Coin where
    toExpr = genericToExpr

instance ToExpr TxOut where
    toExpr = genericToExpr

instance ToExpr TokenBundle where
    toExpr = genericToExpr

instance ToExpr TokenMap where
    toExpr = genericToExpr

instance ToExpr TokenName where
    toExpr = genericToExpr

instance ToExpr TokenPolicyId where
    toExpr = genericToExpr

instance ToExpr TokenQuantity where
    toExpr = genericToExpr

instance ToExpr Address where
    toExpr = genericToExpr

instance ToExpr TxMeta where
    toExpr = genericToExpr

instance ToExpr SealedTx where
    toExpr = defaultExprViaShow

instance ToExpr Percentage where
    toExpr = genericToExpr

instance ToExpr DecentralizationLevel where
    toExpr = genericToExpr

instance ToExpr TxSize where
    toExpr = genericToExpr

instance ToExpr TokenBundleMaxSize where
    toExpr = genericToExpr

instance ToExpr TxParameters where
    toExpr = genericToExpr

instance ToExpr ExecutionUnits where
    toExpr = genericToExpr

instance ToExpr a => ToExpr (LinearFunction a) where
    toExpr = genericToExpr

instance ToExpr FeePolicy where
    toExpr = genericToExpr

instance ToExpr Direction where
    toExpr = genericToExpr

instance ToExpr MWid where
    toExpr = defaultExprViaShow

instance ToExpr StakeKeyCertificate where
    toExpr = genericToExpr

instance ToExpr RewardAccount where
    toExpr = genericToExpr

{-------------------------------------------------------------------------------
  Tagging
-------------------------------------------------------------------------------}

-- | Interesting combinations of commands.
data Tag
    = CreateThreeWallets
      -- ^ Three different wallets created.
    | CreateWalletTwice
      -- ^ The same wallet id is used twice.
    | RemoveWalletTwice
      -- ^ The same wallet is removed twice.
    | CreateThenList
    | SuccessfulReadTxHistory
    | UnsuccessfulReadTxHistory
    | TxUnsortedInputs
      -- ^ Putting a transaction with unsorted inputs.
    | TxUnsortedOutputs
    | SuccessfulReadCheckpoint
      -- ^ Read the checkpoint of a wallet that's been created.
    | UnsuccessfulReadCheckpoint
      -- ^ No such wallet error.
    | SuccessfulReadPrivateKey
      -- ^ Private key was written then read.
    | ReadTxHistoryAfterDelete
      -- ^ wallet deleted, then tx history read.
    | PutCheckpointTwice
      -- ^ Multiple checkpoints are successfully saved to a wallet.
    | RolledBackOnce
      -- ^ We have rolled back at least once
    | RemovePendingTxTwice
      -- ^ The same pending tx is removed twice.
    | ReadMetaAfterPutCert
      -- ^ Reads wallet metadata after having inserted a delegation cert
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | The list of all possible 'Tag' values.
allTags :: [Tag]
allTags = enumerate

tag :: forall s. [Event s Symbolic] -> [Tag]
tag = Foldl.fold $ catMaybes <$> sequenceA
    [ createThreeWallets
    , createWalletTwice
    , removeWalletTwice
    , createThenList
    , readTxHistory (not . null) SuccessfulReadTxHistory
    , readTxHistory null UnsuccessfulReadTxHistory
    , txUnsorted inputs TxUnsortedInputs
    , txUnsorted outputs TxUnsortedOutputs
    , readCheckpoint isJust SuccessfulReadCheckpoint
    , readCheckpoint isNothing UnsuccessfulReadCheckpoint
    , readAfterDelete
    , countAction SuccessfulReadPrivateKey (>= 1) isReadPrivateKeySuccess
    , countAction PutCheckpointTwice (>= 2) isPutCheckpointSuccess
    , countAction RolledBackOnce (>= 1) isRollbackSuccess
    , removePendingTxTwice
    , readMetaAfterPutCert
    ]
  where
    isRollbackSuccess :: Event s Symbolic -> Maybe MWid
    isRollbackSuccess ev = case (cmd ev, mockResp ev, before ev) of
        (At (RollbackTo wid _), Resp (Right Point{}), Model _ wids ) ->
            Just (wids ! wid)
        _otherwise ->
            Nothing

    readAfterDelete :: Fold (Event s Symbolic) (Maybe Tag)
    readAfterDelete = Fold update mempty extract
      where
        update :: Map MWid Int -> Event s Symbolic -> Map MWid Int
        update created ev =
            case (isReadTxHistory ev, cmd ev, mockResp ev, before ev) of
                (Just wid, _, _, _) ->
                    Map.alter (fmap (+1)) wid created
                (Nothing
                    , At (RemoveWallet wid)
                    , Resp (Right _)
                    , Model _ wids) ->
                        Map.insert (wids ! wid) 0 created
                _otherwise ->
                    created

        extract :: Map MWid Int -> Maybe Tag
        extract created | any (> 0) created = Just ReadTxHistoryAfterDelete
                        | otherwise = Nothing

    isReadTxHistory :: Event s Symbolic -> Maybe MWid
    isReadTxHistory ev = case (cmd ev, mockResp ev, before ev) of
        (At (ReadTxHistory wid _ _ _ _), Resp (Right (TxHistory _)), Model _ wids)
            -> Just (wids ! wid)
        _otherwise
            -> Nothing

    createThreeWallets :: Fold (Event s Symbolic) (Maybe Tag)
    createThreeWallets = Fold update Set.empty extract
      where
        update :: Set MWid -> Event s Symbolic -> Set MWid
        update created ev =
            case (cmd ev, mockResp ev) of
                (At (CreateWallet wid _ _ _ _), Resp (Right _)) ->
                    Set.insert wid created
                _otherwise ->
                    created

        extract :: Set MWid -> Maybe Tag
        extract created
            | Set.size created >= 3 = Just CreateThreeWallets
            | otherwise = Nothing

    createWalletTwice :: Fold (Event s Symbolic) (Maybe Tag)
    createWalletTwice = countAction CreateWalletTwice (>= 2) match
      where
        match :: Event s Symbolic -> Maybe MWid
        match ev = case (cmd ev, mockResp ev) of
            (At (CreateWallet wid _ _ _ _), Resp _) -> Just wid
            _otherwise -> Nothing

    removeWalletTwice :: Fold (Event s Symbolic) (Maybe Tag)
    removeWalletTwice = countAction RemoveWalletTwice (>= 2) match
      where
        match ev = case (cmd ev, mockResp ev) of
            (At (RemoveWallet wid), Resp _) ->
                Just wid
            _otherwise ->
                Nothing

    removePendingTxTwice :: Fold (Event s Symbolic) (Maybe Tag)
    removePendingTxTwice = countAction RemovePendingTxTwice (>= 2) match
      where
        match ev = case (cmd ev, mockResp ev) of
            (At (RemovePendingOrExpiredTx wid _), Resp _) ->
                Just wid
            _otherwise ->
                Nothing

    countAction
        :: forall k. Ord k => Tag -> (Int -> Bool)
        -> (Event s Symbolic -> Maybe k)
        -> Fold (Event s Symbolic) (Maybe Tag)
    countAction res enough match = Fold update mempty extract
      where
        update :: Map k Int -> Event s Symbolic -> Map k Int
        update matches ev =
            case match ev of
                Just wid ->
                    Map.insertWith (+) wid 1 matches
                _otherwise ->
                    matches

        extract :: Map k Int -> Maybe Tag
        extract matches
            | any enough matches = Just res
            | otherwise = Nothing

    isReadPrivateKeySuccess :: Event s Symbolic -> Maybe MWid
    isReadPrivateKeySuccess ev = case (cmd ev, mockResp ev, before ev) of
        (At (ReadPrivateKey wid)
            , Resp (Right (PrivateKey (Just _)))
            , Model _ wids )
                -> Just (wids ! wid)
        _otherwise
            -> Nothing

    createThenList :: Fold (Event s Symbolic) (Maybe Tag)
    createThenList =
        Fold update mempty extract
      where
        update :: Map MWid Bool -> Event s Symbolic -> Map MWid Bool
        update created ev =
            case (cmd ev, mockResp ev) of
                (At (CreateWallet wid _ _ _ _), Resp (Right _)) ->
                    Map.insert wid False created
                (At ListWallets, Resp (Right (WalletIds wids))) ->
                    foldr (Map.adjust (const True)) created wids
                _otherwise ->
                    created

        extract :: Map MWid Bool -> Maybe Tag
        extract created
            | or created = Just CreateThenList
            | otherwise = Nothing

    readTxHistory
        :: ([TransactionInfo] -> Bool)
        -> Tag
        -> Fold (Event s Symbolic) (Maybe Tag)
    readTxHistory check res = Fold update False (extractf res)
      where
        update :: Bool -> Event s Symbolic -> Bool
        update didRead ev = didRead || case (cmd ev, mockResp ev) of
            (At ReadTxHistory {}, Resp (Right (TxHistory h))) ->
                check h
            _otherwise ->
                False

    txUnsorted
        :: Ord a
        => (Tx -> [a])
        -> Tag
        -> Fold (Event s Symbolic) (Maybe Tag)
    txUnsorted sel res = Fold update False (extractf res)
      where
        update :: Bool -> Event s Symbolic -> Bool
        update didRead ev = didRead ||
            case (cmd ev, mockResp ev) of
                (At (PutTxHistory _ h), Resp (Right _)) ->
                    any (isUnordered . sel . fst) h
                _otherwise ->
                    False

    readCheckpoint
        :: (Maybe (Wallet s) -> Bool)
        -> Tag
        -> Fold (Event s Symbolic) (Maybe Tag)
    readCheckpoint check res = Fold update False (extractf res)
      where
        update :: Bool -> Event s Symbolic -> Bool
        update didRead ev = didRead ||
            case (cmd ev, mockResp ev) of
                (At (ReadCheckpoint _), Resp (Right (Checkpoint cp))) ->
                    check cp
                _otherwise ->
                    False

    isPutCheckpointSuccess :: Event s Symbolic -> Maybe MWid
    isPutCheckpointSuccess ev = case (cmd ev, mockResp ev, before ev) of
        (At (PutCheckpoint wid _wal)
            , Resp (Right (Unit ()))
            , Model _ wids )
                -> Just (wids ! wid)
        _otherwise
            -> Nothing

    readMetaAfterPutCert :: Fold (Event s Symbolic) (Maybe Tag)
    readMetaAfterPutCert = Fold update mempty extract
      where
        update :: Map MWid Int -> Event s Symbolic -> Map MWid Int
        update acc ev =
            case (isReadWalletMetadata ev, cmd ev, mockResp ev, before ev) of
                (Just wid, _, _, _) ->
                    Map.alter (fmap (+1)) wid acc
                ( Nothing
                  , At (PutDelegationCertificate wid _ _)
                  , Resp (Right _)
                  , Model _ wids
                  ) ->
                    Map.insert (wids ! wid) 0 acc
                _ ->
                    acc

        extract :: Map MWid Int -> Maybe Tag
        extract created
            | any (> 0) created = Just ReadMetaAfterPutCert
            | otherwise = Nothing

    isReadWalletMetadata :: Event s Symbolic -> Maybe MWid
    isReadWalletMetadata ev = case (cmd ev, mockResp ev, before ev) of
        (At (ReadWalletMeta wid), Resp Right{}, Model _ wids) ->
            Just (wids ! wid)
        _ ->
            Nothing

    extractf :: a -> Bool -> Maybe a
    extractf a t = if t then Just a else Nothing

execCmd
    :: Model s Symbolic
    -> QSM.Command (At (Cmd s)) (At (Resp s))
    -> Event s Symbolic
execCmd model (QSM.Command cmd resp _vars) =
    lockstep model cmd resp

execCmds :: QSM.Commands (At (Cmd s)) (At (Resp s)) -> [Event s Symbolic]
execCmds = \(QSM.Commands cs) -> go initModel cs
  where
    go
        :: Model s Symbolic
        -> [QSM.Command (At (Cmd s)) (At (Resp s))]
        -> [Event s Symbolic]
    go _ [] = []
    go m (c : cs) = e : go (after e) cs where e = execCmd m c

{-------------------------------------------------------------------------------
  Finding minimal labelled examples - helper functions
-------------------------------------------------------------------------------}

showLabelledExamples :: forall s k. (TestConstraints s k) => Maybe Int -> IO ()
showLabelledExamples mReplay = do
    replaySeed <- case mReplay of
        Nothing -> getStdRandom $ randomR (1, 999999)
        Just seed -> return seed
    putStrLn $ "Using replaySeed " ++ show replaySeed
    let args = QC.stdArgs
            { maxSuccess = 10000
            , replay = Just (mkQCGen replaySeed, 0)
            }
    labelledExamplesWith args $
        forAllCommands (sm @IO @s @k dbLayerUnused) Nothing $ \cmds ->
            repeatedly collect (tag . execCmds $ cmds) (property True)

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . L.foldl' . flip

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

prop_sequential
    :: forall s k. (TestConstraints s k)
    => (IO (IO (), DBLayer IO s k))
    -> Property
prop_sequential newDB =
    QC.checkCoverage $
    forAllCommands (sm @IO @s @k dbLayerUnused) Nothing $ \cmds ->
        monadicIO $ do
            (destroyDB, db) <- run newDB
            let sm' = sm db
            (hist, _model, res) <- runCommands sm' cmds
            prettyCommands sm' hist
                $ measureTagCoverage cmds
                $ res === Ok
            run destroyDB -- fixme: bracket difficult
  where
    measureTagCoverage :: Commands (At (Cmd s)) (At (Resp s)) -> Property -> Property
    measureTagCoverage cmds prop = foldl' measureTag prop allTags
      where
        measureTag :: Property -> Tag -> Property
        measureTag p t = QC.cover 5 (t `Set.member` matchedTags) (show t) p

        matchedTags :: Set Tag
        matchedTags = Set.fromList $ tag $ execCmds cmds

prop_parallel
    :: forall s k. TestConstraints s k
    => (IO (IO (), DBLayer IO s k))
    -> Property
prop_parallel newDB =
    forAllParallelCommands (sm @IO @s @k dbLayerUnused) nThreads $ \cmds ->
    monadicIO $ do
        (destroyDB, db) <- run newDB
        let sm' = sm db
            cmds' = addCleanDB cmds
        res <- runParallelCommands sm' cmds'
        prettyParallelCommands cmds res
        run destroyDB
  where
    nThreads = Just 4

-- Controls that generators and shrinkers can run within a reasonable amount of
-- time. We have been bitten several times already by generators which took much
-- longer than what they should, causing timeouts in the test suite.
validateGenerators
    :: forall s. (Arbitrary (Wallet s), GenState s)
    => SpecWith ()
validateGenerators = describe "Validate generators & shrinkers" $ do
    forM_ allGenerators $ \(name, (_frequency, gen)) -> do
        let titleGen = "Generator for " <> name
        it titleGen $ expectWithin 1
            (pure gen)
            sanityCheckGen

        let titleShrink = "Shrinker for " <> name
        it titleShrink $ expectWithin 1
            -- NOTE: 97 is prime, i.e. not likely a multiple of any 'scale' or
            -- 'resize' arguments already given to underlying generators.
            (generate (resize 97 gen))
            (sanityCheckShrink . pure . At)
  where
    expectWithin :: NominalDiffTime -> IO a -> (a -> IO ()) -> IO ()
    expectWithin delay pre action = do
        let n = 100
        start <- getCurrentTime
        ticks <- replicateM n $ do
            a <- pre
            race_ (threadDelay (toMicro delay)) (action a)
            getCurrentTime
        let times = zipWith diffUTCTime ticks (start:ticks)
        let avg = (sum (fromEnum <$> times)) `div` n
        when (toEnum (withConfidence avg) >= delay) $
            expectationFailure $ unlines
                [ "Timed out."
                , "Min: " <> show (minimum times)
                , "Max: " <> show (maximum times)
                , "Avg: " <> show (toEnum @NominalDiffTime avg)
                ]
      where
        toMicro :: NominalDiffTime -> Int
        toMicro = (`div` 1000000) . fromEnum

        withConfidence :: Int -> Int
        withConfidence x = x * 12 `div` 10

    allGenerators = generatorWithoutId @s ++ generatorWithWid @s wids
      where wids = QSM.reference . unMockWid . MWid <$> ["a", "b", "c"]

    sanityCheckGen gen = do
        cmds <- generate (sequence [ resize s gen | s <- [0 .. 999] ])
        void . traverse evaluate $ cmds

    sanityCheckShrink = \case
        []  -> pure ()
        [x] -> sanityCheckShrink (concatMap shrinker [x])
        xs  -> sanityCheckShrink (concatMap shrinker [head xs, last xs])

-- | The commands for parallel tests are run multiple times to detect
-- concurrency problems. We need to clean the database before every run. The
-- easiest way is to add a CleanDB command at the beginning of the prefix.
addCleanDB
    :: ParallelCommands (At (Cmd s)) (At (Resp s))
    -> ParallelCommands (At (Cmd s)) (At (Resp s))
addCleanDB (ParallelCommands p s) = ParallelCommands (clean <> p) s
  where
    clean = Commands [cmd resp mempty]
    cmd = Command (At CleanDB)
    resp = At (Resp (Right (Unit ())))

dbLayerUnused :: DBLayer m s k
dbLayerUnused = error "DBLayer not used during command generation"
