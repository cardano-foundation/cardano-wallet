{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
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
    , showLabelledExamples
    ) where

import Prelude hiding
    ( elem )

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrRemovePendingTx (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , cleanDB
    )
import Cardano.Wallet.DB.Arbitrary
    ( GenState, GenTxHistory (..), InitialCheckpoint (..) )
import Cardano.Wallet.DB.Model
    ( Database
    , Err (..)
    , ErrErasePendingTx (..)
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
    , mPutPrivateKey
    , mPutProtocolParameters
    , mPutTxHistory
    , mPutWalletMeta
    , mReadCheckpoint
    , mReadDelegationRewardBalance
    , mReadPrivateKey
    , mReadProtocolParameters
    , mReadTxHistory
    , mReadWalletMeta
    , mRemovePendingTx
    , mRemoveWallet
    , mRollbackTo
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , Depth (..)
    , NetworkDiscriminant (..)
    , PersistPrivateKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPool (..), SeqState (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( Address
    , BlockHeader
    , Coin (..)
    , DecentralizationLevel
    , DelegationCertificate
    , Direction (..)
    , EpochNo (..)
    , FeePolicy
    , GenesisParameters
    , Hash (..)
    , Hash (..)
    , PoolId (..)
    , ProtocolParameters (..)
    , Range (..)
    , SlotId (..)
    , SlotInEpoch (..)
    , SortOrder (..)
    , StakeKeyCertificate
    , TransactionInfo (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxParameters (..)
    , TxStatus
    , UTxO (..)
    , WalletId (..)
    , WalletMetadata (..)
    , inputs
    )
import Control.Foldl
    ( Fold (..) )
import Control.Monad.IO.Class
    ( liftIO )
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
import Data.Maybe
    ( catMaybes, fromJust, isJust, isNothing )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Set
    ( Set )
import Data.TreeDiff
    ( ToExpr (..), defaultExprViaShow, genericToExpr )
import Data.Word
    ( Word64 )
import GHC.Generics
    ( Generic )
import System.Random
    ( getStdRandom, randomR )
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
    , labelledExamplesWith
    , property
    , (===)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )
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
    , elem
    , forAllCommands
    , forAllParallelCommands
    , forall
    , prettyCommands
    , prettyParallelCommands
    , runCommands
    , runParallelCommands
    , (.==)
    )
import Test.StateMachine.Types
    ( Command (..), Commands (..), ParallelCommands, ParallelCommandsF (..) )

import qualified Cardano.Crypto.Wallet as CC
import qualified Control.Foldl as Foldl
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Test.QuickCheck as QC
import qualified Test.StateMachine.Types as QSM
import qualified Test.StateMachine.Types.Rank2 as Rank2

{-# ANN module ("HLint: ignore Unused LANGUAGE pragma" :: String) #-}

{-------------------------------------------------------------------------------
  Mock implementation
-------------------------------------------------------------------------------}

-- | The mock state type uses the model database with mock wallet ID and key
-- types.
type Mock s = Database MWid s MPrivKey

-- | Mock wallet ID -- simple and easy to read.
newtype MWid = MWid String
    deriving (Show, Eq, Ord, Generic)

widPK :: MWid -> PrimaryKey WalletId
widPK = PrimaryKey . unMockWid

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
        -> (k XPrv, Hash "encryption")

    -- | Unstuff the DBLayer private key into the mock type.
    toMockPrivKey
        :: (k XPrv, Hash "encryption")
        -> MPrivKey
    toMockPrivKey (_, Hash h) =
        B8.unpack h

zeroes :: ByteString
zeroes = B8.replicate 256 '0'

instance MockPrivKey (JormungandrKey 'RootK) where
    fromMockPrivKey s = (k, Hash (B8.pack s))
      where (k, _) = unsafeDeserializeXPrv (zeroes, mempty)

instance MockPrivKey (ByronKey 'RootK) where
    fromMockPrivKey s = (k, Hash (B8.pack s))
      where (k, _) = unsafeDeserializeXPrv (zeroes <> ":", mempty)

{-------------------------------------------------------------------------------
  Language
-------------------------------------------------------------------------------}

data Cmd s wid
    = CleanDB
    | CreateWallet MWid (Wallet s) WalletMetadata TxHistory ProtocolParameters
    | RemoveWallet wid
    | ListWallets
    | PutCheckpoint wid (Wallet s)
    | ReadCheckpoint wid
    | ListCheckpoints wid
    | PutWalletMeta wid WalletMetadata
    | ReadWalletMeta wid
    | PutTxHistory wid TxHistory
    | ReadTxHistory wid SortOrder (Range SlotId) (Maybe TxStatus)
    | PutPrivateKey wid MPrivKey
    | ReadPrivateKey wid
    | PutProtocolParameters wid ProtocolParameters
    | ReadProtocolParameters wid
    | RollbackTo wid SlotId
    | RemovePendingTx wid (Hash "Tx")
    | PutDelegationCertificate wid DelegationCertificate SlotId
    | IsStakeKeyRegistered wid
    | PutDelegationRewardBalance wid (Quantity "lovelace" Word64)
    | ReadDelegationRewardBalance wid
    deriving (Show, Functor, Foldable, Traversable)

data Success s wid
    = Unit ()
    | NewWallet wid
    | WalletIds [wid]
    | Checkpoint (Maybe (Wallet s))
    | Metadata (Maybe WalletMetadata)
    | TxHistory [TransactionInfo]
    | PrivateKey (Maybe MPrivKey)
    | ProtocolParams (Maybe ProtocolParameters)
    | BlockHeaders [BlockHeader]
    | Point SlotId
    | DelegationRewardBalance (Quantity "lovelace" Word64)
    | StakeKeyStatus Bool
    deriving (Show, Eq, Functor, Foldable, Traversable)

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

runMock :: Cmd s MWid -> Mock s -> (Resp s MWid, Mock s)
runMock = \case
    CleanDB ->
        first (Resp . fmap Unit) . mCleanDB
    CreateWallet wid wal meta txs pp ->
        first (Resp . fmap (const (NewWallet wid)))
            . mInitializeWallet wid wal meta txs pp
    RemoveWallet wid ->
        first (Resp . fmap Unit) . mRemoveWallet wid
    ListWallets ->
        first (Resp . fmap WalletIds) . mListWallets
    PutCheckpoint wid wal ->
        first (Resp . fmap Unit) . mPutCheckpoint wid wal
    ListCheckpoints wid ->
        first (Resp . fmap BlockHeaders) . mListCheckpoints wid
    ReadCheckpoint wid ->
        first (Resp . fmap Checkpoint) . mReadCheckpoint wid
    PutWalletMeta wid meta ->
        first (Resp . fmap Unit) . mPutWalletMeta wid meta
    ReadWalletMeta wid ->
        first (Resp . fmap Metadata) . mReadWalletMeta wid
    PutDelegationCertificate wid cert sl ->
        first (Resp . fmap Unit) . mPutDelegationCertificate wid cert sl
    IsStakeKeyRegistered wid ->
        first (Resp . fmap StakeKeyStatus) . mIsStakeKeyRegistered wid
    PutTxHistory wid txs ->
        first (Resp . fmap Unit) . mPutTxHistory wid txs
    ReadTxHistory wid order range status ->
        first (Resp . fmap TxHistory) . mReadTxHistory wid order range status
    PutPrivateKey wid pk ->
        first (Resp . fmap Unit) . mPutPrivateKey wid pk
    ReadPrivateKey wid ->
        first (Resp . fmap PrivateKey) . mReadPrivateKey wid
    PutProtocolParameters wid pp ->
        first (Resp . fmap Unit) . mPutProtocolParameters wid pp
    ReadProtocolParameters wid ->
        first (Resp . fmap ProtocolParams) . mReadProtocolParameters wid
    PutDelegationRewardBalance wid amt ->
        first (Resp . fmap Unit) . mPutDelegationRewardBalance wid amt
    ReadDelegationRewardBalance wid ->
        first (Resp . fmap DelegationRewardBalance) . mReadDelegationRewardBalance wid
    RollbackTo wid sl ->
        first (Resp . fmap Point) . mRollbackTo wid sl
    RemovePendingTx wid tid ->
        first (Resp . fmap Unit) . mRemovePendingTx wid tid

{-------------------------------------------------------------------------------
  Interpreter: real I/O
-------------------------------------------------------------------------------}

-- | Type alias for the 'DBLayer', just to reduce noise in type signatures. This
-- 'DBLayer' is specialized to a dummy node backend.
type DBLayerTest s k = DBLayer IO s k

runIO
    :: forall s k. (MockPrivKey (k 'RootK))
    => DBLayerTest s k
    -> Cmd s WalletId
    -> IO (Resp s WalletId)
runIO db@DBLayer{..} = fmap Resp . go
  where
    go
        :: Cmd s WalletId
        -> IO (Either (Err WalletId) (Success s WalletId))
    go = \case
        CleanDB -> do
            Right . Unit <$> cleanDB db
        CreateWallet wid wal meta txs pp ->
            catchWalletAlreadyExists (const (NewWallet (unMockWid wid))) $
            mapExceptT atomically $
            initializeWallet (widPK wid) wal meta txs pp
        RemoveWallet wid -> catchNoSuchWallet Unit $
            mapExceptT atomically $ removeWallet (PrimaryKey wid)
        ListWallets -> Right . WalletIds . fmap unPrimaryKey <$>
            atomically listWallets
        PutCheckpoint wid wal -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putCheckpoint (PrimaryKey wid) wal
        ReadCheckpoint wid -> Right . Checkpoint <$>
            atomically (readCheckpoint $ PrimaryKey wid)
        ListCheckpoints wid -> Right . BlockHeaders <$>
            atomically (listCheckpoints $ PrimaryKey wid)
        PutWalletMeta wid meta -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putWalletMeta (PrimaryKey wid) meta
        ReadWalletMeta wid -> Right . Metadata <$>
            atomically (readWalletMeta $ PrimaryKey wid)
        PutDelegationCertificate wid pool sl -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putDelegationCertificate (PrimaryKey wid) pool sl
        IsStakeKeyRegistered wid -> catchNoSuchWallet StakeKeyStatus $
            mapExceptT atomically $ isStakeKeyRegistered (PrimaryKey wid)
        PutTxHistory wid txs -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putTxHistory (PrimaryKey wid) txs
        ReadTxHistory wid order range status -> Right . TxHistory <$>
            atomically (readTxHistory (PrimaryKey wid) order range status)
        RemovePendingTx wid tid -> catchCannotRemovePendingTx Unit $
            mapExceptT atomically $ removePendingTx (PrimaryKey wid) tid
        PutPrivateKey wid pk -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putPrivateKey (PrimaryKey wid) (fromMockPrivKey pk)
        ReadPrivateKey wid -> Right . PrivateKey . fmap toMockPrivKey <$>
            atomically (readPrivateKey $ PrimaryKey wid)
        PutProtocolParameters wid pp -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putProtocolParameters (PrimaryKey wid) pp
        ReadProtocolParameters wid -> Right . ProtocolParams <$>
            atomically (readProtocolParameters $ PrimaryKey wid)
        PutDelegationRewardBalance wid amt -> catchNoSuchWallet Unit $
            mapExceptT atomically $ putDelegationRewardBalance (PrimaryKey wid) amt
        ReadDelegationRewardBalance wid -> Right . DelegationRewardBalance <$>
            atomically (readDelegationRewardBalance $ PrimaryKey wid)
        RollbackTo wid sl -> catchNoSuchWallet Point $
            mapExceptT atomically $ rollbackTo (PrimaryKey wid) sl

    catchWalletAlreadyExists f =
        fmap (bimap errWalletAlreadyExists f) . runExceptT
    catchNoSuchWallet f =
        fmap (bimap errNoSuchWallet f) . runExceptT
    catchCannotRemovePendingTx f =
        fmap (bimap errCannotRemovePendingTx f) . runExceptT

    errNoSuchWallet :: ErrNoSuchWallet -> Err WalletId
    errNoSuchWallet (ErrNoSuchWallet wid) = NoSuchWallet wid

    errWalletAlreadyExists :: ErrWalletAlreadyExists -> Err WalletId
    errWalletAlreadyExists (ErrWalletAlreadyExists wid) = WalletAlreadyExists wid

    errCannotRemovePendingTx :: ErrRemovePendingTx -> Err WalletId
    errCannotRemovePendingTx (ErrRemovePendingTxNoSuchWallet (ErrNoSuchWallet wid)) =
        CannotRemovePendingTx (ErrErasePendingTxNoSuchWallet wid)
    errCannotRemovePendingTx (ErrRemovePendingTxNoSuchTransaction tid) =
        CannotRemovePendingTx (ErrErasePendingTxNoTx tid)
    errCannotRemovePendingTx (ErrRemovePendingTxTransactionNoMorePending tid) =
        CannotRemovePendingTx (ErrErasePendingTxNoPendingTx tid)

    unPrimaryKey :: PrimaryKey key -> key
    unPrimaryKey (PrimaryKey key) = key

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

{-# ANN generator ("HLint: ignore Use ++" :: String) #-}
generator
    :: forall s. (Arbitrary (Wallet s), GenState s)
    => Model s Symbolic
    -> Maybe (Gen (Cmd s :@ Symbolic))
generator (Model _ wids) = Just $ frequency $ fmap (fmap At) <$> concat
    [ withoutWid
    , if null wids then [] else withWid
    ]
  where
    withoutWid :: [(Int, Gen (Cmd s (Reference WalletId Symbolic)))]
    withoutWid =
        [ (5, CreateWallet
            <$> genId
            <*> (getInitialCheckpoint <$> arbitrary)
            <*> arbitrary
            <*> fmap unGenTxHistory arbitrary
            <*> arbitrary)
        ]

    withWid :: [(Int, Gen (Cmd s (Reference WalletId Symbolic)))]
    withWid =
        [ (3, RemoveWallet <$> genId')
        , (5, pure ListWallets)
        , (5, PutCheckpoint <$> genId' <*> arbitrary)
        , (5, ReadCheckpoint <$> genId')
        , (5, ListCheckpoints <$> genId')
        , (5, PutWalletMeta <$> genId' <*> arbitrary)
        , (5, ReadWalletMeta <$> genId')
        , (5, PutDelegationCertificate <$> genId' <*> arbitrary <*> arbitrary)
        , (1, IsStakeKeyRegistered <$> genId')
        , (5, PutTxHistory <$> genId' <*> fmap unGenTxHistory arbitrary)
        , (5, ReadTxHistory <$> genId' <*> genSortOrder <*> genRange <*> arbitrary)
        , (4, RemovePendingTx <$> genId' <*> arbitrary)
        , (3, PutPrivateKey <$> genId' <*> genPrivKey)
        , (3, ReadPrivateKey <$> genId')
        , (1, RollbackTo <$> genId' <*> arbitrary)
        ]

    genId :: Gen MWid
    genId = MWid <$> elements ["a", "b", "c"]

    genId' :: Gen (Reference WalletId Symbolic)
    genId' = QC.elements (map fst wids)

    genPrivKey :: Gen MPrivKey
    genPrivKey = elements ["pk1", "pk2", "pk3"]

    genSortOrder :: Gen SortOrder
    genSortOrder = arbitraryBoundedEnum

    genRange :: Gen (Range SlotId)
    genRange = applyArbitrary2 Range

isUnordered :: Ord x => [x] -> Bool
isUnordered xs = xs /= L.sort xs

shrinker
    :: (Arbitrary (Wallet s))
    => Model s Symbolic
    -> Cmd s :@ Symbolic -> [Cmd s :@ Symbolic]
shrinker (Model _ _) (At cmd) = case cmd of
    PutCheckpoint wid wal ->
        [ At $ PutCheckpoint wid wal'
        | wal' <- shrink wal ]
    PutTxHistory wid h ->
        [ At $ PutTxHistory wid h'
        | h' <- map unGenTxHistory . shrink . GenTxHistory $ h
        ]
    CreateWallet wid wal met txs pp ->
        [ At $ CreateWallet wid wal' met' txs' pp'
        | (txs', wal', met', pp') <- shrink (txs, wal, met, pp)
        ]
    PutWalletMeta wid met ->
        [ At $ PutWalletMeta wid met'
        | met' <- shrink met
        ]
    RollbackTo wid sid ->
        [ At $ RollbackTo wid sid'
        | sid' <- shrink sid
        ]
    ReadTxHistory wid so range status ->
        [ At $ ReadTxHistory wid so range' status
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
    forall (toList c) (`elem` map fst wids)

postcondition
    :: (Eq s, Show s)
    => Model s Concrete -> Cmd s :@ Concrete -> Resp s :@ Concrete -> Logic
postcondition m c r =
    toMock (after e) r .== mockResp e
  where
    e = lockstep m c r

semantics
    :: MockPrivKey (k 'RootK)
    => DBLayerTest s k
    -> Cmd s :@ Concrete
    -> IO (Resp s :@ Concrete)
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
    )

sm
    :: TestConstraints s k
    => DBLayerTest s k
    -> StateMachine (Model s) (At (Cmd s)) IO (At (Resp s))
sm db = QSM.StateMachine
    { initModel = initModel
    , transition = transition
    , precondition = precondition
    , postcondition = postcondition
    , invariant = Nothing
    , generator = generator
    , distribution = Nothing
    , shrinker = shrinker
    , semantics = semantics db
    , mock = symbolicResp
    }

{-------------------------------------------------------------------------------
  Additional type class instances required to run the QSM tests
-------------------------------------------------------------------------------}

instance CommandNames (At (Cmd s)) where
    cmdName (At CleanDB{}) = "CleanDB"
    cmdName (At CreateWallet{}) = "CreateWallet"
    cmdName (At RemoveWallet{}) = "RemoveWallet"
    cmdName (At ListWallets{}) = "ListWallets"
    cmdName (At PutCheckpoint{}) = "PutCheckpoint"
    cmdName (At ListCheckpoints{}) = "ListCheckpoints"
    cmdName (At ReadCheckpoint{}) = "ReadCheckpoint"
    cmdName (At PutWalletMeta{}) = "PutWalletMeta"
    cmdName (At ReadWalletMeta{}) = "ReadWalletMeta"
    cmdName (At PutDelegationCertificate{}) = "PutDelegationCertificate"
    cmdName (At IsStakeKeyRegistered{}) = "IsStakeKeyRegistered"
    cmdName (At PutTxHistory{}) = "PutTxHistory"
    cmdName (At ReadTxHistory{}) = "ReadTxHistory"
    cmdName (At PutPrivateKey{}) = "PutPrivateKey"
    cmdName (At ReadPrivateKey{}) = "ReadPrivateKey"
    cmdName (At PutProtocolParameters{}) = "PutProtocolParameters"
    cmdName (At ReadProtocolParameters{}) = "ReadProtocolParameters"
    cmdName (At PutDelegationRewardBalance{}) = "PutDelegationRewardBalance"
    cmdName (At ReadDelegationRewardBalance{}) = "ReadDelegationRewardBalance"
    cmdName (At RollbackTo{}) = "RollbackTo"
    cmdName (At RemovePendingTx{}) = "RemovePendingTx"
    cmdNames _ =
        [ "CleanDB"
        , "CreateWallet", "RemoveWallet", "ListWallets"
        , "PutCheckpoint", "ReadCheckpoint", "ListCheckpoints", "RollbackTo"
        , "PutWalletMeta", "ReadWalletMeta"
        , "PutDelegationCertificate", "IsStakeKeyRegistered"
        , "PutTxHistory", "ReadTxHistory", "RemovePendingTx"
        , "PutPrivateKey", "ReadPrivateKey"
        , "PutProtocolParameters", "ReadProtocolParameters"
        , "PutDelegationRewardBalance", "ReadDelegationRewardBalance"
        ]

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

instance ToExpr SlotId where
    toExpr = genericToExpr

instance ToExpr EpochNo where
    toExpr = defaultExprViaShow

instance ToExpr SlotInEpoch where
    toExpr = genericToExpr

instance ToExpr TxStatus where
    toExpr = genericToExpr

instance ToExpr PoolId where
    toExpr = defaultExprViaShow

instance ToExpr (SeqState 'Mainnet JormungandrKey) where
    toExpr = defaultExprViaShow

instance ToExpr (RndState 'Mainnet) where
    toExpr = defaultExprViaShow

instance (Show (key 'AccountK CC.XPub)) =>
    ToExpr (AddressPool
        (chain :: AccountingStyle)
        (key :: Depth -> * -> *)
    ) where
    toExpr = defaultExprViaShow

instance (ToExpr s, ToExpr xprv) => ToExpr (WalletDatabase s xprv) where
    toExpr = genericToExpr

instance ToExpr UTxO where
    toExpr = genericToExpr

instance ToExpr WalletMetadata where
    toExpr = defaultExprViaShow

instance ToExpr Tx where
    toExpr = genericToExpr

instance ToExpr TxIn where
    toExpr = genericToExpr

instance ToExpr Coin where
    toExpr = genericToExpr

instance ToExpr TxOut where
    toExpr = genericToExpr

instance ToExpr Address where
    toExpr = genericToExpr

instance ToExpr TxMeta where
    toExpr = genericToExpr

instance ToExpr Percentage where
    toExpr = genericToExpr

instance ToExpr ProtocolParameters where
    toExpr = genericToExpr

instance ToExpr DecentralizationLevel where
    toExpr = genericToExpr

instance ToExpr TxParameters where
    toExpr = genericToExpr

instance ToExpr FeePolicy where
    toExpr = genericToExpr

instance ToExpr Direction where
    toExpr = genericToExpr

instance ToExpr MWid where
    toExpr = defaultExprViaShow

instance ToExpr StakeKeyCertificate where
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
        (At (ReadTxHistory wid _ _ _), Resp (Right (TxHistory _)), Model _ wids)
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
            (At (RemovePendingTx wid _), Resp _) ->
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
        forAllCommands (sm @s @k dbLayerUnused) Nothing $ \cmds ->
            repeatedly collect (tag . execCmds $ cmds) (property True)

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . L.foldl' . flip

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

prop_sequential :: forall s k. (TestConstraints s k, ToExpr s) => DBLayerTest s k -> Property
prop_sequential db =
    QC.checkCoverage $
    forAllCommands (sm @s @k dbLayerUnused) Nothing $ \cmds ->
    monadicIO $ do
        liftIO $ cleanDB db
        let sm' = sm db
        (hist, _model, res) <- runCommands sm' cmds
        prettyCommands sm' hist
            $ measureTagCoverage cmds
            $ res === Ok
  where
    measureTagCoverage :: Commands (At (Cmd s)) (At (Resp s)) -> Property -> Property
    measureTagCoverage cmds prop = foldl' measureTag prop allTags
      where
        measureTag :: Property -> Tag -> Property
        measureTag p t = QC.cover 5 (t `Set.member` matchedTags) (show t) p

        matchedTags :: Set Tag
        matchedTags = Set.fromList $ tag $ execCmds cmds

prop_parallel :: forall s k. TestConstraints s k => DBLayerTest s k -> Property
prop_parallel db =
    forAllParallelCommands (sm @s @k dbLayerUnused) $ \cmds ->
    monadicIO $ do
        let sm' = sm db
            cmds' = addCleanDB cmds
        prettyParallelCommands cmds =<< runParallelCommands sm' cmds'

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

dbLayerUnused :: DBLayerTest s k
dbLayerUnused = error "DBLayer not used during command generation"
