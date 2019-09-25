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
-- Copyright: © 2018-2019 IOHK
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

import Cardano.Wallet.DB
    ( DBLayer (..)
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    , cleanDB
    )
import Cardano.Wallet.DB.Arbitrary
    ( GenTxHistory (..) )
import Cardano.Wallet.DB.Model
    ( Database
    , Err (..)
    , TxHistory
    , emptyDatabase
    , mCleanDB
    , mCreateWallet
    , mListWallets
    , mPutCheckpoint
    , mPutPrivateKey
    , mPutTxHistory
    , mPutWalletMeta
    , mReadCheckpoint
    , mReadPrivateKey
    , mReadTxHistory
    , mReadWalletMeta
    , mRemoveWallet
    , mRollbackTo
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), PersistKey, XPrv, deserializeXPrv )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( Hash (..)
    , Range (..)
    , SlotId (..)
    , SortOrder (..)
    , TxMeta (..)
    , WalletId (..)
    , WalletMetadata (..)
    )
import Control.Applicative
    ( (<|>) )
import Control.Foldl
    ( Fold (..) )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString, hash )
import Data.Bifunctor
    ( bimap, first )
import Data.Foldable
    ( foldl', toList )
import Data.Functor.Classes
    ( Eq1, Show1 )
import Data.List.Extra
    ( enumerate )
import Data.Map
    ( Map )
import Data.Maybe
    ( catMaybes, fromJust, fromMaybe, isJust, isNothing )
import Data.Set
    ( Set )
import Data.TreeDiff
    ( ToExpr (..), defaultExprViaShow )
import Data.Word
    ( Word32 )
import GHC.Generics
    ( Generic )
import System.Random
    ( getStdRandom, randomR )
import Test.QuickCheck
    ( Arbitrary (..)
    , Args (..)
    , Gen
    , Property
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
type Mock s = Database MWid s DummyTarget MPrivKey

-- | Shortcut for wallet type.
type MWallet s = Wallet s DummyTarget

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

-- | Stuff a mock private key into the type used by 'DBLayer'.
fromMockPrivKey :: PersistKey k => MPrivKey -> (k 'RootK XPrv, Hash "encryption")
fromMockPrivKey s = (k, Hash (B8.pack s))
  where
    -- Produce a key by deserializing zeroes in either SeqKey or RndKey format.
    Right (k, _) = deserializeXPrv (zeroes, mempty) <|>
        deserializeXPrv (zeroes <> ":", mempty)
    zeroes = B8.replicate 256 '0'

-- | Unstuff the DBLayer private key into the mock type.
toMockPrivKey
    :: forall (k :: Depth -> * -> *) (purpose :: Depth).
       (k purpose XPrv, Hash "encryption")
    -> MPrivKey
toMockPrivKey (_, Hash h) = B8.unpack h

{-------------------------------------------------------------------------------
  Language
-------------------------------------------------------------------------------}

data Cmd s wid
    = CleanDB
    | CreateWallet MWid (MWallet s) WalletMetadata
    | RemoveWallet wid
    | ListWallets
    | PutCheckpoint wid (MWallet s)
    | ReadCheckpoint wid
    | PutWalletMeta wid WalletMetadata
    | ReadWalletMeta wid
    | PutTxHistory wid (TxHistory DummyTarget)
    | ReadTxHistory wid SortOrder (Range SlotId)
    | PutPrivateKey wid MPrivKey
    | ReadPrivateKey wid
    | RollbackTo wid SlotId
    deriving (Show, Functor, Foldable, Traversable)

data Success s wid
    = Unit ()
    | NewWallet wid
    | WalletIds [wid]
    | Checkpoint (Maybe (MWallet s))
    | Metadata (Maybe WalletMetadata)
    | TxHistory (TxHistory DummyTarget)
    | PrivateKey (Maybe MPrivKey)
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
    CreateWallet wid wal meta ->
        first (Resp . fmap (const (NewWallet wid))) . mCreateWallet wid wal meta
    RemoveWallet wid ->
        first (Resp . fmap Unit) . mRemoveWallet wid
    ListWallets ->
        first (Resp . fmap WalletIds) . mListWallets
    PutCheckpoint wid wal ->
        first (Resp . fmap Unit) . mPutCheckpoint wid wal
    ReadCheckpoint wid ->
        first (Resp . fmap Checkpoint) . mReadCheckpoint wid
    PutWalletMeta wid meta ->
        first (Resp . fmap Unit) . mPutWalletMeta wid meta
    ReadWalletMeta wid ->
        first (Resp . fmap Metadata) . mReadWalletMeta wid
    PutTxHistory wid txs ->
        first (Resp . fmap Unit) . mPutTxHistory wid (Map.fromList txs)
    ReadTxHistory wid order range ->
        first (Resp . fmap TxHistory) . mReadTxHistory wid order range
    PutPrivateKey wid pk ->
        first (Resp . fmap Unit) . mPutPrivateKey wid pk
    ReadPrivateKey wid ->
        first (Resp . fmap PrivateKey) . mReadPrivateKey wid
    RollbackTo wid sl ->
        first (Resp . fmap Unit) . mRollbackTo wid sl

{-------------------------------------------------------------------------------
  Interpreter: real I/O
-------------------------------------------------------------------------------}

-- | Type alias for the 'DBLayer', just to reduce noise in type signatures. This
-- 'DBLayer' is specialized to a dummy node backend.
type DBLayerTest s k = DBLayer IO s DummyTarget k

runIO
    :: forall s k. (PersistKey k)
    => DBLayerTest s k
    -> Cmd s WalletId
    -> IO (Resp s WalletId)
runIO db = fmap Resp . go
  where
    go
        :: Cmd s WalletId
        -> IO (Either (Err WalletId) (Success s WalletId))
    go = \case
        CleanDB -> do
            Right . Unit <$> cleanDB db
        CreateWallet wid wal meta ->
            catchWalletAlreadyExists (const (NewWallet (unMockWid wid))) $
                createWallet db (widPK wid) wal meta
        RemoveWallet wid ->
            catchNoSuchWallet Unit $
                removeWallet db (PrimaryKey wid)
        ListWallets ->
            Right . WalletIds . fmap unPrimaryKey <$> listWallets db
        PutCheckpoint wid wal ->
            catchNoSuchWallet Unit $ putCheckpoint db (PrimaryKey wid) wal
        ReadCheckpoint wid ->
            Right . Checkpoint <$> readCheckpoint db (PrimaryKey wid)
        PutWalletMeta wid meta ->
            catchNoSuchWallet Unit $ putWalletMeta db (PrimaryKey wid) meta
        ReadWalletMeta wid ->
            Right . Metadata <$> readWalletMeta db (PrimaryKey wid)
        PutTxHistory wid txs ->
            catchNoSuchWallet Unit $
                putTxHistory db (PrimaryKey wid) (Map.fromList txs)
        ReadTxHistory wid order range ->
            Right . TxHistory <$> readTxHistory db (PrimaryKey wid) order range
        PutPrivateKey wid pk ->
            catchNoSuchWallet Unit $
                putPrivateKey db (PrimaryKey wid) (fromMockPrivKey pk)
        ReadPrivateKey wid ->
            Right . PrivateKey . fmap toMockPrivKey
                <$> readPrivateKey db (PrimaryKey wid)
        RollbackTo wid sl ->
            catchNoSuchWallet Unit $ rollbackTo db (PrimaryKey wid) sl

    catchWalletAlreadyExists f =
        fmap (bimap errWalletAlreadyExists f) . runExceptT
    catchNoSuchWallet f =
        fmap (bimap errNoSuchWallet f) . runExceptT

    errNoSuchWallet :: ErrNoSuchWallet -> Err WalletId
    errNoSuchWallet (ErrNoSuchWallet wid) = NoSuchWallet wid

    errWalletAlreadyExists :: ErrWalletAlreadyExists -> Err WalletId
    errWalletAlreadyExists (ErrWalletAlreadyExists wid) = WalletAlreadyExists wid

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

deriving instance Show1 r => Show (Model s r)

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

deriving instance Show1 r => Show (Event s r)

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
generator :: forall s. (Arbitrary (Wallet s DummyTarget)) => Model s Symbolic -> Maybe (Gen (Cmd s :@ Symbolic))
generator (Model _ wids) = Just $ frequency $ fmap (fmap At) <$> concat
    [ withoutWid
    , if null wids then [] else withWid
    ]
  where
    withoutWid :: [(Int, Gen (Cmd s (Reference WalletId Symbolic)))]
    withoutWid =
        [ (5, CreateWallet <$> genId <*> arbitrary <*> arbitrary)
        ]

    withWid :: [(Int, Gen (Cmd s (Reference WalletId Symbolic)))]
    withWid =
        [ (3, RemoveWallet <$> genId')
        , (5, pure ListWallets)
        , (5, PutCheckpoint <$> genId' <*> arbitrary)
        , (5, ReadCheckpoint <$> genId')
        , (5, PutWalletMeta <$> genId' <*> arbitrary)
        , (5, ReadWalletMeta <$> genId')
        , (5, PutTxHistory <$> genId' <*> fmap unGenTxHistory arbitrary)
        , (5, ReadTxHistory <$> genId' <*> genSortOrder <*> genRange)
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
    genRange = Range <$> genSId <*> genSId
      where
        genSId :: Gen (Maybe SlotId)
        genSId = QC.oneof
            [ return Nothing
            , Just <$> (SlotId <$> genWord32 <*> arbitrary)
            ]
        -- FIXME: There's currently an artifical boundary on ~48 bits for the
        -- Sqlite storage of the SlotId's epochNumber.
        genWord32 = fromIntegral <$> arbitrary @Word32

isUnordered :: Ord x => [x] -> Bool
isUnordered xs = xs /= L.sort xs

shrinker :: Arbitrary (Wallet s DummyTarget) => Model s Symbolic -> Cmd s :@ Symbolic -> [Cmd s :@ Symbolic]
shrinker (Model _ _) (At cmd) = case cmd of
    PutCheckpoint wid wal ->
        [ At $ PutCheckpoint wid wal'
        | wal' <- shrink wal ]
    PutTxHistory wid h ->
        [ At $ PutTxHistory wid h'
        | h' <- map unGenTxHistory . shrink . GenTxHistory $ h
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

postcondition :: Eq s => Model s Concrete -> Cmd s :@ Concrete -> Resp s :@ Concrete -> Logic
postcondition m c r =
    toMock (after e) r .== mockResp e
  where
    e = lockstep m c r

semantics :: PersistKey k => DBLayerTest s k -> Cmd s :@ Concrete -> IO (Resp s :@ Concrete)
semantics db (At c) =
    (At . fmap QSM.reference) <$>
        runIO db (fmap QSM.concrete c)

symbolicResp :: Model s Symbolic -> Cmd s :@ Symbolic -> GenSym (Resp s :@ Symbolic)
symbolicResp m c =
    At <$> traverse (const QSM.genSym) resp
  where
    (resp, _mock') = step m c

type TestConstraints s k = (PersistKey k, Eq s, Show s, Arbitrary (Wallet s DummyTarget))

sm :: TestConstraints s k => DBLayerTest s k -> StateMachine (Model s) (At (Cmd s)) IO (At (Resp s))
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
    cmdName (At ReadCheckpoint{}) = "ReadCheckpoint"
    cmdName (At PutWalletMeta{}) = "PutWalletMeta"
    cmdName (At ReadWalletMeta{}) = "ReadWalletMeta"
    cmdName (At PutTxHistory{}) = "PutTxHistory"
    cmdName (At ReadTxHistory{}) = "ReadTxHistory"
    cmdName (At PutPrivateKey{}) = "PutPrivateKey"
    cmdName (At ReadPrivateKey{}) = "ReadPrivateKey"
    cmdName (At RollbackTo{}) = "RollbackTo"
    cmdNames _ =
        [ "CleanDB", "CreateWallet", "CreateWallet", "RemoveWallet"
        , "ListWallets", "PutCheckpoint", "ReadCheckpoint", "PutWalletMeta"
        , "ReadWalletMeta", "PutTxHistory", "ReadTxHistory", "PutPrivateKey"
        , "ReadPrivateKey", "RollbackTo"
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

deriving instance Show s => ToExpr (Model s Concrete)

instance ToExpr (Mock s) where
    toExpr = defaultExprViaShow

instance ToExpr WalletId where
    toExpr = defaultExprViaShow

instance Show s => ToExpr (MWallet s) where
    toExpr = defaultExprViaShow

instance ToExpr WalletMetadata where
    toExpr = defaultExprViaShow

instance ToExpr (Hash "Tx") where
    toExpr = defaultExprViaShow

instance ToExpr Tx where
    toExpr = defaultExprViaShow

instance ToExpr TxMeta where
    toExpr = defaultExprViaShow

instance ToExpr MWid where
    toExpr = defaultExprViaShow

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
    ]
  where
    isRollbackSuccess :: Event s Symbolic -> Maybe MWid
    isRollbackSuccess ev = case (cmd ev, mockResp ev, before ev) of
        (At (RollbackTo wid _), Resp (Right (Unit ())), Model _ wids ) ->
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
        (At (ReadTxHistory wid _ _), Resp (Right (TxHistory _)), Model _ wids)
            -> Just (wids ! wid)
        _otherwise
            -> Nothing

    createThreeWallets :: Fold (Event s Symbolic) (Maybe Tag)
    createThreeWallets = Fold update Set.empty extract
      where
        update :: Set MWid -> Event s Symbolic -> Set MWid
        update created ev =
            case (cmd ev, mockResp ev) of
                (At (CreateWallet wid _ _), Resp (Right _)) ->
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
            (At (CreateWallet wid _ _), Resp _) -> Just wid
            _otherwise -> Nothing

    removeWalletTwice :: Fold (Event s Symbolic) (Maybe Tag)
    removeWalletTwice = countAction RemoveWalletTwice (>= 2) match
      where
        match ev = case (cmd ev, mockResp ev) of
            (At (RemoveWallet wid), Resp _) ->
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
                    Map.alter (Just . (+1) . fromMaybe 0) wid matches
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
                (At (CreateWallet wid _ _), Resp (Right _)) ->
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
        :: (TxHistory DummyTarget -> Bool)
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
                    any (isUnordered . sel . fst) (Map.fromList h)
                _otherwise ->
                    False

    readCheckpoint
        :: (Maybe (MWallet s) -> Bool)
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

prop_sequential :: forall s k. TestConstraints s k => DBLayerTest s k -> Property
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
