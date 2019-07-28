{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
-- License: MIT
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
import Cardano.Wallet.DBSpec
    ( GenTxHistory (..), TxHistory, filterTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Key, XPrv, deserializeXPrv )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..) )
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
import Control.Foldl
    ( Fold (..) )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Hash
    ( hash )
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

-- | Shortcut for wallet type.
type MWallet = Wallet (SeqState DummyTarget) DummyTarget

-- | Mock wallet ID -- simple and easy to read.
newtype MWid = MWid String
    deriving (Show, Eq, Ord, Generic)

widPK :: MWid -> PrimaryKey WalletId
widPK = PrimaryKey . unMockWid

-- | Convert a mock wallet ID to a real one by hashing it.
unMockWid :: MWid -> WalletId
unMockWid (MWid wid) = WalletId . hash . B8.pack $ wid

-- | Represent (XPrv, Hash) as a string.
type MPrivKey = String

-- | Stuff a mock private key into the type used by 'DBLayer'.
fromMockPrivKey :: MPrivKey -> (Key purpose XPrv, Hash "encryption")
fromMockPrivKey s = (k, Hash (B8.pack s))
    where Right (k, _) = deserializeXPrv (B8.replicate 256 '0', mempty)

-- | Unstuff the DBLayer private key into the mock type.
toMockPrivKey :: (Key purpose XPrv, Hash "encryption") -> MPrivKey
toMockPrivKey (_, Hash h) = B8.unpack h

-- | Mock representation of a 'DBLayer'
data Mock = M
    { checkpoints :: Map MWid MWallet
    , metas :: Map MWid WalletMetadata
    , txs :: Map MWid TxHistory
    , privateKeys :: Map MWid MPrivKey
    } deriving (Show, Generic)

emptyMock :: Mock
emptyMock = M mempty mempty mempty mempty

type MockOp a = Mock -> (Either (Err MWid) a, Mock)

mCleanDB :: MockOp ()
mCleanDB _ = (Right (), emptyMock)

mCreateWallet :: MWid -> MWallet -> WalletMetadata -> MockOp ()
mCreateWallet wid wal meta m@(M cp metas txs pks)
    | wid `Map.member` cp = (Left (WalletAlreadyExists wid), m)
    | otherwise =
        ( Right ()
        , M
            { checkpoints = Map.insert wid wal cp
            , metas = Map.insert wid meta metas
            , txs = txs
            , privateKeys = pks
            }
        )

mRemoveWallet :: MWid -> MockOp ()
mRemoveWallet wid m@(M cp metas txs pk)
    | wid `Map.member` cp =
        ( Right ()
        , M
            { checkpoints = Map.delete wid cp
            , metas = Map.delete wid metas
            , txs = Map.delete wid txs
            , privateKeys = Map.delete wid pk
            }
        )
    | otherwise = (Left (NoSuchWallet wid), m)

mListWallets :: MockOp [MWid]
mListWallets m@(M cp _ _ _) = (Right (L.sortOn unMockWid $ Map.keys cp), m)

mPutCheckpoint :: MWid -> MWallet -> MockOp ()
mPutCheckpoint wid wal m@(M cp metas txs pk)
    | wid `Map.member` cp = (Right (), M (Map.insert wid wal cp) metas txs pk)
    | otherwise = (Left (NoSuchWallet wid), m)

mReadCheckpoint :: MWid -> MockOp (Maybe MWallet)
mReadCheckpoint wid m@(M cp _ _ _) = (Right (Map.lookup wid cp), m)

mPutWalletMeta :: MWid -> WalletMetadata -> MockOp ()
mPutWalletMeta wid meta m@(M cp metas txs pk)
    | wid `Map.member` cp = (Right (), M cp (Map.insert wid meta metas) txs pk)
    | otherwise = (Left (NoSuchWallet wid), m)

mReadWalletMeta :: MWid -> MockOp (Maybe WalletMetadata)
mReadWalletMeta wid m@(M _ meta _ _) = (Right (Map.lookup wid meta), m)

mPutTxHistory :: MWid -> TxHistory -> MockOp ()
mPutTxHistory wid txList m@(M cp metas txs pk)
    | wid `Map.member` cp = (Right (), M cp metas txs'' pk)
    | otherwise           = (Left (NoSuchWallet wid), m)
  where
    txs' = Map.fromList txList

    -- Add tx history for the wallet, and then update any Tx in the mock
    -- database that appeared in the given TxHistory.
    txs'' = Map.toList . Map.mapWithKey updateTxs . Map.fromList <$> Map.alter appendTxs wid txs

    -- Add tx history, replacing entries with the same TxId.
    appendTxs = Just . Map.toList . (txs' <>) . Map.fromList . fromMaybe mempty

    -- Update a Tx of the given id, if it is in the given TxHistory.
    updateTxs :: Hash "Tx" -> (Tx, TxMeta) -> (Tx, TxMeta)
    updateTxs txid (tx, meta) = (maybe tx fst (Map.lookup txid txs'), meta)

mReadTxHistory :: MWid -> SortOrder -> Range SlotId -> MockOp TxHistory
mReadTxHistory wid order range m@(M cp _ txs _)
    | wid `Map.member` cp = (Right txHistory, m)
    | otherwise = (Right mempty, m)
  where
    txHistory = filterTxHistory order range
        $ fromMaybe mempty (Map.lookup wid txs)

mPutPrivateKey :: MWid -> MPrivKey -> MockOp ()
mPutPrivateKey wid pk' m@(M cp metas txs pk)
    | wid `Map.member` cp = (Right (), M cp metas txs (Map.insert wid pk' pk))
    | otherwise = (Left (NoSuchWallet wid), m)

mReadPrivateKey :: MWid -> MockOp (Maybe MPrivKey)
mReadPrivateKey wid m@(M cp _ _ pk)
    | wid `Map.member` cp = (Right (Map.lookup wid pk), m)
    | otherwise = (Right Nothing, m)

{-------------------------------------------------------------------------------
  Language
-------------------------------------------------------------------------------}

data Cmd wid
    = CleanDB
    | CreateWallet MWid MWallet WalletMetadata
    | RemoveWallet wid
    | ListWallets
    | PutCheckpoint wid MWallet
    | ReadCheckpoint wid
    | PutWalletMeta wid WalletMetadata
    | ReadWalletMeta wid
    | PutTxHistory wid TxHistory
    | ReadTxHistory wid SortOrder (Range SlotId)
    | PutPrivateKey wid MPrivKey
    | ReadPrivateKey wid
    deriving (Show, Functor, Foldable, Traversable)

data Success wid
    = Unit ()
    | NewWallet wid
    | WalletIds [wid]
    | Checkpoint (Maybe MWallet)
    | Metadata (Maybe WalletMetadata)
    | TxHistory TxHistory
    | PrivateKey (Maybe MPrivKey)
    deriving (Show, Eq, Functor, Foldable, Traversable)

newtype Resp wid
    = Resp (Either (Err wid) (Success wid))
    deriving (Show, Eq)

instance Functor Resp where
    fmap f (Resp r) = Resp (bimap (fmap f) (fmap f) r)

instance Foldable Resp where
    foldMap f (Resp r) = either (foldMap f) (foldMap f) r

instance Traversable Resp where
    traverse f (Resp (Right r)) = Resp . Right <$> traverse f r
    traverse f (Resp (Left e)) = Resp . Left <$> traverse f e

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

data Err wid
    = NoSuchWallet wid
    | WalletAlreadyExists wid
    deriving (Show, Eq, Functor, Foldable, Traversable)

errNoSuchWallet :: ErrNoSuchWallet -> Err WalletId
errNoSuchWallet (ErrNoSuchWallet wid) = NoSuchWallet wid

errWalletAlreadyExists :: ErrWalletAlreadyExists -> Err WalletId
errWalletAlreadyExists (ErrWalletAlreadyExists wid) = WalletAlreadyExists wid

{-------------------------------------------------------------------------------
  Interpreter: mock implementation
-------------------------------------------------------------------------------}

runMock :: Cmd MWid -> Mock -> (Resp MWid, Mock)
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
        first (Resp . fmap Unit) . mPutTxHistory wid txs
    ReadTxHistory wid order range ->
        first (Resp . fmap TxHistory) . mReadTxHistory wid order range
    PutPrivateKey wid pk ->
        first (Resp . fmap Unit) . mPutPrivateKey wid pk
    ReadPrivateKey wid ->
        first (Resp . fmap PrivateKey) . mReadPrivateKey wid

{-------------------------------------------------------------------------------
  Interpreter: real I/O
-------------------------------------------------------------------------------}

-- | Type alias for the 'DBLayer', just to reduce noise in type signatures. This
-- 'DBLayer' is specialized to a dummy node backend, but uses the real 'SeqState'
-- , so that the functions to store/load SeqState are tested. Using concrete
-- types avoids infecting all the types and functions with extra type parameters.
type DBLayerTest = DBLayer IO (SeqState DummyTarget) DummyTarget

runIO
    :: DBLayerTest
    -> Cmd WalletId
    -> IO (Resp WalletId)
runIO db = fmap Resp . go
  where
    go
        :: Cmd WalletId
        -> IO (Either (Err WalletId) (Success WalletId))
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
            catchNoSuchWallet Unit $ putTxHistory db (PrimaryKey wid) (Map.fromList txs)
        ReadTxHistory wid order range ->
            Right . TxHistory <$> readTxHistory db (PrimaryKey wid) order range
        PutPrivateKey wid pk ->
            catchNoSuchWallet Unit $
                putPrivateKey db (PrimaryKey wid) (fromMockPrivKey pk)
        ReadPrivateKey wid ->
            Right . PrivateKey . fmap toMockPrivKey
                <$> readPrivateKey db (PrimaryKey wid)

    catchWalletAlreadyExists f =
        fmap (bimap errWalletAlreadyExists f) . runExceptT
    catchNoSuchWallet f =
        fmap (bimap errNoSuchWallet f) . runExceptT

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

data Model r
    = Model Mock (WidRefs r)
    deriving (Generic)

deriving instance Show1 r => Show (Model r)

initModel :: Model r
initModel = Model emptyMock []

toMock :: (Functor f, Eq1 r) => Model r -> f :@ r -> f MWid
toMock (Model _ wids) (At fr) = fmap (wids !) fr

step :: Eq1 r => Model r -> Cmd :@ r -> (Resp MWid, Mock)
step m@(Model mock _) c = runMock (toMock m c) mock

{-------------------------------------------------------------------------------
  Events
-------------------------------------------------------------------------------}

data Event r = Event
    { before :: Model  r
    , cmd :: Cmd :@ r
    , after :: Model  r
    , mockResp :: Resp MWid
    }

deriving instance Show1 r => Show (Event r)

lockstep
    :: forall r. Eq1 r
    => Model   r
    -> Cmd  :@ r
    -> Resp :@ r
    -> Event   r
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
generator :: Model Symbolic -> Maybe (Gen (Cmd :@ Symbolic))
generator (Model _ wids) = Just $ frequency $ fmap (fmap At) <$> concat
    [ withoutWid
    , if null wids then [] else withWid
    ]
  where
    withoutWid :: [(Int, Gen (Cmd (Reference WalletId Symbolic)))]
    withoutWid =
        [ (5, CreateWallet <$> genId <*> arbitrary <*> arbitrary)
        ]

    withWid :: [(Int, Gen (Cmd (Reference WalletId Symbolic)))]
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
        ]

    genId :: Gen MWid
    genId = MWid <$> elements ["a", "b", "c"]

    genId' :: Gen (Reference WalletId Symbolic)
    genId' = QC.elements (map fst wids)

    genPrivKey :: Gen MPrivKey
    genPrivKey = elements ["pk1", "pk2", "pk3"]

    genSortOrder :: Gen SortOrder
    genSortOrder = QC.elements [Ascending, Descending]

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

shrinker :: Model Symbolic -> Cmd :@ Symbolic -> [Cmd :@ Symbolic]
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

transition :: Eq1 r => Model r -> Cmd :@ r -> Resp :@ r -> Model r
transition m c = after . lockstep m c

precondition :: Model Symbolic -> Cmd :@ Symbolic -> Logic
precondition (Model _ wids) (At c) =
    forall (toList c) (`elem` map fst wids)

postcondition :: Model Concrete -> Cmd :@ Concrete -> Resp :@ Concrete -> Logic
postcondition m c r =
    toMock (after e) r .== mockResp e
  where
    e = lockstep m c r

semantics :: DBLayerTest -> Cmd :@ Concrete -> IO (Resp :@ Concrete)
semantics db (At c) =
    (At . fmap QSM.reference) <$>
        runIO db (fmap QSM.concrete c)

symbolicResp :: Model Symbolic -> Cmd :@ Symbolic -> GenSym (Resp :@ Symbolic)
symbolicResp m c =
    At <$> traverse (const QSM.genSym) resp
  where
    (resp, _mock') = step m c

sm :: DBLayerTest -> StateMachine Model (At Cmd) IO (At Resp)
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

instance CommandNames (At Cmd) where
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
    cmdNames _ =
        [ "CleanDB", "CreateWallet", "CreateWallet", "RemoveWallet"
        , "ListWallets", "PutCheckpoint", "ReadCheckpoint", "PutWalletMeta"
        , "ReadWalletMeta", "PutTxHistory", "ReadTxHistory", "PutPrivateKey"
        , "ReadPrivateKey"
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

deriving instance ToExpr (Model Concrete)
deriving instance ToExpr Mock

instance ToExpr WalletId where
    toExpr = defaultExprViaShow

instance ToExpr MWallet where
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
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | The list of all possible 'Tag' values.
allTags :: [Tag]
allTags = enumerate

tag :: [Event Symbolic] -> [Tag]
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
    ]
  where
    readAfterDelete :: Fold (Event Symbolic) (Maybe Tag)
    readAfterDelete = Fold update mempty extract
      where
        update :: Map MWid Int -> Event Symbolic -> Map MWid Int
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

    isReadTxHistory :: Event Symbolic -> Maybe MWid
    isReadTxHistory ev = case (cmd ev, mockResp ev, before ev) of
        (At (ReadTxHistory wid _ _), Resp (Right (TxHistory _)), Model _ wids)
            -> Just (wids ! wid)
        _otherwise
            -> Nothing

    createThreeWallets :: Fold (Event Symbolic) (Maybe Tag)
    createThreeWallets = Fold update Set.empty extract
      where
        update :: Set MWid -> Event Symbolic -> Set MWid
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

    createWalletTwice :: Fold (Event Symbolic) (Maybe Tag)
    createWalletTwice = countAction CreateWalletTwice (>= 2) match
      where
        match :: Event Symbolic -> Maybe MWid
        match ev = case (cmd ev, mockResp ev) of
            (At (CreateWallet wid _ _), Resp _) -> Just wid
            _otherwise -> Nothing

    removeWalletTwice :: Fold (Event Symbolic) (Maybe Tag)
    removeWalletTwice = countAction RemoveWalletTwice (>= 2) match
      where
        match ev = case (cmd ev, mockResp ev) of
            (At (RemoveWallet wid), Resp _) ->
                Just wid
            _otherwise ->
                Nothing

    countAction
        :: forall k. Ord k => Tag -> (Int -> Bool)
        -> (Event Symbolic -> Maybe k)
        -> Fold (Event Symbolic) (Maybe Tag)
    countAction res enough match = Fold update mempty extract
      where
        update :: Map k Int -> Event Symbolic -> Map k Int
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

    isReadPrivateKeySuccess :: Event Symbolic -> Maybe MWid
    isReadPrivateKeySuccess ev = case (cmd ev, mockResp ev, before ev) of
        (At (ReadPrivateKey wid)
            , Resp (Right (PrivateKey (Just _)))
            , Model _ wids )
                -> Just (wids ! wid)
        _otherwise
            -> Nothing

    createThenList :: Fold (Event Symbolic) (Maybe Tag)
    createThenList =
        Fold update mempty extract
      where
        update :: Map MWid Bool -> Event Symbolic -> Map MWid Bool
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
        :: (TxHistory -> Bool)
        -> Tag
        -> Fold (Event Symbolic) (Maybe Tag)
    readTxHistory check res = Fold update False (extractf res)
      where
        update :: Bool -> Event Symbolic -> Bool
        update didRead ev = didRead || case (cmd ev, mockResp ev) of
            (At ReadTxHistory {}, Resp (Right (TxHistory h))) ->
                check h
            _otherwise ->
                False

    txUnsorted
        :: Ord a
        => (Tx -> [a])
        -> Tag
        -> Fold (Event Symbolic) (Maybe Tag)
    txUnsorted sel res = Fold update False (extractf res)
      where
        update :: Bool -> Event Symbolic -> Bool
        update didRead ev = didRead ||
            case (cmd ev, mockResp ev) of
                (At (PutTxHistory _ h), Resp (Right _)) ->
                    any (isUnordered . sel . fst) (Map.fromList h)
                _otherwise ->
                    False

    readCheckpoint
        :: (Maybe MWallet -> Bool)
        -> Tag
        -> Fold (Event Symbolic) (Maybe Tag)
    readCheckpoint check res = Fold update False (extractf res)
      where
        update :: Bool -> Event Symbolic -> Bool
        update didRead ev = didRead ||
            case (cmd ev, mockResp ev) of
                (At (ReadCheckpoint _), Resp (Right (Checkpoint cp))) ->
                    check cp
                _otherwise ->
                    False

    extractf :: a -> Bool -> Maybe a
    extractf a t = if t then Just a else Nothing

execCmd
    :: Model Symbolic
    -> QSM.Command (At Cmd) (At Resp)
    -> Event Symbolic
execCmd model (QSM.Command cmd resp _vars) =
    lockstep model cmd resp

execCmds :: QSM.Commands (At Cmd) (At Resp) -> [Event Symbolic]
execCmds = \(QSM.Commands cs) -> go initModel cs
  where
    go
        :: Model Symbolic
        -> [QSM.Command (At Cmd) (At Resp)]
        -> [Event Symbolic]
    go _ [] = []
    go m (c : cs) = e : go (after e) cs where e = execCmd m c

{-------------------------------------------------------------------------------
  Finding minimal labelled examples - helper functions
-------------------------------------------------------------------------------}

showLabelledExamples :: Maybe Int -> IO ()
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
        forAllCommands (sm dbLayerUnused) Nothing $ \cmds ->
            repeatedly collect (tag . execCmds $ cmds) (property True)

repeatedly :: (a -> b -> b) -> ([a] -> b -> b)
repeatedly = flip . L.foldl' . flip

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

prop_sequential :: DBLayerTest -> Property
prop_sequential db =
    QC.checkCoverage $
    forAllCommands (sm dbLayerUnused) Nothing $ \cmds ->
    monadicIO $ do
        liftIO $ cleanDB db
        let sm' = sm db
        (hist, _model, res) <- runCommands sm' cmds
        prettyCommands sm' hist
            $ measureTagCoverage cmds
            $ res === Ok
  where
    measureTagCoverage :: Commands (At Cmd) (At Resp) -> Property -> Property
    measureTagCoverage cmds prop = foldl' measureTag prop allTags
      where
        measureTag :: Property -> Tag -> Property
        measureTag p t = QC.cover 5 (t `Set.member` matchedTags) (show t) p

        matchedTags :: Set Tag
        matchedTags = Set.fromList $ tag $ execCmds cmds

prop_parallel :: DBLayerTest -> Property
prop_parallel db =
    forAllParallelCommands (sm dbLayerUnused) $ \cmds ->
    monadicIO $ do
        let sm' = sm db
            cmds' = addCleanDB cmds
        prettyParallelCommands cmds =<< runParallelCommands sm' cmds'

-- | The commands for parallel tests are run multiple times to detect
-- concurrency problems. We need to clean the database before every run. The
-- easiest way is to add a CleanDB command at the beginning of the prefix.
addCleanDB
    :: ParallelCommands (At Cmd) (At Resp)
    -> ParallelCommands (At Cmd) (At Resp)
addCleanDB (ParallelCommands p s) = ParallelCommands (clean <> p) s
  where
    clean = Commands [cmd resp mempty]
    cmd = Command (At CleanDB)
    resp = At (Resp (Right (Unit ())))

dbLayerUnused :: DBLayerTest
dbLayerUnused = error "DBLayer not used during command generation"
