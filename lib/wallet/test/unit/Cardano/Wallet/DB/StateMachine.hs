{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unused-foralls #-}
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
    , validateGenerators
    , TestConstraints
    ) where

import Prelude

import Cardano.Address.Script
    ( ScriptTemplate (..) )
import Cardano.Pool.Types
    ( PoolId (..) )
import Cardano.Wallet.Address.Book
    ( AddressBookIso )
import Cardano.Wallet.Address.Derivation
    ( Depth (..), DerivationPrefix, Index, KeyFingerprint, Role (..) )
import Cardano.Wallet.Address.Derivation.Shared
    ()
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Address.Discovery
    ( IsOurs, PendingIxs )
import Cardano.Wallet.Address.Discovery.Random
    ( RndState )
import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap, SeqState (..) )
import Cardano.Wallet.Address.Discovery.Shared
    ( Readiness
    , SharedAddressPool (..)
    , SharedAddressPools (..)
    , SharedState (..)
    )
import Cardano.Wallet.DB
    ( DBLayer (..), DBLayerParams (..) )
import Cardano.Wallet.DB.Arbitrary
    ( GenState, GenTxHistory (..), InitialCheckpoint (..) )
import Cardano.Wallet.DB.Pure.Implementation
    ( Database (..)
    , Err (..)
    , TxHistory
    , WalletDatabase (..)
    , mInitializeWallet
    , mListCheckpoints
    , mPutDelegationRewardBalance
    , mPutTxHistory
    , mReadCheckpoint
    , mReadDelegationRewardBalance
    , mReadGenesisParameters
    , mReadTxHistory
    , mRollbackTo
    )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyGenesisParameters, dummyTimeInterpreter )
import Cardano.Wallet.Primitive.Model
    ( Wallet )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , ChainPoint
    , DecentralizationLevel
    , EpochNo (..)
    , ExecutionUnits (..)
    , FeePolicy
    , GenesisParameters (..)
    , LinearFunction
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
    , TxCBOR
    , TxMeta
    , TxMetadata
    , TxScriptValidity
    , TxStatus
    , inputs
    )
import Cardano.Wallet.Primitive.Types.Tx.Constraints
    ( TxSize (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Read.Eras.EraValue
    ( eraValueSerialize )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant (..) )
import Control.DeepSeq
    ( NFData )
import Control.Foldl
    ( Fold (..) )
import Control.Monad
    ( forM_, replicateM, void, when )
import Control.Monad.IO.Unlift
    ( MonadIO )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString, hash )
import Data.Bifunctor
    ( first )
import Data.Foldable
    ( foldl', toList )
import Data.Functor.Classes
    ( Eq1, Show1 )
import Data.Generics.Internal.VL
    ( build )
import Data.List.Extra
    ( enumerate )
import Data.Map
    ( Map )
import Data.Map.Strict.NonEmptyMap.Internal
    ( NonEmptyMap )
import Data.Maybe
    ( catMaybes, fromJust )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Set
    ( Set )
import Data.Time.Clock
    ( NominalDiffTime, diffUTCTime, getCurrentTime )
import Data.TreeDiff
    ( ToExpr (..), defaultExprViaShow, genericToExpr )
import Fmt
    ( Buildable )
import GHC.Generics
    ( Generic, Generic1 )
import GHC.Stack
    ( HasCallStack, callStack )
import Test.Hspec
    ( SpecWith, describe, expectationFailure, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , applyArbitrary2
    , arbitraryBoundedEnum
    , frequency
    , generate
    , resize
    , (===)
    )
import Test.QuickCheck.Monadic
    ( monadicIO, run )
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
    , runCommands
    , (.==)
    )
import Test.StateMachine.Types
    ( Commands (..) )
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
    m = fromJust $ digestFromByteString spliced
    spliced = wid' <> B8.drop (B8.length wid') hashed
    hashed = BA.convert (hash wid' :: Digest Blake2b_160)
    wid' = B8.pack wid

-- | Represent (XPrv, Hash) as a string.
type MPrivKey = String


{-------------------------------------------------------------------------------
  Language
-------------------------------------------------------------------------------}

data Cmd s wid
    = CreateWallet
    | ReadCheckpoint
    | ListCheckpoints
    | PutTxHistory TxHistory
    | ReadTxHistory
        (Maybe Coin)
        SortOrder
        (Range SlotNo)
        (Maybe TxStatus)
    | GetTx (Hash "Tx")
    | ReadGenesisParameters
    | RollbackTo wid Slot
    | PutDelegationRewardBalance wid Coin
    | ReadDelegationRewardBalance
    deriving stock (Show, Generic1, Eq, Functor, Foldable, Traversable)
    deriving anyclass (CommandNames)

data Success s wid
    = Unit ()
    | NewWallet wid
    | WalletId' wid
    | Checkpoint (Wallet s)
    | Metadata WalletMetadata
    | TxHistory [TransactionInfo]
    | LocalTxSubmission [LocalTxSubmissionStatus (Hash "Tx")]
    | GenesisParams (Maybe GenesisParameters)
    | ChainPoints [ChainPoint]
    | Point ChainPoint
    | DelegationRewardBalance Coin
    | StakeKeyStatus Bool
    deriving stock (Show, Generic1, Eq, Functor, Foldable, Traversable)

newtype Resp s wid
    = Resp (Either Err (Success s wid))
    deriving (Show, Eq)

instance Functor (Resp s) where
    fmap f (Resp r) = Resp (fmap (fmap f) r)

instance Foldable (Resp s) where
    foldMap f (Resp r) = either mempty (foldMap f) r

instance Traversable (Resp s) where
    traverse f (Resp (Right r)) = Resp . Right <$> traverse f r
    traverse _f (Resp (Left e)) = pure $ Resp . Left $ e

{-------------------------------------------------------------------------------
  Interpreter: mock implementation
-------------------------------------------------------------------------------}

runMock :: HasCallStack => Cmd s MWid -> Mock s -> (Resp s MWid, Mock s)
runMock = \case
    CreateWallet -> \db@(Database wid _ _) -> (Resp $ Right $ NewWallet wid, db)
    ListCheckpoints ->
        first (Resp . fmap ChainPoints) . mListCheckpoints
    ReadCheckpoint ->
        first (Resp . fmap Checkpoint) . mReadCheckpoint

    PutTxHistory txs ->
        first (Resp . fmap Unit) . mPutTxHistory txs
    ReadTxHistory minW order range status ->
        first (Resp . fmap TxHistory)
        . mReadTxHistory timeInterpreter minW order range status
    GetTx _tid ->
        first (Resp . fmap (TxHistory . maybe [] pure))
        -- TODO: Implement mGetTx
        -- . mGetTx wid tid
        . (Right Nothing,)
    ReadGenesisParameters ->
        first (Resp . fmap GenesisParams) . mReadGenesisParameters
    PutDelegationRewardBalance _wid amt ->
        first (Resp . fmap Unit) . mPutDelegationRewardBalance amt
    ReadDelegationRewardBalance ->
        first (Resp . fmap DelegationRewardBalance)
        . mReadDelegationRewardBalance
    RollbackTo _wid point ->
        first (Resp . fmap Point) . mRollbackTo point
  where
    _requireCallStack = callStack
    timeInterpreter = dummyTimeInterpreter

{-------------------------------------------------------------------------------
  Interpreter: real I/O
-------------------------------------------------------------------------------}

runIO
    :: forall m s
     . MonadIO m
    => DBLayer m s
    -> Cmd s WalletId
    -> m (Resp s WalletId)
runIO DBLayer{..} = fmap Resp . go
  where
    wid = walletId_

    runDBSuccess
      :: (stm a -> m a)
         -> (a -> Success s WalletId)
         -> stm a
         -> m (Either Err (Success s WalletId))
    runDBSuccess atomically' s action = do
        r <- atomically' action
        pure $ Right $ s r
    go
        :: Cmd s WalletId
        -> m (Either Err (Success s WalletId))
    go = \case
        CreateWallet -> pure $ Right $ NewWallet wid
        ReadCheckpoint -> Right . Checkpoint <$>
            atomically readCheckpoint
        ListCheckpoints -> Right . ChainPoints <$>
            atomically listCheckpoints
        PutTxHistory txs -> runDBSuccess atomically Unit $ putTxHistory txs
        ReadTxHistory minWith order range status ->
            fmap (Right . TxHistory) $
            atomically $
            readTransactions minWith order range status Nothing
        GetTx tid ->
            runDBSuccess atomically (TxHistory . maybe [] pure) $ getTx tid
        ReadGenesisParameters -> Right . GenesisParams <$>
            atomically readGenesisParameters
        PutDelegationRewardBalance _wid amt -> runDBSuccess atomically Unit $
            putDelegationRewardBalance amt
        ReadDelegationRewardBalance -> Right . DelegationRewardBalance <$>
            atomically readDelegationRewardBalance
        RollbackTo _wid sl -> runDBSuccess atomically Point $ rollbackTo sl


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

initModel :: MWid -> DBLayerParams s -> Model s r
initModel mwid params = Model (mInitializeWallet mwid params) []

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
    :: Model s Symbolic
    -> Maybe (Gen (Cmd s :@ Symbolic))
generator (Model _ wids) = Just $ frequency $ fmap (fmap At) . snd <$> concat
    [ generatorWithoutId
    , if null wids then [] else generatorWithWid (fst <$> wids)
    ]

declareGenerator
    :: String -- ^ A readable name
    -> Int -- ^ Frequency
    -> Gen cmd -- ^ Generator
    -> (String, (Int, Gen cmd))
declareGenerator name f gen = (name, (f, gen))

generatorWithoutId
    :: [(String, (Int, Gen (Cmd s (Reference WalletId r))))]
generatorWithoutId =
    [ declareGenerator "CreateWallet" 5 $ pure CreateWallet
    ]

generatorWithWid
    :: forall s r
     . [Reference WalletId r]
    -> [(String, (Int, Gen (Cmd s (Reference WalletId r))))]
generatorWithWid wids =
    [ declareGenerator "ReadCheckpoint" 5
        $ pure ReadCheckpoint
    , declareGenerator "ListCheckpoints" 5
        $ pure ListCheckpoints
    , declareGenerator "PutTxHistory" 5
        $ PutTxHistory <$> fmap unGenTxHistory arbitrary
    , declareGenerator "ReadTxHistory" 5
        $ ReadTxHistory
            <$> genMinWithdrawal
            <*> genSortOrder
            <*> genRange
            <*> arbitrary
    , declareGenerator "RollbackTo" 1
        $ RollbackTo <$> genId <*> arbitrary
    -- TODO: Implement mPrune
    -- , declareGenerator "Prune" 1
    --     $ Prune <$> genId <*> arbitrary
    , declareGenerator "ReadGenesisParameters" 1
        $ pure ReadGenesisParameters
    ]
  where
    genId :: Gen (Reference WalletId r)
    genId = QC.elements wids

    genSortOrder :: Gen SortOrder
    genSortOrder = arbitraryBoundedEnum

    genRange :: Gen (Range SlotNo)
    genRange = applyArbitrary2 Range

    genMinWithdrawal :: Gen (Maybe Coin)
    genMinWithdrawal = frequency
        [ (10, pure Nothing)
        , (1, Just <$> arbitrary)
        ]

isUnordered :: Ord x => [x] -> Bool
isUnordered xs = xs /= L.sort xs

shrinker
    :: Cmd s :@ r
    -> [Cmd s :@ r]
shrinker (At cmd) = case cmd of
    PutTxHistory h ->
        [ At $ PutTxHistory h'
        | h' <- map unGenTxHistory . shrink . GenTxHistory $ h
        ]
    RollbackTo wid sid ->
        [ At $ RollbackTo wid sid'
        | sid' <- shrink sid
        ]
    ReadTxHistory minW so range status ->
        [ At $ ReadTxHistory minW so range' status
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
    :: MonadIO m
    => DBLayer m s
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

type TestConstraints s =
    ( Eq s
    , GenState s
    , Arbitrary (Wallet s)
    , ToExpr s
    )

sm
    :: (MonadIO m, TestConstraints s)
    => MWid
    -> DBLayerParams s
    -> DBLayer m s
    -> StateMachine (Model s) (At (Cmd s)) m (At (Resp s))
sm mwid params db = QSM.StateMachine
    { initModel = initModel mwid params
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

instance ToExpr (PendingIxs 'CredFromScriptK) where
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

instance ToExpr TxCBOR where
    toExpr = toExpr . fst . build eraValueSerialize

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
    = SuccessfulReadTxHistory
    | UnsuccessfulReadTxHistory
    | TxUnsortedInputs
      -- ^ Putting a transaction with unsorted inputs.
    | TxUnsortedOutputs
    | RolledBackOnce
      -- ^ We have rolled back at least once
    deriving (Bounded, Enum, Eq, Ord, Show)

-- | The list of all possible 'Tag' values.
allTags :: [Tag]
allTags = enumerate

tag :: forall s. [Event s Symbolic] -> [Tag]
tag = Foldl.fold $ catMaybes <$> sequenceA
    [ readTransactions (not . null) SuccessfulReadTxHistory
    , readTransactions null UnsuccessfulReadTxHistory
    , txUnsorted inputs TxUnsortedInputs
    , txUnsorted outputs TxUnsortedOutputs
    , countAction RolledBackOnce (>= 1) isRollbackSuccess
    ]
  where
    isRollbackSuccess :: Event s Symbolic -> Maybe MWid
    isRollbackSuccess ev = case (cmd ev, mockResp ev, before ev) of
        (At (RollbackTo wid _), Resp (Right Point{}), Model _ wids ) ->
            Just (wids ! wid)
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

    readTransactions
        :: ([TransactionInfo] -> Bool)
        -> Tag
        -> Fold (Event s Symbolic) (Maybe Tag)
    readTransactions check res = Fold update False (extractf res)
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
                (At (PutTxHistory h), Resp (Right _)) ->
                    any (isUnordered . sel . fst) h
                _otherwise ->
                    False

    extractf :: a -> Bool -> Maybe a
    extractf a t = if t then Just a else Nothing

execCmd
    :: Model s Symbolic
    -> QSM.Command (At (Cmd s)) (At (Resp s))
    -> Event s Symbolic
execCmd model (QSM.Command cmd resp _vars) =
    lockstep model cmd resp

execCmds
    :: MWid
    -> DBLayerParams s
    -> QSM.Commands (At (Cmd s)) (At (Resp s))
    -> [Event s Symbolic]
execCmds mwid params = \(QSM.Commands cs) -> go (initModel mwid params) cs
  where
    go
        :: Model s Symbolic
        -> [QSM.Command (At (Cmd s)) (At (Resp s))]
        -> [Event s Symbolic]
    go _ [] = []
    go m (c : cs) = e : go (after e) cs where e = execCmd m c

{-------------------------------------------------------------------------------
  Top-level tests
-------------------------------------------------------------------------------}

testMWid :: MWid
testMWid = MWid "test"

testWid :: WalletId
testWid = unMockWid testMWid

genDBParams
    :: ( AddressBookIso s
       , Arbitrary s
       , Buildable s
       , Eq s
       , IsOurs s Address
       , IsOurs s RewardAccount
       , NFData s
       , Show s
       )
    => Gen (DBLayerParams s)
genDBParams =
    DBLayerParams
        <$> (getInitialCheckpoint <$> arbitrary)
        <*> arbitrary
        <*> fmap unGenTxHistory arbitrary
        <*> pure dummyGenesisParameters

prop_sequential
    :: forall s
     . TestConstraints s
    => (WalletId -> DBLayerParams s -> (IO (IO (),DBLayer IO s)))
    -> Property
prop_sequential newDB =
    QC.forAll genDBParams $ \params ->
        let measureTagCoverage :: Commands (At (Cmd s)) (At (Resp s))
                -> Property -> Property
            measureTagCoverage cmds prop = foldl' measureTag prop allTags
              where
                matchedTags :: Set Tag
                matchedTags =
                    Set.fromList $ tag $ (execCmds testMWid params) cmds
                measureTag :: Property -> Tag -> Property
                measureTag p t =
                    QC.cover 5 (t `Set.member` matchedTags) (show t) p
        in QC.checkCoverage $
        forAllCommands (sm @IO @s testMWid params dbLayerUnused) Nothing $
            \cmds -> monadicIO $ do
                (destroyDB, db) <- run (newDB testWid params)
                let sm' = sm testMWid params db
                (hist, _model, res) <- runCommands sm' cmds
                prettyCommands sm' hist
                    $ measureTagCoverage cmds
                    $ res === Ok
                run destroyDB -- fixme: bracket difficult

-- Controls that generators and shrinkers can run within a reasonable amount of
-- time. We have been bitten several times already by generators which took much
-- longer than what they should, causing timeouts in the test suite.
validateGenerators
    :: forall s. SpecWith ()
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

dbLayerUnused :: DBLayer m s
dbLayerUnused = error "DBLayer not used during command generation"
