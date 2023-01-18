{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- This module provides the 'TxSeq' type and related functions.
--
-- The 'TxSeq' type provides a way to model a linear sequence of transactions
-- and intermediate UTxO sets, where each individual transaction 't_i_j' is
-- associated with:
--
--    - a __preceding__ UTxO set 'utxo_i', to which 't_i_j' can be applied; and
--    - a __following__ UTxO set 'utxo_j', which represents the result of
--      applying 't_i_j' to 'utxo_i'.
--
-- Such sequences are __contiguous__, so that the UTxO set immediately
-- following a given transaction is also the UTxO set that immediately
-- precedes the next transaction in the sequence:
--
-- @
--    utxo_0 -> tx_0_1 -> utxo_1
--    utxo_1 -> tx_1_2 -> utxo_2
--    utxo_2 -> tx_2_3 -> utxo_3
--    ...
--    utxo_p -> tx_p_q -> utxo_q
-- @
--
-- The `TxSeq` type has an invariant, maintained by all public operations:
--
--    For every transition 'utxo_i -> tx_i_j -> utxo_j' in a sequence:
--
--    >>> safeApplyTxToUTxO tx_i_j utxo_i == pure utxo_j
--
-- This invariant can be checked wih the 'isValid' function, which evaluates to
-- 'True' if (and only if) the above property holds for all transitions in the
-- sequence.
--
-- Basic usage:
--
--    - Use 'fromUTxO' to construct an empty sequence from a starting UTxO set.
--    - Use 'appendTx' to append a transaction to the end of the sequence.
--    - Use 'toTransitionList' to list all UTxO set transitions of a sequence.
--    - Use 'toTxList' to list all transactions of a sequence.
--
-- In addition to appending transactions, it's also possible to specify
-- transaction groupings:
--
--    - Use 'appendTxGroupBoundary' to start a new transaction group.
--    - Use 'toTxGroupList' to view a 'TxSeq' as a list of transaction groups.
--
-- By default, all transactions are held within a single group.
--
module Cardano.Wallet.Primitive.Types.Tx.TxSeq
    (
    -- * Types
      TxSeq

    -- * Constructors
    , empty
    , fromUTxO

    -- * Measurements
    , length
    , txCount
    , txGroupCount
    , txGroupBoundaryCount

    -- * Validation
    , isValid
    , safeApplyTxToUTxO

    -- * Conversions
    , toTxList
    , toTxGroupList
    , toTransitionList

    -- * Views
    , assetIds
    , txIds
    , headUTxO
    , lastUTxO

    -- * Maps
    , mapAssetIds
    , mapTxIds

    -- * Extension
    , appendTx
    , appendTxGroupBoundary

    -- * Shrinking
    , dropHeadTx
    , dropLastTx
    , dropNullTx
    , dropNullTxs
    , dropGroupBoundary
    , dropGroupBoundaries
    , prefixes
    , suffixes
    , removeAssetId
    , removeAssets
    , shrinkAssetIds
    , shrinkTxIds
    ) where

import Prelude hiding
    ( length, seq )

import Cardano.Wallet.Primitive.Model
    ( applyTxToUTxO )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), txAssetIds, txMapAssetIds, txMapTxIds, txRemoveAssetId )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Data.Bifoldable
    ( Bifoldable (..) )
import Data.Bifunctor
    ( bimap )
import Data.Either
    ( isLeft, isRight, lefts, rights )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Set
    ( Set )

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq
import qualified Cardano.Wallet.Primitive.Types.Tx as Tx
import qualified Cardano.Wallet.Primitive.Types.Tx.TxOut as TxOut
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

--------------------------------------------------------------------------------
-- Public interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | The 'TxSeq' type provides a way to model a linear sequence of transactions
--   and intermediate UTxO sets, where each individual transaction 't_i_j'' is
--   associated with:
--
--    - a __preceding__ UTxO set 'utxo_i', to which 't_i_j' can be applied; and
--    - a __following__ UTxO set 'utxo_j', which represents the result of
--      applying 't_i_j' to 'utxo_i'.
--
-- Such sequences are __contiguous__, so that the UTxO set immediately
-- following a given transaction is also the UTxO set that immediately
-- precedes the next transaction in the sequence:
--
-- @
--    utxo_0 -> tx_0_1 -> utxo_1
--    utxo_1 -> tx_1_2 -> utxo_2
--    utxo_2 -> tx_2_3 -> utxo_3
--    ...
--    utxo_p -> tx_p_q -> utxo_q
-- @
--
newtype TxSeq = TxSeq
    {unTxSeq :: StateDeltaSeq UTxO (Either TxSeqGroupBoundary Tx)}
    deriving (Eq, Show)

-- | An internal type representing a boundary between groups of transactions.
--
-- See 'appendTxGroupBoundary'.
--
data TxSeqGroupBoundary = TxSeqGroupBoundary
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

-- | Constructs a 'TxSeq' from an empty 'UTxO' set.
--
empty :: TxSeq
empty = fromUTxO mempty

-- | Constructs a 'TxSeq' from an initial 'UTxO' set.
--
fromUTxO :: UTxO -> TxSeq
fromUTxO = TxSeq . Seq.fromState

--------------------------------------------------------------------------------
-- Measurements
--------------------------------------------------------------------------------

-- | Counts the total number of transitions in the sequence.
--
-- A transition can either be a transaction, or a group boundary.
--
length :: TxSeq -> Int
length = F.length . unTxSeq

-- | Counts the total number of transactions in the sequence.
--
txCount :: TxSeq -> Int
txCount = F.length . toTxList

-- | Counts the total number of transaction groups in the sequence.
--
txGroupCount :: TxSeq -> Int
txGroupCount = succ . txGroupBoundaryCount

-- | Counts the total number of transaction group boundaries in the sequence.
--
txGroupBoundaryCount :: TxSeq -> Int
txGroupBoundaryCount = F.length . lefts . Seq.toDeltaList . unTxSeq

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Returns 'True' if (and only if) the sequence is valid.
--
-- A 'TxSeq' is valid if (and only if) the following property holds for every
-- transition 'utxo_i -> tx_i_j -> utxo_j' in the sequence:
--
-- >>> safeApplyTxToUTxO tx_i_j utxo_i == pure utxo_j
--
isValid :: TxSeq -> Bool
isValid = (Just True ==) . Seq.isValidM safeAppendTxM . unTxSeq

-- | Safely applies a transaction to a UTxO set, producing an updated UTxO set.
--
-- A 'TxSeq' is valid if (and only if) the following property holds for every
-- transition 'utxo_i -> tx_i_j -> utxo_j' in the sequence:
--
-- >>> safeApplyTxToUTxO tx_i_j utxo_i == pure utxo_j
--
-- To check the validity of a 'TxSeq', use the 'isValid' function.
--
safeApplyTxToUTxO :: MonadFail m => Tx -> UTxO -> m UTxO
safeApplyTxToUTxO tx u
    | tx `canApplyTxToUTxO` u =
        pure $ tx `applyTxToUTxO` u
    | otherwise = fail
        "cannot spend an input that does not refer to a known UTxO"

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

-- | Generates the complete list of transactions for a given 'TxSeq'.
--
-- Satisfies the following properties:
--
-- >>> toTxList s == mconcat (toTxGroupList s)
-- >>> toTxList s == (\(_, tx, _) -> tx) <$> toTransitionList s
--
toTxList :: TxSeq -> [Tx]
toTxList = rights . Seq.toDeltaList . unTxSeq

-- | Generates the complete list of transaction groups for a given 'TxSeq'.
--
toTxGroupList :: TxSeq -> NonEmpty [Tx]
toTxGroupList = F.foldr acc (pure []) . unTxSeq
  where
    acc :: Either TxSeqGroupBoundary Tx -> NonEmpty [Tx] -> NonEmpty [Tx]
    acc delta groups@(h :| t) = case delta of
        Left TxSeqGroupBoundary ->
            [] `NE.cons` groups
        Right tx ->
            (tx : h) :| t

-- | Converts the given 'TxSeq' to a list of 'UTxO' set transitions.
--
-- For any consecutive pair of transitions in the resultant list:
--
-- @
--     [ ...
--     , (utxo_i, tx_i_j, utxo_j)
--     , (utxo_p, tx_p_q, utxo_q)
--     , ...
--     ]
-- @
--
-- The 'UTxO' set 'utxo_j' following 'tx_i_j' within the first transition is
-- guaranteed to be identical to the 'UTxO' set 'utxo_p' preceding 'tx_p_q'
-- within the second transition.
--
toTransitionList :: TxSeq -> [(UTxO, Tx, UTxO)]
toTransitionList (TxSeq s) =
    mapMaybe maybeTxTransition (Seq.toTransitionList s)
  where
    maybeTxTransition :: (u, Either a Tx, u) -> Maybe (u, Tx, u)
    maybeTxTransition (u0, e, u1) = e & either
        (const Nothing)
        (\tx -> Just (u0, tx, u1))

--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------

-- | Generates the set of all asset identifiers within a 'TxSeq'.
--
assetIds :: TxSeq -> Set AssetId
assetIds = bifoldMap UTxO.assetIds (either (const mempty) txAssetIds) . unTxSeq

-- | Generates the set of all transaction identifiers within a 'TxSeq'.
--
txIds :: TxSeq -> Set (Hash "Tx")
txIds
    = bifoldMap UTxO.txIds (either (const mempty) (Set.singleton . txId))
    . unTxSeq

-- | Views the head (initial) UTxO set of a 'TxSeq'.
--
headUTxO :: TxSeq -> UTxO
headUTxO = Seq.headState . unTxSeq

-- | Views the last (final) UTxO set of a 'TxSeq'.
--
lastUTxO :: TxSeq -> UTxO
lastUTxO = Seq.lastState . unTxSeq

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

-- | Applies a function to all asset identifiers within a 'TxSeq'.
--
-- Caution: specifying a non-injective mapping function may cause different
-- assets (and the quantities associated with them) to be coalesced together in
-- the resulting sequence.
--
-- However, provided that the given 'TxSeq' is valid when checked with the
-- 'isValid' function, the resultant sequence will also be valid, regardless of
-- whether or not the given mapping function is injective.
--
mapAssetIds :: (AssetId -> AssetId) -> TxSeq -> TxSeq
mapAssetIds f
    = TxSeq
    . bimap (UTxO.mapAssetIds f) (fmap (txMapAssetIds f))
    . unTxSeq

-- | Applies a function to all transaction identifiers within a 'TxSeq'.
--
-- Caution: specifying a non-injective mapping function may produce a 'TxSeq'
-- where transaction identifiers are no longer unique, which may cause the
-- 'TxSeq' to be invalid when checked with the 'isInvalid' function.
--
mapTxIds :: (Hash "Tx" -> Hash "Tx") -> TxSeq -> TxSeq
mapTxIds f
    = TxSeq
    . bimap (UTxO.mapTxIds f) (fmap (txMapTxIds f))
    . unTxSeq

--------------------------------------------------------------------------------
-- Extension
--------------------------------------------------------------------------------

-- | Extends a 'TxSeq' with a single additional transaction.
--
-- Returns an updated 'TxSeq' if and only if the given transaction can be
-- safely applied with 'safeApplyTxToUTxO' to last 'UTxO' set in the sequence
-- (see 'lastUTxO').
--
-- Otherwise, returns 'Nothing'.
--
appendTx :: Tx -> TxSeq -> Maybe TxSeq
appendTx tx =
    fmap TxSeq . Seq.applyDeltaM safeAppendTxM (Right tx) . unTxSeq

-- | Extends a 'TxSeq' with a transaction group boundary.
--
-- Transactions added after the boundary will appear within a new group when
-- the 'TxSeq' is viewed with the 'toTxGroupList' function.
--
appendTxGroupBoundary :: TxSeq -> TxSeq
appendTxGroupBoundary =
    TxSeq . Seq.applyDelta const (Left TxSeqGroupBoundary) . unTxSeq

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

-- | Removes the head (left-most) transaction of a 'TxSeq'.
--
-- The head UTxO set is also removed (see 'headUTxO').
--
dropHeadTx :: TxSeq -> Maybe TxSeq
dropHeadTx = fmap TxSeq . Seq.dropHead . unTxSeq

-- | Removes the last (right-most) transaction of a 'TxSeq'.
--
-- The last UTxO set is also removed (see 'lastUTxO').
--
dropLastTx :: TxSeq -> Maybe TxSeq
dropLastTx = fmap TxSeq . Seq.dropLast . unTxSeq

-- | For a given sequence 's', generates all proper subsequences of 's' where
--   exactly one null transaction has been removed.
--
-- In the context of a 'TxSeq', a null transaction is a transaction 't' that
-- has no effect: the 'UTxO' set that precedes 't' is identical to the 'UTxO'
-- set that follows 't'.
--
dropNullTx :: TxSeq -> [TxSeq]
dropNullTx (TxSeq s) = TxSeq <$> Seq.dropEmptyTransitionWhere isRight s

-- | Removes all null transactions from a 'TxSeq'.
--
-- In the context of a 'TxSeq', a null transaction is a transaction 't' that
-- has no effect: the 'UTxO' set that precedes 't' is identical to the 'UTxO'
-- set that follows 't'.
--
dropNullTxs :: TxSeq -> TxSeq
dropNullTxs (TxSeq s) = TxSeq $ Seq.dropEmptyTransitionsWhere isRight s

-- | For a given sequence 's', generates all proper subsequences of 's' where
--   exactly one group boundary has been removed.
--
-- All transactions in the sequence are preserved.
--
dropGroupBoundary :: TxSeq -> [TxSeq]
dropGroupBoundary (TxSeq s) = TxSeq <$> Seq.dropEmptyTransitionWhere isLeft s

-- | Removes all group boundaries from a 'TxSeq'.
--
dropGroupBoundaries :: TxSeq -> TxSeq
dropGroupBoundaries (TxSeq s) = TxSeq $ Seq.dropEmptyTransitionsWhere isLeft s

-- | Lists all proper prefixes of the given 'TxSeq'.
--
-- The list is sorted into ascending order of length, such that each element is
-- a proper prefix of the subsequent element.
--
prefixes :: TxSeq -> [TxSeq]
prefixes = fmap TxSeq . Seq.prefixes . unTxSeq

-- | Lists all proper suffixes of the given 'TxSeq'.
--
-- The list is sorted into ascending order of length, such that each element is
-- a proper suffix of the subsequent element.
--
suffixes :: TxSeq -> [TxSeq]
suffixes = fmap TxSeq . Seq.suffixes . unTxSeq

-- | Removes a given asset from all transactions in a 'TxSeq'.
--
removeAssetId :: TxSeq -> AssetId -> TxSeq
removeAssetId (TxSeq s) a = TxSeq $
    bimap (`UTxO.removeAssetId` a) (fmap (`txRemoveAssetId` a)) s

-- | Removes all non-ada assets from all transactions within a 'TxSeq'.
--
removeAssets :: TxSeq -> TxSeq
removeAssets s0 = F.foldl' removeAssetId s0 (assetIds s0)

-- | Simplifies the set of asset identifiers within a 'TxSeq'.
--
-- The number of unique assets is preserved, but the length of each asset
-- identifier is minimized.
--
shrinkAssetIds :: TxSeq -> TxSeq
shrinkAssetIds s = mapAssetIds toSimpleAssetId s
  where
    toSimpleAssetId :: AssetId -> AssetId
    toSimpleAssetId = mapToFunction
        (head simpleAssetIds)
        (Map.fromList $ F.toList (assetIds s) `zip` simpleAssetIds)

-- | Simplifies the set of transaction identifiers within a 'TxSeq'.
--
-- The number of transactions is preserved, but the length of each transaction
-- identifier is minimized.
--
shrinkTxIds :: TxSeq -> TxSeq
shrinkTxIds s = mapTxIds toSimpleTxId s
  where
    toSimpleTxId :: Hash "Tx" -> Hash "Tx"
    toSimpleTxId = mapToFunction
        (head simpleTxIds)
        (Map.fromList $ F.toList (txIds s) `zip` simpleTxIds)

--------------------------------------------------------------------------------
-- Internal interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Domain-specific constants
--------------------------------------------------------------------------------

simpleAssetIds :: [AssetId]
simpleAssetIds
    = AssetId (UnsafeTokenPolicyId $ Hash mempty)
    . UnsafeTokenName
    . T.encodeUtf8
    . T.pack
    . show <$> [0 :: Integer ..]

simpleTxIds :: [Hash "Tx"]
simpleTxIds = Hash . T.encodeUtf8 . T.pack . show <$> [0 :: Integer ..]

--------------------------------------------------------------------------------
-- Domain-specific functions
--------------------------------------------------------------------------------

canApplyTxToUTxO :: Tx -> UTxO -> Bool
canApplyTxToUTxO tx u =  (&&)
    (all inputRefIsValid (Tx.resolvedInputs tx))
    (all inputRefIsValid (Tx.resolvedCollateralInputs tx))
  where
    inputRefIsValid :: (TxIn, Maybe TxOut) -> Bool
    inputRefIsValid (ti, c) =
        case UTxO.lookup ti u of
            Nothing -> False
            Just c' -> TxOut.coin c' == maybe (Coin 0) TxOut.coin c

safeAppendTx :: MonadFail m => UTxO -> Tx -> m UTxO
safeAppendTx = flip safeApplyTxToUTxO

safeAppendTxM :: MonadFail m => UTxO -> Either TxSeqGroupBoundary Tx -> m UTxO
safeAppendTxM u = either (const (pure u)) (safeAppendTx u)

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

mapToFunction :: Ord k => v -> Map k v -> (k -> v)
mapToFunction = flip . Map.findWithDefault
