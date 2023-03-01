{-# LANGUAGE TypeFamilies #-}

{- |
 Copyright: © 2023 IOHK
 License: Apache-2.0

 Abstract data type that describes a chain of checkpoints
-}
module Cardano.Wallet.Checkpoints.Chain (
    Chain ,
    DeltaChain (..),
    member,
    tip,
    summary,
    origin,
    bootChain
) where

import Prelude

import Data.Delta
    ( Delta (..) )
import Data.Foldable
    ( fold, toList )
import Data.Map.Strict
    ( Map, lookupMax )
import Data.Maybe
    ( fromMaybe )
import Data.Set
    ( Set )

import qualified Data.Map.Strict as Map

-- | A chain of checkpoints, ordered from oldest to newest
data Chain timestamp transition = Chain
    { -- | The origin of the chain
      origin :: timestamp
    , -- | The rest of the chain, ordered from oldest to newest
      chain :: Map timestamp transition
    }
    deriving (Eq, Show)

-- | The empty chain, with a given origin.
bootChain ::
    Ord timestamp =>
    timestamp ->
    Chain timestamp transition
bootChain o = Chain o mempty

-- | The tip of the chain, always present because we hold at least the origin with mempty.
tip :: Monoid transition => Chain timestamp transition -> (timestamp, transition)
tip (Chain o c) = fromMaybe (o, mempty) $ lookupMax c

-- | Check if a timestamp is a member of the chain.
member :: Ord timestamp => timestamp -> Chain timestamp transition -> Bool
member t (Chain o c)
    | t < o = False
    | t == o = True
    | otherwise = Map.member t c

{- | The summary of the chain, i.e. the summary of all transitions in the chain,
  folded together via the monoid instance.
-}
summary :: Monoid transition => Chain timestamp transition -> transition
summary = fold . chain

-- | Append a new tip to the chain. It will be ignored if it is not newwer than the current tip.
appendTip ::
    (Ord timestamp, Monoid transition) =>
    timestamp ->
    transition ->
    Chain timestamp transition ->
    Chain timestamp transition
appendTip t transition ch@(Chain o c)
    | t > fst (tip ch) = Chain o (Map.insert t transition c)
    | otherwise = ch

{- | Remove a set of timestamps from the chain. All unknown timestamps, or origin, are ignored.
 The summary of the chain will be preserved.
-}
collapseTimestamps ::
    (Ord timestamp, Monoid transition) =>
    Set timestamp ->
    Chain timestamp transition ->
    Chain timestamp transition
collapseTimestamps ts ch@(Chain o c) = case dropWhile (<= o) $ toList ts of
    [] -> ch
    ts' -> Chain o $ Map.fromList $ go mempty ts' $ Map.assocs c
  where
    go ::
        (Ord timestamp, Monoid transition) =>
        transition ->
        [timestamp] ->
        [(timestamp, transition)] ->
        [(timestamp, transition)]
    go _ _ [] = []
    go old  [] ((t',transition) :xs) = (t', old <> transition) : xs
    go old tss@(t : ts') xss@((t', transition) : xs)
        | t > t' = (t', old <> transition) : go mempty tss xs
        | t == t' = go (old <> transition) ts' xs
        | otherwise = go old ts' xss

{- | Rollback the chain to a given timestamp. Origin cannot be removed.
 All timestamps newer than the given timestamp are removed.
-}
rollbackTo :: Ord timestamp => timestamp -> Chain timestamp transition -> Chain timestamp transition
rollbackTo t (Chain o c) = Chain o $ Map.takeWhileAntitone (<= t) c

data DeltaChain timestamp transition
    = AppendTransition timestamp transition
    | CollapseTransitions (Set timestamp)
    | RollbackTo timestamp

-- | Apply a delta to a chain
instance (Ord timestamp, Monoid transition) => Delta (DeltaChain timestamp transition) where
    type Base (DeltaChain timestamp transition) = Chain timestamp transition
    apply (AppendTransition t transition) ch = appendTip t transition ch
    apply (CollapseTransitions ts) ch = collapseTimestamps ts ch
    apply (RollbackTo t) ch = rollbackTo t ch
