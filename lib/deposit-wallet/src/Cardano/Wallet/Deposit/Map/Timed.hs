{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Deposit.Map.Timed
    (
    -- * Timed
      Timed (..)
    -- * TimedSeq
    , TimedSeq
    -- ** Construction
    , fromList
    , singleton
    -- ** Destruction
    , toList
    -- ** Query
    , takeAfter
    , takeUpTo
    , extractInterval
    , minKey
    , maxKey
    -- ** Modification
    , dropAfter
    , dropBefore
    -- ** Functor
    , fmapTimedSeq
    )
where

import Prelude hiding
    ( null
    )

import Data.Bifunctor
    ( Bifunctor (..)
    )
import Data.FingerTree
    ( FingerTree
    , Measured (..)
    , ViewL (..)
    , ViewR (..)
    , dropUntil
    , fmap'
    , split
    , takeUntil
    , viewl
    , viewr
    , (<|)
    )
import Data.Function
    ( (&)
    )
import Data.Monoid
    ( Last (..)
    )

import qualified Data.FingerTree as FingerTree
import qualified Data.Foldable as F

-- | A value paired with a timestamp.
data Timed t a = Timed
    { time :: Last t
    , monoid :: a
    }
    deriving (Eq, Ord, Show, Functor, Foldable)

instance Semigroup a => Semigroup (Timed t a) where
    Timed t1 a1 <> Timed t2 a2 = Timed (t1 <> t2) (a1 <> a2)

instance Monoid a => Monoid (Timed t a) where
    mempty = Timed mempty mempty

instance Monoid a => Measured (Timed t a) (Timed t a) where
    measure = id

-- | A sequence of timed values with a monoidal annotation as itself.
-- These values have a semigroup instance that will collapse adjacent values
-- with the same timestamp.
-- It's up to the user to maintain the invariant that
-- the sequence is sorted by timestamp.
newtype TimedSeq t a = TimedSeq
    { unTimedSeq :: FingerTree (Timed t a) (Timed t a)
    }
    deriving (Eq, Show)

fmapTimedSeq
    :: (Monoid a1, Monoid a2) => (a1 -> a2) -> TimedSeq t a1 -> TimedSeq t a2
fmapTimedSeq f = TimedSeq . fmap' (fmap f) . unTimedSeq

singleton :: Monoid a => Timed t a -> TimedSeq t a
singleton = TimedSeq . FingerTree.singleton

instance Monoid a => Measured (Timed t a) (TimedSeq t a) where
    measure = measure . unTimedSeq

instance Foldable (TimedSeq t) where
    foldMap f = foldMap (f . monoid) . unTimedSeq

onFingerTree
    :: ( FingerTree (Timed t a) (Timed t a)
         -> FingerTree (Timed t a) (Timed t a)
       )
    -> TimedSeq t a
    -> TimedSeq t a
onFingerTree f = TimedSeq . f . unTimedSeq

instance (Semigroup a, Monoid a, Eq t) => Semigroup (TimedSeq t a) where
    TimedSeq a <> TimedSeq b = case (viewr a, viewl b) of
        (EmptyR, _) -> TimedSeq b
        (_, EmptyL) -> TimedSeq a
        (a' :> Timed t1 v1, Timed t2 v2 :< b')
            | t1 == t2 -> TimedSeq $ a' <> (Timed t1 (v1 <> v2) <| b')
            | otherwise -> TimedSeq $ a <> b

instance (Monoid a, Eq t) => Monoid (TimedSeq t a) where
    mempty = TimedSeq FingerTree.empty

-- | Construct a 'TimedSeq' from a list of 'Timed' values.
fromList :: (Monoid a, Eq t) => [Timed t a] -> TimedSeq t a
fromList = mconcat . fmap singleton

-- | Convert a 'TimedSeq' to a list of 'Timed' values.
-- This is not the inverse of 'fromList' as some values may have been merged. But
-- fromList . toList == id.
toList :: TimedSeq t a -> [Timed t a]
toList = F.toList . unTimedSeq

takeAfterElement
    :: (Monoid a, Ord q)
    => (t -> q)
    -> TimedSeq t a
    -> Maybe (Timed t a, TimedSeq t a)
takeAfterElement bucket (TimedSeq tseq) = case viewl tseq of
    EmptyL -> Nothing
    hd :< _ ->
        let
            (taken, rest) =
                split (\q -> (bucket <$> time q) > (bucket <$> time hd)) tseq
        in
            Just (measure taken, TimedSeq rest)

takeBeforeElement
    :: (Monoid a, Ord q)
    => (t -> q)
    -> TimedSeq t a
    -> Maybe (Timed t a, TimedSeq t a)
takeBeforeElement bucket (TimedSeq tseq) = case viewr tseq of
    EmptyR -> Nothing
    _ :> hd ->
        let
            (rest, taken) =
                split (\q -> (bucket <$> time q) >= (bucket <$> time hd)) tseq
        in
            Just (measure taken, TimedSeq rest)

takeAfterElements
    :: (Monoid a, Ord q, Ord t)
    => (t -> q)
    -> Maybe Int
    -> TimedSeq t a
    -> (TimedSeq t a, Maybe t)
takeAfterElements _dt (Just 0) (TimedSeq tseq) =
    ( mempty
    , case viewl tseq of
        EmptyL -> Nothing
        Timed (Last hd) _ :< _ -> hd
    )
takeAfterElements bucket mn tseq =
    case takeAfterElement bucket tseq of
        Just (v, rest) ->
            first (onFingerTree (v <|))
                $ takeAfterElements bucket (subtract 1 <$> mn) rest
        _ -> (mempty, Nothing)

takeBeforeElements
    :: (Monoid a, Ord q, Ord t)
    => (t -> q)
    -> Maybe Int
    -> TimedSeq t a
    -> (TimedSeq t a, Maybe t)
takeBeforeElements _dt (Just 0) (TimedSeq tseq) =
    ( mempty
    , case viewr tseq of
        EmptyR -> Nothing
        _ :> Timed (Last hd) _ -> hd
    )
takeBeforeElements bucket mn tseq = case takeBeforeElement bucket tseq of
    Just (v, rest) ->
        first (onFingerTree (v <|))
            $ takeBeforeElements bucket (subtract 1 <$> mn) rest
    _ -> (mempty, Nothing)

-- | Extract the first n elements from a timed seq after and including
-- a given start time after applying a bucketing function.
-- The result is a map of the extracted elements and the next time to start from.
takeAfter
    :: (Monoid a, Ord q, Ord t)
    => (t -> q)
    -- ^ A function to bucket the timestamps.
    -> Maybe t
    -- ^ The start time to extract elements from.
    -> Maybe Int
    -- ^ The number of elements to extract.
    -> TimedSeq t a
    -- ^ The timed sequence to extract elements from.
    -> (TimedSeq t a, Maybe t)
takeAfter bucket mstart mcount =
    takeAfterElements bucket mcount
        . onFingerTree
            ( dropUntil
                ( \q -> mstart & maybe True (\t -> time q >= Last (Just t))
                )
            )

-- | Extract the last n elements from a timed seq before and excluding
-- a given start time after applying a bucketing function.
-- The result is a map of the extracted elements and the next time to start from.
takeUpTo
    :: (Monoid a, Ord q, Ord t)
    => (t -> q)
    -- ^ A function to bucket the timestamps.
    -> Maybe t
    -- ^ The start time to extract elements from.
    -> Maybe Int
    -- ^ The number of elements to extract.
    -> TimedSeq t a
    -- ^ The timed sequence to extract elements from.
    -> (TimedSeq t a, Maybe t)
takeUpTo bucket mstart mcount =
    takeBeforeElements bucket mcount
        . onFingerTree
            ( takeUntil
                (\q -> mstart & maybe False (\t -> time q > Last (Just t)))
            )

-- | Try to extract the first element time from a tseq.
minKey :: Monoid a => TimedSeq t a -> Maybe t
minKey (TimedSeq tseq) = case viewl tseq of
    Timed (Last (Just t)) _ :< _ -> Just t
    _ -> Nothing

-- | Try to extract the last element time from a tseq.
maxKey :: Monoid a => TimedSeq t a -> Maybe t
maxKey (TimedSeq tseq) = case viewr tseq of
    _ :> Timed (Last (Just t)) _ -> Just t
    _ -> Nothing

-- | Extract all elements from a tseq that are within the given time interval.
extractInterval
    :: (Monoid a, Ord t) => t -> t -> TimedSeq t a -> Timed t a
extractInterval t0 t1 (TimedSeq tseq) =
    measure
        $ takeUntil (\q -> time q > Last (Just t1))
        $ dropUntil (\q -> time q >= Last (Just t0)) tseq

-- | Drop all elements from a tseq that are after the given time.
dropAfter :: (Ord t, Monoid a) => t -> TimedSeq t a -> TimedSeq t a
dropAfter t = onFingerTree $ takeUntil (\q -> time q > Last (Just t))

-- | Drop all elements from a tseq that are before the given time.
dropBefore :: (Ord t, Monoid a) => t -> TimedSeq t a -> TimedSeq t a
dropBefore t = onFingerTree $ dropUntil (\q -> time q >= Last (Just t))
