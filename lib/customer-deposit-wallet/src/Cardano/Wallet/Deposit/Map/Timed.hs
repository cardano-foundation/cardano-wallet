{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Deposit.Map.Timed
    ( Timed (..)
    , TimedSeq
    , takeAfter
    , takeUpTo
    , extractInterval
    , minKey
    , maxKey
    , dropAfter
    , dropBefore
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

-- | A sequence of timed values with a monoidal annotation as itself
type TimedSeq t a = FingerTree (Timed t a) (Timed t a)

takeAfterElement
    :: (Monoid a, Ord q)
    => (t -> q)
    -> TimedSeq t a
    -> Maybe (Timed t a, TimedSeq t a)
takeAfterElement bucket tseq = case viewl tseq of
    EmptyL -> Nothing
    hd :< _ ->
        let
            (taken, rest) =
                split (\q -> (bucket <$> time q) > (bucket <$> time hd)) tseq
        in
            Just (measure taken, rest)

takeBeforeElement
    :: (Monoid a, Ord q)
    => (t -> q)
    -> TimedSeq t a
    -> Maybe (Timed t a, TimedSeq t a)
takeBeforeElement bucket tseq = case viewr tseq of
    EmptyR -> Nothing
    _ :> hd ->
        let
            (rest, taken) =
                split (\q -> (bucket <$> time q) >= (bucket <$> time hd)) tseq
        in
            Just (measure taken, rest)

takeAfterElements
    :: (Monoid a, Ord q, Ord t)
    => (t -> q)
    -> Maybe Int
    -> TimedSeq t a
    -> (TimedSeq t a, Maybe t)
takeAfterElements _dt (Just 0) tseq =
    ( mempty
    , case viewl tseq of
        EmptyL -> Nothing
        Timed (Last hd) _ :< _ -> hd
    )
takeAfterElements bucket mn tseq = case takeAfterElement bucket tseq of
    Just (v, rest) ->
        first (v <|)
            $ takeAfterElements bucket (subtract 1 <$> mn) rest
    _ -> (mempty, Nothing)

takeBeforeElements
    :: (Monoid a, Ord q, Ord t)
    => (t -> q)
    -> Maybe Int
    -> TimedSeq t a
    -> (TimedSeq t a, Maybe t)
takeBeforeElements _dt (Just 0) tseq =
    ( mempty
    , case viewr tseq of
        EmptyR -> Nothing
        _ :> Timed (Last hd) _ -> hd
    )
takeBeforeElements bucket mn tseq = case takeBeforeElement bucket tseq of
    Just (v, rest) ->
        first (v <|)
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
takeAfter bucket mstart mcount tseq =
    takeAfterElements bucket mcount
        $ dropUntil
            ( \q -> mstart & maybe True (\t -> time q >= Last (Just t))
            )
            tseq

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
takeUpTo bucket mstart mcount tseq =
    takeBeforeElements bucket mcount
        $ takeUntil
            (\q -> mstart & maybe False (\t -> time q > Last (Just t)))
            tseq

-- | Try to extract the first element time from a tseq.
minKey :: Monoid a => TimedSeq t a -> Maybe t
minKey tseq = case viewl tseq of
    Timed (Last (Just t)) _ :< _ -> Just t
    _ -> Nothing

-- | Try to extract the last element time from a tseq.
maxKey :: Monoid a => TimedSeq t a -> Maybe t
maxKey tseq = case viewr tseq of
    _ :> Timed (Last (Just t)) _ -> Just t
    _ -> Nothing

-- | Extract all elements from a tseq that are within the given time interval.
extractInterval
    :: (Monoid a, Ord t) => t -> t -> TimedSeq t a -> Timed t a
extractInterval t0 t1 tseq =
    measure
        $ takeUntil (\q -> time q > Last (Just t1))
        $ dropUntil (\q -> time q >= Last (Just t0)) tseq

-- | Drop all elements from a tseq that are after the given time.
dropAfter :: (Ord t, Monoid a) => t -> TimedSeq t a -> TimedSeq t a
dropAfter t = takeUntil (\q -> time q > Last (Just t))

-- | Drop all elements from a tseq that are before the given time.
dropBefore :: (Ord t, Monoid a) => t -> TimedSeq t a -> TimedSeq t a
dropBefore t = dropUntil (\q -> time q >= Last (Just t))
