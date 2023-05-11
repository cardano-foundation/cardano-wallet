{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2023 IOHK
-- License: Apache-2.0
module Test.Store
    ( genUpdates
    , prop_StoreUpdates
    , GenDelta
    , Updates (..)
    , unitTestStore
    , applyS
    , checkLaw
    , reset
    , context
    , observe
    , ignore
    ) where

import Prelude

import Control.Exception.Safe
    ( impureThrow )
import Control.Monad
    ( forM_ )
import Control.Monad.RWS
    ( RWST, evalRWST, lift )
import Control.Monad.RWS.Class
    ( MonadReader (ask)
    , MonadState (get, put)
    , MonadWriter (listen, tell)
    , censor
    )
import Data.Delta
    ( Delta (..) )
import Data.Either
import Data.Store
    ( Store (loadS, updateS, writeS) )
import Fmt
    ( Buildable, listF, pretty )
import Test.QuickCheck
    ( Blind (Blind), Gen, Property, conjoin, counterexample, sized, (===) )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monitor, pick )

-- | Given a value, generate a random delta starting from this value.
type GenDelta da = Base da -> Gen da

-- The update that is applied *last* appears in the list *first*.
newtype Updates da = Updates [(Base da, da)]

instance Show da => Show (Updates da) where
    show (Updates xs) = show . map snd $ xs

-- | Randomly generate a sequence of updates
genUpdates :: Delta da => Gen (Base da) -> GenDelta da -> Gen (Updates da)
genUpdates gen0 more = sized $ \n -> go n [] =<< gen0
  where
    go 0 das _ = pure $ Updates das
    go n das a0 = do
        da <- more a0
        let a1 = apply da a0
        go (n - 1) ((a1, da) : das) a1

-- | Test whether 'updateS' and 'loadS' behave as expected.
--
-- TODO: Shrinking of the update sequence.
prop_StoreUpdates
    :: (Monad m, Delta da, Eq (Base da), Buildable da, Show (Base da))
    => (forall b. m b -> PropertyM IO b)
    -- ^ Function to embed the monad in 'IO'
    -> Store m qa da
    -- ^ Store that is to be tested.
    -> Gen (Base da)
    -- ^ Generator for the initial value.
    -> GenDelta da
    -- ^ Generator for deltas.
    -> PropertyM IO ()
prop_StoreUpdates toPropertyM store gen0 more = do
    -- randomly generate a sequence of updates
    Blind a0 <- pick $ Blind <$> gen0
    Blind (Updates adas) <- pick $ Blind <$> genUpdates (pure a0) more
    let as = map fst adas ++ [a0]
        das = map snd adas

    monitor
        $ counterexample
        $ "\nUpdates applied:\n" <> pretty (listF das)

    -- apply those updates
    ea <- toPropertyM $ do
        writeS store a0
        -- first update is applied last!
        let updates = reverse $ zip das (drop 1 as)
        forM_ updates $ \(da, a) -> updateS store (Just a) da
        loadS store

    -- check whether the last value is correct
    case ea of
        Left err -> impureThrow err
        Right a -> do
            monitor $ counterexample $ "\nExpected:\n" <> show (head as)
            monitor $ counterexample $ "\nGot:\n" <> show a
            assert $ a == head as

-- | A DSL to unit test a 'Store'.
type StoreUnitTest m qa da =
    RWST
        (Store m qa da)
        [Property]
        (Base da, Base da, [da])
        m

-- | Apply a delta to the current value.
applyS :: (Monad m, Delta da) => da -> StoreUnitTest m qa da ()
applyS r = do
    s <- ask
    (q, x, ds) <- get
    put (q, apply r x, r : ds)
    lift $ updateS s (Just x) r

-- | Check the store laws.
checkLaw
    :: (Monad m, Eq (Base da), Show (Base da), Show da)
    => StoreUnitTest m qa da ()
checkLaw = do
    (_, x, reverse -> ds) <- get
    x' <- ask >>= lift . loadS
    tell
        [ counterexample (show (ds, leftOf x')) (isRight x')
        , counterexample (show ds) $ rightOf x' === x
        ]
  where
    leftOf (Left x) = x
    leftOf _ = undefined
    rightOf (Right x) = x
    rightOf _ = undefined

-- | Reset the store state to the initial value.
reset :: Monad m => StoreUnitTest m qa da ()
reset = do
    s <- ask
    (q, _, _) <- get
    lift $ writeS s q
    put (q, q, [])

-- | Run a unit test for a 'Store'.
unitTestStore
    :: (Monad m, Eq (Base da), Show (Base da), Show da)
    => Store m qa da
    -> StoreUnitTest m qa da a
    -> m Property
unitTestStore s f = do
    mx <- loadS s
    w <- case mx of
        Right x -> snd <$> evalRWST (f >> checkLaw) s (x, x, [])
        Left e -> pure [counterexample (show e) (isRight mx)]
    pure $ conjoin w

-- | Add a context to test.
context
    :: Monad m
    => (Property -> Property)
    -> StoreUnitTest m qa da x
    -> StoreUnitTest m qa da x
context d f = do
    (x, w) <- listen f
    tell $ fmap d w
    pure x

-- | Observe a property on the current value of the store.
observe :: Monad m => (Base da -> Property) -> StoreUnitTest m qa da ()
observe f = do
    (_, s, _) <- get
    tell [f s]

-- | Ignore the properties of a sub-test.
ignore :: Monad m => StoreUnitTest m qa da x -> StoreUnitTest m qa da x
ignore = censor (const [])
