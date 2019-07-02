{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.TransactionSpecShared
    ( estimateMaxNumberOfInputsSpec
    , propMaxNumberOfInputsEstimation
    ) where

import Prelude

import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Small (..)
    , choose
    , counterexample
    , elements
    , property
    , (.&&.)
    )

{-------------------------------------------------------------------------------
                             Max inputs estimation
-------------------------------------------------------------------------------}

estimateMaxNumberOfInputsSpec :: TransactionLayer t -> Spec
estimateMaxNumberOfInputsSpec tl =
    describe "estimateMaxNumberOfInputs" $ do
        it "Property for mainnet addresses"
            (property $ propMaxNumberOfInputsEstimation tl)

propMaxNumberOfInputsEstimation
    :: TransactionLayer t
    -> Quantity "byte" Word16
    -> Quantity "byte" Word16
    -> Property
propMaxNumberOfInputsEstimation tl qa@(Quantity ma) qb@(Quantity mb) =
    counterexample debug
    (isIncreasingFunction .&&. estIsSmallerThanSize)
  where
    isIncreasingFunction = if (ma < mb) then (est qa <= est qb) else (est qa >= est qb)
    estIsSmallerThanSize = (est qa <= ma) .&&. (est qb <= mb)
    est = fromIntegral . estimateMaxNumberOfInputs tl
    debug = unwords
        [ "sizeA = " <> show ma, "sizeB = " <> show mb
        , "estA = " <> show (est qa), "estB = " <> show (est qb)
        ]

instance Arbitrary (Quantity "byte" Word16) where
    shrink (Quantity n) = Quantity <$> shrink n
    arbitrary = do
        a <- choose (0, maxBound)
        Small n <- arbitrary
        Quantity <$> elements [a, n, a - n]
