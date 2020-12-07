{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.TransactionSpecShared
    ( propMaxNumberOfInputsEstimation
    ) where

import Prelude

import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word8 )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Small (..)
    , choose
    , counterexample
    , elements
    , (.&&.)
    )

{-------------------------------------------------------------------------------
                             Max inputs estimation
-------------------------------------------------------------------------------}

propMaxNumberOfInputsEstimation
    :: TransactionLayer key
    -> Quantity "byte" Word16
    -> Quantity "byte" Word16
    -> Word8
    -> Word8
    -> Property
propMaxNumberOfInputsEstimation tl qa@(Quantity ma) qb@(Quantity mb) oa ob =
    counterexample debug
    (isIncreasingFunction .&&. moreOutputsLessInputs .&&. estIsSmallerThanSize)
  where
    estAA = est qa oa
    estBA = est qb oa
    estAB = est qa ob
    isIncreasingFunction = if ma < mb then estAA <= estBA else estAA >= estBA
    moreOutputsLessInputs = if oa < ob then estAA >= estAB else estAA <= estAB
    estIsSmallerThanSize = (estAA < ma || ma == 0) .&&. (estBA < mb || mb == 0)
    est no = fromIntegral . estimateMaxNumberOfInputs tl no Nothing
    debug = unlines
        [ "sizeA = " <> show ma, "sizeB = " <> show mb
        , "numOutputsA = " <> show oa, "numOutputsB = " <> show ob
        , "estAA = " <> show estAA
        , "estBA = " <> show estBA
        , "estAB = " <> show estAB
        ]

instance Arbitrary (Quantity "byte" Word16) where
    shrink (Quantity n) = Quantity <$> shrink n
    arbitrary = do
        a <- choose (0, maxBound)
        Small n <- arbitrary
        Quantity <$> elements [a, n, a - n]
