{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Submissions.PrimitivesSpec
    ( spec
    , genPrimitiveDelta
    , genPrimitiveSubmissionsHistory
    ) where

import Prelude

import Cardano.Wallet.Submissions.Gen
    ( GenSubmissionsHistory (..), genSlot, genTx, prop_submissionHistory )
import Cardano.Wallet.Submissions.Primitives
    ( Primitive (..), applyPrimitive )
import Cardano.Wallet.Submissions.Properties.Primitives
    ( properties )
import Cardano.Wallet.Submissions.Submissions
    ( Submissions )
import System.Random
    ( Random )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary, Gen, Testable (..), frequency )

spec :: Spec
spec = do
    describe "submissions primitive operations" $ do
        it "respects specifications"
            $ property
                $ prop_submissionHistory genPrimitiveSubmissionsHistory

genPrimitiveDelta
    :: (Arbitrary tx, Random slot, Num slot)
    => Submissions slot tx
    -> Gen (Primitive slot tx)
genPrimitiveDelta s =
    frequency
        [ (2 , do
            tx <- genTx 4 1 1 1 s
            expiration <- genSlot 1 1 4 s
            pure $ AddSubmission expiration tx
          )
        , (4, do
            tx <- genTx 1 6 1 1 s
            acceptance <- genSlot 1 1 4 s
            pure $ MoveToLedger acceptance tx
          )
        , (2, do
            newtip <- genSlot 1 3 3 s
            pure $ MoveTip newtip
          )
        , (1, do
            newfinality <- genSlot 1 3 1 s
            pure $ MoveFinality newfinality
          )
        , (1, do
            tx <- genTx 1 2 2 2 s
            pure $ Forget tx
          )
        ]

genPrimitiveSubmissionsHistory :: GenSubmissionsHistory Primitive
genPrimitiveSubmissionsHistory = GenSubmissionsHistory
    { stepProperties = properties
    , genDelta = genPrimitiveDelta
    , applyDelta = applyPrimitive
    }
