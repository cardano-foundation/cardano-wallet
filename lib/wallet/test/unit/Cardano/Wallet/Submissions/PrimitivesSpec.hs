{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Submissions.PrimitivesSpec
    ( spec
    )
where

import Prelude

import Cardano.Wallet.Submissions.Generation
    ( SubmissionsHG (..), prop_submission_history, slotG, txG )
import Cardano.Wallet.Submissions.Primitives
    ( Primitive (..), applyPrimitive )
import Cardano.Wallet.Submissions.Properties.Primitives
    ( properties )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Testable (..), frequency )

spec :: Spec
spec = do
    describe "submissions primitive operations" $ do
        it "respects specifications"
            $ property
                $ prop_submission_history primitiveSubmissionsHG

primitiveSubmissionsHG :: SubmissionsHG Primitive
primitiveSubmissionsHG = SubmissionsHG
    { stepProperties = properties
    , deltaG = \s -> do
        n :: Int <- frequency
                [(2,pure 1), (4, pure 2), (2, pure 3), (1, pure 4), (1, pure 5)]
        case n of
            1 -> do
                tx <- txG 4 1 1 1 s
                expiration <- slotG 1 1 4 s
                pure $ AddSubmission expiration tx
            2 -> do
                tx <- txG 1 6 1 1 s
                acceptance <- slotG 1 1 4 s
                pure $ MoveToLedger acceptance tx
            3 -> do
                newtip <- slotG 1 3 3 s
                pure $ MoveTip newtip
            4 -> do
                newfinality <- slotG 1 3 1 s
                pure $ MoveFinality newfinality
            5 -> do
                tx <- txG 1 2 2 2 s
                pure $ Forget tx
            _ -> error "not a change"
    , applyDelta = applyPrimitive
    }
