{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Submissions.OperationsSpec
    ( spec
    )
where

import Prelude

import Cardano.Wallet.Submissions.Generation
    ( SubmissionsHG (..), prop_submission_history, slotG, txG )
import Cardano.Wallet.Submissions.Operations
    ( Operation (..), applyOperations )
import Cardano.Wallet.Submissions.Properties.Operations
    ( properties )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Testable (..), frequency, listOf, scale )

spec :: Spec
spec = do
    describe "submissions API operations" $ do
        it "respects specifications"
            $ property
                $ prop_submission_history
                operationsSubmissionsHG

operationsSubmissionsHG :: SubmissionsHG Operation
operationsSubmissionsHG = SubmissionsHG
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
                txs <- scale (`div` 4) $ listOf $ txG 1 6 1 1 s
                slots <- scale (`div` 4) $ listOf $ slotG 1 1 4 s
                acceptance <- slotG 1 1 4 s
                pure $ RollForward acceptance $ zip slots txs
            3 -> do
                newtip <- slotG 1 3 3 s
                pure $ RollBack newtip
            4 -> do
                newfinality <- slotG 1 3 1 s
                pure $ Prune newfinality
            5 -> do
                tx <- txG 1 2 2 2 s
                pure $ Forget tx
            _ -> error "not a change"
    , applyDelta = applyOperations
    }
