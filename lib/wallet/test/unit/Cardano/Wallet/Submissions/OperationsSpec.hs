{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Submissions.OperationsSpec
  ( spec
  , genOperationsDelta
  )
where

import Cardano.Wallet.Submissions.Gen
  ( GenSubmissionsHistory (..)
  , genSlot
  , genTx
  , prop_submissionHistory
  )
import Cardano.Wallet.Submissions.Operations
  ( Operation (..)
  , applyOperations
  )
import Cardano.Wallet.Submissions.Properties.Operations
  ( properties
  )
import Cardano.Wallet.Submissions.Submissions
  ( Submissions
  )
import Cardano.Wallet.Submissions.TxStatus
  ( HasTxId (..)
  )
import System.Random
  ( Random
  )
import Test.Hspec
  ( Spec
  , describe
  , it
  )
import Test.QuickCheck
  ( Arbitrary
  , Gen
  , Testable (..)
  , frequency
  , listOf
  , scale
  )
import Prelude

spec :: Spec
spec = do
  describe "submissions API operations" $ do
    it "respects specifications"
      $ property
      $ prop_submissionHistory
        genOperationsSubmissionsHistory

genOperationsDelta
  :: (Arbitrary tx, Random slot, Num slot, HasTxId tx)
  => Gen meta
  -> Submissions meta slot tx
  -> Gen (Operation meta slot tx)
genOperationsDelta genMeta s =
  frequency
    [
      ( 2
      , do
          tx <- genTx 4 1 1 1 s
          expiration <- genSlot 1 1 4 s
          AddSubmission expiration tx <$> genMeta
      )
    ,
      ( 4
      , do
          txs <- scale (`div` 4) $ listOf $ genTx 1 6 1 1 s
          slots <- scale (`div` 4) $ listOf $ genSlot 1 1 4 s
          acceptance <- genSlot 1 1 4 s
          pure $ RollForward acceptance $ zip slots $ txId <$> txs
      )
    ,
      ( 2
      , do
          newtip <- genSlot 1 3 3 s
          pure $ RollBack newtip
      )
    ,
      ( 1
      , do
          newfinality <- genSlot 1 3 1 s
          pure $ Prune newfinality
      )
    ,
      ( 1
      , do
          tx <- genTx 1 2 2 2 s
          pure $ Forget $ txId tx
      )
    ]

genOperationsSubmissionsHistory :: GenSubmissionsHistory Operation
genOperationsSubmissionsHistory =
  GenSubmissionsHistory
    { stepProperties = properties
    , genDelta = genOperationsDelta (pure ())
    , applyDelta = applyOperations
    }
