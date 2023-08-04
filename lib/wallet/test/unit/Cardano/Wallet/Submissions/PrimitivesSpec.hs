{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Submissions.PrimitivesSpec
  ( spec
  , genPrimitiveDelta
  , genPrimitiveSubmissionsHistory
  )
where

import Cardano.Wallet.Submissions.Gen
  ( GenSubmissionsHistory (..)
  , genSlot
  , genTx
  , prop_submissionHistory
  )
import Cardano.Wallet.Submissions.Primitives
  ( Primitive (..)
  , applyPrimitive
  )
import Cardano.Wallet.Submissions.Properties.Primitives
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
  )
import Prelude

spec :: Spec
spec = do
  describe "submissions primitive operations" $ do
    it "respects specifications"
      $ property
      $ prop_submissionHistory genPrimitiveSubmissionsHistory

genPrimitiveDelta
  :: (Arbitrary tx, Random slot, Num slot, HasTxId tx)
  => Gen meta
  -> Submissions meta slot tx
  -> Gen (Primitive meta slot tx)
genPrimitiveDelta genMeta s =
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
          tx <- genTx 1 6 1 1 s
          acceptance <- genSlot 1 1 4 s
          pure $ MoveToLedger acceptance $ txId tx
      )
    ,
      ( 2
      , do
          newtip <- genSlot 1 3 3 s
          pure $ MoveTip newtip
      )
    ,
      ( 1
      , do
          newfinality <- genSlot 1 3 1 s
          pure $ MoveFinality newfinality
      )
    ,
      ( 1
      , do
          tx <- genTx 1 2 2 2 s
          pure $ Forget $ txId tx
      )
    ]

genPrimitiveSubmissionsHistory :: GenSubmissionsHistory Primitive
genPrimitiveSubmissionsHistory =
  GenSubmissionsHistory
    { stepProperties = properties
    , genDelta = genPrimitiveDelta (pure ())
    , applyDelta = applyPrimitive
    }
