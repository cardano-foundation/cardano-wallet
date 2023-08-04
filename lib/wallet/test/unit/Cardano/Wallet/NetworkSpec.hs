{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.NetworkSpec
  ( spec
  )
where

import Cardano.Wallet.Gen
  ( genBlockHeader
  , genChainPoint
  , genSlotNo
  )
import Cardano.Wallet.Network
  ( ChainSyncLog (..)
  , ErrPostTx (..)
  , emptyStats
  , updateStats
  )
import Cardano.Wallet.Primitive.Types
  ( BlockHeader (..)
  , ChainPoint (..)
  )
import Data.List.NonEmpty qualified as NE
import Data.Time.Clock
  ( getCurrentTime
  )
import NoThunks.Class
  ( wNoThunks
  )
import Test.Hspec
  ( Spec
  , describe
  , expectationFailure
  , it
  )
import Test.QuickCheck
  ( Arbitrary (..)
  , getNonEmpty
  , oneof
  , property
  )
import Prelude

spec :: Spec
spec = do
  describe "Pointless tests to cover 'Show' instances for errors" $ do
    testShow $ ErrPostTxValidationError mempty

  describe "updateStats" $ do
    it "results in no unexpected thunks"
      $ property
      $ \(msg :: ChainSyncLog () ChainPoint) -> do
        -- This test is not /fully/ fool-proof. Adding lots of nested types to
        -- LogState and logic in updateStats not covered by the generator
        -- might cause us to miss a thunk.
        --
        -- But it does provide some sanity.
        t <- getCurrentTime
        let
          s0 = emptyStats t
        let
          s = updateStats msg s0
        wNoThunks [] s >>= \case
          Nothing -> return ()
          Just x -> expectationFailure $ show x

instance Arbitrary block => Arbitrary (ChainSyncLog block ChainPoint) where
  arbitrary =
    oneof
      [ MsgChainRollForward <$> genNonEmpty <*> genChainPoint
      , MsgChainRollBackward <$> genChainPoint <*> arbitrary
      , MsgChainTip <$> genChainPoint
      , MsgLocalTip <$> genChainPoint
      ]
    where
      genNonEmpty = (NE.fromList . getNonEmpty) <$> arbitrary

-- Shrinking not that important here

instance Arbitrary BlockHeader where
  arbitrary = genBlockHeader =<< genSlotNo

testShow :: Show a => a -> Spec
testShow a = it (show a) True
