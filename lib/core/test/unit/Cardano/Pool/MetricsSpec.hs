{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Pool.MetricsSpec (spec) where

import Prelude

import Cardano.Pool.Metrics
    ( Block (..), State (..), applyBlock, combineMetrics, withinSameTip )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochLength (..)
    , Hash (..)
    , PoolId (..)
    , SlotId (..)
    , flatSlot
    , fromFlatSlot
    )
import Control.Concurrent.MVar
    ( MVar, modifyMVar, newMVar, readMVar )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Trans.Except
    ( runExceptT, throwE )
import Control.Retry
    ( limitRetries )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32, Word64 )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , NonNegative (..)
    , Property
    , Small (..)
    , checkCoverage
    , cover
    , expectFailure
    , frequency
    , label
    , property
    , vectorOf
    , withMaxSuccess
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "withinSameTip" $ do
        it "Retry the action and eventually get a result"
            (property prop_withinSameTipEventually)

        it "Does not retry more than necessary"
            (property prop_withinSameTipMaxRetries)

    describe "Counting how many blocks each pool produced" $ do
        describe "State" $ do
            it "stores the last applied blockHeader"
                $ property $ \s b@(Block h _) -> do
                tip (applyBlock b s) `shouldBe` h

            it "counts every block it applies (total activity increases by 1\
               \when a block is applied"
                $ property $ \s block -> do
                let s' = applyBlock block s
                let count = Map.foldl (\r l -> r + (length l)) 0 . activity
                count s' === (count s + 1)

        describe "combineMetrics" $ do
            it "pools with no entry for productions are included" $
                property $ \stakeDistr -> do
                    combineMetrics stakeDistr Map.empty
                    `shouldBe`
                    (Right $ Map.map (, Quantity 0) stakeDistr)

            it "it fails if a block-producer is not in the stake distr" $ do
                checkCoverage
                . property
                . withMaxSuccess 1000
                $ \stakeDistr poolProd ->
                    let
                        aPoolWithoutStakeProduced =
                            not . Map.null $ Map.difference poolProd stakeDistr
                    in cover 20 aPoolWithoutStakeProduced
                        "A pool without stake produced" $
                       cover 1 (not aPoolWithoutStakeProduced)
                        "Successfully combined the maps" $
                        case combineMetrics stakeDistr poolProd of
                            Left _ ->
                                aPoolWithoutStakeProduced === True
                            Right x ->
                                Map.map fst x === stakeDistr

{-------------------------------------------------------------------------------
                                withinSameTip
-------------------------------------------------------------------------------}

data WithinSameTip header =
    WithinSameTip (NonEmpty header) Int
    deriving Show

prop_withinSameTipEventually
    :: WithinSameTip Char
    -> Property
prop_withinSameTipEventually (WithinSameTip source maxRetry) = monadicIO $ do
    retries <- run $ newMVar (0 :: Int)
    getNetworkTip <- run (mkNetworkTipGetter <$> newMVar source)
    let action _ = liftIO $ modifyMVar retries $ \n -> pure (n+1, ())
    let policy = limitRetries maxRetry
    result <- run $ runExceptT $ withinSameTip policy throwE getNetworkTip action
    run (readMVar retries) >>= monitor . label . ("retry="<>) . show
    assert (result == Right ())

prop_withinSameTipMaxRetries
    :: WithinSameTip Char
    -> Property
prop_withinSameTipMaxRetries (WithinSameTip source maxRetry) =
    maxRetry > 0 ==> expectFailure $ monadicIO $ do
        getNetworkTip <- run (mkNetworkTipGetter <$> newMVar source)
        let action _ = pure ()
        let policy = limitRetries (maxRetry - 1)
        result <- run $ runExceptT $ withinSameTip policy throwE getNetworkTip action
        assert (result == Right ())

mkNetworkTipGetter
    :: MonadIO m => MVar (NonEmpty header) -> m header
mkNetworkTipGetter = liftIO . flip modifyMVar (\headers -> case headers of
    (h :| []) -> pure (headers, h)
    (h :| q)  -> pure (NE.fromList q, h))

instance Arbitrary (WithinSameTip Char) where
    arbitrary = do
        NonNegative (Small maxRetry) <- arbitrary
        headers <- vectorOf (2*maxRetry) genTip
        h0 <- genTip
        pure $ WithinSameTip (h0 :| headers) maxRetry
      where
        genTip = frequency [(5, pure 'a'), (5, pure 'b')]

instance Arbitrary BlockHeader where
    arbitrary = BlockHeader
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink = genericShrink

instance Arbitrary State where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary SlotId where
    arbitrary = fromFlatSlot epochLength <$> arbitrary
    shrink sl = fromFlatSlot epochLength <$> shrink (flatSlot epochLength sl)

-- | Epoch length used to generate arbitrary @SlotId@
epochLength :: EpochLength
epochLength = EpochLength 50

instance Arbitrary (Hash tag) where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ Hash $ BS.pack $ take 32 bytes
    shrink x = [zeros | x /= zeros]
      where
        zeros = Hash $ BS.pack $ replicate 32 0

instance Arbitrary Block where
     arbitrary = genericArbitrary
     shrink = genericShrink

instance Arbitrary (Quantity "block" Word32) where
     arbitrary = Quantity . fromIntegral <$> (arbitrary @Word)

instance Arbitrary (Quantity "lovelace" Word64) where
     arbitrary = Quantity . fromIntegral <$> (arbitrary @Word64)

instance Arbitrary (Quantity "block" Natural) where
     arbitrary = Quantity . fromIntegral <$> (arbitrary @Word64)

instance Arbitrary PoolId where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ PoolId $ BS.pack $ take 32 bytes
    shrink x = [zeros | x /= zeros]
      where
        zeros = PoolId $ BS.pack $ replicate 32 0
