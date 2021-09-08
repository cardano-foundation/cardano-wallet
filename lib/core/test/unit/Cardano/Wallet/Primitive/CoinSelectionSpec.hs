{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2021 IOHK
-- License: Apache-2.0
--
module Cardano.Wallet.Primitive.CoinSelectionSpec (spec) where

import Prelude

import Algebra.PartialOrd
    ( PartialOrd (..) )
import Cardano.Wallet.Primitive.CoinSelection
    ( ErrPrepareOutputs (..)
    , ErrWalletSelection (..)
    , SelectionConstraints (..)
    , SelectionParams (..)
    , makeWrapper
    , runWalletCoinSelection
    )
import Cardano.Wallet.Primitive.CoinSelection.Balance
    ( PerformSelection (..)
    , SelectionError (..)
    , SelectionLimitOf (..)
    , SelectionResult (..)
    , fullBalance
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin, shrinkCoin )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.Tx
    ( TokenBundleSizeAssessment (..)
    , TokenBundleSizeAssessor (..)
    , TxOut (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxOut, shrinkTxOut )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Gen
    ( genUTxOIndex, shrinkUTxOIndex )
import Cardano.Wallet.Primitive.Types.UTxOIndex.Internal
    ( UTxOIndex )
import Control.Monad.Random.Lazy
    ( Rand, StdGen, evalRand, mkStdGen )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Data.Generics.Internal.VL
    ( view )
import Fmt
    ( (+||), (||+) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , Testable
    , conjoin
    , counterexample
    , elements
    , frequency
    , label
    , property
    , suchThat
    , (===)
    )
import Test.Utils.Pretty
    ( Pretty (..) )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
import qualified Data.Foldable as F

spec :: Spec
spec = do
    prop "makeWrapper" prop_makeWrapper
    prop "runWalletCoinSelection" prop_runWalletCoinSelection

prop_makeWrapper :: Pretty SelectionParams -> Property
prop_makeWrapper (Pretty sp) = case makeWrapper testSelectionConstraints sp of
    Right (_transform, PerformSelection{..}) -> property True
    Left e -> constructorLabel e $ case e of
        ErrPrepareOutputsTokenBundleSizeExceedsLimit _ -> property False
        ErrPrepareOutputsTokenQuantityExceedsLimit _ -> property False
        ErrPrepareOutputsTxOutMissing -> view #outputsToCover sp === mempty

prop_runWalletCoinSelection :: StdGen -> Pretty SelectionParams -> Property
prop_runWalletCoinSelection g (Pretty sp) = counterexample ce $ case res of
    Right sel -> label "success" $ conjoin
        [ property (balanceSufficient sp)
        , outputsCovered sel === outputsToCover sp
        ]
    Left (ErrWalletSelectionBalance e) -> constructorLabel e $ case e of
        BalanceInsufficient _ -> balanceSufficient sp === False
        OutputsInsufficient _ -> todo
        SelectionInsufficient _ -> todo
        InsufficientMinCoinValues _ -> property True
        UnableToConstructChange _ -> todo
    Left (ErrWalletSelectionOutputs e) -> constructorLabel e todo
  where
    res = eval g $ runWalletCoinSelection testSelectionConstraints sp
    ce = "res = "+||Pretty res||+""
    todo = label "incomplete" True

constructorLabel :: (Show a, Testable prop) => a -> prop -> Property
constructorLabel = label . head . words . show

eval :: StdGen -> ExceptT e (Rand StdGen) a -> Either e a
eval s a = evalRand (runExceptT a) s

balanceSufficient :: SelectionParams -> Bool
balanceSufficient SelectionParams{..} =
    balanceRequired `leq` balanceAvailable
  where
    balanceRequired =
        F.foldMap (view #tokens) outputsToCover
            <> TB.fromTokenMap assetsToBurn
    balanceAvailable =
        fullBalance utxoAvailable (Just extraCoinSource)
            <> TB.fromTokenMap assetsToMint
    extraCoinSource = rewardWithdrawals

testSelectionConstraints :: SelectionConstraints
testSelectionConstraints = SelectionConstraints
    { bundleSizeAssessor = TokenBundleSizeAssessor $ const TokenBundleSizeWithinLimit
    , computeMinimumAdaQuantity = const (Coin 1_000_000)
    , computeMinimumCost = const (Coin 500_000)
    , computeSelectionLimit = const NoLimit
    , maximumCollateralInputCount = 5
    , depositAmount = Coin 10_000_000
    }

{-------------------------------------------------------------------------------
                              Arbitrary test data
-------------------------------------------------------------------------------}

instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

instance Arbitrary UTxOIndex where
    arbitrary = genUTxOIndex
    shrink = shrinkUTxOIndex

instance Arbitrary TxOut where
    arbitrary = genTxOut
    shrink = shrinkTxOut

instance Arbitrary TokenMap where
    arbitrary = genTokenMap
    shrink = shrinkTokenMap

instance Arbitrary Coin where
    arbitrary = genCoin
    shrink = shrinkCoin

instance Arbitrary SelectionParams where
    arbitrary = (arbitrary >>= genFromUTxO) `suchThat` hasOutput
      where
        genFromUTxO utxo = SelectionParams mempty mempty
            <$> frequency [(7, pure (Coin 0)), (3, arbitrary)]
            <*> elements [0, 0, 0, 1, 2]
            <*> elements [0, 0, 0, 1, 2]
            <*> arbitrary
            <*> pure utxo
    shrink (SelectionParams b m w dt dr o u) =
        [ sp'
        | (b', m', w', dt', dr', o', u') <- shrink (b, m, w, i dt, i dr, o, u)
        , let sp' = SelectionParams b' m' w' (nat dt') (nat dr') o' u'
        , hasOutput sp'
        ]
      where
        i = fromIntegral :: Natural -> Int
        nat = fromIntegral :: Int -> Natural

hasOutput :: SelectionParams -> Bool
hasOutput SelectionParams{..} =
    not (null outputsToCover) ||
    certificateDepositsTaken > 0 ||
    certificateDepositsReturned > 0
