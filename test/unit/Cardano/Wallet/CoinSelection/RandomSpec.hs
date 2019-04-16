{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), CoinSelectionError (..), CoinSelectionOptions (..) )
import Cardano.Wallet.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.CoinSelection.Random
    ( random )
import Cardano.Wallet.CoinSelectionSpec
    ( CoinSelProp (..), CoinSelectionFixture (..), coinSelectionUnitTest )
import Cardano.Wallet.Primitive.Types
    ( balance, excluding )
import Control.Monad
    ( replicateM, when )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Random
    ( SystemDRG, getSystemDRG )
import Crypto.Random.Types
    ( withDRG )
import Data.Either
    ( isLeft, isRight, rights )
import Data.Functor.Identity
    ( Identity (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Statistics.Sample
    ( mean, variance )
import Test.Hspec
    ( Spec, before, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( Property, property, (===), (==>) )


import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.Vector as V


spec :: Spec
spec = do
    describe "Unit tests" $ do
        coinSelectionUnitTest random "" (Right [1,1,1,1]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [1,1,1,1,1,1]
            , txOutputs = 2 :| []
            }

        coinSelectionUnitTest random "" (Right [1,1,1,1,1,1]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [1,1,1,1,1,1]
            , txOutputs = 2 :| [1]
            }

        coinSelectionUnitTest random "" (Right [1,1,1,1,1]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [1,1,1,1,1]
            , txOutputs = 2 :| [1]
            }

        coinSelectionUnitTest random "with fallback" (Right [1,1,1]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [1,1,1,1]
            , txOutputs = 2 :| [1]
            }

        coinSelectionUnitTest random "" (Right [5]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [5,5,5]
            , txOutputs = 2 :| []
            }

        coinSelectionUnitTest random "" (Right [10,10]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [10,10,10]
            , txOutputs = 2 :| [2]
            }

        coinSelectionUnitTest random "cannot cover aim, but only min"
            (Right [1,1,1,1]) $ CoinSelectionFixture
            { maxNumOfInputs = 4
            , utxoInputs = [1,1,1,1,1,1]
            , txOutputs = 3 :| []
            }

        coinSelectionUnitTest random "" (Left $ MaximumInputsReached 2) $ CoinSelectionFixture
            { maxNumOfInputs = 2
            , utxoInputs = [1,1,1,1,1,1]
            , txOutputs = 3 :| []
            }

        coinSelectionUnitTest random "" (Left $ NotEnoughMoney 39 40) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [12,10,17]
            , txOutputs = 40 :| []
            }

        coinSelectionUnitTest random "" (Left $ NotEnoughMoney 39 43) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [12,10,17]
            , txOutputs = 40 :| [1,1,1]
            }

        coinSelectionUnitTest random "" (Left $ UtxoNotEnoughFragmented 3 4) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [12,20,17]
            , txOutputs = 40 :| [1,1,1]
            }

    before getSystemDRG $ describe "Property Tests" $ do
        it "forall (UTxO, NonEmpty TxOut), \
           \ running algorithm gives not less UTxO fragmentation than LargestFirst algorithm"
            (property . propFragmentation)
        it "forall (UTxO, NonEmpty TxOut), \
           \ running algorithm gives the same errors as LargestFirst algorithm"
            (property . propErrors)
        it "forall (UTxO, NonEmpty TxOut), \
           \ running algorithm multiple times gives roughly the same end UTxO balances"
            (property . propAverageUTxO)

{-------------------------------------------------------------------------------
                              Properties
-------------------------------------------------------------------------------}

propFragmentation
    :: SystemDRG
    -> CoinSelProp
    -> Property
propFragmentation drg (CoinSelProp utxo txOuts) = do
    isRight selection1 && isRight selection2 ==>
        let (Right s1, Right s2) =
                (selection1, selection2)
        in prop (s1, s2)
  where
    prop (CoinSelection inps1 _ _, CoinSelection inps2 _ _) =
        L.length inps1 `shouldSatisfy` (>= L.length inps2)
    (selection1,_) = withDRG drg
        (runExceptT $ random (CoinSelectionOptions 100) utxo txOuts)
    selection2 = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts

propErrors
    :: SystemDRG
    -> CoinSelProp
    -> Property
propErrors drg (CoinSelProp utxo txOuts) = do
    isLeft selection1 && isLeft selection2 ==>
        let (Left s1, Left s2) = (selection1, selection2)
        in prop (s1, s2)
  where
    prop (err1, err2) =
        err1 === err2
    (selection1,_) = withDRG drg
        (runExceptT $ random (CoinSelectionOptions 1) utxo txOuts)
    selection2 = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 1) utxo txOuts

-- We are picking (utxo, txOuts) that gives rise to successful random coin selection.
-- Then we are repeating the random selection 2x99 times in two batches. For each batch
-- we are calculating balance of UTxO after each coin selection. Then, we compare those two batches
-- and do expect them to have statistically the same mean post-coin selection UTxO balance.
--
-- The two-sample t-test (Snedecor and Cochran, 1989) is used to determine if two population means are equal.
-- The test is defined as:
-- H0 : μ1 = μ2
-- H1 : μ1 /= μ2
-- We calculate test statistics T (see implementation below) and need to choose significance, α.
-- We reject the null hypothesis that the two means are equal if
--- |T| > (t1-α/2,ν)
-- where (t1-α/2,ν) is critical value of t-distribution
-- for a given significance α and ν which specifies the degrees of freedom
-- (we use 100 sample batches so it is about 100)
-- Let's say the significance level is α = 0.002. For a two-sided test, we compute 1 - α/2,
-- or 1 - 0.002/2 = 0.999. If the absolute value of the test statistic is greater than
-- the critical value (0.999), then we reject the null hypothesis.
--
-- Critical values of Student's t distribution with ν degrees of freedom
--     Probability less than the critical value (t1-α,ν)
--
--  ν         0.90    0.95   0.975    0.99   0.995   0.999
--
--  1.        3.078   6.314  12.706  31.821  63.657 318.313
--  2.        1.886   2.920   4.303   6.965   9.925  22.327
--  3.        1.638   2.353   3.182   4.541   5.841  10.215
--
--  97.       1.290   1.661   1.985   2.365   2.627   3.176
--  98.       1.290   1.661   1.984   2.365   2.627   3.175
--  99.       1.290   1.660   1.984   2.365   2.626   3.175
-- 100.       1.290   1.660   1.984   2.364   2.626   3.174
-- infinity   1.282   1.645   1.960   2.326   2.576   3.090
--
-- So, for 3-sigma we expect the calculated values to be less than 3.090.
-- In software engineering we aim for at least 6-sigma,
-- which means α < 0.0001. For,ν=100, t-value (two-sided) is 4.053
-- (see, https://goodcalculators.com/student-t-value-calculator/
-- or any good statistics book) and use this conservative value in the test
propAverageUTxO
    :: SystemDRG
    -> CoinSelProp
    -> Property
propAverageUTxO drg (CoinSelProp utxo txOuts) = do
    isRight selection ==> let Right s = selection in prop s
  where
    prop seed = do
        resultsFromSampleOne <-
            replicateM 99 (runExceptT $ random (CoinSelectionOptions 100) utxo txOuts)
        resultsFromSampleTwo <-
            replicateM 99 (runExceptT $ random (CoinSelectionOptions 100) utxo txOuts)
        let balancesAfter res =
                V.fromList
                $ map
                ((toDouble . balance . (excluding utxo)) .
                 Set.fromList . map fst . inputs)
                (seed : rights res)
        let mean1 = mean $ balancesAfter resultsFromSampleOne
        let var1 = variance $ balancesAfter resultsFromSampleOne
        let mean2 = mean $ balancesAfter resultsFromSampleTwo
        let var2 = variance $ balancesAfter resultsFromSampleTwo
        when (var1 /=0 && var2 /= 0) $ do
            let testStatistics = abs $ (mean1 - mean2) / (sqrt ((var1+var2)/100))
            testStatistics `shouldSatisfy` (< 4.053)
    (selection,_) = withDRG drg
        (runExceptT $ random (CoinSelectionOptions 100) utxo txOuts)
    toDouble :: (Integral a) =>  a -> Double
    toDouble = fromIntegral
