{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.LargestFirstSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), CoinSelectionOptions (..), ErrCoinSelection (..) )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.CoinSelectionSpec
    ( CoinSelProp (..)
    , CoinSelectionFixture (..)
    , CoinSelectionResult (..)
    , coinSelectionUnitTest
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..), excluding )
import Control.Monad
    ( unless )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isRight )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Test.Hspec
    ( Spec, describe, it, parallel, shouldSatisfy )
import Test.QuickCheck
    ( Property, expectFailure, property, (===), (==>) )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    parallel $ describe "Coin selection : LargestFirst algorithm unit tests" $ do
        coinSelectionUnitTest largestFirst ""
            (Right $ CoinSelectionResult
                { rsInputs = [17]
                , rsChange = []
                , rsOutputs = [17]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [10,10,17]
                , txOutputs = 17 :| []
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst ""
            (Right $ CoinSelectionResult
                { rsInputs = [17]
                , rsChange = [16]
                , rsOutputs = [1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 1 :| []
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst ""
            (Right $ CoinSelectionResult
                { rsInputs = [12, 17]
                , rsChange = [11]
                , rsOutputs = [18]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 18 :| []
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst ""
            (Right $ CoinSelectionResult
                { rsInputs = [10, 12, 17]
                , rsChange = [9]
                , rsOutputs = [30]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 30 :| []
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst ""
            (Right $ CoinSelectionResult
                { rsInputs = [6,10]
                , rsChange = [4]
                , rsOutputs = [11,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 3
                , utxoInputs = [1,2,10,6,5]
                , txOutputs = 11 :| [1]
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst "with withdrawal"
            (Right $ CoinSelectionResult
                { rsInputs = [1]
                , rsChange = []
                , rsOutputs = [100]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [1]
                , txOutputs = 100 :| []
                , totalWithdrawal = 99
                })

        coinSelectionUnitTest largestFirst "with withdrawal & change"
            (Right $ CoinSelectionResult
                { rsInputs = [30]
                , rsChange = [40]
                , rsOutputs = [40]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [10,30]
                , txOutputs = 40 :| []
                , totalWithdrawal = 50
                })

        coinSelectionUnitTest largestFirst "withdrawal requires at least one input"
            (Left ErrInputsDepleted)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = []
                , txOutputs = 1 :| []
                , totalWithdrawal = 10
                })

        coinSelectionUnitTest largestFirst "not enough coins"
            (Left $ ErrNotEnoughMoney 39 40)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| []
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst "not enough coin & fragmentation doesn't matter"
            (Left $ ErrNotEnoughMoney 39 43)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| [1,1,1]
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst "enough coins, fragmentation doesn't matter"
            (Right $ CoinSelectionResult
                { rsInputs = [12,17,20]
                , rsChange = [6]
                , rsOutputs = [40,1,1,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,20,17]
                , txOutputs = 40 :| [1,1,1]
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst
            "enough coins, one output does not deplete all inputs"
            (Right $ CoinSelectionResult
                { rsInputs = [12,17,20]
                , rsChange = [8]
                , rsOutputs = [40,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,20,17]
                , txOutputs = 40 :| [1]
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst "each output needs <maxNumOfInputs"
            (Left $ ErrMaximumInputsReached 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , utxoInputs = replicate 100 1
                , txOutputs = NE.fromList (replicate 100 1)
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest largestFirst "each output needs >maxNumInputs"
            (Left $ ErrMaximumInputsReached 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , utxoInputs = replicate 100 1
                , txOutputs = NE.fromList (replicate 10 10)
                , totalWithdrawal = 0
                })

    parallel $ describe "Coin selection properties : LargestFirst algorithm" $ do
        it "forall (UTxO, NonEmpty TxOut), running algorithm twice yields \
            \exactly the same result"
            (property propDeterministic)
        it "There exists (UTxO, NonEmpty TxOut) for which at there are less \
            \inputs selected than there are requested outputs"
            (expectFailure $ property propAtLeast)
        it "forall (UTxO, NonEmpty TxOut), for all selected input, there's no \
            \bigger input in the UTxO that is not already in the selected inputs"
            (property propInputDecreasingOrder)

{-------------------------------------------------------------------------------
                                  Properties
-------------------------------------------------------------------------------}

propDeterministic
    :: CoinSelProp
    -> Property
propDeterministic (CoinSelProp utxo wdrl txOuts) = do
    let opts = CoinSelectionOptions (const 100)
    let resultOne = runIdentity $ runExceptT $ largestFirst opts txOuts wdrl utxo
    let resultTwo = runIdentity $ runExceptT $ largestFirst opts txOuts wdrl utxo
    resultOne === resultTwo

propAtLeast
    :: CoinSelProp
    -> Property
propAtLeast (CoinSelProp utxo wdrl txOuts) =
    isRight selection ==> let Right (s,_) = selection in prop s
  where
    prop cs =
        L.length (inputs cs) `shouldSatisfy` (>= NE.length txOuts)
    selection = runIdentity $ runExceptT $ do
        let opts = CoinSelectionOptions (const 100)
        largestFirst opts txOuts wdrl utxo

propInputDecreasingOrder
    :: CoinSelProp
    -> Property
propInputDecreasingOrder (CoinSelProp utxo wdrl txOuts) =
    isRight selection ==> let Right (s,_) = selection in prop s
  where
    prop cs =
        let
            utxo' = (Map.toList . getUTxO) $
                utxo `excluding` (Set.fromList . map fst $ inputs cs)
        in unless (L.null utxo') $
            getExtremumValue L.minimum (inputs cs)
            `shouldSatisfy`
            (>= (getExtremumValue L.maximum utxo'))
    getExtremumValue f = f . map (unCoin . coin . snd)
    selection = runIdentity $ runExceptT $ do
        let opts = CoinSelectionOptions (const 100)
        largestFirst opts txOuts wdrl utxo
