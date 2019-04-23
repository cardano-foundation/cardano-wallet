{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.Policy.LargestFirstSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), CoinSelectionError (..), CoinSelectionOptions (..) )
import Cardano.Wallet.CoinSelection.Policy.LargestFirst
    ( largestFirst )
import Cardano.Wallet.CoinSelectionSpec
    ( CoinSelProp (..), CoinSelectionFixture (..), coinSelectionUnitTest )
import Cardano.Wallet.Primitive.Types
    ( Coin (..), TxOut (..), UTxO (..), excluding )
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
    ( Spec, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( Property, property, (===), (==>) )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Coin selection : LargestFirst algorithm unit tests" $ do
        coinSelectionUnitTest largestFirst "" (Right [17]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [10,10,17]
            , txOutputs = 17 :| []
            }

        coinSelectionUnitTest largestFirst "" (Right [17]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [12,10,17]
            , txOutputs = 1 :| []
            }

        coinSelectionUnitTest largestFirst "" (Right [12, 17]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [12,10,17]
            , txOutputs = 18 :| []
            }

        coinSelectionUnitTest largestFirst "" (Right [10, 12, 17]) $ CoinSelectionFixture
            { maxNumOfInputs = 100
            , utxoInputs = [12,10,17]
            , txOutputs = 30 :| []
            }

        coinSelectionUnitTest largestFirst "" (Right [6,10,5]) $ CoinSelectionFixture
            { maxNumOfInputs = 3
            , utxoInputs = [1,2,10,6,5]
            , txOutputs = 11 :| [1]
            }

        coinSelectionUnitTest
            largestFirst
            "not enough coins"
            (Left $ NotEnoughMoney 39 40)
            $ CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| []
                }

        coinSelectionUnitTest
            largestFirst
            "not enough coin & not fragmented enough"
            (Left $ NotEnoughMoney 39 43)
            $ CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| [1,1,1]
                }

        coinSelectionUnitTest
            largestFirst
            "enough coins, but not fragmented enough"
            (Left $ UtxoNotEnoughFragmented 3 4)
            $ CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,20,17]
                , txOutputs = 40 :| [1,1,1]
                }

        coinSelectionUnitTest
            largestFirst
            "enough coins but, strict maximumNumberOfInputs"
            (Left $ MaximumInputsReached 2)
            $ CoinSelectionFixture
                { maxNumOfInputs = 2
                , utxoInputs = [1,2,10,6,5]
                , txOutputs = 11 :| [1]
                }

    describe "Coin selection properties : LargestFirst algorithm" $ do
        it "forall (UTxO, NonEmpty TxOut), running algorithm twice yields \
            \exactly the same result"
            (property propDeterministic)
        it "forall (UTxO, NonEmpty TxOut), there's at least as many selected \
            \inputs as there are requested outputs"
            (property propAtLeast)
        it "forall (UTxO, NonEmpty TxOut), for all selected input, there's no \
            \bigger input in the UTxO that is not already in the selected inputs"
            (property propInputDecreasingOrder)

{-------------------------------------------------------------------------------
                                  Properties
-------------------------------------------------------------------------------}

propDeterministic
    :: CoinSelProp
    -> Property
propDeterministic (CoinSelProp utxo txOuts) = do
    let opts = CoinSelectionOptions 100
    let resultOne = runIdentity $ runExceptT $ largestFirst opts utxo txOuts
    let resultTwo = runIdentity $ runExceptT $ largestFirst opts utxo txOuts
    resultOne === resultTwo

propAtLeast
    :: CoinSelProp
    -> Property
propAtLeast (CoinSelProp utxo txOuts) =
    isRight selection ==> let Right s = selection in prop s
  where
    prop (CoinSelection inps _ _) =
        L.length inps `shouldSatisfy` (>= NE.length txOuts)
    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts

propInputDecreasingOrder
    :: CoinSelProp
    -> Property
propInputDecreasingOrder (CoinSelProp utxo txOuts) =
    isRight selection ==> let Right s = selection in prop s
  where
    prop (CoinSelection inps _ _) =
        let
            utxo' = (Map.toList . getUTxO) $
                utxo `excluding` (Set.fromList . map fst $ inps)
        in unless (L.null utxo') $
            (getExtremumValue L.minimum inps)
            `shouldSatisfy`
            (>= (getExtremumValue L.maximum utxo'))
    getExtremumValue f = f . map (getCoin . coin . snd)
    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts
