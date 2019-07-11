{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.CoinSelection.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..), CoinSelectionOptions (..), ErrCoinSelection (..) )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.CoinSelection.Random
    ( random )
import Cardano.Wallet.Primitive.CoinSelectionSpec
    ( CoinSelProp (..)
    , CoinSelectionFixture (..)
    , CoinSelectionResult (..)
    , ErrValidation (..)
    , alwaysFail
    , coinSelectionUnitTest
    , noValidation
    )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Random
    ( SystemDRG, getSystemDRG )
import Crypto.Random.Types
    ( withDRG )
import Data.Either
    ( isLeft, isRight )
import Data.Functor.Identity
    ( Identity (..) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Test.Hspec
    ( Spec, before, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( Property, property, (===), (==>) )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do
    describe "Coin selection : random algorithm unit tests" $ do
        let oneAda = 1000000

        coinSelectionUnitTest random ""
            (Right $ CoinSelectionResult
                { rsInputs = [1,1,1,1]
                , rsChange = [2]
                , rsOutputs = [2]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 2 :| []
                })

        coinSelectionUnitTest random ""
            (Right $ CoinSelectionResult
                { rsInputs = [1,1,1,1,1,1]
                , rsChange = [2,1]
                , rsOutputs = [2,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 2 :| [1]
                })

        coinSelectionUnitTest random ""
            (Right $ CoinSelectionResult
                { rsInputs = [1,1,1,1,1]
                , rsChange = [2]
                , rsOutputs = [2,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [1,1,1,1,1]
                , txOutputs = 2 :| [1]
                })

        coinSelectionUnitTest random ""
            (Right $ CoinSelectionResult
                { rsInputs = [1,1,1,1]
                , rsChange = [1]
                , rsOutputs = [2,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [1,1,1,1]
                , txOutputs = 2 :| [1]
                })

        coinSelectionUnitTest random ""
            (Right $ CoinSelectionResult
                { rsInputs = [5]
                , rsChange = [3]
                , rsOutputs = [2]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [5,5,5]
                , txOutputs = 2 :| []
                })

        coinSelectionUnitTest random ""
            (Right $ CoinSelectionResult
                { rsInputs = [10,10]
                , rsChange = [8,8]
                , rsOutputs = [2,2]
                }
            )
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [10,10,10]
                , txOutputs = 2 :| [2]
                })

        coinSelectionUnitTest random "cannot cover aim, but only min"
            (Right $ CoinSelectionResult
                { rsInputs = [1,1,1,1]
                , rsChange = [1]
                , rsOutputs = [3]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 4
                , validateSelection = noValidation
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 3 :| []
                })

        coinSelectionUnitTest random "REG CO-450: no fallback"
            (Right $ CoinSelectionResult
                { rsInputs = [oneAda, oneAda, oneAda, oneAda]
                , rsChange = [oneAda, oneAda `div` 2]
                , rsOutputs = [2*oneAda,oneAda `div` 2]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 4
                , validateSelection = noValidation
                , utxoInputs = [oneAda, oneAda, oneAda, oneAda]
                , txOutputs = 2*oneAda :| [oneAda `div` 2]
                })

        coinSelectionUnitTest random "enough funds, proper fragmentation, inputs depleted"
            (Left ErrInputsDepleted)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [10,10,10,10]
                , txOutputs = 38 :| [1]
                })

        coinSelectionUnitTest random ""
            (Left $ ErrMaximumInputsReached 2)
            (CoinSelectionFixture
                { maxNumOfInputs = 2
                , validateSelection = noValidation
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 3 :| []
                })

        coinSelectionUnitTest random "each output needs <maxNumOfInputs"
            (Left $ ErrMaximumInputsReached 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , validateSelection = noValidation
                , utxoInputs = replicate 100 1
                , txOutputs = NE.fromList (replicate 100 1)
                })

        coinSelectionUnitTest random "each output needs >maxNumInputs"
            (Left $ ErrMaximumInputsReached 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , validateSelection = noValidation
                , utxoInputs = replicate 100 1
                , txOutputs = NE.fromList (replicate 10 10)
                })

        coinSelectionUnitTest random ""
            (Left $ ErrNotEnoughMoney 39 40)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| []
                })

        coinSelectionUnitTest random ""
            (Left $ ErrNotEnoughMoney 39 43)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| [1,1,1]
                })

        coinSelectionUnitTest random ""
            (Left $ ErrUtxoNotEnoughFragmented 3 4)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [12,20,17]
                , txOutputs = 40 :| [1,1,1]
                })

        coinSelectionUnitTest random "custom validation"
            (Left $ ErrInvalidSelection ErrValidation)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = alwaysFail
                , utxoInputs = [1,1]
                , txOutputs = 2 :| []
                })

    before getSystemDRG $ describe "Coin selection properties : random algorithm" $ do
        it "forall (UTxO, NonEmpty TxOut), \
           \ running algorithm gives not less UTxO fragmentation than LargestFirst algorithm"
            (property . propFragmentation)
        it "forall (UTxO, NonEmpty TxOut), \
           \ running algorithm gives the same errors as LargestFirst algorithm"
            (property . propErrors)

{-------------------------------------------------------------------------------
                              Properties
-------------------------------------------------------------------------------}

propFragmentation
    :: SystemDRG
    -> CoinSelProp
    -> Property
propFragmentation drg (CoinSelProp utxo txOuts) = do
    isRight selection1 && isRight selection2 ==>
        let (Right (s1,_), Right (s2,_)) =
                (selection1, selection2)
        in prop (s1, s2)
  where
    prop (CoinSelection inps1 _ _, CoinSelection inps2 _ _) =
        L.length inps1 `shouldSatisfy` (>= L.length inps2)
    (selection1,_) = withDRG drg
        (runExceptT $ random (CoinSelectionOptions 100 noValidation) txOuts utxo)
    selection2 = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100 noValidation) txOuts utxo

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
        (runExceptT $ random (CoinSelectionOptions 1 noValidation) txOuts utxo)
    selection2 = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 1 noValidation) txOuts utxo
