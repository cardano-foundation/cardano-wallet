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
import Data.Quantity
    ( Quantity (..) )
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
                , totalWithdrawal = 0
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
                , totalWithdrawal = 0
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
                , totalWithdrawal = 0
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
                , totalWithdrawal = 0
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
                , totalWithdrawal = 0
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
                , totalWithdrawal = 0
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
                , totalWithdrawal = 0
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
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random "withdrawal simple"
            (Right $ CoinSelectionResult
                { rsInputs = [1]
                , rsChange = []
                , rsOutputs = [2]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [1]
                , txOutputs = 2 :| []
                , totalWithdrawal = 1
                })

        coinSelectionUnitTest random "withdrawal multi-output"
            (Right $ CoinSelectionResult
                { rsInputs = [1,1]
                , rsChange = []
                , rsOutputs = [2,2]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [1,1]
                , txOutputs = 2 :| [2]
                , totalWithdrawal = 2
                })

        coinSelectionUnitTest random "withdrawal not even"
            -- 10 Ada available as withdrawal
            --
            -- - 7 Ada goes to the first output (10/14 * 10 ~= 7)
            -- - 2 Ada goes to the other output ( 4/14 * 10 ~= 2)
            (Right $ CoinSelectionResult
                { rsInputs = [5,5]
                , rsChange = [2,3]
                , rsOutputs = [10, 4]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [5,5]
                , txOutputs = 10 :| [4]
                , totalWithdrawal = 10
                })

        coinSelectionUnitTest random "withdrawal cover next output"
            -- 20 Ada available as withdrawal
            --
            -- - 10 Ada goes to the first output
            -- - 10 Ada goes to the other output
            --
            -- The first output has to select an available input first, which
            -- leaves no input for the second, but it can be covered with the
            -- withdrawal.
            (Right $ CoinSelectionResult
                { rsInputs = [1]
                , rsChange = [1]
                , rsOutputs = [10, 10]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [1]
                , txOutputs = 10 :| [10]
                , totalWithdrawal = 20
                })

        coinSelectionUnitTest random "withdrawal requires at least one input"
            (Left ErrInputsDepleted)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = []
                , txOutputs = 1 :| []
                , totalWithdrawal = 10
                })

        coinSelectionUnitTest random "not enough funds, withdrawal correctly counted"
            (Left $ ErrNotEnoughMoney 11 100)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [1]
                , txOutputs = 100 :| []
                , totalWithdrawal = 10
                })


        coinSelectionUnitTest random "enough funds, proper fragmentation, inputs depleted"
            (Left ErrInputsDepleted)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [10,10,10,10]
                , txOutputs = 38 :| [1]
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random ""
            (Left $ ErrMaximumInputsReached 2)
            (CoinSelectionFixture
                { maxNumOfInputs = 2
                , validateSelection = noValidation
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 3 :| []
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random "each output needs <maxNumOfInputs"
            (Left $ ErrMaximumInputsReached 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , validateSelection = noValidation
                , utxoInputs = replicate 100 1
                , txOutputs = NE.fromList (replicate 100 1)
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random "each output needs >maxNumInputs"
            (Left $ ErrMaximumInputsReached 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , validateSelection = noValidation
                , utxoInputs = replicate 100 1
                , txOutputs = NE.fromList (replicate 10 10)
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random ""
            (Left $ ErrNotEnoughMoney 39 40)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| []
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random ""
            (Left $ ErrNotEnoughMoney 39 43)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| [1,1,1]
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random ""
            (Left $ ErrUtxoNotEnoughFragmented 3 4)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = noValidation
                , utxoInputs = [12,20,17]
                , txOutputs = 40 :| [1,1,1]
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random "custom validation"
            (Left $ ErrInvalidSelection ErrValidation)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , validateSelection = alwaysFail
                , utxoInputs = [1,1]
                , txOutputs = 2 :| []
                , totalWithdrawal = 0
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
    prop (cs1, cs2) =
        L.length (inputs cs1) `shouldSatisfy` (>= L.length (inputs cs2))
    (selection1,_) = withDRG drg
        (runExceptT $ random opt txOuts (Quantity 0) utxo)
    selection2 = runIdentity $ runExceptT $
        largestFirst opt txOuts (Quantity 0) utxo
    opt = CoinSelectionOptions (const 100) noValidation

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
        (runExceptT $ random opt txOuts (Quantity 0) utxo)
    selection2 = runIdentity $ runExceptT $
        largestFirst opt txOuts (Quantity 0) utxo
    opt = (CoinSelectionOptions (const 1) noValidation)
