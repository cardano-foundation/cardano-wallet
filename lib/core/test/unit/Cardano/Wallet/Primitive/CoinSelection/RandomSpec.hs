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
    , coinSelectionUnitTest
    )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isLeft, isRight )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Property, counterexample, property )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, pre, run )

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do
    parallel $ describe "Coin selection : random algorithm unit tests" $ do
        let oneAda = 1000000

        coinSelectionUnitTest random ""
            (Right $ CoinSelectionResult
                { rsInputs = [1,1,1,1]
                , rsChange = [2]
                , rsOutputs = [2]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
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
                , utxoInputs = [1,1]
                , txOutputs = 2 :| [2]
                , totalWithdrawal = 2
                })

        coinSelectionUnitTest random "withdrawal cover next output, no improvement"
            -- NOTE
            -- There's no change because the withdrawal covers it all _just_
            -- perfectly, the exceeding withdrawal remains available as a
            -- positive delta for fee balancing.
            (Right $ CoinSelectionResult
                { rsInputs = [1]
                , rsChange = []
                , rsOutputs = [10, 10]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [1]
                , txOutputs = 10 :| [10]
                , totalWithdrawal = 20
                })

        coinSelectionUnitTest random "withdrawal cover next, with input improvement"
            -- NOTE
            -- This one is tricky, but here's what's happening
            --
            -- 1. A first input is selected
            -- 2. Part of the withdrawal (5) is used to cover for the remainder
            -- 3. Rest of the withdrawal (1) is used to cover the second output
            -- 4. The first input selection is "improved", so another input is
            --    picked (hence the change of roughly the same size)
            -- 5. There are no more inputs available, so the second input can't
            --    be improved.
            --
            -- At the end, still remains 4 Lovelace that can be used in the fee
            -- balancing.
            (Right $ CoinSelectionResult
                { rsInputs = [5,5]
                , rsChange = [5]
                , rsOutputs = [10, 1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [5,5]
                , txOutputs = 10 :| [1]
                , totalWithdrawal = 10
                })

        coinSelectionUnitTest random "withdrawal can cover many next outputs"
            (Right $ CoinSelectionResult
                { rsInputs = [1]
                , rsChange = []
                , rsOutputs = [1,1,1,1,1,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [1]
                , txOutputs = 1 :| [1,1,1,1,1]
                , totalWithdrawal = 5
                })

        coinSelectionUnitTest random "withdrawal requires at least one input"
            (Left ErrInputsDepleted)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = []
                , txOutputs = 1 :| []
                , totalWithdrawal = 10
                })

        coinSelectionUnitTest random "not enough funds, withdrawal correctly counted"
            (Left $ ErrNotEnoughMoney 11 100)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [1]
                , txOutputs = 100 :| []
                , totalWithdrawal = 10
                })

        coinSelectionUnitTest random ""
            (Left $ ErrMaximumInputsReached 2)
            (CoinSelectionFixture
                { maxNumOfInputs = 2
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 3 :| []
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random "each output needs <maxNumOfInputs"
            (Left $ ErrMaximumInputsReached 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , utxoInputs = replicate 100 1
                , txOutputs = NE.fromList (replicate 100 1)
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random "each output needs >maxNumInputs"
            (Left $ ErrMaximumInputsReached 9)
            (CoinSelectionFixture
                { maxNumOfInputs = 9
                , utxoInputs = replicate 100 1
                , txOutputs = NE.fromList (replicate 10 10)
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random ""
            (Left $ ErrNotEnoughMoney 39 40)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| []
                , totalWithdrawal = 0
                })

        coinSelectionUnitTest random ""
            (Left $ ErrNotEnoughMoney 39 43)
            (CoinSelectionFixture
                { maxNumOfInputs = 100
                , utxoInputs = [12,10,17]
                , txOutputs = 40 :| [1,1,1]
                , totalWithdrawal = 0
                })

    parallel $ describe "Coin selection properties : random algorithm" $ do
        it "forall (UTxO, NonEmpty TxOut), \
           \ running algorithm gives not less UTxO fragmentation than LargestFirst algorithm"
            (property propFragmentation)
        it "forall (UTxO, NonEmpty TxOut), \
           \ running algorithm gives the same errors as LargestFirst algorithm"
            (property propErrors)

{-------------------------------------------------------------------------------
                              Properties
-------------------------------------------------------------------------------}

propFragmentation
    :: CoinSelProp
    -> Property
propFragmentation (CoinSelProp utxo wdrl txOuts) = monadicIO $ do
    let opts = CoinSelectionOptions (const 100)
    selection1 <- run $ runExceptT $ random opts txOuts wdrl utxo
    selection2 <- run $ runExceptT $ largestFirst opts txOuts wdrl utxo
    pre (isRight selection1)
    pre (isRight selection2)
    let (Right (s1,_), Right (s2,_)) = (selection1, selection2)
    monitor $ counterexample $ unlines
        [ "selection (random): " <> show s1
        , "selection (largestFirst): " <> show s2
        ]
    assert $ L.length (inputs s1) >= L.length (inputs s2)

propErrors
    :: CoinSelProp
    -> Property
propErrors (CoinSelProp utxo wdrl txOuts) = monadicIO $ do
    let opts = CoinSelectionOptions (const 1)
    selection1 <- run $ runExceptT $ random opts txOuts wdrl utxo
    selection2 <- run $ runExceptT $ largestFirst opts txOuts wdrl utxo
    pre (isLeft selection1)
    pre (isLeft selection2)
    let (Left e1, Left e2) = (selection1, selection2)
    monitor $ counterexample $ unlines
        [ "error (random): " <> show e1
        , "error (largestFirst): " <> show e2
        ]
    assert (e1 == e2)
