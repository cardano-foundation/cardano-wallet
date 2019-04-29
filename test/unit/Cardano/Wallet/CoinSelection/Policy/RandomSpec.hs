{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.Policy.RandomSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), CoinSelectionError (..), CoinSelectionOptions (..) )
import Cardano.Wallet.CoinSelection.Policy.LargestFirst
    ( largestFirst )
import Cardano.Wallet.CoinSelection.Policy.Random
    ( random )
import Cardano.Wallet.CoinSelectionSpec
    ( CoinSelProp (..)
    , CoinSelectionFixture (..)
    , CoinSelectionResult (..)
    , coinSelectionUnitTest
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


spec :: Spec
spec = do
    describe "Unit tests" $ do
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
                })

        coinSelectionUnitTest random "with fallback"
            (Right $ CoinSelectionResult
                { rsInputs = [1,1,1]
                , rsChange = []
                , rsOutputs = [2,1]
                })
            (CoinSelectionFixture
                { maxNumOfInputs = 100
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
                , utxoInputs = [1,1,1,1,1,1]
                , txOutputs = 3 :| []
                })

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
