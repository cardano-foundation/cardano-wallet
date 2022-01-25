{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Shelley.Integration
    ( specWithServer, withTestsSetup )
import Test.Hspec.Core.Spec
    ( describe, parallel, sequential )
import Test.Hspec.Extra
    ( hspecMain )
import Test.PlutusE2E.Base

main :: forall n. (n ~ 'Mainnet) => IO ()
main = withTestsSetup $ \testDir tracers -> do
    hspecMain $ do
        specWithServer testDir tracers $ do
            describe "API Specifications" $ do
                parallel $ do

  where
    parallelIf flag = if flag then parallel else sequential
