{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
            describe "Plutus E2E crowdfunding" $ do
                spec @n
