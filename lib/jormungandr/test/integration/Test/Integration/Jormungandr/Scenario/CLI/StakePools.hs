{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.StakePools
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( DecodeAddress, EncodeAddress )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, it, shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand (..), listStakePoolsViaCLI )

spec
    :: forall t. (EncodeAddress t, DecodeAddress t, KnownCommand t)
    => SpecWith (Context t)
spec = do
    it "STAKE_POOLS_LIST_01 - List stake pools" $ \ctx -> do
        (Exit c, Stdout o, Stderr e) <- listStakePoolsViaCLI @t ctx
        e `shouldBe` "It looks like something unexpected went wrong. \
            \Unfortunately I don't yet know how to handle this type of \
            \situation. Here's some information about what happened: \n"
        o `shouldBe` mempty
        c `shouldBe` ExitFailure 1
