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
    ( Context (..), KnownCommand (..), eventually, listStakePoolsViaCLI )

spec
    :: forall t. (EncodeAddress t, DecodeAddress t, KnownCommand t)
    => SpecWith (Context t)
spec = do
    it "STAKE_POOLS_LIST_01 - List stake pools" $ \ctx -> do
        eventually $ do
            (Exit c, Stdout _, Stderr e) <- listStakePoolsViaCLI @t ctx
            e `shouldBe` "Ok.\n"
            c `shouldBe` ExitSuccess
