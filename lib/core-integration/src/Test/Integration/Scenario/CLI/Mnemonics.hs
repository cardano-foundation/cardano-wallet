{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Mnemonics
    ( spec
    ) where

import Prelude

import Control.Monad
    ( forM_ )
import Data.List
    ( length )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( KnownCommand, generateMnemonicsViaCLI )

spec :: forall t. KnownCommand t => SpecWith ()
spec = do
    it "CLI_MNEMONICS_01 - Can generate mnemonics with default size" $  do
        (Exit c, Stdout out) <- generateMnemonicsViaCLI @t []
        length (words out) `shouldBe` 15
        c `shouldBe` ExitSuccess

    describe "CLI_MNEMONICS_01 - Can generate mnemonics with different sizes" $ do
        let test size = it ("--size=" <> show size) $ do
                (Exit c, Stdout out) <-
                    generateMnemonicsViaCLI @t ["--size", show size]
                length (words out) `shouldBe` size
                c `shouldBe` ExitSuccess
        forM_ [9, 12, 15, 18, 21, 24] test

    describe "CLI_MNEMONICS_02 - It can't generate mnemonics with an invalid size" $ do
        let sizes =
                ["15.5", "3", "6", "14", "abc", "👌", "0", "~!@#%" , "-1000", "1000"]
        forM_ sizes $ \(size) -> it ("--size=" <> size) $ do
            (Exit c, Stdout out, Stderr err) <-
                generateMnemonicsViaCLI @t ["--size", size]
            c `shouldBe` ExitFailure 1
            err `shouldContain`
                "Invalid mnemonic size. Expected one of: 9, 12, 15, 18, 21, 24."
            out `shouldBe` mempty
