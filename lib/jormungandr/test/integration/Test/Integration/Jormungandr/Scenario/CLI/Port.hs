{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Jormungandr.Scenario.CLI.Port
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port (..), getPort )
import Control.Monad
    ( forM_ )
import Data.Generics.Product.Typed
    ( HasType )
import System.Command
    ( Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( KnownCommand (..), cardanoWalletCLI )

spec
    :: forall t s. (HasType (Port "wallet") s, KnownCommand t)
    => SpecWith s
spec = do
    describe "PORT_04_JORM - Fail nicely when port is out-of-bounds" $ do
        let tests =
                [ ("serve", "--node-port", getPort minBound - 6)
                , ("serve", "--node-port", getPort maxBound + 523)
                , ("launch", "--port", getPort minBound - 1)
                , ("launch", "--port", getPort maxBound + 59375)
                , ("launch", "--node-port", getPort minBound - 5621)
                , ("launch", "--node-port", getPort maxBound + 1)
                ]
        forM_ tests $ \(cmd, opt, port) -> let args = [cmd, opt, show port] in
            it (unwords args) $ \_ -> do
                (exit, Stdout (_ :: String), Stderr err) <- cardanoWalletCLI @t @_ @IO args
                exit `shouldBe` ExitFailure 1
                err `shouldContain`
                    (  "expected a TCP port number between "
                    <> show (getPort minBound)
                    <> " and "
                    <> show (getPort maxBound)
                    )
