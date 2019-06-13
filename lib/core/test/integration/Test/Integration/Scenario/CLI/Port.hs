{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Port
    ( specWithDefaultPort
    ) where

import Prelude

import Data.Generics.Internal.VL.Lens
    ( over )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Text
    ( Text )
import Network.Wai.Handler.Warp
    ( Port )
import System.Command
    ( Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( createWalletViaCLI, generateMnemonicsViaCLI )

import qualified Data.Text as T

specWithDefaultPort :: HasType Port s => SpecWith s
specWithDefaultPort = do
    it "PORT_01 - Can't reach server with wrong port (wallet create)" $ \ctx -> do
        let ctx' = over (typed @Port) (+1) ctx
        let name = "Wallet created via CLI"
        Stdout mnemonics <- generateMnemonicsViaCLI ["--size", "15"]
        let pwd = "Secure passphrase"
        res <- createWalletViaCLI ctx' [name] mnemonics "\n" pwd
        shouldBeConnectionRefused res
  where
    shouldBeConnectionRefused :: (ExitCode, String, Text) -> IO ()
    shouldBeConnectionRefused (c, out, err) = do
        c `shouldBe` (ExitFailure 1)
        out `shouldBe` ""
        T.unpack err `shouldContain` "does not exist (Connection refused)"
