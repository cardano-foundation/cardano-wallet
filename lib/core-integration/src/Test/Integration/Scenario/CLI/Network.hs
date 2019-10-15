{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Network
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port (..) )
import Cardano.Wallet.Api.Types
    ( ApiNetworkInformation )
import Control.Monad
    ( void )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( typed )
import Data.Proxy
    ( Proxy (..) )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand, cardanoWalletCLI, expectValidJSON )
import Test.Integration.Framework.TestData
    ( cmdOk )

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = do
    it "CLI_NETWORK - cardano-wallet network information" $ \ctx -> do
        let port = show (ctx ^. typed @(Port "wallet"))
        (Exit c, Stderr e, Stdout o) <- cardanoWalletCLI @t
            ["network", "information", "--port", port ]
        c `shouldBe` ExitSuccess
        e `shouldContain` cmdOk
        void $ expectValidJSON (Proxy @ApiNetworkInformation) o
