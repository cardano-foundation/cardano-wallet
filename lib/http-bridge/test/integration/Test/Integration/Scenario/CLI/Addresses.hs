{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Addresses
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Proxy
    ( Proxy (..) )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe )
import Test.Integration.Framework.DSL
    ( Context (..)
    , emptyWallet
    , expectValidJSON
    , listAddressesViaCLI
    , walletId
    )

import qualified Data.Text as T

spec :: SpecWith Context
spec = do

    it "CLI - Can list addresses" $ \ctx -> do
        walId <- emptyWallet' ctx
        (Exit c, Stdout out, Stderr err) <- listAddressesViaCLI walId
        err `shouldBe` "Ok.\n"
        expectValidJSON (Proxy @[ApiAddress HttpBridge]) out
        c `shouldBe` ExitSuccess

  where
    emptyWallet' :: Context -> IO String
    emptyWallet' = fmap (T.unpack . view walletId) . emptyWallet
