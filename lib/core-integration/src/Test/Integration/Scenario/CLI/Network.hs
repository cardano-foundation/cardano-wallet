{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLI.Network
    ( spec
    ) where

import Prelude

import Cardano.CLI
    ( Port (..) )
import Cardano.Wallet.Api.Types
    ( ApiNetworkInformation (..), ApiNetworkParameters )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..), epochPred )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Generics.Product.Typed
    ( typed )
import Data.Maybe
    ( fromMaybe )
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
    ( Context (..)
    , KnownCommand
    , cardanoWalletCLI
    , expectValidJSON
    , expectedBlockchainParams
    )
import Test.Integration.Framework.TestData
    ( cmdOk, errMsg400MalformedEpoch, errMsg404NoEpochNo )

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = do
    it "CLI_NETWORK - cardano-wallet network information" $ \ctx -> do
        info <- getNetworkInfoViaCLI ctx
        let nextEpochNum =
                info ^. (#nextEpoch . #epochNumber . #getApiT)
        nextEpochNum `shouldBe` (currentEpochNo info) + 1

    it "CLI_NETWORK - cardano-wallet network parameters - \
       \proper parameter epoch parameter" $ \ctx -> do
        params1 <- getNetworkParamsViaCliExpectingSuccess ctx "latest"
        params1 `shouldBe` expectedBlockchainParams

        params2 <- getNetworkParamsViaCliExpectingSuccess ctx "0"
        params2 `shouldBe` expectedBlockchainParams

        info <- getNetworkInfoViaCLI ctx
        let (EpochNo currentEpoch) = currentEpochNo info
        params3 <-
            getNetworkParamsViaCliExpectingSuccess ctx (show currentEpoch)
        params3 `shouldBe` expectedBlockchainParams

        let (EpochNo prevEpoch) =
                fromMaybe minBound (epochPred $ EpochNo currentEpoch)
        params4 <- getNetworkParamsViaCliExpectingSuccess ctx (show prevEpoch)
        params4 `shouldBe` expectedBlockchainParams

    it "CLI_NETWORK - cardano-wallet network parameters - \
       \improper parameter epoch parameter" $ \ctx -> do

        let wrong1 = "earlier"
        params1 <- getNetworkParamsViaCliExpectingFailure ctx "earlier"
        params1 `shouldContain` (errMsg400MalformedEpoch wrong1)

        let wrong2 = "1.1"
        params2 <- getNetworkParamsViaCliExpectingFailure ctx wrong2
        params2 `shouldContain` (errMsg400MalformedEpoch wrong2)

        info <- getNetworkInfoViaCLI ctx
        let (EpochNo currentEpoch) = currentEpochNo info
        let wrong3 = show $ currentEpoch + 10
        params3 <- getNetworkParamsViaCliExpectingFailure ctx wrong3
        params3 `shouldContain` (errMsg404NoEpochNo wrong3)
  where
      getNetworkParamsViaCliExpectingSuccess
          :: Context t
          -> String
          -> IO ApiNetworkParameters
      getNetworkParamsViaCliExpectingSuccess ctx epoch = do
          let port = show (ctx ^. typed @(Port "wallet"))
          (Exit c, Stderr e, Stdout o) <- cardanoWalletCLI @t
              ["network", "parameters", "--port", port, epoch ]
          c `shouldBe` ExitSuccess
          e `shouldContain` cmdOk
          expectValidJSON (Proxy @ApiNetworkParameters) o

      getNetworkParamsViaCliExpectingFailure
          :: Context t
          -> String
          -> IO String
      getNetworkParamsViaCliExpectingFailure ctx epoch = do
          let port = show (ctx ^. typed @(Port "wallet"))
          (Exit c, Stderr e, Stdout o) <- cardanoWalletCLI @t
              ["network", "parameters", "--port", port, epoch ]
          c `shouldBe` (ExitFailure 1)
          o `shouldBe` ""
          pure e

      getNetworkInfoViaCLI
          :: Context t
          -> IO ApiNetworkInformation
      getNetworkInfoViaCLI ctx = do
          let port = show (ctx ^. typed @(Port "wallet"))
          (Exit c, Stderr e, Stdout o) <- cardanoWalletCLI @t
              ["network", "information", "--port", port ]
          c `shouldBe` ExitSuccess
          e `shouldContain` cmdOk
          expectValidJSON (Proxy @ApiNetworkInformation) o

      currentEpochNo :: ApiNetworkInformation -> EpochNo
      currentEpochNo netInfo =
          netInfo ^. (#networkTip . #epochNumber . #getApiT)
