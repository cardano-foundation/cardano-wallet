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
    ( ApiNetworkClock (..)
    , ApiNetworkInformation (..)
    , ApiNetworkParameters
    , NtpSyncingStatus (..)
    , toApiNetworkParameters
    )
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
import Data.Word.Odd
    ( Word31 )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , KnownCommand
    , cardanoWalletCLI
    , eventually
    , expectCliField
    , expectValidJSON
    )
import Test.Integration.Framework.TestData
    ( cmdOk, errMsg404NoEpochNo )

spec :: forall t. KnownCommand t => SpecWith (Context t)
spec = do
    it "CLI_NETWORK - cardano-wallet network information" $ \ctx -> do
        info <- getNetworkInfoViaCLI ctx
        let nextEpochNum =
                info ^. (#nextEpoch . #epochNumber . #getApiT)
        nextEpochNum `shouldBe` (currentEpochNo info) + 1

    describe "NETWORK_PARAMS_01 - Valid epoch values" $ do
        it "Epoch = latest" $ \ctx -> do
            params1 <- getNetworkParamsViaCliExpectingSuccess ctx "latest"
            params1 `shouldBe` toApiNetworkParameters (ctx ^. #_blockchainParameters)

        it "Epoch = 0" $ \ctx -> do
            params2 <- getNetworkParamsViaCliExpectingSuccess ctx "0"
            params2 `shouldBe` toApiNetworkParameters (ctx ^. #_blockchainParameters)

        it "Current epoch" $ \ctx -> do
            info <- getNetworkInfoViaCLI ctx
            let (EpochNo currentEpoch) = currentEpochNo info
            params3 <-
                getNetworkParamsViaCliExpectingSuccess ctx (show currentEpoch)
            params3 `shouldBe` toApiNetworkParameters (ctx ^. #_blockchainParameters)

        it "Previous epoch" $ \ctx -> do
            info <- getNetworkInfoViaCLI ctx
            let (EpochNo currentEpoch) = currentEpochNo info
            let (EpochNo prevEpoch) =
                    fromMaybe minBound (epochPred $ EpochNo currentEpoch)
            params4 <- getNetworkParamsViaCliExpectingSuccess ctx (show prevEpoch)
            params4 `shouldBe` toApiNetworkParameters (ctx ^. #_blockchainParameters)

    describe "NETWORK_PARAMS_02 - Cannot query future epoch" $ do
        it "Future epoch" $ \ctx -> do
            info <- getNetworkInfoViaCLI ctx
            let (EpochNo currentEpoch) = currentEpochNo info
            let futureEpoch = show $ currentEpoch + 10
            params <- getNetworkParamsViaCliExpectingFailure ctx futureEpoch
            params `shouldContain` (errMsg404NoEpochNo futureEpoch)

        it "Max epoch" $ \ctx -> do
            let maxEpoch = show $ fromIntegral @Word31 @Int maxBound
            params <- getNetworkParamsViaCliExpectingFailure ctx maxEpoch
            params `shouldContain` (errMsg404NoEpochNo maxEpoch)

    it "CLI_NETWORK - network clock" $ \ctx -> do
        eventually "ntp status = available" $ do
            clock <- getNetworkClockViaCLI ctx
            expectCliField (#ntpStatus . #status)
                (`shouldBe` NtpSyncingStatusAvailable) clock
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

      getNetworkClockViaCLI
          :: Context t
          -> IO ApiNetworkClock
      getNetworkClockViaCLI ctx = do
          let port = show (ctx ^. typed @(Port "wallet"))
          (Exit c, Stderr e, Stdout o) <- cardanoWalletCLI @t
              ["network", "clock", "--port", port ]
          c `shouldBe` ExitSuccess
          e `shouldContain` cmdOk
          expectValidJSON (Proxy @ApiNetworkClock) o

      currentEpochNo :: ApiNetworkInformation -> EpochNo
      currentEpochNo netInfo =
          netInfo ^. (#networkTip . #epochNumber . #getApiT)
