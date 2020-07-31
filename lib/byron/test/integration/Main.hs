{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, logInfo )
import Cardano.CLI
    ( Port (..), withLogging )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Startup
    ( withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( ApiByronWallet, WalletStyle (..) )
import Cardano.Wallet.Byron
    ( serveWallet, setupTracers, tracerSeverities )
import Cardano.Wallet.Byron.Compatibility
    ( Byron )
import Cardano.Wallet.Byron.Faucet
    ( initFaucet )
import Cardano.Wallet.Byron.Launch
    ( withCardanoNode )
import Cardano.Wallet.Byron.Transaction.Size
    ( maxSizeOf, minSizeOf, sizeOfSignedTx )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , FeePolicy (..)
    , Hash (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , TxIn (..)
    , TxOut (..)
    , TxParameters (..)
    )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( forM_, void )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Numeric.Natural
    ( Natural )
import System.IO
    ( BufferMode (..), hSetBuffering, stdout )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( Spec, SpecWith, after, describe, hspec, parallel )
import Test.Hspec.Extra
    ( aroundAll )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , KnownCommand (..)
    , Payload (..)
    , TxDescription (..)
    , request
    , unsafeRequest
    )
import Test.Utils.Paths
    ( getTestData )

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Test.Integration.Byron.Scenario.API.Migrations as MigrationsByron
import qualified Test.Integration.Byron.Scenario.API.Transactions as TransactionsByron
import qualified Test.Integration.Byron.Scenario.CLI.Transactions as TransactionsByronCLI
import qualified Test.Integration.Scenario.API.Byron.Addresses as AddressesByron
import qualified Test.Integration.Scenario.API.Byron.HWWallets as HWWalletsByron
import qualified Test.Integration.Scenario.API.Byron.Network as NetworkByron
import qualified Test.Integration.Scenario.API.Byron.Transactions as TransactionsByronCommon
import qualified Test.Integration.Scenario.API.Byron.Wallets as WalletsByron
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.CLI.Byron.Addresses as AddressesByronCLI
import qualified Test.Integration.Scenario.CLI.Byron.Wallets as WalletsByronCLI
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI

-- | Define the actual executable name for the bridge CLI
instance KnownCommand Byron where
    commandName = "cardano-wallet-byron"

main :: forall t . (t ~ Byron) => IO ()
main = withUtf8Encoding $ withLogging Nothing Info $ \(_, tr) -> do
    let n = Mainnet
    hSetBuffering stdout LineBuffering
    hspec $ do
        describe "No backend required" $ do
            describe "Miscellaneous CLI tests" $ parallel (MiscellaneousCLI.spec @t)
        describe "API Specifications" $ specWithServer tr $ do
<<<<<<< HEAD
            WalletsByron.spec @n
            HWWalletsByron.spec @n
            AddressesByron.spec @n
            TransactionsByron.spec @n
            MigrationsByron.spec @n
            TransactionsByronCommon.spec @n
=======
            WalletsByron.spec
            HWWalletsByron.spec
            AddressesByron.spec
            TransactionsByron.spec n
            TransactionsByronCommon.spec
>>>>>>> 59d9eb545... Refactor type-level NetworkDiscriminant
            Network.spec
            NetworkByron.spec
        describe "CLI Specifications" $ specWithServer tr $ do
            WalletsByronCLI.spec
            TransactionsByronCLI.spec n
            AddressesByronCLI.spec n
            PortCLI.spec @t
            NetworkCLI.spec @t

specWithServer
    :: Trace IO Text
    -> SpecWith (Context Byron)
    -> Spec
specWithServer tr = aroundAll withContext . after tearDown
  where
    withContext :: (Context Byron -> IO ()) -> IO ()
    withContext action = do
        ctx <- newEmptyMVar
        let setupContext np wAddr = do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                logInfo tr baseUrl
                let sixtySeconds = 60*1000*1000 -- 60s in microseconds
                manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                    { managerResponseTimeout =
                        responseTimeoutMicro sixtySeconds
                    })
                faucet <- initFaucet
                putMVar ctx $ Context
                    { _cleanup = pure ()
                    , _manager = manager
                    , _walletPort = Port . fromIntegral $ unsafePortNumber wAddr
                    , _faucet = faucet
                    , _feeEstimator = mkFeeEstimator
                        $ getFeePolicy
                        $ txParameters
                        $ protocolParameters np
                    , _networkParameters = np
                    , _network = Mainnet
                    , _target = Proxy
                    }
        race (takeMVar ctx >>= action) (withServer setupContext) >>=
            either pure (throwIO . ProcessHasExited "integration")

    withServer action =
        withCardanoNode tr $(getTestData) Info $ \socketPath block0 (np, vData) ->
        withSystemTempDirectory "cardano-wallet-databases" $ \db ->
            serveWallet
                Mainnet
                (setupTracers (tracerSeverities (Just Info)) tr)
                (SyncTolerance 10)
                (Just db)
                "127.0.0.1"
                ListenOnRandomPort
                Nothing
                socketPath
                block0
                (np, vData)
                (action np)

    -- | teardown after each test (currently only deleting all wallets)
    tearDown :: Context t -> IO ()
    tearDown ctx = do
        (_, wallets) <- unsafeRequest @[ApiByronWallet] ctx
            (Link.listWallets @'Byron) Empty
        forM_ wallets $ \w -> void $ request @Aeson.Value ctx
            (Link.deleteWallet @'Byron w) Default Empty

mkFeeEstimator :: FeePolicy -> TxDescription -> (Natural, Natural)
mkFeeEstimator policy = \case
    PaymentDescription nInps nOuts nChgs ->
        let
            inps = take nInps
                [ TxIn (Hash (BS.replicate 32 0)) tix | tix <- [0..] ]

            outsMin = replicate (nOuts + nChgs) (TxOut addr_ coin_)
              where
                coin_ = minBound
                addr_ = Address $ flip BS.replicate 0 $ minimum
                    [ minSizeOf @Address @IcarusKey Mainnet
                    , minSizeOf @Address @ByronKey Mainnet
                    ]

            outsMax = replicate (nOuts + nChgs) (TxOut addr_ coin_)
              where
                coin_ = maxBound
                addr_ = Address $ flip BS.replicate 0 $ maximum
                    [ maxSizeOf @Address @IcarusKey Mainnet
                    , maxSizeOf @Address @ByronKey Mainnet
                    ]

        in
            ( round a + round b * fromIntegral (sizeOfSignedTx inps outsMin)
            , round a + round b * fromIntegral (sizeOfSignedTx inps outsMax)
            )

    DelegDescription{} ->
        error "mkFeeEstimator: can't estimate fee for certificate in Byron!"
  where
    LinearFee (Quantity a) (Quantity b) _ = policy
