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
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , FeePolicy (..)
    , GenesisBlockParameters (..)
    , Hash (..)
    , SyncTolerance (..)
    , TxIn (..)
    , TxOut (..)
    , TxParameters (..)
    )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , serveWallet
    , setupTracers
    , tracerSeverities
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley )
import Cardano.Wallet.Shelley.Faucet
    ( initFaucet )
import Cardano.Wallet.Shelley.Launch
    ( withCardanoNode )
import Cardano.Wallet.Shelley.Transaction.Size
    ( MaxSizeOf (..), MinSizeOf (..), sizeOfSignedTx )
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
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.API.Shelley.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Shelley.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Shelley.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Keys as KeyCLI
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Mnemonics as MnemonicsCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI

-- | Define the actual executable name for the bridge CLI
instance KnownCommand Shelley where
    commandName = "cardano-wallet-shelley"

main :: forall t n . (t ~ Shelley, n ~ 'Mainnet) => IO ()
main = withUtf8Encoding $ withLogging Nothing Info $ \(_, tr) -> do
    hSetBuffering stdout LineBuffering
    hspec $ do
        describe "No backend required" $ do
            describe "Mnemonics CLI tests" $ parallel (MnemonicsCLI.spec @t)
            describe "Miscellaneous CLI tests" $ parallel (MiscellaneousCLI.spec @t)
            describe "Key CLI tests" $ parallel (KeyCLI.spec @t)
        describe "API Specifications" $ specWithServer tr $ do
            Addresses.spec @n
            Transactions.spec @n
            Wallets.spec @n
            Network.spec
        describe "CLI Specifications" $ specWithServer tr $ do
            PortCLI.spec @t
            NetworkCLI.spec @t

specWithServer
    :: Trace IO Text
    -> SpecWith (Context Shelley)
    -> Spec
specWithServer tr = aroundAll withContext . after tearDown
  where
    withContext :: (Context Shelley -> IO ()) -> IO ()
    withContext action = do
        ctx <- newEmptyMVar
        let setupContext gbp wAddr = do
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
                    , _feeEstimator = mkFeeEstimator (getFeePolicy (txParameters gbp))
                    , _blockchainParameters = staticParameters gbp
                    , _target = Proxy
                    }
        race (takeMVar ctx >>= action) (withServer setupContext) >>=
            either pure (throwIO . ProcessHasExited "integration")

    withServer action =
        withCardanoNode tr $(getTestData) Info $ \socketPath block0 (bp,vData) ->
        withSystemTempDirectory "cardano-wallet-databases" $ \db -> do
            serveWallet @(IO Shelley)
                (SomeNetworkDiscriminant $ Proxy @'Mainnet)
                (setupTracers (tracerSeverities (Just Info)) tr)
                (SyncTolerance 10)
                (Just db)
                "127.0.0.1"
                ListenOnRandomPort
                Nothing
                socketPath
                block0
                (bp, vData)
                (action bp)

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
                [ ( TxIn (Hash (BS.replicate 32 0)) tix
                  , TxOut (Address mempty) minBound
                  )
                | tix <- [0..]
                ]

            outsMin = replicate (nOuts + nChgs) (TxOut addr_ coin_)
              where
                coin_ = minBound
                addr_ = Address $ flip BS.replicate 0 $ minimum
                    [ minSizeOf @Address @'Mainnet @ShelleyKey
                    ]

            outsMax = replicate (nOuts + nChgs) (TxOut addr_ coin_)
              where
                coin_ = maxBound
                addr_ = Address $ flip BS.replicate 0 $ maximum
                    [ maxSizeOf @Address @'Mainnet @ShelleyKey
                    ]

        in
            ( round a + round b * fromIntegral (sizeOfSignedTx inps outsMin)
            , round a + round b * fromIntegral (8 + sizeOfSignedTx inps outsMax)
            )

    DelegDescription{} ->
        error "mkFeeEstimator: can't estimate fee for certificate in Byron!"
  where
    LinearFee (Quantity a) (Quantity b) _ = policy
