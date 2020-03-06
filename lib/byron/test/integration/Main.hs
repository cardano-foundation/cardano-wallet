{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Cardano.Wallet.Byron.Config
    ( withCardanoNode )
import Cardano.Wallet.Byron.Faucet
    ( initFaucet )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( SyncTolerance (..) )
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
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Hspec
    ( Spec, SpecWith, after, describe, hspec )
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

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Test.Integration.Byron.Scenario.API.Transactions as TransactionsByron
import qualified Test.Integration.Scenario.API.ByronTransactions as TransactionsCommon
import qualified Test.Integration.Scenario.API.ByronWallets as WalletsCommon
import qualified Test.Integration.Scenario.API.Network as Network

-- | Define the actual executable name for the bridge CLI
instance KnownCommand Byron where
    commandName = "cardano-wallet-byron"

main :: forall t. (t ~ Byron) => IO ()
main = withUtf8Encoding $ withLogging Nothing Info $ \(_, tr) -> do
    hspec $ do
        describe "API Specifications" $ specWithServer tr $ do
            WalletsCommon.spec
            TransactionsByron.spec
            TransactionsCommon.spec
            Network.spec

specWithServer
    :: Trace IO Text
    -> SpecWith (Context Byron)
    -> Spec
specWithServer tr = aroundAll withContext . after tearDown
  where
    withContext :: (Context Byron -> IO ()) -> IO ()
    withContext action = do
        ctx <- newEmptyMVar
        let setupContext bp wAddr = do
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
                    , _blockchainParameters = bp
                    , _target = Proxy
                    }
        race (takeMVar ctx >>= action) (withServer setupContext) >>=
            either pure (throwIO . ProcessHasExited "integration")

    withServer action =
        withCardanoNode tr $ \addrInfo block0 (bp,vData) ->
        withSystemTempDirectory "cardano-wallet-databases" $ \db -> do
            serveWallet @'Mainnet
                (setupTracers (tracerSeverities (Just Info)) tr)
                (SyncTolerance 10)
                (Just db)
                "127.0.0.1"
                ListenOnRandomPort
                addrInfo
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

-- FIXME: Write fee estimator for Byron nodes.
mkFeeEstimator :: TxDescription -> (Natural, Natural)
mkFeeEstimator = \case
    PaymentDescription _nInps _nOuts _nChgs ->
        (0,0)
    DelegDescription _nInps _nOuts _nCerts ->
        (0,0)
