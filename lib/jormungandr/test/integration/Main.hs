{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
    ( Trace )
import Cardano.CLI
    ( Port (..), withLogging )
import Cardano.Faucet
    ( initFaucet )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Jormungandr
    ( serveWallet )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Launch
    ( withConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( throwIO )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import GHC.IO.Encoding
    ( mkTextEncoding, setLocaleEncoding )
import Network.HTTP.Client
    ( defaultManagerSettings, newManager )
import Network.Socket
    ( SockAddr (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, SpecWith, after, describe, hspec, parallel )
import Test.Hspec.Extra
    ( aroundAll )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand (..), TxDescription (..), tearDown )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Pool.MetricsSpec as MetricsSpec
import qualified Cardano.Wallet.Jormungandr.NetworkSpec as NetworkLayer
import qualified Data.Text as T
import qualified Test.Integration.Jormungandr.Scenario.API.StakePools as StakePoolsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.API.Transactions as TransactionsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Launcher as LauncherCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Server as ServerCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.StakePools as StakePoolsCliJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Transactions as TransactionsCliJormungandr
import qualified Test.Integration.Scenario.API.Addresses as Addresses
import qualified Test.Integration.Scenario.API.ByronTransactions as ByronTransactions
import qualified Test.Integration.Scenario.API.ByronWallets as ByronWallets
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.API.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Mnemonics as MnemonicsCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Wallets as WalletsCLI

-- | Define the actual executable name for the bridge CLI
instance KnownCommand Jormungandr where
    commandName = "cardano-wallet-jormungandr"

main :: forall t. (t ~ Jormungandr) => IO ()
main = withLogging Nothing Info $ \logging -> do
    setUtf8LenientCodecs
    hspec $ do
        describe "No backend required" $ do
            describe "Cardano.Wallet.NetworkSpec" $ parallel NetworkLayer.spec
            describe "Mnemonics CLI tests" $ parallel (MnemonicsCLI.spec @t)
            describe "Miscellaneous CLI tests" $ parallel (MiscellaneousCLI.spec @t)
            describe "Launcher CLI tests" $ parallel (LauncherCLI.spec @t)
            describe "Stake Pool Metrics" MetricsSpec.spec

        describe "API Specifications" $ specWithServer logging $ do
            Addresses.spec
            StakePoolsApiJormungandr.spec
            Transactions.spec
            TransactionsApiJormungandr.spec @t
            TransactionsCliJormungandr.spec @t
            Wallets.spec
            ByronWallets.spec
            ByronTransactions.spec
            Network.spec

        describe "CLI Specifications" $ specWithServer logging $ do
            AddressesCLI.spec @t
            ServerCLI.spec @t
            StakePoolsCliJormungandr.spec @t
            TransactionsCLI.spec @t
            WalletsCLI.spec @t
            PortCLI.spec @t
            NetworkCLI.spec @t

specWithServer
    :: (CM.Configuration, Trace IO Text)
    -> SpecWith (Context Jormungandr)
    -> Spec
specWithServer logCfg = aroundAll withContext . after tearDown
  where
    withContext :: (Context Jormungandr -> IO ()) -> IO ()
    withContext action = do
        ctx <- newEmptyMVar
        let setupContext wAddr nPort bp = do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                manager <- (baseUrl,) <$> newManager defaultManagerSettings
                faucet <- initFaucet
                putMVar ctx $ Context
                    { _cleanup = pure ()
                    , _manager = manager
                    , _nodePort = nPort
                    , _walletPort = sockAddrPort wAddr
                    , _faucet = faucet
                    , _feeEstimator = mkFeeEstimator (getFeePolicy bp)
                    , _target = Proxy
                    }
        race (takeMVar ctx >>= action) (withServer setupContext) >>=
            either pure (throwIO . ProcessHasExited "integration")

    withServer setup = withConfig $ \jmCfg ->
        serveWallet @'Testnet logCfg Nothing "127.0.0.1"
            ListenOnRandomPort (Launch jmCfg) setup

-- | Set a utf8 text encoding that doesn't crash when non-utf8 bytes are
-- encountered.
setUtf8LenientCodecs :: IO ()
setUtf8LenientCodecs = mkTextEncoding "UTF-8//IGNORE" >>= setLocaleEncoding

sockAddrPort :: SockAddr -> Port a
sockAddrPort addr = Port . fromIntegral $ case addr of
    SockAddrInet p _ -> p
    SockAddrInet6 p _ _ _ -> p
    _ -> 0

mkFeeEstimator :: FeePolicy -> TxDescription -> (Natural, Natural)
mkFeeEstimator policy (TxDescription nInps nOuts) =
    let
        LinearFee (Quantity a) (Quantity b) (Quantity _c) = policy
        nChanges = nOuts
        -- NOTE¹
        -- We safely round BEFORE the multiplication because we know that
        -- Jormungandr' fee are necessarily naturals constants. We carry doubles
        -- here because of the legacy with Byron. In the end, it matters not
        -- because in the spectrum of numbers we're going to deal with, naturals
        -- can be represented without any rounding issue using 'Double' (or,
        -- transactions have suddenly become overly expensive o_O)
        fee = fromIntegral $ (round a) + (nInps + nOuts + nChanges) * (round b)
    in
        -- NOTE²
        -- We use a range (min, max) and call it an "estimator" because for the
        -- bridge (and probably cardano-node on Shelley), it's not possible to
        -- compute the fee precisely by only knowing the number of inputs and
        -- ouputs since the exact fee cost depends on the values of the
        -- outputs and the values of the input indexes.
        (fee, fee)
