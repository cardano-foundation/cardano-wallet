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
    ( SomeNetworkDiscriminant (..)
    , serveWallet
    , setupTracers
    , tracerSeverities
    )
import Cardano.Wallet.Byron.Compatibility
    ( Byron )
import Cardano.Wallet.Byron.Config
    ( withCardanoNode )
import Cardano.Wallet.Byron.Faucet
    ( initFaucet )
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
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , BlockchainParameters (..)
    , FeePolicy (..)
    , Hash (..)
    , SyncTolerance (..)
    , TxIn (..)
    , TxOut (..)
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
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Test.Integration.Byron.Scenario.API.Transactions as TransactionsByron
import qualified Test.Integration.Scenario.API.ByronTransactions as TransactionsCommon
import qualified Test.Integration.Scenario.API.ByronWallets as WalletsCommon
import qualified Test.Integration.Scenario.API.Network as Network

-- | Define the actual executable name for the bridge CLI
instance KnownCommand Byron where
    commandName = "cardano-wallet-byron"

main :: forall t n. (t ~ Byron, n ~ 'Mainnet) => IO ()
main = withUtf8Encoding $ withLogging Nothing Info $ \(_, tr) -> do
    hspec $ do
        describe "API Specifications" $ specWithServer tr $ do
            WalletsCommon.spec
            (TransactionsByron.spec @n)
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
                    , _feeEstimator = mkFeeEstimator (getFeePolicy bp)
                    , _blockchainParameters = bp
                    , _target = Proxy
                    }
        race (takeMVar ctx >>= action) (withServer setupContext) >>=
            either pure (throwIO . ProcessHasExited "integration")

    withServer action =
        withCardanoNode tr $ \addrInfo block0 (bp,vData) ->
        withSystemTempDirectory "cardano-wallet-databases" $ \db -> do
            serveWallet
                (SomeNetworkDiscriminant $ Proxy @'Mainnet)
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
                    [ minSizeOf @Address @'Mainnet @IcarusKey
                    , minSizeOf @Address @'Mainnet @ByronKey
                    ]

            outsMax = replicate (nOuts + nChgs) (TxOut addr_ coin_)
              where
                coin_ = maxBound
                addr_ = Address $ flip BS.replicate 0 $ maximum
                    [ maxSizeOf @Address @'Mainnet @IcarusKey
                    , maxSizeOf @Address @'Mainnet @ByronKey
                    ]

        in
            ( round a + round b * fromIntegral (sizeOfSignedTx inps outsMin)
            , round a + round b * fromIntegral (sizeOfSignedTx inps outsMax)
            )

    DelegDescription{} ->
        error "mkFeeEstimator: can't estimate fee for certificate in Byron!"
  where
    LinearFee (Quantity a) (Quantity b) _ = policy
