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

import Cardano.Address.Derivation
    ( XPrv, xprvFromBytes, xpubFromBytes )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Trace
    ( appendName )
import Cardano.CLI
    ( Port (..), parseLoggingSeverity, withLogging )
import Cardano.Launcher
    ( ProcessHasExited (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Startup
    ( withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Api.Types
    ( ApiByronWallet, ApiWallet, WalletStyle (..) )
import Cardano.Wallet.Logging
    ( BracketLog (..), bracketTracer, trMessageText )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Network.Ports
    ( unsafePortNumber )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), NetworkDiscriminant (..), paymentAddress, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , FeePolicy (..)
    , Hash (..)
    , NetworkParameters (..)
    , ProtocolParameters (..)
    , SyncTolerance (..)
    , TxIn (..)
    , TxOut (..)
    , TxParameters (..)
    )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , Tracers
    , serveWallet
    , setupTracers
    , tracerSeverities
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley, initialFundsPseudoTxIn, toStakeKeyRegCert )
import Cardano.Wallet.Shelley.Faucet
    ( initFaucet )
import Cardano.Wallet.Shelley.Launch
    ( ClusterLog, withCluster, withSystemTempDir, withTempDir )
import Cardano.Wallet.Shelley.Network
    ( withNetworkLayer )
import Cardano.Wallet.Shelley.Transaction
    ( TxPayload (..), mkTx, _minimumFee )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeRunExceptT )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( forM_, void )
import Control.Tracer
    ( Tracer (..), contramap, nullTracer, traceWith )
import Data.Maybe
    ( fromJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Numeric.Natural
    ( Natural )
import System.Environment
    ( lookupEnv )
import System.Exit
    ( die )
import System.IO
    ( BufferMode (..), hSetBuffering, stdout )
import System.Random
    ( mkStdGen, randoms )
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

import qualified Cardano.Wallet.Api.Link as Link
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Text as T
-- TODO: enable when byron transactions/addresses supported in the cardano-node
-- import qualified Test.Integration.Scenario.API.Byron.Addresses as ByronAddresses
-- import qualified Test.Integration.Scenario.API.Byron.Migrations as ByronMigrations
-- import qualified Test.Integration.Byron.Scenario.API.Transactions as ByronTransactions
-- import qualified Test.Integration.Scenario.API.Byron.Transactions as ByronTransactionsCommon
-- import qualified Test.Integration.Scenario.API.Byron.HWWallets as ByronHWWallets
import qualified Test.Integration.Scenario.API.Byron.Wallets as ByronWallets
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.API.Shelley.Addresses as Addresses
import qualified Test.Integration.Scenario.API.Shelley.HWWallets as HWWallets
import qualified Test.Integration.Scenario.API.Shelley.Migrations as Migrations
import qualified Test.Integration.Scenario.API.Shelley.Network as Network_
import qualified Test.Integration.Scenario.API.Shelley.StakePools as StakePools
import qualified Test.Integration.Scenario.API.Shelley.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Shelley.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Shelley.HWWallets as HWWalletsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Shelley.Wallets as WalletsCLI

-- | Define the actual executable name for the bridge CLI
instance KnownCommand Shelley where
    commandName = "cardano-wallet-shelley"

main :: forall t n . (t ~ Shelley, n ~ 'Mainnet) => IO ()
main = withUtf8Encoding $ withTracers $ \tracers -> do
    hSetBuffering stdout LineBuffering
    hspec $ do
        describe "No backend required" $ do
            describe "Miscellaneous CLI tests" $ parallel (MiscellaneousCLI.spec @t)
        specWithServer tracers $ do
            describe "API Specifications" $ do
                Addresses.spec @n
                ByronWallets.spec @n
                Migrations.spec @n
                Transactions.spec @n
                Wallets.spec @n
                HWWallets.spec @n
                Network.spec
                Network_.spec
                StakePools.spec @n
            describe "CLI Specifications" $ do
                AddressesCLI.spec @n
                TransactionsCLI.spec @n
                WalletsCLI.spec @n
                HWWalletsCLI.spec @n
                PortCLI.spec @t
                NetworkCLI.spec @t

specWithServer
    :: (Tracer IO TestsLog, Tracers IO)
    -> SpecWith (Context Shelley)
    -> Spec
specWithServer (tr, tracers) = aroundAll withContext . after tearDown
  where
    withContext :: (Context Shelley -> IO ()) -> IO ()
    withContext action = bracketTracer' tr "withContext" $ do
        ctx <- newEmptyMVar
        let setupContext np wAddr = bracketTracer' tr "setupContext" $ do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                traceWith tr $ MsgBaseUrl baseUrl
                let threeMinutes = 180*1000*1000 -- 180s in microseconds
                manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                    { managerResponseTimeout =
                        responseTimeoutMicro threeMinutes
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
                    , _target = Proxy
                    }

        let action' = bracketTracer' tr "spec" . action
        race (takeMVar ctx >>= action') (withServer setupContext) >>=
            either pure (throwIO . ProcessHasExited "integration")

    withServer onStart = bracketTracer' tr "withServer" $ do
        minSev <- nodeMinSeverityFromEnv
        let tr' = contramap MsgCluster tr
        withSystemTempDir tr' "test" $ \dir ->
            withCluster tr' minSev 3 dir $ \socketPath block0 (gp, vData) ->
                withTempDir tr' dir "wallets" $ \db -> do
                    withNetworkLayer nullTracer gp socketPath vData $ \nl -> do
                        preregisterStakingKeysForTests nl
                        serveWallet @(IO Shelley)
                            (SomeNetworkDiscriminant $ Proxy @'Mainnet)
                            tracers
                            (SyncTolerance 10)
                            (Just db)
                            "127.0.0.1"
                            ListenOnRandomPort
                            Nothing
                            socketPath
                            block0
                            (gp, vData)
                            (onStart gp)

    -- | teardown after each test (currently only deleting all wallets)
    tearDown :: Context t -> IO ()
    tearDown ctx = bracketTracer' tr "tearDown" $ do
        (_, byronWallets) <- unsafeRequest @[ApiByronWallet] ctx
            (Link.listWallets @'Byron) Empty
        forM_ byronWallets $ \w -> void $ request @Aeson.Value ctx
            (Link.deleteWallet @'Byron w) Default Empty
        (_, wallets) <- unsafeRequest @[ApiWallet] ctx
            (Link.listWallets @'Shelley) Empty
        forM_ wallets $ \w -> void $ request @Aeson.Value ctx
            (Link.deleteWallet @'Shelley w) Default Empty

mkFeeEstimator :: FeePolicy -> TxDescription -> (Natural, Natural)
mkFeeEstimator policy = \case
    PaymentDescription i o c ->
        let
            fee = computeFee (dummySelection i o c) Nothing
        in
            ( fee, fee )

    DelegDescription action ->
        let
            feeMin = computeFee (dummySelection 0 0 0) (Just action)
            feeMax = computeFee (dummySelection 1 0 1) (Just action)
        in
            ( feeMin, feeMax )
  where
    genTxId i = Hash $ BS.pack $ take 32 $ randoms $ mkStdGen (fromIntegral i)

    dummySelection nInps nOuts nChgs =
        let
            inps = take nInps
                [ ( TxIn (genTxId ix) ix
                  , TxOut (Address mempty) minBound
                  )
                | ix <- [0..]
                ]

            outs =
                replicate (nOuts + nChgs) (Coin minBound)
        in
            mempty { inputs = inps, change = outs }

    computeFee selection action =
        fromIntegral $ getFee $ _minimumFee policy action selection

{-------------------------------------------------------------------------------
                                    Logging
-------------------------------------------------------------------------------}

data TestsLog
    = MsgBracket Text BracketLog
    | MsgBaseUrl Text
    | MsgCluster ClusterLog
    deriving (Show)

instance ToText TestsLog where
    toText = \case
        MsgBracket name b -> name <> ": " <> toText b
        MsgBaseUrl txt -> txt
        MsgCluster msg -> toText msg

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
    getSeverityAnnotation = \case
        MsgBracket _ _ -> Debug
        MsgBaseUrl _ -> Notice
        MsgCluster msg -> getSeverityAnnotation msg

withTracers
    :: ((Tracer IO TestsLog, Tracers IO) -> IO a)
    -> IO a
withTracers action = do
    minSeverity <- walletMinSeverityFromEnv
    withLogging Nothing minSeverity $ \(_, tr) -> do
        let trTests = appendName "integration" tr
        let tracers = setupTracers (tracerSeverities (Just Info)) tr
        action (trMessageText trTests, tracers)

bracketTracer' :: Tracer IO TestsLog -> Text -> IO a -> IO a
bracketTracer' tr name = bracketTracer (contramap (MsgBracket name) tr)

-- Allow configuring @cardano-node@ log level with the
-- @CARDANO_NODE_TRACING_MIN_SEVERITY@ environment variable.
nodeMinSeverityFromEnv :: IO Severity
nodeMinSeverityFromEnv =
    minSeverityFromEnv Error "CARDANO_NODE_TRACING_MIN_SEVERITY"

-- Allow configuring integration tests and wallet log level with
-- @CARDANO_WALLET_TRACING_MIN_SEVERITY@ environment variable.
walletMinSeverityFromEnv :: IO Severity
walletMinSeverityFromEnv =
    minSeverityFromEnv Info "CARDANO_WALLET_TRACING_MIN_SEVERITY"

minSeverityFromEnv :: Severity -> String -> IO Severity
minSeverityFromEnv def var = lookupEnv var >>= \case
    Nothing -> pure def
    Just "" -> pure def
    Just arg -> either die pure (parseLoggingSeverity arg)

-- | Pre-register a staking key for the STAKE_POOLS_JOIN_05 test.
preregisterStakingKeysForTests :: NetworkLayer IO t b -> IO ()
preregisterStakingKeysForTests nl = do
    let payload = TxPayload
            [toStakeKeyRegCert rewardPub]
            (const mempty)
    let keystore = const (Just (xprv, mempty))
    let txIn = initialFundsPseudoTxIn addr

    let dummyOut = TxOut addr (Coin 10000000000)

    let cs = CoinSelection
            { inputs = [(txIn, dummyOut)]
            , withdrawal = 0
            , reclaim = 0
            , outputs = []
            , change = []
            , deposit = 100000 -- keyDeposit
            }

    let Right (_, tx) = mkTx
            @ShelleyKey
            payload
            (SlotNo 5000)
            (ShelleyKey rewardPrv, mempty)
            keystore
            cs
    unsafeRunExceptT $ postTx nl tx
  where

    addr :: Address
    addr = paymentAddress @'Mainnet (publicKey xprv)

    -- The wallet's reward key
    Just rewardPub = xpubFromBytes $ unsafeFromHex
            "949fc9e6b7e1e12e933ac35de5a565c9264b0ac5b631b4f5a21548bc6d65616f30\
            \42af27ce48e0fce0f88696b6ed3476f8c3412cce2f984931fb7658dee1872e"

    Just rewardPrv = xprvFromBytes $ unsafeFromHex
        "784cda4d590b72c795792ec5d05b2a4216d153e36dad0b5376b6ea6308008d4e3cb008\
        \8852dce3b89577c7a4fb262ebf6a71f44f2c8ca45794ccfaa76bd95bcb3042af27ce48\
        \e0fce0f88696b6ed3476f8c3412cce2f984931fb7658dee1872e"

    xprv :: ShelleyKey 'AddressK XPrv
    xprv = ShelleyKey $ fromJust $  xprvFromBytes $ unsafeFromHex
            "90b23d7d7d2d77e943bf81b89af4f4b263049b4c2c7f52b9ae\
            \2093bff8ff8c4e845d4583dcbf226613bc1b823811fe682483\
            \c71bf0de92dc7f8f05bacdaba79994f3d3f3cc1793d8afe804\
            \53ab06a875d21ed1adcfad913617796c8662d375fc"
