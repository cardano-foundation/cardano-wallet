{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.BM.Plugin
    ( loadPlugin )
import Cardano.CLI
    ( LogOutput (..)
    , Port
    , ekgEnabled
    , getEKGURL
    , getPrometheusURL
    , withLoggingNamed
    )
import Cardano.Startup
    ( setDefaultFilePermissions, withUtf8Encoding )
import Cardano.Wallet.Api.Types
    ( EncodeAddress (..) )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , serveWallet
    , setupTracers
    , tracerSeverities
    )
import Cardano.Wallet.Shelley.Launch
    ( ClusterLog (..)
    , RunningNode (..)
    , moveInstantaneousRewardsTo
    , nodeMinSeverityFromEnv
    , oneMillionAda
    , poolConfigsFromEnv
    , sendFaucetFundsTo
    , testMinSeverityFromEnv
    , walletListenFromEnv
    , walletMinSeverityFromEnv
    , withCluster
    , withSystemTempDir
    )
import Control.Arrow
    ( first )
import Control.Monad
    ( void, when )
import Control.Tracer
    ( contramap, traceWith )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import System.Directory
    ( createDirectory )
import System.FilePath
    ( (</>) )
import System.IO
    ( BufferMode (..), hSetBuffering, stdout )
import Test.Integration.Faucet
    ( genRewardAccounts, mirMnemonics, shelleyIntegrationTestFunds )

import qualified Cardano.BM.Backend.EKGView as EKG
import qualified Data.Text as T

-- |
-- # OVERVIEW
--
-- This starts a cluster of Cardano nodes with:
--
-- - 1 relay node
-- - 1 BFT leader
-- - 4 stake pools
--
-- The BFT leader and pools are all fully connected. The network starts in the
-- Byron Era and transitions into the Shelley era. Once in the Shelley era and
-- once pools are registered and up-and-running, an instance of cardano-wallet
-- is started.
--
-- Pools have slightly different settings summarized in the table below:
--
-- | #       | Pledge | Retirement      | Metadata       |
-- | ---     | ---    | ---             | ---            |
-- | Pool #0 | 2M Ada | Never           | Genesis Pool A |
-- | Pool #1 | 1M Ada | Epoch 3         | Genesis Pool B |
-- | Pool #2 | 1M Ada | Epoch 100_000   | Genesis Pool C |
-- | Pool #3 | 1M Ada | Epoch 1_000_000 | Genesis Pool D |
--
-- Pools' metadata are hosted on static local servers started alongside pools.
--
-- # PRE-REGISTERED DATA
--
-- The cluster also comes with a large number of pre-existing faucet wallets and
-- special wallets identified by recovery phrases. Pre-registered wallets can be
-- seen in
--
--   `lib/core-integration/src/Test/Integration/Faucet.hs`.
--
-- All wallets (Byron, Icarus, Shelley) all have 10 UTxOs worth 100_000 Ada
-- each (so 1M Ada in total). Additionally, the file also contains a set of
-- wallets with pre-existing rewards (1M Ada) injected via MIR certificates.
-- These wallets have the same UTxOs as other faucet wallets.
--
-- Some additional wallets of interest:
--
-- - (Shelley) Has a pre-registered stake key but no delegation.
--
--     [ "over", "decorate", "flock", "badge", "beauty"
--     , "stamp", "chest", "owner", "excess", "omit"
--     , "bid", "raccoon", "spin", "reduce", "rival"
--     ]
--
-- - (Shelley) Contains only small coins (but greater than the minUTxOValue)
--
--     [ "either" , "flip" , "maple" , "shift" , "dismiss"
--     , "bridge" , "sweet" , "reveal" , "green" , "tornado"
--     , "need" , "patient" , "wall" , "stamp" , "pass"
--     ]
--
-- - (Shelley) Contains 100 UTxO of 100_000 Ada, and 100 UTxO of 1 Ada
--
--     [ "radar", "scare", "sense", "winner", "little"
--     , "jeans", "blue", "spell", "mystery", "sketch"
--     , "omit", "time", "tiger", "leave", "load"
--     ]
--
-- - (Icarus) Has only addresses that start at index 500
--
--     [ "erosion", "ahead", "vibrant", "air", "day"
--     , "timber", "thunder", "general", "dice", "into"
--     , "chest", "enrich", "social", "neck", "shine"
--     ]
--
-- - (Byron) Has only 5 UTxOs of 1,2,3,4,5 Lovelace
--
--     [ "suffer", "decorate", "head", "opera"
--     , "yellow", "debate", "visa", "fire"
--     , "salute", "hybrid", "stone", "smart"
--     ]
--
-- - (Byron) Has 200 UTxO, 100 are worth 1 Lovelace, 100 are woth 100_000 Ada.
--
--     [ "collect", "fold", "file", "clown"
--     , "injury", "sun", "brass", "diet"
--     , "exist", "spike", "behave", "clip"
--     ]
--
-- - (Ledger) Created via the Ledger method for master key generation
--
--     [ "struggle", "section", "scissors", "siren"
--     , "garbage", "yellow", "maximum", "finger"
--     , "duty", "require", "mule", "earn"
--     ]
--
-- - (Ledger) Created via the Ledger method for master key generation
--
--     [ "vague" , "wrist" , "poet" , "crazy" , "danger" , "dinner"
--     , "grace" , "home" , "naive" , "unfold" , "april" , "exile"
--     , "relief" , "rifle" , "ranch" , "tone" , "betray" , "wrong"
--     ]
--
-- # CONFIGURATION
--
-- There are several environment variables that can be set to make debugging
-- easier if needed:
--
-- - CARDANO_WALLET_PORT: choose a port for the API to listen on (default: random)
--
-- - CARDANO_NODE_TRACING_MIN_SEVERITY: increase or decrease the logging
--                                      severity of the nodes. (default: Info)
--
-- - CARDANO_WALLET_TRACING_MIN_SEVERITY: increase or decrease the logging
--                                        severity of cardano-wallet. (default: Info)
--
-- - TESTS_TRACING_MIN_SEVERITY: increase or decrease the logging severity of
--                               the test cluster framework. (default: Notice)
--
-- - NO_POOLS: If set, the cluster will only start a BFT leader and a relay, no
--             stake pools. This can be used for running test scenarios which do
--             not require delegation-specific features without paying the
--             startup cost of creating and funding pools.
--
-- - NO_CLEANUP: If set, the temporary directory used as a state directory for
--               nodes and wallet data won't be cleaned up.
main :: IO ()
main = withUtf8Encoding $ do
    hSetBuffering stdout LineBuffering
    setDefaultFilePermissions

    nodeMinSeverity    <- nodeMinSeverityFromEnv
    walletMinSeverity  <- walletMinSeverityFromEnv
    clusterMinSeverity <- testMinSeverityFromEnv

    let clusterLogs =
            [ LogToStdout clusterMinSeverity
            ]
    let walletLogs dir =
            [ LogToStdout walletMinSeverity
            , LogToFile (dir </> "wallet.log")
                (min walletMinSeverity Info)
            ]

    poolConfigs <- poolConfigsFromEnv
    withLoggingNamed "test-cluster" clusterLogs $ \(_, (_, trCluster)) ->
        withSystemTempDir (trMessageText trCluster) "test-cluster" $ \dir ->
        withCluster
            (contramap MsgCluster $ trMessageText trCluster)
            nodeMinSeverity
            poolConfigs
            dir
            Nothing
            whenByron
            (whenShelley dir (trMessageText trCluster))
            (whenReady dir (trMessageText trCluster) (walletLogs dir))
  where
    whenByron _ = pure ()

    whenShelley dir trCluster _ = do
        traceWith trCluster MsgSettingUpFaucet
        let trCluster' = contramap MsgCluster trCluster
        let encodeAddr = T.unpack . encodeAddress @'Mainnet
        let addresses = map (first encodeAddr) shelleyIntegrationTestFunds
        let accts = concatMap genRewardAccounts mirMnemonics
        let rewards = (,Coin $ fromIntegral oneMillionAda) <$> accts
        sendFaucetFundsTo trCluster' dir addresses
        moveInstantaneousRewardsTo trCluster' dir rewards

    whenReady dir trCluster logs (RunningNode socketPath block0 (gp, vData)) =
        withLoggingNamed "cardano-wallet" logs $ \(sb, (cfg, tr)) -> do
            ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)

            let tracers = setupTracers (tracerSeverities (Just Info)) tr
            let db = dir </> "wallets"
            createDirectory db
            listen <- walletListenFromEnv

            prometheusUrl <- (maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "Prometheus") p)
                )
                <$> getPrometheusURL
            ekgUrl <- (maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "EKG") p)
                )
                <$> getEKGURL

            void $ serveWallet
                (SomeNetworkDiscriminant $ Proxy @'Mainnet)
                tracers
                (SyncTolerance 10)
                (Just db)
                Nothing
                "127.0.0.1"
                listen
                Nothing
                Nothing
                socketPath
                block0
                (gp, vData)
                (\u -> traceWith trCluster $ MsgBaseUrl (T.pack . show $ u)
                    ekgUrl prometheusUrl)

-- Logging

data TestsLog
    = MsgBaseUrl Text Text Text -- wallet url, ekg url, prometheus url
    | MsgSettingUpFaucet
    | MsgCluster ClusterLog
    deriving (Show)

instance ToText TestsLog where
    toText = \case
        MsgBaseUrl walletUrl ekgUrl prometheusUrl -> mconcat
            [ "Wallet url: " , walletUrl
            , ", EKG url: " , ekgUrl
            , ", Prometheus url:", prometheusUrl
            ]
        MsgSettingUpFaucet -> "Setting up faucet..."
        MsgCluster msg -> toText msg

instance HasPrivacyAnnotation TestsLog
instance HasSeverityAnnotation TestsLog where
    getSeverityAnnotation = \case
        MsgSettingUpFaucet -> Notice
        MsgBaseUrl {} -> Notice
        MsgCluster msg -> getSeverityAnnotation msg
