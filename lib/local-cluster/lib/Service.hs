{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Service where

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
    ( installSignalHandlers, setDefaultFilePermissions, withUtf8Encoding )
import Cardano.Wallet.Api.Http.Shelley.Server
    ( walletListenFromEnv )
import Cardano.Wallet.Faucet
    ( byronIntegrationTestFunds
    , genRewardAccounts
    , hwWalletFunds
    , maryIntegrationTestAssets
    , mirMnemonics
    , shelleyIntegrationTestFunds
    )
import Cardano.Wallet.Launch
    ( envFromText, withSystemTempDir )
import Cardano.Wallet.Launch.Cluster
    ( ClusterLog (..)
    , Credential (..)
    , FaucetFunds (..)
    , RunningNode (..)
    , localClusterConfigFromEnv
    , oneMillionAda
    , testMinSeverityFromEnv
    , tokenMetadataServerFromEnv
    , walletMinSeverityFromEnv
    , withCluster
    )
import Cardano.Wallet.Logging
    ( stdoutTextTracer, trMessageText )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkId (..), SNetworkId (..) )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Shelley
    ( serveWallet, setupTracers, tracerSeverities )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( decodeAddress )
import Control.Arrow
    ( first )
import Control.Monad
    ( void, when )
import Control.Tracer
    ( contramap, traceWith )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Ouroboros.Network.Client.Wallet
    ( tunedForMainnetPipeliningStrategy )
import System.Directory
    ( createDirectory )
import System.FilePath
    ( (</>) )

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
--   `lib/wallet/src/Test/Integration/Faucet.hs`.
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
-- - (Byron) Has only 5 UTxOs of 1,2,3,4,5 Lovelace
--
--     [ "suffer", "decorate", "head", "opera"
--     , "yellow", "debate", "visa", "fire"
--     , "salute", "hybrid", "stone", "smart"
--     ]
--
-- - (Byron) Has 200 UTxO, 100 are worth 1 Lovelace, 100 are worth 100_000 Ada.
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
-- - CARDANO_WALLET_PORT  (default: random)
--     choose a port for the API to listen on
--
-- - CARDANO_NODE_TRACING_MIN_SEVERITY  (default: Info)
--     increase or decrease the logging severity of the nodes.
--
-- - CARDANO_WALLET_TRACING_MIN_SEVERITY  (default: Info)
--     increase or decrease the logging severity of cardano-wallet.
--
-- - TESTS_TRACING_MIN_SEVERITY  (default: Notice)
--     increase or decrease the logging severity of the test cluster framework.
--
-- - LOCAL_CLUSTER_ERA  (default: Mary)
--     By default, the cluster will start in the latest era by enabling
--     "virtual hard forks" in the node config files.
--     The final era can be changed with this variable.
--
-- - TOKEN_METADATA_SERVER  (default: none)
--     Use this URL for the token metadata server.
--
-- - NO_CLEANUP  (default: temp files are cleaned up)
--     If set, the temporary directory used as a state directory for
--     nodes and wallet data won't be cleaned up.
main :: IO ()
main = withLocalClusterSetup $ \dir clusterLogs walletLogs ->
    withLoggingNamed "cluster" clusterLogs $ \(_, (_, trCluster)) -> do
        let tr' = contramap MsgCluster $ trMessageText trCluster
        clusterCfg <- localClusterConfigFromEnv
        withCluster tr' dir clusterCfg faucetFunds
            (whenReady dir (trMessageText trCluster) walletLogs)
  where
    unsafeDecodeAddr = either (error . show) id . decodeAddress SMainnet

    faucetFunds = FaucetFunds
        { pureAdaFunds =
            shelleyIntegrationTestFunds
             <> byronIntegrationTestFunds
             <> map (first unsafeDecodeAddr) hwWalletFunds
        , maFunds =
            maryIntegrationTestAssets (Coin 10_000_000)
        , mirFunds =
            first KeyCredential
            . (,Coin $ fromIntegral oneMillionAda)
            <$> concatMap genRewardAccounts mirMnemonics
        }

    whenReady dir trCluster logs (RunningNode socketPath block0 (gp, vData) _) =
        withLoggingNamed "cardano-wallet" logs $ \(sb, (cfg, tr)) -> do
            ekgEnabled >>= flip when (EKG.plugin cfg tr sb >>= loadPlugin sb)

            let tracers = setupTracers (tracerSeverities (Just Debug)) tr
            let db = dir </> "wallets"
            createDirectory db
            listen <- walletListenFromEnv envFromText
            tokenMetadataServer <- tokenMetadataServerFromEnv

            prometheusUrl <- (maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "Prometheus") p)
                )
                <$> getPrometheusURL
            ekgUrl <- (maybe "none"
                    (\(h, p) -> T.pack h <> ":" <> toText @(Port "EKG") p)
                )
                <$> getEKGURL

            void $ serveWallet
                (NodeSource socketPath vData (SyncTolerance 10))
                gp
                tunedForMainnetPipeliningStrategy
                NMainnet
                []
                tracers
                (Just db)
                Nothing
                "127.0.0.1"
                listen
                Nothing
                Nothing
                tokenMetadataServer
                block0
                (\u -> traceWith trCluster $ MsgBaseUrl (T.pack . show $ u)
                    ekgUrl prometheusUrl)

-- Do all the program setup required for running the local cluster, create a
-- temporary directory, log output configurations, and pass these to the given
-- main action.
withLocalClusterSetup
    :: (FilePath -> [LogOutput] -> [LogOutput] -> IO a)
    -> IO a
withLocalClusterSetup action = do
    -- Handle SIGTERM properly
    installSignalHandlers (putStrLn "Terminated")

    -- Ensure key files have correct permissions for cardano-cli
    setDefaultFilePermissions

    -- Set UTF-8, regardless of user locale
    withUtf8Encoding $
        -- This temporary directory will contain logs, and all other data
        -- produced by the local test cluster.
        withSystemTempDir stdoutTextTracer "test-cluster" $ \dir -> do
            let logOutputs name minSev =
                    [ LogToFile (dir </> name) (min minSev Info)
                    , LogToStdStreams minSev ]

            clusterLogs <- logOutputs "cluster.log" <$> testMinSeverityFromEnv
            walletLogs <- logOutputs "wallet.log" <$> walletMinSeverityFromEnv

            action dir clusterLogs walletLogs

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
