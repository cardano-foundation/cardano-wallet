{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.LocalCluster where

import Prelude

import Cardano.Address.Style.Shelley
    ( shelleyTestnet
    )
import Cardano.BM.Extra
    ( stdoutTextTracer
    )
import Cardano.Launcher.Node
    ( nodeSocketFile
    )
import Cardano.Mnemonic
    ( SomeMnemonic (..)
    )
import Cardano.Startup
    ( installSignalHandlers
    , setDefaultFilePermissions
    )
import Cardano.Wallet.Faucet
    ( byronIntegrationTestFunds
    , deriveShelleyRewardAccount
    , maryIntegrationTestFunds
    , shelleyIntegrationTestFunds
    )
import Cardano.Wallet.Launch.Cluster
    ( FaucetFunds (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Control.Applicative
    ( (<**>)
    )
import Control.Lens
    ( over
    )
import Control.Monad.Trans.Resource
    ( allocate
    , runResourceT
    )
import Data.Tagged
    ( Tagged (..)
    )
import Main.Utf8
    ( withUtf8
    )
import System.Environment.Extended
    ( isEnvSet
    )
import System.IO.Temp.Extra
    ( SkipCleanup (..)
    , withSystemTempDir
    )
import UnliftIO.Concurrent
    ( threadDelay
    )

import qualified Cardano.Address.Style.Shelley as Shelley
import qualified Cardano.Node.Cli.Launcher as NC
import qualified Cardano.Wallet.Cli.Launcher as WC
import qualified Cardano.Wallet.Faucet.Mnemonics as Mnemonics
import qualified Cardano.Wallet.Launch.Cluster as Cluster
import qualified Options.Applicative as O
import qualified Path
import qualified Path.IO as PathIO

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
main = withUtf8 $ do
    -- Handle SIGTERM properly
    installSignalHandlers (putStrLn "Terminated")

    -- Ensure key files have correct permissions for cardano-cli
    setDefaultFilePermissions

    skipCleanup <- SkipCleanup <$> isEnvSet "NO_CLEANUP"
    let tr = stdoutTextTracer
    withSystemTempDir tr "test-cluster" skipCleanup $ \clusterPath -> do
        clusterEra <- Cluster.clusterEraFromEnv
        cfgNodeLogging <-
            Cluster.logFileConfigFromEnv
                (Just (Cluster.clusterEraToString clusterEra))
        CommandLineOptions { clusterConfigsDir } <- parseCommandLineOptions
        let clusterCfg = Cluster.Config
                { cfgStakePools = Cluster.defaultPoolConfigs
                , cfgLastHardFork = clusterEra
                , cfgNodeLogging
                , cfgClusterDir = Tagged clusterPath
                , cfgClusterConfigs = clusterConfigsDir
                , cfgTestnetMagic = Cluster.TestnetMagic 42
                , cfgShelleyGenesisMods = [ over #sgSlotLength \_ -> 0.2 ]
                }
        Cluster.withCluster stdoutTextTracer clusterCfg faucetFunds $ \node -> do
            clusterDir <- Path.parseAbsDir clusterPath
            let walletDir = clusterDir Path.</> [Path.reldir|wallet|]
            PathIO.createDirIfMissing False walletDir
            nodeSocket <-
                Path.parseAbsFile . nodeSocketFile
                    $ Cluster.runningNodeSocketPath node

            runResourceT do
                (_releaseKey, (_walletInstance, _walletApi)) <- allocate
                    ( WC.start WC.WalletProcessConfig
                        { WC.walletDir =
                            walletDir
                        , WC.walletNodeApi =
                            NC.NodeApi nodeSocket
                        , WC.walletDatabase =
                            clusterDir Path.</> [Path.reldir|db|]
                        , WC.walletListenHost =
                            Nothing
                        , WC.walletListenPort =
                            Nothing
                        , WC.walletByronGenesisForTestnet = Just $
                            clusterDir Path.</> [Path.relfile|genesis.byron.json|]
                        }
                    )
                    (WC.stop . fst)
                threadDelay maxBound -- wait for Ctrl+C
  where
    networkTag = shelleyTestnet
    faucetFunds =
        Cluster.FaucetFunds
            { pureAdaFunds =
                shelleyIntegrationTestFunds networkTag
                    <> byronIntegrationTestFunds networkTag
            , maFunds =
                maryIntegrationTestFunds (Coin 10_000_000)
            , mirFunds =
                [ ( Cluster.KeyCredential $ Shelley.getKey xPub
                  , Coin (fromIntegral Cluster.oneMillionAda)
                  )
                | m <- Mnemonics.mir
                , let (xPub, _xPrv) = deriveShelleyRewardAccount (SomeMnemonic m)
                ]
            }

newtype CommandLineOptions = CommandLineOptions
    { clusterConfigsDir :: Tagged "cluster-configs" FilePath }
    deriving stock (Show)

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = O.execParser $
    O.info
        (fmap CommandLineOptions clusterConfigsDirParser <**> O.helper)
        (O.progDesc "Local Cluster for testing")

clusterConfigsDirParser :: O.Parser (Tagged "cluster-configs" FilePath)
clusterConfigsDirParser =
    Tagged <$> O.strOption
        ( O.long "cluster-configs"
        <> O.metavar "LOCAL_CLUSTER_CONFIGS"
        <> O.help "Path to the local cluster configuration directory"
        )
