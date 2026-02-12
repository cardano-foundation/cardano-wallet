{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Cardano.BM.ToTextTracer
    ( ToTextTracer (ToTextTracer)
    , withToTextTracer
    )
import Cardano.BM.Tracing
    ( HasSeverityAnnotation (..)
    , Severity (..)
    )
import Cardano.Launcher
    ( ProcessHandles (..)
    )
import Cardano.Launcher.Node
    ( MaybeK (..)
    )
import Cardano.Launcher.Wallet
    ( CardanoWalletConn (CardanoWalletConn)
    )
import Cardano.Startup
    ( installSignalHandlers
    )
import Cardano.Wallet.Benchmark.Memory.Pmap
    ( Line
    , Pmap (..)
    , command
    , memory
    , pmap
    )
import Cardano.Wallet.Benchmarks.Collect
    ( Benchmark (..)
    , Result (..)
    , Unit (..)
    , mkSemantic
    , newReporterFromEnv
    , noSemantic
    , report
    )
import Control.Concurrent
    ( threadDelay
    )
import Control.Monad
    ( when
    )
import Control.Monad.Cont
    ( ContT (..)
    , evalContT
    )
import Control.Monad.IO.Class
    ( MonadIO (..)
    )
import Control.Tracer
    ( Tracer (..)
    , traceWith
    )
import Data.List
    ( intersperse
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( ToText (..)
    )
import Main.Utf8
    ( withUtf8
    )
import System.FilePath
    ( takeBaseName
    , (</>)
    )
import System.IO
    ( stdout
    )
import System.IO.Temp
    ( withSystemTempDirectory
    )
import Text.Read
    ( readMaybe
    )
import Prelude hiding
    ( takeWhile
    )

import qualified Cardano.Launcher as C
import qualified Cardano.Launcher.Node as C
import qualified Cardano.Launcher.Wallet as C
import qualified Data.Text as T
import qualified Options.Applicative as O
import qualified System.Process as S

{-----------------------------------------------------------------------------
    Configuration
------------------------------------------------------------------------------}
testSnapshot :: FilePath
testSnapshot = "membench-snapshot.tgz"

testBlockHeight :: Int
testBlockHeight = 7280

testMnemonic :: [String]
testMnemonic =
    [ "slab"
    , "praise"
    , "suffer"
    , "rabbit"
    , "during"
    , "dream"
    , "arch"
    , "harvest"
    , "culture"
    , "book"
    , "owner"
    , "loud"
    , "wool"
    , "salon"
    , "table"
    , "animal"
    , "vivid"
    , "arrow"
    , "dirt"
    , "divide"
    , "humble"
    , "tornado"
    , "solution"
    , "jungle"
    ]

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

data Config = Config
    { nodeExe :: FilePath
    , walletExe :: FilePath
    , snapshot :: FilePath
    , workingDir :: FilePath
    }

configParser :: O.Parser Config
configParser =
    Config
        <$> O.strOption
            ( O.long "node"
                <> O.metavar "PATH"
                <> O.help "Path to the cardano-node executable"
            )
        <*> O.strOption
            ( O.long "wallet"
                <> O.metavar "PATH"
                <> O.help "Path to the cardano-wallet executable"
            )
        <*> O.strOption
            ( O.long "snapshot"
                <> O.metavar "PATH"
                <> O.help "Path to the snapshot archive"
            )
        <*> O.strOption
            ( O.long "work-dir"
                <> O.metavar "PATH"
                <> O.help "Path to the working directory"
            )

configInfo :: O.ParserInfo Config
configInfo =
    O.info (configParser O.<**> O.helper)
        $ mconcat
            [ O.fullDesc
            , O.progDesc "Run the memory benchmark"
            , O.header
                "memory-benchmark - a benchmark for cardano-wallet memory usage"
            ]

newtype MemoryBenchLog = MemoryBenchLog Text

instance HasSeverityAnnotation MemoryBenchLog where
    getSeverityAnnotation _ = Notice

instance ToText MemoryBenchLog where
    toText (MemoryBenchLog t) = t

main :: IO ()
main = withUtf8 $ do
    Config{..} <- O.execParser configInfo
    requireExecutable nodeExe "--version"
    requireExecutable walletExe "version"
    requireExecutable "curl" "--version"
    requireExecutable "jq" "--version"

    installSignalHandlers (pure ())

    evalContT $ do
        ToTextTracer tr <- withToTextTracer (Left stdout) (Just Notice)
        let trace = liftIO . traceWith tr . MemoryBenchLog
        trace "Starting memory benchmark"
        tmp <- ContT $ withSystemTempDirectory "wallet"
        trace $ "Copying snapshot to " <> T.pack tmp
        cfg <- liftIO $ copyNodeSnapshot snapshot tmp
        trace "Starting the node"
        node <- ContT $ withCardanoNode tr nodeExe cfg
        sleep 5
        trace "Starting the wallet"
        wallet@(CardanoWalletConn _c p) <-
            ContT $ withCardanoWallet tr workingDir walletExe cfg node
        sleep 1
        trace "Creating a wallet"
        liftIO $ createWallet wallet testMnemonic
        sleep 1
        trace "Waiting for synchronization"
        liftIO $ waitUntilSynchronized tr wallet
        reporter <- newReporterFromEnv tr $ mkSemantic ["memory"]
        trace "Running pmap on the wallet process"
        Pmap{pmapLines} <- liftIO $ pmap $ processHandle p
        let usage = highestMemoryUsageFor "cardano-wallet" pmapLines
        trace "Reporting the result"
        liftIO
            $ report reporter
            $ pure
            $ Benchmark noSemantic
            $ Result (fromIntegral usage) KiloBytes 1
        trace "End of the benchmark"

highestMemoryUsageFor :: String -> [Line] -> Int
highestMemoryUsageFor cmd =
    maximum
        . map memory
        . filter (\l -> command l == cmd)

waitUntilSynchronized
    :: Tracer IO Text -> C.CardanoWalletConn -> IO ()
waitUntilSynchronized tr wallet = do
    height <- getLatestBlockHeight tr wallet

    when (height < testBlockHeight) $ do
        sleep 10
        waitUntilSynchronized tr wallet

copyNodeSnapshot :: FilePath -> FilePath -> IO BenchmarkConfig
copyNodeSnapshot snapshot tmp = do
    copyFile snapshot tmp
    let dir = tmp </> takeBaseName testSnapshot
    decompress (tmp </> testSnapshot) tmp
    pure
        $ BenchmarkConfig
            { nodeConfigDir = dir </> "config"
            , nodeDatabaseDir = dir </> "db-node"
            , walletDatabaseDir = dir </> "db-wallet"
            }

{-----------------------------------------------------------------------------
    Cardano commands
------------------------------------------------------------------------------}
getLatestBlockHeight
    :: Tracer IO Text -> C.CardanoWalletConn -> IO Int
getLatestBlockHeight _tr wallet = do
    fmap (fromMaybe 0 . readMaybe)
        . flip S.readCreateProcess ""
        . S.shell
        $ curlGetCommand wallet "/wallets"
            <> " | jq '.[0].tip.height.quantity'"

curlGetCommand
    :: C.CardanoWalletConn -> String -> String
curlGetCommand wallet path =
    "curl -X GET " <> walletURL wallet <> path

createWallet :: C.CardanoWalletConn -> [String] -> IO ()
createWallet wallet mnemonic =
    curlPostJSON wallet "/wallets"
        $ unwords
            [ "{ \"mnemonic_sentence\": " <> showMnemonic mnemonic <> ","
            , "  \"passphrase\": \"Secure Passphrase\","
            , "  \"name\": \"Memory Benchmark\","
            , "  \"address_pool_gap\": 20"
            , "}"
            ]
  where
    braces l r s = l <> s <> r
    showMnemonic =
        braces "[" "]" . unwords . intersperse "," . map (braces "\"" "\"")

curlPostJSON :: C.CardanoWalletConn -> String -> String -> IO ()
curlPostJSON wallet path json =
    S.callProcess
        "curl"
        [ "-X"
        , "POST"
        , "-d"
        , json
        , "-H"
        , "Content-Type: application/json"
        , walletURL wallet <> path
        ]

walletURL :: C.CardanoWalletConn -> String
walletURL wallet =
    "http://localhost:" <> show (C.getWalletPort wallet) <> "/v2"

data BenchmarkConfig = BenchmarkConfig
    { nodeConfigDir :: FilePath
    , nodeDatabaseDir :: FilePath
    , walletDatabaseDir :: FilePath
    }
    deriving (Eq, Show)

-- | Start a `cardano-wallet` process on the benchmark configuration.
withCardanoWallet
    :: Tracer IO C.LauncherLog
    -> FilePath
    -> FilePath
    -> BenchmarkConfig
    -> C.CardanoNodeConn
    -> (C.CardanoWalletConn -> IO r)
    -> IO r
withCardanoWallet tr workingDir walletExe BenchmarkConfig{..} node action = do
    C.withCardanoWallet
        tr
        node
        C.CardanoWalletConfig
            { C.walletPort =
                8060
            , C.walletNetwork =
                C.Testnet $ nodeConfigDir </> "byron-genesis.json"
            , C.walletDatabaseDir =
                walletDatabaseDir
            , C.extraArgs =
                profilingOptions
            , C.executable =
                Just walletExe
            , C.workingDir = Just workingDir
            }
        action
  where
    profilingOptions = words "+RTS -N1 -qg -A1m -I0 -T -h -i0.01 -RTS"

-- | Start a `cardano-node` process on the benchmark configuration.
withCardanoNode
    :: Tracer IO C.LauncherLog
    -> FilePath
    -> BenchmarkConfig
    -> (C.CardanoNodeConn -> IO r)
    -> IO r
withCardanoNode tr nodeExe BenchmarkConfig{..} action =
    C.withCardanoNode
        tr
        C.CardanoNodeConfig
            { C.nodeDir = nodeDatabaseDir
            , C.nodeConfigFile = nodeConfigDir </> "config.json"
            , C.nodeTopologyFile = nodeConfigDir </> "topology.json"
            , C.nodeDatabaseDir = nodeDatabaseDir
            , C.nodeDlgCertFile = Nothing
            , C.nodeSignKeyFile = Nothing
            , C.nodeOpCertFile = Nothing
            , C.nodeKesKeyFile = Nothing
            , C.nodeVrfKeyFile = Nothing
            , C.nodePort = Just (C.NodePort 8061)
            , C.nodeLoggingHostname = Nothing
            , C.nodeExecutable = Just nodeExe
            , C.nodeOutputFile = Nothing
            , C.nodeSocketPathFile = JustK $ nodeDatabaseDir </> "node.socket"
            }
        $ \(JustK c) -> action c

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
sleep :: MonadIO m => Int -> m ()
sleep seconds = liftIO $ threadDelay (seconds * 1000 * 1000)

-- | Throw an exception if the executable is not in the `$PATH`.
requireExecutable :: FilePath -> String -> IO ()
requireExecutable name cmd = S.callProcess name [cmd]

copyFile :: FilePath -> FilePath -> IO ()
copyFile source destination = S.callProcess "cp" [source, destination]

decompress :: FilePath -> FilePath -> IO ()
decompress source destination =
    S.callProcess "tar" ["-xzvf", source, "-C", destination]
