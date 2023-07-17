{-# LANGUAGE RecordWildCards, OverloadedStrings, PackageImports #-}

import Prelude

import Cardano.BM.Data.Tracer
    ( HasPrivacyAnnotation (..), HasSeverityAnnotation (..) )
import Cardano.Startup
    ( installSignalHandlers, withUtf8Encoding )
import Control.Concurrent
    ( threadDelay )
import Control.Monad
    ( unless, void, when )
import Control.Tracer
    ( Tracer (..), contramap, traceWith )
import Data.List
    ( intersperse )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import System.FilePath
    ( takeBaseName, (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Text.Read
    ( readMaybe )

import qualified Cardano.BM.Configuration.Model as Log
import qualified Cardano.BM.Configuration.Static as Log
import qualified Cardano.BM.Data.BackendKind as Log
import qualified Cardano.BM.Data.LogItem as Log
import qualified Cardano.BM.Data.Severity as Log
import qualified Cardano.BM.Setup as Log
import qualified Cardano.Launcher as C
import qualified Cardano.Launcher.Node as C
import qualified Cardano.Launcher.Wallet as C
import qualified System.Process as S
import qualified Data.Text as T
import qualified "optparse-applicative" Options.Applicative as O

{-----------------------------------------------------------------------------
    Configuration
------------------------------------------------------------------------------}
testSnapshot :: FilePath
testSnapshot = "membench-snapshot.tgz"

testBlockHeight :: Int
testBlockHeight = 7280

testMnemonic :: [String]
testMnemonic =
    [ "slab","praise","suffer","rabbit","during","dream"
    , "arch","harvest","culture","book","owner","loud"
    , "wool","salon","table","animal","vivid","arrow"
    , "dirt","divide","humble","tornado","solution","jungle"
    ]

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}



data Config = Config
  { nodeExe :: FilePath
  , walletExe :: FilePath
  , snapshot :: FilePath
  }


configParser :: O.Parser Config
configParser = Config
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

configInfo :: O.ParserInfo Config
configInfo = O.info (configParser O.<**> O.helper) $ mconcat
    [ O.fullDesc
    , O.progDesc "Run the memory benchmark"
    , O.header "memory-benchmark - a benchmark for cardano-wallet memory usage"
    ]



main :: IO ()
main = withUtf8Encoding $ do
    Config{..} <- O.execParser configInfo
    requireExecutable nodeExe "--version"
    requireExecutable walletExe "version"
    requireExecutable "curl" "--version"
    requireExecutable "jq" "--version"

    installSignalHandlers (pure ())
    trText <- initLogging "memory-benchmark" Log.Debug
    let tr0 = trText
        tr = contramap toText tr0

    withSystemTempDirectory "wallet" $ \tmp -> do
        cfg <- copyNodeSnapshot snapshot tmp
        void $ withCardanoNode tr cfg $ \node -> do
            sleep 5
            withCardanoWallet tr cfg node $ \wallet -> void $ do
                sleep 1
                createWallet wallet testMnemonic
                sleep 1
                waitUntilSynchronized tr0 wallet

waitUntilSynchronized :: Tracer IO Text  -> C.CardanoWalletConn -> IO ()
waitUntilSynchronized tr wallet = do

    height <- getLatestBlockHeight tr wallet
    traceWith tr $ "-------------------------------------------Current block height: " <> T.pack (show height)

    when (height < testBlockHeight) $ do
        sleep 10
        waitUntilSynchronized tr wallet

copyNodeSnapshot :: FilePath -> FilePath -> IO BenchmarkConfig
copyNodeSnapshot snapshot tmp = do
    copyFile snapshot tmp
    let dir = tmp </> takeBaseName testSnapshot
    decompress (tmp </> testSnapshot) tmp
    pure $ BenchmarkConfig
        { nodeConfigDir = dir </> "config"
        , nodeDatabaseDir = dir </> "db-node"
        , walletDatabaseDir = dir </> "db-wallet"
        }

{-----------------------------------------------------------------------------
    Cardano commands
------------------------------------------------------------------------------}
getLatestBlockHeight :: Tracer IO Text -> C.CardanoWalletConn -> IO Int
getLatestBlockHeight tr wallet = do
    r <- flip S.readCreateProcess ""
        . S.shell
        $ curlGetCommand wallet "/wallets"
            <> " "
    traceWith tr $ "++++++++++++++++++++++++++++++++++++getLatestBlockHeight: " <> T.pack r
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
    curlPostJSON wallet "/wallets" $ unwords
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
        [ "-X", "POST"
        , "-d", json
        , "-H", "Content-Type: application/json"
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
    -> BenchmarkConfig
    -> C.CardanoNodeConn
    -> (C.CardanoWalletConn -> IO r)
    -> IO (Either C.ProcessHasExited r)
withCardanoWallet tr BenchmarkConfig{..} node =
    C.withCardanoWallet tr node
        C.CardanoWalletConfig
            { C.walletPort =
                8060
            , C.walletNetwork =
                C.Testnet $ nodeConfigDir </> "byron-genesis.json"
            , C.walletDatabaseDir =
                walletDatabaseDir
            , C.extraArgs =
                profilingOptions
            }
  where
    profilingOptions = words "+RTS -N1 -qg -A1m -I0 -T -h -i0.01 -RTS"

-- | Start a `cardano-node` process on the benchmark configuration.
withCardanoNode
    :: Tracer IO C.LauncherLog
    -> BenchmarkConfig
    -> (C.CardanoNodeConn -> IO r)
    -> IO (Either C.ProcessHasExited r)
withCardanoNode tr BenchmarkConfig{..} =
    C.withCardanoNode tr
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
            }

{-----------------------------------------------------------------------------
    Utilities
------------------------------------------------------------------------------}
sleep :: Int -> IO ()
sleep seconds = threadDelay (seconds * 1000 * 1000)

-- | Throw an exception if the executable is not in the `$PATH`.
requireExecutable :: FilePath -> String -> IO ()
requireExecutable name cmd = S.callProcess name [cmd]

copyFile :: FilePath -> FilePath -> IO ()
copyFile source destination = S.callProcess "cp" [source,destination]

decompress :: FilePath -> FilePath -> IO ()
decompress source destination =
    S.callProcess "tar" ["-xzvf", source, "-C", destination]

{-----------------------------------------------------------------------------
    Logging
------------------------------------------------------------------------------}
initLogging :: Text -> Log.Severity -> IO (Tracer IO Text)
initLogging name minSeverity = do
    c <- Log.defaultConfigStdout
    Log.setMinSeverity c minSeverity
    Log.setSetupBackends c [Log.KatipBK, Log.AggregationBK]
    (tr, _sb) <- Log.setupTrace_ c name
    pure (trMessageText tr)

-- | Tracer transformer which transforms traced items to their 'ToText'
-- representation and further traces them as a 'Log.LogObject'.
-- If the 'ToText' representation is empty, then no tracing happens.
trMessageText
    :: (ToText a, HasPrivacyAnnotation a, HasSeverityAnnotation a)
    => Tracer IO (Log.LoggerName, Log.LogObject Text)
    -> Tracer IO a
trMessageText tr = Tracer $ \arg -> do
    let msg = toText arg
    unless (msg == mempty) $ do
        meta <- Log.mkLOMeta
            (getSeverityAnnotation arg)
            (getPrivacyAnnotation arg)
        traceWith tr (mempty, Log.LogObject mempty meta (Log.LogMessage msg))
