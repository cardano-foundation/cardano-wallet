{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- Bounded smoke: start two wallet workers, send the shipped SIGTERM,
-- and observe the matching close callbacks before process exit.
module Test.Integration.Framework.ShutdownDrain
    ( spec
    )
where

import Cardano.Faucet.Mnemonics
    ( MnemonicLength (..)
    , generateSome
    )
import Cardano.Launcher.Node
    ( nodeSocketFile
    )
import Cardano.Mnemonic.Extended
    ( someMnemonicToWords
    )
import Cardano.Wallet.Launch.Cluster
    ( FaucetFunds (..)
    , RunningNode (..)
    )
import Cardano.Wallet.Launch.Cluster.Process
    ( RunMonitorQ
    , WalletPresence (..)
    , defaultEnvVars
    , waitForRunningNode
    , withLocalCluster
    )
import Cardano.Wallet.Network.Ports
    ( getRandomPort
    )
import Control.Monad
    ( filterM
    , unless
    )
import Control.Monad.Cont
    ( evalContT
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.Aeson
    ( encode
    , object
    , (.=)
    )
import Data.Char
    ( isDigit
    )
import Data.List
    ( isInfixOf
    , isPrefixOf
    , isSuffixOf
    , nub
    )
import Data.Maybe
    ( catMaybes
    , mapMaybe
    )
import Data.Text
    ( Text
    )
import Network.HTTP.Client
    ( Manager
    , RequestBody (RequestBodyLBS)
    , defaultManagerSettings
    , httpLbs
    , method
    , newManager
    , parseRequest
    , requestBody
    , requestHeaders
    , responseStatus
    )
import Network.HTTP.Types.Status
    ( status200
    , status201
    )
import System.Directory
    ( canonicalizePath
    , createDirectory
    , doesFileExist
    , getSymbolicLinkTarget
    , listDirectory
    )
import System.Exit
    ( ExitCode
    )
import System.FilePath
    ( takeDirectory
    , (</>)
    )
import System.IO
    ( BufferMode (LineBuffering)
    , Handle
    , IOMode (AppendMode)
    , hClose
    , hSetBuffering
    , openFile
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.Hspec.Core.Spec
    ( sequential
    )
import Test.Hspec.Expectations.Lifted
    ( shouldBe
    , shouldSatisfy
    )
import UnliftIO.Async
    ( race
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Exception
    ( SomeException
    , bracket
    , catch
    )
import UnliftIO.Process
    ( CreateProcess (..)
    , ProcessHandle
    , StdStream (UseHandle)
    , proc
    , terminateProcess
    , waitForProcess
    , withCreateProcess
    )
import UnliftIO.Temporary
    ( withSystemTempDirectory
    )
import Prelude

spec :: Spec
spec = sequential $ describe "shutdown drain" $ do
    it
        "fails when observed acquired wallet files are fewer than two"
        rejectObservedAcquiredBelowTwo
    it
        "fails when observed close files do not match acquired files"
        rejectObservedCloseMismatch
    it
        "SIGTERM drain observes two wallet close callbacks"
        runSigtermDrainSmoke

rejectObservedAcquiredBelowTwo :: IO ()
rejectObservedAcquiredBelowTwo =
    checkObservedDrainCounts [] []
        `shouldBe` Left "acquired count 0 is below 2"

rejectObservedCloseMismatch :: IO ()
rejectObservedCloseMismatch =
    checkObservedDrainCounts
        ["she.a.sqlite", "she.b.sqlite"]
        ["she.a.sqlite"]
        `shouldBe` Left
            "closed count 1 does not equal acquired count 2"

-- | Both counts must come from observed paths. A literal stand-in for
-- either side cannot pass these checks.
checkObservedDrainCounts
    :: [FilePath]
    -> [FilePath]
    -> Either String (Int, Int)
checkObservedDrainCounts acquiredPaths closedPaths
    | acquired < 2 =
        Left $ "acquired count " <> show acquired <> " is below 2"
    | closed /= acquired =
        Left
            $ "closed count "
                <> show closed
                <> " does not equal acquired count "
                <> show acquired
    | otherwise = Right (acquired, closed)
  where
    acquired = length acquiredPaths
    closed = length closedPaths

runSigtermDrainSmoke :: IO ()
runSigtermDrainSmoke = evalContT $ do
    ((runMonitorQ, _), _) <-
        withLocalCluster
            "shutdown-drain-smoke"
            NoWallet
            defaultEnvVars
            emptyFunds
    liftIO $ do
        node <- waitForCluster runMonitorQ
        let socket = nodeSocketFile (runningNodeSocketPath node)
        genesis <- findByronGenesis socket
        port <- getRandomPort
        withSystemTempDirectory "shutdown-drain-wallet" $ \dir ->
            runWalletSmoke
                socket
                genesis
                (fromIntegral port)
                dir

emptyFunds :: FaucetFunds
emptyFunds =
    FaucetFunds
        { pureAdaFunds = []
        , maryAllegraFunds = []
        , massiveWalletFunds = []
        }

waitForCluster :: RunMonitorQ IO -> IO RunningNode
waitForCluster runMonitorQ = do
    outcome <-
        race
            (threadDelay 180_000_000)
            (waitForRunningNode runMonitorQ)
    case outcome of
        Left () -> fail "cluster start timed out"
        Right node -> pure node

runWalletSmoke
    :: FilePath
    -> FilePath
    -> Int
    -> FilePath
    -> IO ()
runWalletSmoke socket genesis port dir = do
    let dbDir = dir </> "db"
        logPath = dir </> "wallet.log"
    createDirectory dbDir
    manager <- newManager defaultManagerSettings
    outcome <-
        bracket
            ( do
                h <- openFile logPath AppendMode
                hSetBuffering h LineBuffering
                pure h
            )
            (\h -> hClose h `catch` (\(_ :: SomeException) -> pure ()))
            $ \logHandle ->
                withCreateProcess
                    (walletProc socket genesis dbDir port logHandle)
                    $ \_ _ _ ph -> do
                        waitForApi manager port
                        createShelleyWallet manager port "drain-one"
                        createShelleyWallet manager port "drain-two"
                        acquiredPaths <- sheSqliteFiles dbDir
                        terminateProcess ph
                        code <- waitForExit ph
                        pure (acquiredPaths, code)
    logs <- readFile logPath
    let (acquiredPaths, exit) = outcome
        closedPaths = nub $ mapMaybe closePath (lines logs)
        sawSigTerm = "Terminated by signal." `isInfixOf` logs
    counts <- case checkObservedDrainCounts acquiredPaths closedPaths of
        Left msg -> fail msg
        Right pair -> pure pair
    let (acquired, closed) = counts
    putStrLn
        $ "shutdown drain acquired="
            <> show acquired
            <> " closed="
            <> show closed
            <> " acquired_paths="
            <> show acquiredPaths
            <> " closed_paths="
            <> show closedPaths
            <> " sigterm="
            <> show sawSigTerm
            <> " exit="
            <> show exit
    sawSigTerm `shouldBe` True
    acquired `shouldSatisfy` (>= 2)
    closed `shouldBe` acquired

walletProc
    :: FilePath
    -> FilePath
    -> FilePath
    -> Int
    -> Handle
    -> CreateProcess
walletProc socket genesis dbDir port logHandle =
    ( proc
        "cardano-wallet"
        [ "serve"
        , "--node-socket"
        , socket
        , "--testnet"
        , genesis
        , "--database"
        , dbDir
        , "--listen-address"
        , "127.0.0.1"
        , "--port"
        , show port
        ]
    )
        { std_out = UseHandle logHandle
        , std_err = UseHandle logHandle
        }

waitForApi :: Manager -> Int -> IO ()
waitForApi manager port = go 90_000_000
  where
    url =
        "http://127.0.0.1:"
            <> show port
            <> "/v2/network/information"
    step = 200_000
    go remaining
        | remaining <= 0 = fail $ "timeout waiting for " <> url
        | otherwise = do
            ok <-
                check
                    `catch` (\(_ :: SomeException) -> pure False)
            unless ok $ do
                threadDelay step
                go (remaining - step)
    check = do
        req <- parseRequest url
        resp <- httpLbs req manager
        pure $ responseStatus resp == status200

createShelleyWallet :: Manager -> Int -> Text -> IO ()
createShelleyWallet manager port name = do
    mnemonic <- generateSome M15
    initReq <-
        parseRequest
            $ "http://127.0.0.1:"
                <> show port
                <> "/v2/wallets"
    let body =
            object
                [ "name" .= name
                , "mnemonic_sentence"
                    .= someMnemonicToWords mnemonic
                , "passphrase" .= ("cardano-wallet" :: Text)
                ]
        req =
            initReq
                { method = "POST"
                , requestBody = RequestBodyLBS (encode body)
                , requestHeaders =
                    [("Content-Type", "application/json")]
                }
    resp <- httpLbs req manager
    responseStatus resp `shouldBe` status201

waitForExit :: ProcessHandle -> IO ExitCode
waitForExit ph = do
    outcome <-
        race
            (threadDelay 60_000_000)
            (waitForProcess ph)
    case outcome of
        Left () ->
            fail "SIGTERM drain timed out waiting for exit"
        Right code -> pure code

sheSqliteFiles :: FilePath -> IO [FilePath]
sheSqliteFiles dir = do
    names <- listDirectory dir
    let walletFiles =
            filter isWalletSqlite names
    pure $ fmap (dir </>) walletFiles
  where
    isWalletSqlite name =
        "she." `isPrefixOf` name
            && ".sqlite" `isSuffixOf` name
            && not ("-wal" `isSuffixOf` name)
            && not ("-shm" `isSuffixOf` name)

closePath :: String -> Maybe FilePath
closePath line
    | "Closing single database connection" `isInfixOf` line
        && "she." `isInfixOf` line =
        case break (== '(') line of
            (_, '(' : rest) ->
                let path = takeWhile (/= ')') rest
                in  if "she." `isInfixOf` path
                        then Just path
                        else Nothing
            _ -> Nothing
    | otherwise = Nothing

findByronGenesis :: FilePath -> IO FilePath
findByronGenesis socketPath = do
    socket <- canonicalizePath socketPath
    pids <- filter (all isDigit) <$> listDirectory "/proc"
    found <- catMaybes <$> mapM (pidOwnsSocket socket) pids
    case found of
        [] ->
            fail $ "no process holds node socket " <> socket
        (pid : _) -> genesisFromPid pid

pidOwnsSocket :: FilePath -> FilePath -> IO (Maybe FilePath)
pidOwnsSocket socket pid = do
    let cmdPath = "/proc" </> pid </> "cmdline"
    present <- doesFileExist cmdPath
    if not present
        then pure Nothing
        else do
            raw <-
                readFile cmdPath
                    `catch` (\(_ :: SomeException) -> pure "")
            pure
                $ if socket `isInfixOf` raw
                    then Just pid
                    else Nothing

genesisFromPid :: FilePath -> IO FilePath
genesisFromPid pid = do
    nodeCwd <-
        getSymbolicLinkTarget ("/proc" </> pid </> "cwd")
            `catch` (\(_ :: SomeException) -> pure "")
    let candidates =
            [ nodeCwd </> "byron-genesis.json"
            , takeDirectory nodeCwd </> "byron-genesis.json"
            , takeDirectory (takeDirectory nodeCwd)
                </> "byron-genesis.json"
            ]
    existing <- filterM doesFileExist candidates
    case existing of
        (path : _) -> canonicalizePath path
        [] ->
            fail
                $ "byron-genesis.json not found from pid "
                    <> pid
                    <> " cwd="
                    <> nodeCwd
