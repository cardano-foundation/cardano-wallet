{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- Start two wallet workers, observe SIGTERM drain, then reopen the same
-- databases and verify fixed-address settings and exact journal replay.
module Test.Integration.Framework.ShutdownDrain
    ( spec
    )
where

import Cardano.DB.Sqlite
    ( SqliteContext (..)
    , noAutoMigrations
    , noManualMigration
    , withSqliteContextFile
    )
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
import Cardano.Wallet.DB.Sqlite.Types
    ( DappSubmissionStatusEnum (..)
    )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( DurableSubmission (..)
    , claimDurableSubmissionAttempt
    , insertOrClassifyDurableSubmission
    , readDurableSubmissions
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
import Control.Tracer
    ( nullTracer
    )
import Data.Aeson
    ( Key
    , Value (..)
    , eitherDecode
    , encode
    , object
    , (.=)
    )
import Data.ByteString
    ( ByteString
    )
import Data.Char
    ( isDigit
    )
import Data.List
    ( find
    , isInfixOf
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
import Data.Text.Class
    ( fromText
    )
import Data.Time
    ( getCurrentTime
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
    , responseBody
    )
import Network.HTTP.Types.Status
    ( Status
    , status200
    , status400
    , status201
    , status409
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
    , shouldReturn
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
    , onException
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

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T

spec :: Spec
spec = sequential $ describe "shutdown drain" $ do
    it
        "fails when observed acquired wallet files are fewer than two"
        rejectObservedAcquiredBelowTwo
    it
        "fails when observed close files do not match acquired files"
        rejectObservedCloseMismatch
    it
        "SIGTERM drain and restart preserve V7 mode and exact journal replay"
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
                `onException` (readFile (dir </> "wallet.log") >>= putStrLn)

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
                        wid <- createShelleyWallet manager port "drain-one" False
                        otherWid <- createShelleyWallet manager port "drain-two" True
                        capabilities <-
                            requestJson manager port "GET"
                                "/v2/dapp-capabilities" status200 Nothing
                        network <- field "network" capabilities
                        let requestNetwork = case network of
                                Object fields ->
                                    Object $ KeyMap.delete "current_era" fields
                                _ -> network
                            submit tx expected =
                                requestJson manager port "POST"
                                    ("/v2/wallets/" <> T.unpack wid
                                        <> "/transaction-submission")
                                    expected
                                    $ Just $ object
                                        [ "revision" .= (1 :: Int)
                                        , "network" .= requestNetwork
                                        , "transaction" .= (tx :: Text)
                                        ]
                        -- Well-formed Conway body; the node rejects its zero fee.
                        original <-
                            submit "84a4008001800200031a7fffffffa0f5f6" status409
                        field "code" original
                            `shouldReturn` String "dapp_submission_failed"
                        field "message" original
                            `shouldReturn` String "Transaction submission failed"
                        acquiredPaths <- sheSqliteFiles dbDir
                        terminateProcess ph
                        code <- waitForExit ph
                        seedInterruptedBroadcast acquiredPaths wid otherWid
                        -- withCreateProcess consumes UseHandle; reopen on restart.
                        bracket
                            (openFile logPath AppendMode)
                            (\h -> hClose h `catch` (\(_ :: SomeException) -> pure ()))
                            $ \restartLog ->
                            withCreateProcess
                                (walletProc socket genesis dbDir port restartLog)
                            $ \_ _ _ restarted -> do
                                waitForApi manager port
                                restored <-
                                    requestJson manager port "GET"
                                        ("/v2/wallets/" <> T.unpack wid)
                                        status200 Nothing
                                field "single_address_mode" restored
                                    `shouldReturn` Bool False
                                replay <-
                                    submit "84a4008001800200031a7fffffffa0f5f6" status409
                                replay `shouldBe` original
                                -- Same body, with a valid native-script witness.
                                conflict <-
                                    submit "84a4008001800200031a7fffffffa101d9010281820400f5f6"
                                        status400
                                field "code" conflict
                                    `shouldReturn` String "dapp_identity_conflict"
                                unknown <-
                                    requestJson manager port "POST"
                                        ("/v2/wallets/" <> T.unpack otherWid
                                            <> "/transaction-submission")
                                        status200
                                        $ Just $ object
                                            [ "revision" .= (1 :: Int)
                                            , "network" .= requestNetwork
                                            , "transaction"
                                                .= ("84a4008001800200031a7fffffffa0f5f6" :: Text)
                                            ]
                                field "status" unknown
                                    `shouldReturn` String "outcome_unknown"
                                otherRestored <-
                                    requestJson manager port "GET"
                                        ("/v2/wallets/" <> T.unpack otherWid)
                                        status200 Nothing
                                field "single_address_mode" otherRestored
                                    `shouldReturn` Bool True
                                terminateProcess restarted
                                _ <- waitForExit restarted
                                pure ()
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
    length (filter (isInfixOf "Posting transaction") $ lines logs)
        `shouldBe` 1

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
        , "--trace-network"
        , "debug"
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

createShelleyWallet :: Manager -> Int -> Text -> Bool -> IO Text
createShelleyWallet manager port name singleAddressMode = do
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
                , "single_address_mode" .= singleAddressMode
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
    value <- either fail pure $ eitherDecode $ responseBody resp
    ident <- field "id" value
    case ident of
        String wid -> pure wid
        _ -> fail "wallet id is not a string"

requestJson
    :: Manager
    -> Int
    -> ByteString
    -> String
    -> Status
    -> Maybe Value
    -> IO Value
requestJson manager port verb path expected body = do
    initial <- parseRequest $ "http://127.0.0.1:" <> show port <> path
    let req = initial
            { method = verb
            , requestBody = RequestBodyLBS $ maybe mempty encode body
            , requestHeaders = [("Content-Type", "application/json")]
            }
    resp <- httpLbs req manager
    responseStatus resp `shouldBe` expected
    either fail pure $ eitherDecode $ responseBody resp

field :: Key -> Value -> IO Value
field key (Object fields) =
    maybe (fail $ "missing JSON field " <> show key) pure $ KeyMap.lookup key fields
field _ _ = fail "expected JSON object"

-- Seed the exact durable state left by an interrupted network attempt,
-- using the real store/claim operations between two real daemon lifetimes.
seedInterruptedBroadcast :: [FilePath] -> Text -> Text -> IO ()
seedInterruptedBroadcast paths sourceId targetId = do
    source <- either (fail . show) pure $ fromText sourceId
    target <- either (fail . show) pure $ fromText targetId
    let walletFile wid =
            maybe (fail "wallet database is missing") pure $
                find (isInfixOf $ T.unpack wid) paths
    sourceFile <- walletFile sourceId
    targetFile <- walletFile targetId
    original <-
        withSqliteContextFile nullTracer sourceFile noManualMigration noAutoMigrations $
            \db -> runQuery db $ readDurableSubmissions source
    [row] <- either (fail . show) pure original
    let authorized = row
            { durableWalletId = target
            , durableAuthorized = True
            , durableStatus = AuthorizedE
            , durableAttemptGeneration = 0
            , durableBroadcastGeneration = Nothing
            , durableBroadcastStarted = Nothing
            , durableAcceptance = Nothing
            , durableRejectionCode = Nothing
            }
    started <- getCurrentTime
    seeded <-
        withSqliteContextFile nullTracer targetFile noManualMigration noAutoMigrations $
            \db -> runQuery db $ do
                _ <- insertOrClassifyDurableSubmission authorized []
                claimDurableSubmissionAttempt target (durableTxId row) 0 started
    claimed <- either (fail . show) pure seeded
    fmap durableStatus claimed `shouldBe` Just BroadcastingE

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
