{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: Apache-2.0
--
-- Database migrations testing executable.
--
-- This program runs a wallet backend using the given command line parameters
-- and then uses untyped HTTP requests to run a testing action.
--
-- To test database migrations, the testing actions should be run against
-- different wallet server versions with the same database directory.

module Main where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Setup
    ( withTrace )
import Cardano.BM.Trace
    ( Trace, logError, logInfo, logNotice )
import Cardano.Launcher
    ( Command (..), ProcessHasExited (..), StdStream (..), withBackendProcess )
import Cardano.Wallet.Api.Types
    ( DecodeAddress, EncodeAddress )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network.Ports
    ( waitForPort )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Mnemonic
    ( entropyToMnemonic, genEntropy, mnemonicToText )
import Control.Concurrent
    ( threadDelay )
import Control.Lens
    ( (^.), (^..), (^?) )
import Control.Monad
    ( forever, mapM_, void )
import Control.Retry
    ( RetryPolicy
    , constantDelay
    , limitRetries
    , limitRetriesByCumulativeDelay
    , retrying
    )
import Data.Aeson
    ( Value (..), object, toJSON, (.=) )
import Data.Aeson.Lens
    ( key, values, _Integral, _String )
import Data.Maybe
    ( isNothing )
import Data.Text
    ( Text )
import Network.Wreq
    ( get, post, responseBody )
import System.Environment
    ( getArgs )
import System.Exit
    ( ExitCode (..), exitFailure, exitWith )

import qualified Data.Text as T

main :: IO ()
main = do
    (testAction, launchArgs) <- parseArgs @('Testnet 0)

    cfg <- defaultConfigStdout
    withTrace cfg "migration-test" $ \tr ->
        testMain @('Testnet 0) tr 8090 testAction launchArgs >>= exitWith

-- | Something to do while the server is running.
type TestAction (t :: NetworkDiscriminant) = Trace IO Text -> ApiBase -> IO ExitCode

testMain
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Trace IO Text
    -> Int
    -> TestAction t
    -> [String]
    -> IO ExitCode
testMain tr serverPort testAction launchArgs = do
    let apiBase = mkApiBase serverPort
    let cmd = Command "cardano-wallet-itn" launchArgs (pure ()) Inherit Inherit
    res <- withBackendProcess (trMessageText tr) cmd $ do
        waitForWalletServer serverPort
        testAction tr apiBase

    case res of
        Right st -> pure st
        Left (ProcessDidNotStart _ e) -> do
            logError tr ("Failed to start process: " <> T.pack (show e))
            pure (ExitFailure 13)
        Left (ProcessHasExited _ ExitSuccess) ->
            pure ExitSuccess
        Left (ProcessHasExited _ st) -> do
            logError tr ("Process exited with status " <> T.pack (show st))
            pure (ExitFailure 14)

-- | @run@ action
doRun
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Trace IO Text
    -> ApiBase
    -> IO ExitCode
doRun _ _ = forever $ threadDelay maxBound

testWalletName :: Text
testWalletName = "test"

-- | @step1@ action. Sets up the database on the old version.
doStep1
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Trace IO Text
    -> ApiBase
    -> IO ExitCode
doStep1 tr apiBase = do
    waitForSync tr apiBase
    mnem <- mnemonicToText @15 . entropyToMnemonic <$> genEntropy :: IO [Text]
    let postData = object
            [ "mnemonic_sentence" .= toJSON mnem
            , "name" .= testWalletName
            , "passphrase" .= String "0000000000000000000000000000"
            ]
    wal <- post (url apiBase "wallets") (toJSON postData)
    let walId = wal ^. responseBody . key "id" . _String
    logInfo tr $ "Create wallet with id " <> walId
    waitForRestore tr apiBase walId
    pure ExitSuccess

-- | @step2@ action. Checks the contents of the database on the new version.
doStep2
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => Trace IO Text
    -> ApiBase
    -> IO ExitCode
doStep2 tr apiBase = do
    waitForSync tr apiBase
    waitForBlock tr apiBase
    wals <- get (url apiBase "wallets")
    let walIds = wals ^.. responseBody . values . key "id" . _String
    logInfo tr $ "wallets are " <> T.unwords walIds
    let walNames = wals ^.. responseBody . values . key "name" . _String
    if walNames == [testWalletName]
        then do
            mapM_ (waitForRestore tr apiBase) walIds
            pure ExitSuccess
        else do
            logError tr $ "Expected wallet name " <> testWalletName <>
                " but got wallet names:\n" <> T.unlines walNames
            pure $ ExitFailure 1

{-------------------------------------------------------------------------------
                                   Api Client
-------------------------------------------------------------------------------}

newtype ApiBase = ApiBase { getApiBase :: String }
    deriving (Show, Eq)

mkApiBase :: Int -> ApiBase
mkApiBase port = ApiBase $ "http://localhost:" ++ show port ++ "/v2/"

url :: ApiBase -> Text -> String
url (ApiBase base) path = base ++ T.unpack path

-- | Poll a wallet by ID until it has restored.
waitForRestore :: Trace IO Text -> ApiBase -> Text -> IO ()
waitForRestore tr base wid =
    void $ waitForSomething tr "restore" action shouldRetry
  where
    shouldRetry res = (res ^. responseBody . restoreStatus) /= "ready"
    restoreStatus = key "state" . key "status" . _String
    action = get (url base ("wallets/" <> wid))

-- | Poll the wallet server until it reports that it has synced with the
-- network.
waitForSync :: Trace IO Text -> ApiBase -> IO ()
waitForSync tr base =
    void $ waitForSomething tr "sync" action shouldRetry
  where
    shouldRetry res = (res ^. responseBody . syncStatus) /= "ready"
    syncStatus = key "sync_progress" . key "status" . _String
    action = get (url base "network/information")

-- | Poll the node's block height until it changes.
waitForBlock :: Trace IO Text -> ApiBase -> IO ()
waitForBlock tr base = void $ do
    bh <- waitForSomething tr "initial block height"
        getBlockHeight isNothing
    case bh of
        Just _ ->
            waitForSomething tr "block height increase"
                getBlockHeight (<= bh)
        Nothing -> do
            logError tr "Could not extract block height from network info"
            exitFailure

  where
    getBlockHeight :: IO (Maybe Integer)
    getBlockHeight = do
        res <- get (url base "network/information")
        pure (res ^? responseBody . blockHeight)

    blockHeight = key "node_tip" . key "height" . key "quantity" . _Integral

waitForSomething :: Trace IO Text -> Text -> IO res -> (res -> Bool) -> IO res
waitForSomething tr name action shouldRetry = do
    logNotice tr $ "Waiting for " <> name
    res <- retrying retryPolicy (const (pure . shouldRetry)) (const action)
    if shouldRetry res
        then do
            logError tr $ "Timed out waiting for " <> name
            exitFailure
        else do
            logNotice tr $ name <> " is done."
            pure res

retryPolicy :: RetryPolicy
retryPolicy = limitRetriesByCumulativeDelay (60 * sec) (constantDelay sec)
  where
    sec = 1000 * 1000

{-------------------------------------------------------------------------------
                                  Port helper
-------------------------------------------------------------------------------}

-- | Poll the given port for up to 5 seconds.
waitForWalletServer :: Int -> IO ()
waitForWalletServer port = void $ waitForPort pol (fromIntegral port)
  where
    pol = constantDelay 100000 <> limitRetries 50

{-------------------------------------------------------------------------------
                              Command line parsing
-------------------------------------------------------------------------------}

parseArgs
    :: forall t. (DecodeAddress t, EncodeAddress t)
    => IO (TestAction t, [String])
parseArgs = getArgs >>= \case
    [] -> usage
    ("run":args) -> pure (doRun @t, args)
    ("step1":args) -> pure (doStep1 @t, args)
    ("step2":args) -> pure (doStep2 @t, args)
    _ -> usage
  where
    usage = do
        putStrLn "Usage: migration-test (run|step1|step2) LAUNCHER ARGS...\n"
        putStrLn "  run - just run the launcher and do nothing."
        putStrLn "  step1 - Wait for sync and restore a wallet."
        putStrLn "  step2 - List the wallets and print their details."
        putStrLn "\nThis program runs a wallet backend using the given command-line"
        putStrLn "parameters and then connects to the server to run a testing action."
        exitFailure
