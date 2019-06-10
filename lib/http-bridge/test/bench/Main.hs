{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude

import Cardano.Launcher
    ( Command (Command), StdStream (..), installSignalHandlers, launch )
import Cardano.Wallet
    ( WalletLayer (..), newWalletLayer, unsafeRunExceptT )
import Cardano.Wallet.DB.Sqlite
    ( PersistState )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.HttpBridge.Network
    ( newNetworkLayer )
import Cardano.Wallet.HttpBridge.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Network
    ( NetworkLayer (..), networkTip )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..)
    , Passphrase (..)
    , digest
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery
    , GenChange
    , IsOwned
    , KnownAddresses
    , SeqState
    , defaultAddressPoolGap
    , mkSeqState
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Any
    ( AnyAddressState, initAnyState )
import Cardano.Wallet.Primitive.AddressDiscovery.Any.TH
    ( migrateAll )
import Cardano.Wallet.Primitive.Model
    ( totalBalance, totalUTxO )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , SlotId (..)
    , UTxO (..)
    , WalletId (..)
    , WalletName (..)
    , WalletState (..)
    )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.DeepSeq
    ( NFData, rnf )
import Control.Exception
    ( bracket, evaluate, throwIO )
import Control.Monad
    ( forM, mapM_, void )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( runExceptT )
import Criterion.Measurement
    ( getTime, initializeTime, secs )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock.POSIX
    ( POSIXTime, getPOSIXTime )
import Database.Persist.Sql
    ( runMigrationSilent )
import Fmt
    ( fmt, (+|), (+||), (|+), (||+) )
import Say
    ( sayErr )
import System.Environment
    ( getArgs )
import System.IO
    ( BufferMode (..), hSetBuffering, stderr, stdout )
import System.IO.Temp
    ( emptySystemTempFile )

import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- | Run all available benchmarks. Can accept one argument that is a target
-- network against which benchmarks below should be ran
--
-- (e.g. `--benchmark-arguments "mainnet"`)
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    installSignalHandlers
    network <- getArgs >>= parseNetwork
    case network of
        Testnet -> do
            let proxy = Proxy @'Testnet
            prepareNode proxy
            runBenchmarks
                [ bench ("restore " <> toText network <> " seq")
                    (bench_restoration proxy (walletSeq @'Testnet))
                , bench ("restore " <> toText network <> " 10% ownership")
                    (bench_restoration proxy wallet10p)
                ]
        Mainnet -> do
            let proxy = Proxy @'Mainnet
            prepareNode proxy
            runBenchmarks
                [ bench ("restore " <> toText network <> " seq")
                    (bench_restoration proxy (walletSeq @'Mainnet))
                , bench ("restore " <> toText network <> " 10% ownership")
                    (bench_restoration proxy wallet10p)
                ]
        Staging ->
            return ()
  where
    walletSeq
        :: KeyToAddress (HttpBridge n)
        => (WalletId, WalletName, SeqState (HttpBridge n))
    walletSeq =
        let
            seed = Passphrase
                "involve key curtain arrest fortune custom lens marine before \
                \material wheel glide cause weapon wrap"
            xprv = generateKeyFromSeed (seed, mempty) mempty
            wid = WalletId $ digest $ publicKey xprv
            wname = WalletName "Benchmark Sequential Wallet"
            s = mkSeqState (xprv, mempty) defaultAddressPoolGap
        in
            (wid, wname, s)

    wallet10p :: (WalletId, WalletName, AnyAddressState)
    wallet10p =
        initAnyState "Benchmark 10% Wallet" 0.1

runBenchmarks :: [IO (Text, Double)] -> IO ()
runBenchmarks bs = do
    initializeTime
    -- NOTE: Adding an artificial delay between successive runs to get a better
    -- output for the heap profiling.
    rs <- forM bs $ \io -> io <* let _2s = 2000000 in threadDelay _2s
    sayErr "\n\nAll results:"
    mapM_ (uncurry printResult) rs

bench :: Text -> IO () -> IO (Text, Double)
bench benchName action = do
    sayErr $ "Running " <> benchName
    start <- getTime
    res <- action
    evaluate (rnf res)
    finish <- getTime
    let dur = finish - start
    printResult benchName dur
    pure (benchName, dur)

printResult :: Text -> Double -> IO ()
printResult benchName dur = sayErr . fmt $ "  "+|benchName|+": "+|secs dur|+""

parseNetwork :: MonadFail m => [String] -> m Network
parseNetwork = \case
    [] ->
        return Testnet
    ["testnet"] ->
        return Testnet
    ["mainnet"] ->
        return Mainnet
    _ ->
        fail "invalid network provided to benchmark: not 'testnet' nor 'mainnet'."

{-------------------------------------------------------------------------------
                                  Benchmarks
-------------------------------------------------------------------------------}

{-# ANN bench_restoration ("HLint: ignore Use camelCase" :: String) #-}
bench_restoration
    :: forall (n :: Network) s.
        ( IsOwned s
        , GenChange s
        , NFData s
        , Show s
        , CompareDiscovery s
        , KnownAddresses s
        , PersistState s
        , KnownNetwork n
        )
    => Proxy n
    -> (WalletId, WalletName, s)
    -> IO ()
bench_restoration _ (wid, wname, s) = withHttpBridge network $ \port -> do
    (conn, dbLayer) <- emptySystemTempFile "bench.db" >>= Sqlite.newDBLayer . Just
    Sqlite.runQuery conn (void $ runMigrationSilent migrateAll)
    networkLayer <- newNetworkLayer port
    let transactionLayer = newTransactionLayer
    BlockHeader sl _ <- unsafeRunExceptT $ networkTip networkLayer
    sayErr . fmt $ network ||+ " tip is at " +|| sl ||+ ""
    w <- newWalletLayer @_ @(HttpBridge n) dbLayer networkLayer transactionLayer
    wallet <- unsafeRunExceptT $ createWallet w wid wname s
    unsafeRunExceptT $ restoreWallet w wallet
    waitForWalletSync w wallet
    (wallet', _) <- unsafeRunExceptT $ readWallet w wid
    sayErr "Wallet restored!"
    sayErr . fmt $ "Balance: " +|| totalBalance wallet' ||+ " lovelace"
    sayErr . fmt $ "UTxO: " +|| Map.size (getUTxO $ totalUTxO wallet') ||+ " entries"
    unsafeRunExceptT $ removeWallet w wid
  where
    network = networkVal @n

logChunk :: SlotId -> IO ()
logChunk slot = sayErr . fmt $ "Processing "+||slot||+""

withHttpBridge :: Network -> (Int -> IO a) -> IO a
withHttpBridge network action = bracket start stop (const (action port))
  where
    port = 8002
    start = do
        handle <- async $ launch
            [ Command "cardano-http-bridge"
                [ "start"
                , "--port", show port
                , "--template", T.unpack (toText network)
                ]
                (return ())
                Inherit
            ]
        threadDelay 1000000 -- wait for listening socket
        pure handle
    stop handle = do
        cancel handle
        threadDelay 1000000 -- wait for socket to be closed

prepareNode :: forall n. KnownNetwork n => Proxy n -> IO ()
prepareNode _ = do
    sayErr . fmt $ "Syncing "+|toText network|+" node... "
    sl <- withHttpBridge network $ \port -> do
        bridge <- newNetworkLayer @n port
        waitForNodeSync bridge (toText network) logQuiet
    sayErr . fmt $ "Completed sync of "+|toText network|+" up to "+||sl||+""
  where
    network = networkVal @n

-- | Regularly poll the wallet to monitor it's syncing progress. Block until the
-- wallet reaches 100%.
waitForWalletSync
    :: WalletLayer s t
    -> WalletId
    -> IO ()
waitForWalletSync walletLayer wid = do
    (_, meta) <- unsafeRunExceptT $ readWallet walletLayer wid
    case meta ^. #status of
        Ready -> return ()
        Restoring (Quantity p) -> do
            sayErr . fmt $ "[INFO] restoring: "+||p||+"%"
            threadDelay 1000000
            waitForWalletSync walletLayer wid


-- | Poll the network tip until it reaches the slot corresponding to the current
-- time.
waitForNodeSync
    :: NetworkLayer t IO
    -> Text
    -> (SlotId -> SlotId -> IO ())
    -> IO SlotId
waitForNodeSync bridge networkName logSlot = loop 10
  where
    loop :: Int -> IO SlotId
    loop retries = runExceptT (networkTip bridge) >>= \case
        Right (BlockHeader tipBlockSlot _) -> do
            currentSlot <- getCurrentSlot networkName
            logSlot tipBlockSlot currentSlot
            if tipBlockSlot < currentSlot
                then do
                    -- 2 seconds poll interval
                    threadDelay 2000000
                    loop retries
                else
                    pure tipBlockSlot
        Left e | retries > 0 -> do
                     sayErr "Fetching tip failed, retrying shortly..."
                     threadDelay 15000000
                     loop (retries - 1)
               | otherwise -> throwIO e

-- | Calculate the current slot, because the network layer doesn't know it.
getCurrentSlot :: Text -> IO SlotId
getCurrentSlot net = calcSlot <$> startTime net <*> getPOSIXTime
  where
    calcSlot :: POSIXTime -> POSIXTime -> SlotId
    calcSlot start now = SlotId ep idx
      where
        d = now - start
        slotDur = 20
        epochDur = slotDur * 21600
        ep = floor (d / epochDur)
        idx = floor ((d - (fromIntegral ep) * epochDur) / slotDur)

    startTime :: MonadFail m => Text -> m POSIXTime
    startTime "mainnet" = pure 1506203091
    startTime "staging" = pure 1506450213
    startTime "testnet" = pure 1537941600
    startTime n = fail $ "Unknown network name: " ++ T.unpack n

logQuiet :: SlotId -> SlotId -> IO ()
logQuiet _ _ = pure ()
