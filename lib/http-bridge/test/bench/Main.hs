{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Setup
    ( setupTrace_ )
import Cardano.BM.Trace
    ( Trace )
import Cardano.Launcher
    ( StdStream (..), installSignalHandlers )
import Cardano.Wallet
    ( WalletLayer (..) )
import Cardano.Wallet.DB
    ( DBLayer )
import Cardano.Wallet.DB.Sqlite
    ( PersistState, PersistTx )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, byronBlockchainParameters )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.HttpBridge.Network
    ( HttpBridgeBackend (..), HttpBridgeConfig (..), withNetworkLayer )
import Cardano.Wallet.HttpBridge.Primitive.Types
    ( Tx )
import Cardano.Wallet.HttpBridge.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Network
    ( NetworkLayer (..), networkTip )
import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..), Passphrase (..), PersistKey, digest, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey, generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs, IsOwned )
import Cardano.Wallet.Primitive.AddressDiscovery.Any
    ( AnyAddressState, initAnyState )
import Cardano.Wallet.Primitive.AddressDiscovery.Any.TH
    ( migrateAll )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState, defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..), totalBalance, totalUTxO )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DefineTx
    , SlotId (..)
    , StartTime (..)
    , UTxO (..)
    , WalletId (..)
    , WalletName (..)
    , WalletState (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Concurrent
    ( threadDelay )
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
    ( POSIXTime, getPOSIXTime, utcTimeToPOSIXSeconds )
import Database.Persist.Sql
    ( runMigrationSilent )
import Fmt
    ( fmt, (+|), (+||), (|+), (||+) )
import Say
    ( sayErr, sayErrShow )
import System.Environment
    ( getArgs )
import System.Exit
    ( exitFailure )
import System.IO
    ( BufferMode (..), hSetBuffering, stderr, stdout )
import System.IO.Temp
    ( withSystemTempFile )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import qualified Cardano.Wallet as W
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
    (logCfg, tr) <- initBenchmarkLogging Info
    installSignalHandlers tr
    network <- getArgs >>= parseNetwork
    case network of
        Testnet -> do
            prepareNode (Proxy @'Testnet) tr
            runBenchmarks
                [ bench ("restore " <> toText network <> " seq")
                    (bench_restoration @'Testnet @SeqKey (logCfg, tr) (walletSeq @'Testnet))
                , bench ("restore " <> toText network <> " 10% ownership")
                    (bench_restoration @'Testnet @SeqKey (logCfg, tr) wallet10p)
                ]
        Mainnet -> do
            prepareNode (Proxy @'Mainnet) tr
            runBenchmarks
                [ bench ("restore " <> toText network <> " seq")
                    (bench_restoration @'Mainnet @SeqKey (logCfg, tr) (walletSeq @'Mainnet))
                , bench ("restore " <> toText network <> " 10% ownership")
                    (bench_restoration @'Mainnet @SeqKey (logCfg, tr) wallet10p)
                ]
  where
    walletSeq
        :: KeyToAddress (HttpBridge n) SeqKey
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
        fail
            "invalid network provided to benchmark: \
            \not 'testnet' nor 'mainnet'."

initBenchmarkLogging :: Severity -> IO (CM.Configuration, Trace IO Text)
initBenchmarkLogging minSeverity = do
    c <- defaultConfigStdout
    CM.setMinSeverity c minSeverity
    CM.setSetupBackends c [CM.KatipBK, CM.AggregationBK]
    (tr, _sb) <- setupTrace_ c "bench-restore"
    pure (c, tr)

{-------------------------------------------------------------------------------
                                  Benchmarks
-------------------------------------------------------------------------------}

{-# ANN bench_restoration ("HLint: ignore Use camelCase" :: String) #-}
bench_restoration
    :: forall (n :: Network) k s t.
        ( IsOwned s k
        , NFData s
        , Show s
        , PersistState s
        , KnownNetwork n
        , t ~ HttpBridge n
        , KeyToAddress t k
        , PersistKey k
        )
    => (CM.Configuration, Trace IO Text)
    -> (WalletId, WalletName, s)
    -> IO ()
bench_restoration (logConfig, tracer) (wid, wname, s) =
    withBenchNetworkLayer @n tracer $ \nw -> do
        withBenchDBLayer logConfig tracer $ \db -> do
            BlockHeader sl _ _ <- unsafeRunExceptT $ networkTip nw
            sayErr . fmt $ network ||+ " tip is at " +|| sl ||+ ""
            let g0 = staticBlockchainParameters nw
            let w = WalletLayer tracer g0 nw tl db
            wallet <- unsafeRunExceptT $ W.createWallet w wid wname s
            waitForWalletSync w wallet
            (wallet', _, pending) <- unsafeRunExceptT $ W.readWallet w wid
            sayErr "Wallet restored!"
            sayErr . fmt $ "Balance: " +|| totalBalance pending wallet' ||+ " lovelace"
            sayErr . fmt $
                "UTxO: " +|| Map.size (getUTxO $ totalUTxO pending wallet') ||+ " entries"
            unsafeRunExceptT $ W.removeWallet w wid
  where
    tl = newTransactionLayer @n @k
    network = networkVal @n

withBenchNetworkLayer
    :: forall n a. KnownNetwork n
    => Trace IO Text
    -> (NetworkLayer IO Tx (Block Tx) -> IO a)
    -> IO a
withBenchNetworkLayer tr action =
    withNetworkLayer @n tr (Launch cfg) $ \case
        Right (_, nw) -> action nw
        Left e -> do
            sayErr "There was some error starting the network layer:"
            sayErrShow e
            exitFailure
  where
    cfg = HttpBridgeConfig
          { _networkName = Right (networkVal @n)
          , _restApiPort = Nothing
          , _extraArgs = []
          , _outputStream = Inherit
           -- Use hermes default for chain storage, will persist between nightly
           -- runs.
          , _stateDir = Nothing
          }

withBenchDBLayer
    :: forall s t k a. (IsOurs s, NFData s, Show s, PersistState s, PersistTx t, PersistKey k)
    => CM.Configuration
    -> Trace IO Text
    -> (DBLayer IO s t k -> IO a)
    -> IO a
withBenchDBLayer logConfig tr action =
    withSystemTempFile "bench.db" $ \dbFile _ ->
        bracket (before dbFile) after between
  where
    before dbFile = Sqlite.newDBLayer logConfig tr (Just dbFile)
    after = Sqlite.destroyDBLayer . fst
    between (ctx, db) = do
        Sqlite.unsafeRunQuery ctx (void $ runMigrationSilent migrateAll)
        action db

logChunk :: SlotId -> IO ()
logChunk slot = sayErr . fmt $ "Processing "+||slot||+""

prepareNode :: forall n. KnownNetwork n => Proxy n -> Trace IO Text -> IO ()
prepareNode _ tr = do
    sayErr . fmt $ "Syncing "+|toText network|+" node... "
    sl <- withBenchNetworkLayer @n tr $ \bridge ->
        waitForNodeSync bridge (toText network) logQuiet
    sayErr . fmt $ "Completed sync of "+|toText network|+" up to "+||sl||+""
  where
    network = networkVal @n

-- | Regularly poll the wallet to monitor it's syncing progress. Block until the
-- wallet reaches 100%.
waitForWalletSync
    :: DefineTx t
    => WalletLayer s t k
    -> WalletId
    -> IO ()
waitForWalletSync walletLayer wid = do
    (_, meta, _) <- unsafeRunExceptT $ W.readWallet walletLayer wid
    case meta ^. #status of
        Ready -> return ()
        Restoring (Quantity p) -> do
            sayErr . fmt $ "[INFO] restoring: "+||p||+"%"
            threadDelay 1000000
            waitForWalletSync walletLayer wid

-- | Poll the network tip until it reaches the slot corresponding to the current
-- time.
waitForNodeSync
    :: NetworkLayer IO Tx (Block Tx)
    -> Text
    -> (SlotId -> SlotId -> IO ())
    -> IO SlotId
waitForNodeSync bridge networkName logSlot = loop 10
  where
    loop :: Int -> IO SlotId
    loop retries = runExceptT (networkTip bridge) >>= \case
        Right (BlockHeader tipBlockSlot _ _) -> do
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
    startTime "mainnet" = pure
        $ (\(StartTime t) -> utcTimeToPOSIXSeconds t)
        $ getGenesisBlockDate
        $ byronBlockchainParameters @'Mainnet
    startTime "testnet" = pure
        $ (\(StartTime t) -> utcTimeToPOSIXSeconds t)
        $ getGenesisBlockDate
        $ byronBlockchainParameters @'Testnet
    startTime n =
        fail $ "Unknown network name: " ++ T.unpack n

logQuiet :: SlotId -> SlotId -> IO ()
logQuiet _ _ = pure ()
