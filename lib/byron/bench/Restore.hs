{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude

import Cardano.BM.Configuration.Static
    ( defaultConfigStdout )
import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Setup
    ( setupTrace_ )
import Cardano.BM.Trace
    ( Trace, nullTracer )
import Cardano.DB.Sqlite
    ( destroyDBLayer, unsafeRunQuery )
import Cardano.Launcher
    ( Command (..), StdStream (..), withBackendProcess )
import Cardano.Startup
    ( installSignalHandlers )
import Cardano.Wallet
    ( WalletLayer (..), WalletLog (..) )
import Cardano.Wallet.Byron.Compatibility
    ( Byron
    , KnownNetwork (blockchainParameters, versionData)
    , emptyGenesis
    , fromByronBlock
    , fromNetworkMagic
    )
import Cardano.Wallet.Byron.Network
    ( withNetworkLayer )
import Cardano.Wallet.Byron.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Byron.Transaction.Size
    ( MaxSizeOf (..) )
import Cardano.Wallet.DB
    ( DBLayer )
import Cardano.Wallet.DB.Sqlite
    ( PersistState, newDBLayer )
import Cardano.Wallet.Logging
    ( trMessageText )
import Cardano.Wallet.Network
    ( FollowLog (..), NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal (..)
    , PersistPrivateKey
    , SomeMnemonic (..)
    , WalletKey
    , digest
    , hex
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs, IsOwned )
import Cardano.Wallet.Primitive.AddressDiscovery.Any
    ( initAnyState )
import Cardano.Wallet.Primitive.AddressDiscovery.Any.TH
    ( migrateAll )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState, mkRndState )
import Cardano.Wallet.Primitive.Model
    ( currentTip, totalBalance, totalUTxO )
import Cardano.Wallet.Primitive.Types
    ( Address
    , Block (..)
    , BlockHeader (..)
    , BlockchainParameters (..)
    , ChimericAccount
    , SlotId (..)
    , SyncProgress (..)
    , UTxO (..)
    , WalletId (..)
    , WalletName (..)
    , mkSyncTolerance
    , slotAt
    , slotParams
    , syncProgressRelativeToTime
    )
import Cardano.Wallet.Unsafe
    ( unsafeMkMnemonic, unsafeRunExceptT )
import Control.Concurrent
    ( forkIO, threadDelay )
import Control.DeepSeq
    ( NFData, rnf )
import Control.Exception
    ( bracket, evaluate, throwIO )
import Control.Monad
    ( forM, mapM_, void )
import Control.Monad.IO.Class
    ( MonadIO (..) )
import Control.Monad.Trans.Except
    ( runExceptT )
import Control.Tracer
    ( Tracer (..), traceWith )
import Criterion.Measurement
    ( getTime, initializeTime, secs )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Time.Clock.POSIX
    ( getCurrentTime, utcTimeToPOSIXSeconds )
import Database.Persist.Sql
    ( runMigrationSilent )
import Fmt
    ( fmt, pretty, (+|), (+||), (|+), (||+) )
import Ouroboros.Network.NodeToClient
    ( NodeToClientVersionData (..) )
import Say
    ( sayErr )
import System.Environment
    ( getEnv )
import System.FilePath
    ( (</>) )
import System.IO
    ( BufferMode (..)
    , IOMode (..)
    , hFlush
    , hSetBuffering
    , stderr
    , stdout
    , withFile
    )
import System.IO.Temp
    ( withSystemTempFile )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.BM.Data.BackendKind as CM
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


-- | Run all available benchmarks. Can accept one argument that is a target
-- network against which benchmarks below should be ran
--
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    (_logCfg, tr) <- initBenchmarkLogging Info
    installSignalHandlers (return ())

    -- TODO: We might want to support testnet via a
    -- `--benchmark-arguments "testnet"`
    let network = Mainnet

    -- Reading setup from env vars.
    --
    -- If you usually run ./scripts/mainnet.sh in the cardano-node repository,
    -- it can be useful to set
    --
    --     export BYRON_CONFIGS=$CARDANO_NODE_DIR/configuration
    --     export NODE_DB=$CARDANO_NODE_DIR/db
    --
    -- before running this benchmark manually with stack.
    configs <- getEnv "BYRON_CONFIGS"
    nodeDB <- getEnv "NODE_DB"

    let genesisHash =
            B8.unpack
            . hex
            . getGenesisBlockHash
            $ blockchainParameters @'Mainnet
    let socketPath = "cardano-node.socket"
    let args =
            [ "run"
            , "--database-path", nodeDB
            , "--genesis-file", configs </> "mainnet-genesis.json"
            , "--genesis-hash", genesisHash
            , "--topology", configs </> "mainnet-topology.json"
            , "--socket-path", socketPath
            , "--config", configs </> "configuration-mainnet.yaml"
            , "--port", "7776"
            ]
    -- NOTE: It is warned against using @NoStream@ to supress output on POSIX
    -- systems, since it could lead to the process throwing errors.
    -- It appears to work fine stopping the live view of cardano-node,
    -- however.
    let cmd = Command "cardano-node" args (return ()) NoStream NoStream
    void $ withBackendProcess nullTracer cmd $ do
        case network of
            Testnet _ -> error "Testnet: not supported yet"
            Mainnet -> do
                prepareNode (Proxy @'Mainnet) socketPath
                runBenchmarks
                    [ bench ("restore " <> "mainnet" <> " seq")
                        (bench_restoration @'Mainnet @ByronKey
                            tr
                            socketPath
                            "mainnet-seq.dat"
                            (walletRnd @'Mainnet))

                    , bench ("restore " <> "mainnet" <> " 1% ownership")
                        (bench_restoration @'Mainnet @IcarusKey
                            tr
                            socketPath
                            "mainnet-1-percent.dat"
                            (initAnyState "Benchmark 1% Wallet" 0.01))

                    , bench ("restore " <> "mainnet" <> " 2% ownership")
                        (bench_restoration @'Mainnet @IcarusKey
                            tr
                            socketPath
                            "mainnet-2-percent.dat"
                            (initAnyState "Benchmark 2% Wallet" 0.02))
                    ]
  where
    walletRnd
        :: (WalletId, WalletName, RndState n)
    walletRnd =
        let
            seed = SomeMnemonic . unsafeMkMnemonic @15 $ T.words
                "involve key curtain arrest fortune custom lens marine before \
                \material wheel glide cause weapon wrap"
            xprv = Byron.generateKeyFromSeed seed mempty
            wid = WalletId $ digest $ publicKey xprv
            wname = WalletName "Benchmark Sequential Wallet"
            rngSeed = 0
            s = mkRndState xprv rngSeed
        in
            (wid, wname, s)

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
    :: forall (n :: NetworkDiscriminant) (k :: Depth -> * -> *) s t.
        ( IsOurs s Address
        , IsOurs s ChimericAccount
        , IsOwned s k
        , WalletKey k
        , NFData s
        , Show s
        , MaxSizeOf Address n k
        , PersistState s
        , PersistPrivateKey (k 'RootK)
        , KnownNetwork n
        , NetworkDiscriminantVal n
        , t ~ IO Byron
        )
    => Trace IO Text
    -> FilePath
       -- ^ Socket path
    -> FilePath
       -- ^ Log output
    -> (WalletId, WalletName, s)
    -> IO ()
bench_restoration tracer socketPath progressLogFile (wid, wname, s) = do
    let bp = blockchainParameters @n
    let vData = versionData @n
    let networkText = networkDiscriminantVal @n
    let pm = fromNetworkMagic $ networkMagic $ fst vData
    let tl = newTransactionLayer @n @k @(IO Byron) (Proxy) pm
    withNetworkLayer nullTracer bp socketPath vData $ \nw' -> do
        let convert = fromByronBlock (getGenesisBlockHash bp) (getEpochLength bp)
        let nw = convert <$> nw'
        withBenchDBLayer @s @k tracer $ \db -> do
            BlockHeader sl _ _ _ <- unsafeRunExceptT $ currentNodeTip nw
            sayErr . fmt $ networkText ||+ " tip is at " +|| sl ||+ ""

            withFile progressLogFile WriteMode $ \h -> do
                -- Use a custom tracer to output (time, blockHeight) to a file
                -- each time we apply blocks.
                let fileTr = Tracer $ \msg -> do
                        liftIO . B8.hPut h . T.encodeUtf8 . (<> "\n") $ msg
                        hFlush h
                let w = WalletLayer
                        (traceProgressForPlotting fileTr)
                        (emptyGenesis bp, bp, mkSyncTolerance 3600)
                        nw
                        tl
                        db
                wallet <- unsafeRunExceptT $ W.createWallet w wid wname s
                void $ forkIO $ unsafeRunExceptT $ W.restoreWallet @_ @s @t @k w wid
                waitForWalletSync @n w wallet
                (wallet', _, pending) <- unsafeRunExceptT $ W.readWallet w wid
                sayErr "Wallet restored!"
                sayErr . fmt $ "Balance: " +|| totalBalance pending wallet' ||+ " lovelace"
                sayErr . fmt $
                    "UTxO: " +|| Map.size (getUTxO $ totalUTxO pending wallet') ||+ " entries"
                unsafeRunExceptT $ W.deleteWallet w wid

traceProgressForPlotting :: Tracer IO Text -> Tracer IO WalletLog
traceProgressForPlotting tr = Tracer $ \case
    MsgFollow (MsgApplyBlocks bs) -> do
        let tip = pretty . getQuantity . blockHeight . NE.last $ bs
        time <- pretty . utcTimeToPOSIXSeconds <$> getCurrentTime
        traceWith tr (time <> " " <> tip)
    _ -> return ()

withBenchDBLayer
    :: forall s k a.
        ( IsOwned s k
        , NFData s
        , Show s
        , PersistState s
        , IsOurs s ChimericAccount
        , IsOurs s Address
        , PersistPrivateKey (k 'RootK)
        )
    => Trace IO Text
    -> (DBLayer IO s k -> IO a)
    -> IO a
withBenchDBLayer tr action =
    withSystemTempFile "bench.db" $ \dbFile _ -> do
        let before = newDBLayer (trMessageText tr) migrationDefaultValues (Just dbFile)
        let after = destroyDBLayer . fst
        bracket before after $ \(ctx, db) -> do
            migrateDB ctx
            action db
  where
    migrationDefaultValues = Sqlite.DefaultFieldValues
        { Sqlite.defaultActiveSlotCoefficient = 1
        }

    -- This tweaks the DB support the AnyAddressState.
    migrateDB ctx = unsafeRunQuery ctx (void $ runMigrationSilent migrateAll)


logChunk :: SlotId -> IO ()
logChunk slot = sayErr . fmt $ "Processing "+||slot||+""

prepareNode
    :: forall n. (KnownNetwork n, NetworkDiscriminantVal n)
    => Proxy n
    -> FilePath
    -> IO ()
prepareNode _ socketPath = do
    sayErr . fmt $ "Syncing "+|networkDiscriminantVal @n|+" node... "
    let bp = blockchainParameters @n
    let vData = versionData @n
    sl <- withNetworkLayer nullTracer bp socketPath vData $ \nw' -> do
        let convert = fromByronBlock (getGenesisBlockHash bp) (getEpochLength bp)
        let nw = convert <$> nw'
        waitForNodeSync @n nw logQuiet
    sayErr . fmt $ "Completed sync of "+|networkDiscriminantVal @n|+" up to "+||sl||+""

-- | Regularly poll the wallet to monitor it's syncing progress. Block until the
-- wallet reaches 100%.
waitForWalletSync
    :: forall (n :: NetworkDiscriminant) s t k. (KnownNetwork n)
    => WalletLayer s t k
    -> WalletId
    -> IO ()
waitForWalletSync walletLayer wid = do
    (w, _, _) <- unsafeRunExceptT $ W.readWallet walletLayer wid
    let tol = mkSyncTolerance 3600
    let bp = blockchainParameters @n
    prog <- syncProgressRelativeToTime
                tol
                (slotParams bp)
                (currentTip w)
                <$> getCurrentTime
    case prog of
        Ready -> return ()
        NotResponding -> do
            threadDelay 1000000
            waitForWalletSync @n walletLayer wid
        Syncing (Quantity p) -> do
            sayErr . fmt $ "[INFO] restoring: "+|p|+""
            threadDelay 1000000
            waitForWalletSync @n walletLayer wid

-- | Poll the network tip until it reaches the slot corresponding to the current
-- time.
waitForNodeSync
    :: forall (n :: NetworkDiscriminant). (KnownNetwork n)
    => NetworkLayer IO (IO Byron) Block
    -> (SlotId -> SlotId -> IO ())
    -> IO SlotId
waitForNodeSync nw logSlot = loop 10
  where
    loop :: Int -> IO SlotId
    loop retries = runExceptT (currentNodeTip nw) >>= \case
        Right (BlockHeader tipBlockSlot _ _ _) -> do
            currentSlot <- getCurrentSlot
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

    getCurrentSlot :: IO SlotId
    getCurrentSlot = do
        let sp = slotParams (blockchainParameters @n)
        fromMaybe (error errMsg) . slotAt sp <$> getCurrentTime
      where
        errMsg = "getCurrentSlot: is the current time earlier than the\
                 \start time of the blockchain"

logQuiet :: SlotId -> SlotId -> IO ()
logQuiet _ _ = pure ()
