{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude

import Cardano.Environment.HttpBridge
    ( network )
import Cardano.Launcher
    ( Command (Command), StdStream (..), installSignalHandlers, launch )
import Cardano.Wallet
    ( WalletLayer (..), mkWalletLayer, unsafeRunExceptT )
import Cardano.Wallet.Compatibility.HttpBridge
    ( HttpBridge )
import Cardano.Wallet.Network
    ( NetworkLayer (..), networkTip )
import Cardano.Wallet.Network.HttpBridge
    ( newNetworkLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..), digest, generateKeyFromSeed, publicKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( GenChange, IsOwned, SeqState, defaultAddressPoolGap, mkSeqState )
import Cardano.Wallet.Primitive.AddressDiscovery.Any
    ( AnyAddressState, initAnyState )
import Cardano.Wallet.Primitive.Model
    ( totalBalance, totalUTxO )
import Cardano.Wallet.Primitive.Types
    ( SlotId (..)
    , UTxO (..)
    , WalletId (..)
    , WalletName (..)
    , WalletState (..)
    )
import Cardano.Wallet.Transaction.HttpBridge
    ( newTransactionLayer )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.DeepSeq
    ( NFData, rnf )
import Control.Exception
    ( bracket, evaluate, throwIO )
import Control.Monad
    ( forM, mapM_ )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( runExceptT )
import Criterion.Measurement
    ( getTime, initializeTime, secs )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( ToText (..) )
import Data.Time.Clock.POSIX
    ( POSIXTime, getPOSIXTime )
import Fmt
    ( fmt, (+|), (+||), (|+), (||+) )
import Say
    ( sayErr )
import System.Environment
    ( getArgs, setEnv )
import System.IO
    ( BufferMode (..), hSetBuffering, stderr, stdout )

import qualified Cardano.Wallet.DB.MVar as MVar
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
    getArgs >>= overrideEnvironment
    prepareNode
    runBenchmarks
        [ bench ("restore " <> toText network <> " seq")
            (bench_restoration walletSeq)
        , bench ("restore " <> toText network <> " 10% ownership")
            (bench_restoration wallet10p)
        ]
  where
    walletSeq :: (WalletId, WalletName, SeqState HttpBridge)
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

-- FIXME This only exists because somehow, in buildkite, we can't pass ENV var
-- to the haskell executable? There's probably some Nix magic going on and I
-- don't have time for this.
overrideEnvironment :: [String] -> IO ()
overrideEnvironment [ntwrk] =
    setEnv "NETWORK" ntwrk
overrideEnvironment [] =
    return ()
overrideEnvironment _ =
    fail "benchmark expects only one argument to override the '$NETWORK' ENV var"

{-------------------------------------------------------------------------------
                                  Benchmarks
-------------------------------------------------------------------------------}

{-# ANN bench_restoration ("HLint: ignore Use camelCase" :: String) #-}
bench_restoration
    :: (IsOwned s, GenChange s, NFData s, Show s)
    => (WalletId, WalletName, s)
    -> IO ()
bench_restoration (wid, wname, s) = withHttpBridge $ \port -> do
    dbLayer <- MVar.newDBLayer
    networkLayer <- newNetworkLayer port
    let transactionLayer = newTransactionLayer
    (_, bh) <- unsafeRunExceptT $ networkTip networkLayer
    sayErr . fmt $ network ||+ " tip is at " +|| (bh ^. #slotId) ||+ ""
    let w = mkWalletLayer @_ @HttpBridge dbLayer networkLayer transactionLayer
    wallet <- unsafeRunExceptT $ createWallet w wid wname s
    unsafeRunExceptT $ restoreWallet w wallet
    waitForWalletSync w wallet
    (wallet', _) <- unsafeRunExceptT $ readWallet w wid
    sayErr "Wallet restored!"
    sayErr . fmt $ "Balance: " +|| totalBalance wallet' ||+ " lovelace"
    sayErr . fmt $ "UTxO: " +|| Map.size (getUTxO $ totalUTxO wallet') ||+ " entries"
    unsafeRunExceptT $ removeWallet w wid

logChunk :: SlotId -> IO ()
logChunk slot = sayErr . fmt $ "Processing "+||slot||+""

withHttpBridge :: (Int -> IO a) -> IO a
withHttpBridge action = bracket start stop (const (action port))
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

prepareNode :: IO ()
prepareNode = do
    sayErr . fmt $ "Syncing "+|toText network|+" node... "
    sl <- withHttpBridge $ \port -> do
        bridge <- newNetworkLayer port
        waitForNodeSync bridge (toText network) logQuiet
    sayErr . fmt $ "Completed sync of "+|toText network|+" up to "+||sl||+""

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
    :: NetworkLayer IO
    -> Text
    -> (SlotId -> SlotId -> IO ())
    -> IO SlotId
waitForNodeSync bridge networkName logSlot = loop 10
  where
    loop :: Int -> IO SlotId
    loop retries = runExceptT (networkTip bridge) >>= \case
        Right (_, hdr) -> do
            let tipBlockSlot = hdr ^. #slotId
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
