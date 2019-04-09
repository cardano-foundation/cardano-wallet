{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Prelude

import Cardano.CLI
    ( Network (..) )
import Cardano.Launcher
    ( Command (Command), StdStream (..), installSignalHandlers, launch )
import Cardano.Wallet
    ( NewWallet (..), WalletLayer (..), mkWalletLayer, unsafeRunExceptT )
import Cardano.Wallet.Network
    ( NetworkLayer (..), networkTip )
import Cardano.Wallet.Network.HttpBridge
    ( newNetworkLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( mkAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( SlotId (..), WalletName (..) )
import Control.Arrow
    ( left )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.DeepSeq
    ( rnf )
import Control.Exception
    ( bracket, evaluate, throwIO )
import Control.Monad
    ( mapM_ )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( runExceptT )
import Criterion.Measurement
    ( getTime, initializeTime, secs )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Time.Clock.POSIX
    ( POSIXTime, getPOSIXTime )
import Fmt
    ( fmt, (+|), (+||), (|+), (||+) )
import Say
    ( say )
import System.Environment
    ( getArgs )

import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Data.Text as T

-- | Run all available benchmarks. Can accept one argument that is a target
-- network against which benchmarks below should be ran
--
-- (e.g. `--benchmark-arguments "mainnet"`)
main :: IO ()
main = do
    network <- getArgs >>= either fail return . parseArgs
    installSignalHandlers
    prepareNode network
    runBenchmarks
        [ bench ("restore " <> toText network <> " seq")
            (bench_restoration network walletSeq)
        ]

-- | Very simplistic benchmark argument parser. If anything more is ever needed,
-- it's probably a good idea to go for `optparse-application` or similar for a
-- more structured approach to argument parsing.
parseArgs :: [String] -> Either String Network
parseArgs = \case
    [] ->
        Right Testnet
    [h] ->
        left getTextDecodingError $ fromText (T.pack h)
    _ ->
        Left "invalid arguments provided to benchmark suite: I expect a\
            \ single string with the target network (e.g. \"mainnet\")."

runBenchmarks :: [IO (Text, Double)] -> IO ()
runBenchmarks bs = do
    initializeTime
    rs <- sequence bs
    say "\n\nAll results:"
    mapM_ (uncurry printResult) rs

bench :: Text -> IO () -> IO (Text, Double)
bench benchName action = do
    say $ "Running " <> benchName
    start <- getTime
    res <- action
    evaluate (rnf res)
    finish <- getTime
    let dur = finish - start
    printResult benchName dur
    pure (benchName, dur)

printResult :: Text -> Double -> IO ()
printResult benchName dur = say . fmt $ "  "+|benchName|+": "+|secs dur|+""

{-------------------------------------------------------------------------------
                                  Benchmarks
-------------------------------------------------------------------------------}

{-# ANN bench_restoration ("HLint: ignore Use camelCase" :: String) #-}
bench_restoration :: Network -> NewWallet -> IO ()
bench_restoration network nw = withHttpBridge network $ \port -> do
    dbLayer <- MVar.newDBLayer
    networkLayer <- newNetworkLayer networkName port
    (_, bh) <- unsafeRunExceptT $ networkTip networkLayer
    say . fmt $ "Note: the "+|networkName|+" tip is at "+||(bh ^. #slotId)||+""
    let walletLayer = mkWalletLayer dbLayer networkLayer
    wallet <- unsafeRunExceptT $ createWallet walletLayer nw
    processWallet walletLayer logChunk wallet
  where
    networkName = toText network

logChunk :: SlotId -> IO ()
logChunk slot = say . fmt $ "Processing "+||slot||+""

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


baseWallet :: NewWallet
baseWallet = NewWallet (Passphrase "") (Passphrase "")
             (WalletName "") (Passphrase "") gap20
    where Right gap20 = mkAddressPoolGap 20

walletSeq :: NewWallet
walletSeq = baseWallet
    { seed = Passphrase "involve key curtain arrest fortune custom lens marine before material wheel glide cause weapon wrap"
    , name = WalletName "Benchmark Sequential Wallet"
    }

prepareNode :: Network -> IO ()
prepareNode net = do
    say . fmt $ "Syncing "+|toText net|+" node... "
    sl <- withHttpBridge net $ \port -> do
        network <- newNetworkLayer (toText net) port
        waitForNodeSync network (toText net) logQuiet
    say . fmt $ "Completed sync of "+|toText net|+" up to "+||sl||+""

-- | Poll the network tip until it reaches the slot corresponding to the current
-- time.
waitForNodeSync
    :: NetworkLayer IO
    -> Text
    -> (SlotId -> SlotId -> IO ())
    -> IO SlotId
waitForNodeSync network networkName logSlot = loop 10
  where
    loop :: Int -> IO SlotId
    loop retries = runExceptT (networkTip network) >>= \case
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
                     say "Fetching tip failed, retrying shortly..."
                     threadDelay 15000000
                     loop (retries - 1)
               | otherwise -> throwIO e

-- | Calculate the current slot, because the network layer doesn't know it.
getCurrentSlot :: Text -> IO SlotId
getCurrentSlot network = calcSlot <$> startTime network <*> getPOSIXTime
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
