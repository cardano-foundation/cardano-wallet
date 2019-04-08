{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Prelude

import Cardano.Launcher
    ( Command (Command), StdStream (..), installSignalHandlers, launch )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.DeepSeq
    ( rnf )
import Control.Exception
    ( Exception, bracket, evaluate, throwIO )
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
import Data.Time.Clock.POSIX
    ( POSIXTime, getPOSIXTime )
import Fmt
    ( fmt, (+|), (+||), (|+), (||+) )
import Say
    ( say )
import System.Exit
    ( die )

import Cardano.Wallet
    ( NewWallet (..), WalletLayer (..), mkWalletLayer )
import Cardano.Wallet.Network
    ( NetworkLayer, networkTip )
import Cardano.Wallet.Network.HttpBridge
    ( newNetworkLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( mkAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( SlotId (..), WalletName (..) )

import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Data.Text as T

main :: IO ()
main = do
    installSignalHandlers
    mapM_ prepareNode ["testnet", "mainnet"]
    runBenchmarks
        [ bench "restore - testnet - walletRnd" $ test1 "testnet" walletRnd
        , bench "restore - mainnet - walletRnd" $ test1 "mainnet" walletRnd
        , bench "restore - testnet - walletSeq" $ test1 "testnet" walletSeq
        , bench "restore - mainnet - walletSeq" $ test1 "mainnet" walletSeq
        ]

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


test1 :: Text -> NewWallet -> IO ()
test1 netName nw = withHttpBridge netName $ \port -> do
    db <- MVar.newDBLayer
    network <- newNetworkLayer netName port
    runExceptT (networkTip network) >>= \case
        Right (_, bh) -> say . fmt $ "Note: the "+|netName|+" tip is at "+||(bh ^. #slotId)||+""
        Left err -> die .fmt $ "Could not get the network tip. Is the node backend running?\n"+||err||+""
    let testWalletLayer = mkWalletLayer db network
    res <- runExceptT $ createWallet testWalletLayer nw
    case res of
        Right wal -> processWallet testWalletLayer logChunk wal
        Left err -> die $ show err

logChunk :: SlotId -> IO ()
logChunk slot = say . fmt $ "Processing "+||slot||+""

withHttpBridge :: Text -> (Int -> IO a) -> IO a
withHttpBridge netName action = bracket start stop (const (action port))
    where
        port = 8002
        start = do
            handle <- async $ launch
                [ Command "cardano-http-bridge"
                    [ "start"
                    , "--port", show port
                    , "--template", T.unpack netName
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

walletRnd :: NewWallet
walletRnd = baseWallet
    { seed = Passphrase "skull skin weird piece oak absorb apart above female dial drink traffic"
    , name = WalletName "Benchmark Daedalus Wallet"
    }

walletSeq :: NewWallet
walletSeq = baseWallet
    { seed = Passphrase "involve key curtain arrest fortune custom lens marine before material wheel glide cause weapon wrap"
    , name = WalletName "Benchmark Yoroi Wallet"
    }

prepareNode :: Text -> IO ()
prepareNode netName = do
    say . fmt $ "Syncing "+|netName|+" node... "
    sl <- withHttpBridge netName $ \port -> do
        network <- newNetworkLayer netName port
        waitForNodeSync network netName logQuiet
    say . fmt $ "Completed sync of "+|netName|+" up to "+||sl||+""

-- | Poll the network tip until it reaches the slot corresponding to the current
-- time.
waitForNodeSync
    :: Exception e1
    => NetworkLayer IO e0 e1
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
