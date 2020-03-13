{-# LANGUAGE CPP #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Unit tests for 'withShutdownHandler' using pipes within a single process.

module Cardano.StartupSpec
    ( spec
    ) where

import Prelude

import Cardano.Startup
    ( ShutdownHandlerLog (..), withShutdownHandler' )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race )
import Control.Exception
    ( bracket, throwIO )
import Control.Tracer
    ( Tracer, nullTracer )
import System.IO
    ( Handle, IOMode (..), hClose, hWaitForInput, stdin, withFile )
import System.IO.Error
    ( isUserError )
import System.Process
    ( createPipe )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldContain, shouldReturn, shouldThrow )
import Test.Utils.Trace
    ( captureLogging )
import Test.Utils.Windows
    ( nullFileName, pendingOnWindows )

#if defined(mingw32_HOST_OS)
import Control.Concurrent
    ( forkIO )
import Control.Concurrent.MVar
    ( MVar, newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( IOException, catch )
#endif

import qualified Data.ByteString as BS

spec :: Spec
spec = describe "withShutdownHandler" $ do
    let decisecond = 100000

    describe "sanity tests" $ do
        -- check file handle for input, allowing interruptions on windows
        -- example
        -- https://github.com/input-output-hk/ouroboros-network/blob/69d62063e59f966dc90bda5b4d0ac0a11efd3657/Win32-network/src/System/Win32/Async/Socket.hs#L46-L59
        let hWait :: Handle -> IO Bool
#if defined(mingw32_HOST_OS)
            hWait h = do
                v <- newEmptyMVar :: IO (MVar (Either IOException Bool))
                _ <- forkIO ((hWaitForInput h (-1) >>= putMVar v . Right) `catch` (putMVar v . Left))
                takeMVar v >>= either throwIO pure
#else
            hWait h = hWaitForInput h (-1)
#endif
        it "race hWaitForInput stdin" $ do
            res <- race (hWait stdin) (threadDelay decisecond)
            res `shouldBe` Right ()

        it "race hWaitForInput pipe" $ withPipe $ \(a, _) -> do
            res <- race (hWait a) (threadDelay decisecond)
            res `shouldBe` Right ()

        let getChunk :: Handle -> IO BS.ByteString
            getChunk h = BS.hGet h 1000

        it "race stdin" $ do
            pendingOnWindows "deadlocks on windows"
            res <- race (getChunk stdin) (threadDelay decisecond)
            res `shouldBe` Right ()

        it "race pipe" $ withPipe $ \(a, _) -> do
            pendingOnWindows "deadlocks on windows"
            res <- race (getChunk a) (threadDelay decisecond)
            res `shouldBe` Right ()

    it "action completes immediately" $ withPipe $ \(a, _) -> do
        logs <- captureLogging' $ \tr -> do
            withShutdownHandler' tr a (pure ())
                `shouldReturn` Just ()
        logs `shouldContain` [MsgShutdownHandler True]

    it "action completes with delay" $ withPipe $ \(a, _) -> do
        res <- withShutdownHandler' nullTracer a $ do
            threadDelay decisecond
            pure ()
        res `shouldBe` Just ()

    it "handle is closed immediately" $ withPipe $ \(a, b) -> do
        logs <- captureLogging' $ \tr -> do
            res <- withShutdownHandler' tr a $ do
                hClose b
                threadDelay decisecond -- give handler a chance to run
                pure ()
            res `shouldBe` Nothing
        logs `shouldContain` [MsgShutdownEOF]

    it "handle is closed with delay" $ withPipe $ \(a, b) -> do
        res <- withShutdownHandler' nullTracer a $ do
            threadDelay decisecond
            hClose b
            threadDelay decisecond -- give handler a chance to run
            pure ()
        res `shouldBe` Nothing

    it "action throws exception" $ withPipe $ \(a, _) -> do
        let bomb = userError "bomb"
        logs <- captureLogging' $ \tr -> do
            withShutdownHandler' tr a (throwIO bomb)
                `shouldThrow` isUserError
        logs `shouldBe` [MsgShutdownHandler True]

    it ("handle is " ++ nullFileName ++ " (immediate EOF)") $ do
        pendingOnWindows $ "Can't open " ++ nullFileName ++ " for reading"
        logs <- captureLogging' $ \tr ->
            withFile nullFileName ReadMode $ \h -> do
                res <- withShutdownHandler' tr h $ do
                    threadDelay decisecond -- give handler a chance to run
                    pure ()
                res `shouldBe` Nothing
        logs `shouldContain` [MsgShutdownEOF]

    it "handle is already closed" $ withPipe $ \(a, b) -> do
        hClose a
        hClose b
        logs <- captureLogging' $ \tr -> do
            res <- withShutdownHandler' tr a $ do
                threadDelay decisecond
                hClose b
                threadDelay decisecond -- give handler a chance to run
                pure ()
            res `shouldBe` Just ()
        logs `shouldContain` [MsgShutdownHandler False]

withPipe :: ((Handle, Handle) -> IO a) -> IO a
withPipe = bracket createPipe closePipe
    where closePipe (a, b) = hClose b >> hClose a

captureLogging' :: (Tracer IO msg -> IO a) -> IO [msg]
captureLogging' = fmap fst . captureLogging
