{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
    ( ShutdownHandlerLog (..)
    , withShutdownHandler'
    )
import Control.Monad
    ( unless
    )
import Control.Tracer
    ( Tracer
    , nullTracer
    )
import System.IO
    ( Handle
    , IOMode (..)
    , hClose
    , hWaitForInput
    , stdin
    , withFile
    )
import System.IO.Error
    ( isUserError
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldContain
    , shouldReturn
    , shouldThrow
    )
import Test.Hspec.Core.Spec
    ( ResultStatus (..)
    )
import Test.Hspec.Expectations
    ( Expectation
    , HasCallStack
    )
import Test.Utils.Platform
    ( nullFileName
    , pendingOnWindows
    )
import Test.Utils.Trace
    ( captureLogging
    )
import UnliftIO.Async
    ( race
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.Exception
    ( IOException
    , bracket
    , catch
    , throwIO
    )
import UnliftIO.Process
    ( createPipe
    )

#if defined(WINDOWS)
import UnliftIO.Concurrent
    ( forkIO )
import UnliftIO.MVar
    ( MVar, newEmptyMVar, putMVar, takeMVar )
#endif

import qualified Data.ByteString as BS

spec :: Spec
spec = describe "withShutdownHandler" $ do
    let decisecond = 100000

    describe "sanity tests" $ do
        -- check file handle for input,
        let hWait :: Handle -> IO Bool
            hWait h = hWaitForInput h (-1)

        -- read a small amount from a file handle
        let getChunk :: Handle -> IO BS.ByteString
            getChunk h = BS.hGet h 1000

        it "race hWaitForInput stdin" $ do
            skipWhenNullStdin
            res <- race (wrapIO $ hWait stdin) (threadDelay decisecond)
            res `shouldBe` Right ()

        it "race hWaitForInput pipe" $ withPipe $ \(a, _) -> do
            res <- race (wrapIO $ hWait a) (threadDelay decisecond)
            res `shouldBe` Right ()

        it "race hGet stdin" $ do
            pendingOnWindows "deadlocks on windows"
            skipWhenNullStdin
            res <- race (getChunk stdin) (threadDelay decisecond)
            res `shouldBe` Right ()

        it "race hGet pipe" $ withPipe $ \(a, _) -> do
            pendingOnWindows "deadlocks on windows"
            res <- race (getChunk a) (threadDelay decisecond)
            res `shouldBe` Right ()

        it "race hGet stdin wrapped" $ do
            skipWhenNullStdin
            res <- race (wrapIO $ getChunk stdin) (threadDelay decisecond)
            res `shouldBe` Right ()

        it "race hGet pipe wrapped" $ withPipe $ \(a, _) -> do
            res <- race (wrapIO $ getChunk a) (threadDelay decisecond)
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
  where
    closePipe (a, b) = hClose b >> hClose a

captureLogging' :: (Tracer IO msg -> IO a) -> IO [msg]
captureLogging' = fmap fst . captureLogging

-- | Run an IO action allowing interruptions on windows.
-- Any 'IOException's are rethrown.
-- The action should not throw any exception other than 'IOException'.
-- Example: https://github.com/input-output-hk/ouroboros-network/blob/69d62063e59f966dc90bda5b4d0ac0a11efd3657/Win32-network/src/System/Win32/Async/Socket.hs#L46-L59
wrapIO :: IO a -> IO a
#if defined(WINDOWS)
wrapIO action = do
    v <- newEmptyMVar :: IO (MVar (Either IOException a))
    _ <- forkIO ((action >>= putMVar v . Right) `catch` (putMVar v . Left))
    takeMVar v >>= either throwIO pure
#else
wrapIO = id
#endif

-- | Detect environment where 'stdin' is set to =/dev/null= and skip test.
skipWhenNullStdin :: (HasCallStack) => Expectation
skipWhenNullStdin = do
    let onError :: IOException -> IO Bool
        onError _ = pure False
    usableStdin <- hWaitForInput stdin 50 `catch` onError
    unless usableStdin $ throwIO Success
