{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

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
    ( ShutdownHandlerLog (..), withShutdownHandler, wrapUninterruptableIO )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, tryTakeMVar )
import Control.Exception
    ( IOException, bracket, catch, throwIO, try )
import Control.Monad
    ( unless )
import Control.Tracer
    ( Tracer, nullTracer )
import Data.Maybe
    ( mapMaybe )
import GHC.IO.FD
    ( FD (..) )
import System.IO
    ( Handle, IOMode (..), hClose, hGetChar, hWaitForInput, openFile, stdin )
import System.IO.Error
    ( catchIOError, isUserError )
import System.IO.Temp
    ( withSystemTempFile )
import System.Posix.Types
    ( Fd (Fd) )
import System.Process
    ( createPipe )
import Test.Hspec
    ( Spec
    , before_
    , describe
    , it
    , shouldBe
    , shouldContain
    , shouldReturn
    , shouldSatisfy
    , shouldThrow
    )
import Test.Hspec.Core.Spec
    ( ResultStatus (..) )
import Test.Hspec.Expectations
    ( Expectation, HasCallStack )
import Test.Utils.Trace
    ( captureLogging )
import Test.Utils.Windows
    ( nullFileName, pendingOnWindows )

import qualified Data.ByteString.Char8 as B8
import qualified GHC.IO.Handle.FD as IO

spec :: Spec
spec = do
    sanitySpec
    withShutdownHandlerSpec

sanitySpec :: Spec
sanitySpec = describe "IO sanity tests" $ do
    -- check file handle for input,
    let hWait :: Handle -> IO Bool
        hWait h = hWaitForInput h (-1)

    -- read a small amount from a file handle
    let getChunk :: Handle -> IO B8.ByteString
        getChunk h = B8.hGet h 1000

    let tryGetChar :: Handle -> IO (Either IOException Char)
        tryGetChar h = try $ hGetChar h

    it "race hWaitForInput stdin" $ do
        skipWhenNullStdin
        res <- race (wrapUninterruptableIO $ hWait stdin) pause
        res `shouldBe` Right ()

    it "race hWaitForInput pipe" $ withPipe $ \(a, _) -> do
        res <- race (wrapUninterruptableIO $ hWait a) pause
        res `shouldBe` Right ()

    it "race hGet stdin" $ do
        pendingOnWindows "deadlocks on windows"
        skipWhenNullStdin
        res <- race (getChunk stdin) pause
        res `shouldBe` Right ()

    it "race hGet pipe" $ withPipe $ \(a, _) -> do
        pendingOnWindows "deadlocks on windows"
        res <- race (getChunk a) pause
        res `shouldBe` Right ()

    it "race hGet stdin wrapped" $ do
        skipWhenNullStdin
        res <- race (wrapUninterruptableIO $ getChunk stdin) pause
        res `shouldBe` Right ()

    it "race hGet pipe wrapped" $ withPipe $ \(a, _) -> do
        res <- race (wrapUninterruptableIO $ getChunk a) pause
        res `shouldBe` Right ()

    it "race hGetChar" $ withPipe $ \(a, _) ->
        race (wrapUninterruptableIO $ tryGetChar a) pause
            `shouldReturn` Right ()

withShutdownHandlerSpec :: Spec
withShutdownHandlerSpec = describe "withShutdownHandler" $ before_ pause $ do
    it "action completes immediately" $ withFdPipe $ \(a, _) -> do
        logs <- captureLogging' $ \tr -> do
            withShutdownHandler tr (Just a) (pure ())
                `shouldReturn` Just ()
        logs `shouldContain` [MsgShutdownHandlerEnabled a]

    it "action completes with delay" $ withFdPipe $ \(a, _) ->
        withShutdownHandler nullTracer (Just a) pause
           `shouldReturn` Just ()

    it "handle is closed immediately" $ withFdPipe $ \(a, b) -> do
        logs <- captureLogging' $ \tr -> do
            res <- withShutdownHandler tr (Just a) $ do
                hCloseAny b
                pause -- give handler a chance to run
                pure ()
            res `shouldBe` Nothing
        logs `shouldContain` [MsgShutdownEOF]

    it "handle is closed with delay" $ withFdPipe $ \(a, b) -> do
        run <- newEmptyMVar
        res <- withShutdownHandler nullTracer (Just a) $ do
            pause -- the delay
            putMVar run True
            hCloseAny b
            pause -- give handler a chance to run
        res `shouldBe` Nothing
        tryTakeMVar run `shouldReturn` Just True

    it "action throws exception" $ withFdPipe $ \(a, _) -> do
        let bomb = userError "bomb"
        logs <- captureLogging' $ \tr -> do
            withShutdownHandler tr (Just a) (throwIO bomb :: IO ())
                `shouldThrow` isUserError
        logs `shouldContain` [MsgShutdownHandlerEnabled a]

    it ("handle is " ++ nullFileName ++ " (immediate EOF)") $ do
        pendingOnWindows $ "Can't open " ++ nullFileName ++ " for reading"
        let withNullFile = bracket (openFile nullFileName ReadMode) hCloseAny
        logs <- captureLogging' $ \tr ->
            withNullFile $ \h -> do
                fd <- handleToFd h
                res <- withShutdownHandler tr (Just fd)
                    pause -- give handler a chance to run
                res `shouldBe` Nothing
        logs `shouldContain` [MsgShutdownEOF]

    it "other end is already closed" $ withPipe $ \(a, b) -> do
        fd <- handleToFd a
        hCloseAny b
        logs <- captureLogging' $ \tr -> do
            res <- withShutdownHandler tr (Just fd)
                pause -- give handler a chance to run
            res `shouldBe` Nothing
        logs `shouldContain` [MsgShutdownHandlerEnabled fd]
        last logs
            `shouldSatisfy`
            (\msg -> msg == MsgShutdownEOF || isMsgShutdownError msg)

    it "incorrect fd given (already closed)" $ do
        bad <- withSystemTempFile "StartupSpec" (const handleToFd)
        logs <- captureLogging' $ \tr -> do
            res <- withShutdownHandler tr (Just bad) pause
            res `shouldBe` Nothing
        mapMaybe isMsgBadFd logs `shouldBe` [bad]

pause :: IO ()
pause = threadDelay decisecond
  where
    -- enough time to give other thread a chance to run
    decisecond :: Int
    decisecond = 100000

withPipe :: ((Handle, Handle) -> IO a) -> IO a
withPipe = bracket createPipe closePipe
  where
    closePipe (a, b) = hCloseAny b >> hCloseAny a

-- | Close a file descriptor, ignoring errors.
hCloseAny :: Handle -> IO ()
hCloseAny h = hClose h `catchIOError` const (pure ())

withFdPipe :: ((Fd, Handle) -> IO a) -> IO a
withFdPipe action = withPipe $ \(a, b) -> do
    fd <- handleToFd a
    action (fd, b)

handleToFd :: Handle -> IO Fd
handleToFd = fmap (Fd . fdFD) . IO.handleToFd

captureLogging' :: (Tracer IO msg -> IO a) -> IO [msg]
captureLogging' = fmap fst . captureLogging

isMsgBadFd :: ShutdownHandlerLog -> Maybe Fd
isMsgBadFd (MsgShutdownBadFd fd _) = Just fd
isMsgBadFd _ = Nothing

isMsgShutdownError :: ShutdownHandlerLog -> Bool
isMsgShutdownError (MsgShutdownError _) = True
isMsgShutdownError _ = False

-- | Detect environment where 'stdin' is set to @/dev/null@ and skip test.
skipWhenNullStdin :: HasCallStack => Expectation
skipWhenNullStdin = do
    let onError :: IOException -> IO Bool
        onError _ = pure False
    usableStdin <- hWaitForInput stdin 50 `catch` onError
    unless usableStdin $ throwIO Success
