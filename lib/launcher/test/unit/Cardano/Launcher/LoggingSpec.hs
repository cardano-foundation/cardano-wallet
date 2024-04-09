{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Launcher.LoggingSpec (spec) where

import Prelude

import Cardano.Launcher
    ( Command (Command)
    , ProcessHandles (..)
    , ProcessHasExited
    , StdStream (CreatePipe, NoStream)
    , withBackendProcess
    )
import Cardano.Launcher.Logging
    ( traceHandle
    )
import Control.Monad
    ( forM_
    )
import Control.Tracer
    ( Tracer (..)
    , nullTracer
    )
import Data.Functor.Contravariant
    ( (>$<)
    )
import Data.IORef
    ( modifyIORef
    , newIORef
    , readIORef
    )
import Data.Text
    ( Text
    )
import System.IO
    ( SeekMode (..)
    , hSeek
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import UnliftIO
    ( try
    , withSystemTempFile
    )

import qualified Data.Text.IO as T

holdTrace :: IO (Tracer IO Text, IO [Text])
holdTrace = do
    ref <- newIORef []
    let tracer = Tracer $ \line -> modifyIORef ref (line :)
    pure (tracer, reverse <$> readIORef ref)

prepend :: Text -> Text -> Text
prepend prefix = ((prefix <> ": ") <>)

spec :: Spec
spec = do
    describe "traceHandle" $ do
        it "traces lines from a handle" $ do
            let ls = ["line 1", "line 2", "line 3"]
            withSystemTempFile "traceHandle" $ \_ handle -> do
                forM_ ls $ T.hPutStrLn handle
                hSeek handle AbsoluteSeek 0
                (tracer, readTrace) <- holdTrace
                traceHandle (("traced:" <>) >$< tracer) handle
                readTrace `shouldReturn` (("traced:" <>) <$> ls)
        it "traces output of a process" $ do
            let c = Command
                    "echo"
                    ["cwd"]
                    (pure ())
                    NoStream
                    CreatePipe
                run (ProcessHandles _ (Just handle) _ _) = do
                    (tracer, readTrace) <- holdTrace
                    traceHandle (prepend "stdout" >$< tracer) handle
                    readTrace `shouldReturn` prepend "stdout" <$> ["cwd"]
                run _ = fail "no stdout"
            r <- try $ withBackendProcess nullTracer c run
            r `shouldBe` (Right @ProcessHasExited) ()
            pure ()
