module Cardano.Wallet.Network.ImplementationSpec
    ( spec
    ) where

import Cardano.Wallet.Network.Implementation
    ( handleIOException
    , recoveringNodeConnection
    , retryOnConnectionLost
    )
import Control.Exception
    ( throwIO
    )
import Control.Monad
    ( when
    )
import Control.Retry
    ( RetryAction (..)
    )
import Control.Tracer
    ( nullTracer
    )
import Data.IORef
    ( atomicModifyIORef'
    , newIORef
    , readIORef
    )
import Foreign.C.Error
    ( Errno
    , eACCES
    , eINVAL
    , errnoToIOError
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Prelude

import qualified Network.Mux as Mux

spec :: Spec
spec = do
    describe "handleIOException" $ do
        it "retries a failed read on a Windows named pipe"
            $ classify (pipeError "withIODataPtr (readHandle)" eINVAL)
                >>= (`shouldBe` ConsultPolicy)

        it "retries a failed write on a Windows named pipe"
            $ classify (pipeError "withIODataPtr (writeHandle)" eINVAL)
                >>= (`shouldBe` ConsultPolicy)

        it "does not retry an invalid argument raised elsewhere"
            $ classify (pipeError "recv" eINVAL)
                >>= (`shouldBe` DontRetry)

        it "does not retry a permission error at the pipe location"
            $ classify (pipeError "withIODataPtr (readHandle)" eACCES)
                >>= (`shouldBe` DontRetry)

    describe "recoveringNodeConnection" $ do
        it "retries a mux error from an aborted pipe read"
            $ do
                calls <- newIORef (0 :: Int)
                let action = do
                        n <- atomicModifyIORef' calls
                            $ \c -> (c + 1, c)
                        when (n == 0) $ throwIO abortedRead
                recoveringNodeConnection
                    nullTracer
                    (retryOnConnectionLost nullTracer)
                    action
                readIORef calls >>= (`shouldBe` 2)
  where
    classify = handleIOException nullTracer
    abortedRead =
        Mux.IOException
            (pipeError "withIODataPtr (readHandle)" eINVAL)
            "readHandle errored"

-- | Build an error the way the Win32 package does.
pipeError :: String -> Errno -> IOError
pipeError loc errno = errnoToIOError loc errno Nothing Nothing
