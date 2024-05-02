module Control.Monitoring.MonitorSpec
    ( spec
    )
where

import Prelude

import Control.Concurrent
    ( threadDelay
    )
import Control.Foldl
    ( Fold
    )
import Control.Monad
    ( replicateM_
    , (<=<)
    )
import Control.Monitoring.Folder
    ( mkTracingFromFold
    )
import Control.Monitoring.Monitor
    ( Monitor
    , mkMonitor
    , observe
    , step
    , trace
    )
import Control.Monitoring.Tracing
    ( AnyTracing (AnyTracing)
    , StateS (..)
    )
import Data.Foldable
    ( forM_
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import UnliftIO
    ( async
    , forConcurrently_
    , link
    )

import qualified Control.Foldl as L

mkTracing :: StateS x -> Fold a b -> AnyTracing a b
mkTracing w f = AnyTracing w $ mkTracingFromFold f w

test
    :: (Show b, Eq b)
    => Int
    -> StateS x
    -> Fold a b
    -> b
    -> (Int -> Monitor IO a b -> IO ())
    -> IO ()
test cycles state fold target action = forM_ [1 .. cycles] $ \cycle' -> do
    monitor <- mkMonitor (mkTracing state fold) pure
    action cycle' monitor
    (result, _) <- observe monitor
    result `shouldBe` target

spec :: Spec
spec = do
    describe "a Monitor" $ do
        it "can trace in single thread in run mode"
            $ test 100 RunS L.sum (101 :: Int)
            $ \_ m -> do
                trace m 1
                trace m 100
        it "can trace in multiple threads in run mode"
            $ test 100 RunS L.sum (1111 :: Int)
            $ \_ m -> do
                forConcurrently_ [1, 10, 100, 1000] $ trace m
        it "can trace in multiple thread in step mode"
            $ test 100 StepS L.sum (1111 :: Int)
            $ \_ m -> do
                link <=< async
                    $ forConcurrently_ [1, 10, 100, 1000]
                    $ trace m
                -- step is asynchronous so we the monitor some time to
                -- process the trace
                threadDelay 1000
                replicateM_ 4 $ do
                    step m
                    threadDelay 1000
