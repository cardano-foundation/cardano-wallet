-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0

module Cardano.DB.Sqlite.DeleteSpec (spec) where

import Prelude

import Cardano.DB.Sqlite.Delete
    ( newRefCount, waitForFree', withRef )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( concurrently )
import Control.Concurrent.MVar
    ( isEmptyMVar, newEmptyMVar, putMVar )
import Control.Retry
    ( RetryPolicy, constantDelay, limitRetries )
import Control.Tracer
    ( nullTracer )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldReturn )

spec :: Spec
spec = describe "RefCount" $ do
    let testId = 1 :: Int
        otherId = 2 :: Int

    it "resource can be allocated multiple times" $ do
        ref <- newRefCount
        withRef ref testId $ withRef ref testId $ pure ()
        waitForFree' nullTracer testPol ref testId $ flip shouldBe 0

    it "waitForFree waits for withRef to finish" $ do
        ref <- newRefCount
        closed <- newEmptyMVar

        let conn = withRef ref testId $ do
                threadDelay 500000
                putMVar closed ()
        let rm = waitForFree' nullTracer testPol ref testId $ \n -> do
                n `shouldBe` 0
                isEmptyMVar closed

        concurrently conn (threadDelay 10 >> rm) `shouldReturn` ((), False)

    it "waitForFree uses correct id" $ do
        ref <- newRefCount
        withRef ref testId $
            waitForFree' nullTracer testPol ref otherId $
                flip shouldBe 0

    it "waitForFree times out" $ do
        ref <- newRefCount
        withRef ref testId $
            waitForFree' nullTracer quickPol ref testId $
                flip shouldBe 1

testPol :: RetryPolicy
testPol = constantDelay 50000 <> limitRetries 20

quickPol :: RetryPolicy
quickPol = constantDelay 1000 <> limitRetries 1
