{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
module Cardano.DB.Sqlite.DeleteSpec (spec) where

import Cardano.DB.Sqlite.Delete
    ( newRefCount
    , waitForFree'
    , withRef
    )
import Control.Retry
    ( RetryPolicy
    , constantDelay
    , limitRetries
    )
import Control.Tracer
    ( Tracer
    , mkTracer
    )
import Test.Hspec
    ( Spec
    , describe
    , shouldBe
    , shouldReturn
    )
import Test.Hspec.Extra
    ( itWithDiagnosticTimeout
    )
import UnliftIO.Async
    ( concurrently
    )
import UnliftIO.Concurrent
    ( threadDelay
    )
import UnliftIO.MVar
    ( isEmptyMVar
    , newEmptyMVar
    , putMVar
    )
import Prelude

spec :: Spec
spec = describe "RefCount" $ do
    let testId = 1 :: Int
        otherId = 2 :: Int

    itWithDiagnosticTimeout 60 "resource can be allocated multiple times"
        $ \publish _ -> do
            publish $ diagnostic testId RefCountNotObserved PreparingRefCount
            ref <- newRefCount
            publish $ diagnostic testId RefCountNotObserved AcquiringReferences
            withRef ref testId $ withRef ref testId $ pure ()
            publish $ diagnostic testId RefCountNotObserved WaitingForFree
            waitForFree'
                (refCountTracer publish testId)
                testPol
                ref
                testId
                $ withObservedCount publish testId CheckingCount
                $ flip shouldBe 0

    itWithDiagnosticTimeout 60 "waitForFree waits for withRef to finish"
        $ \publish _ -> do
            publish $ diagnostic testId RefCountNotObserved PreparingRefCount
            ref <- newRefCount
            closed <- newEmptyMVar

            let conn = withRef ref testId $ do
                    publish
                        $ diagnostic
                            testId
                            (RefCountObserved $ Just 1)
                            HoldingReference
                    threadDelay 500_000
                    putMVar closed ()
                    publish
                        $ diagnostic
                            testId
                            (RefCountObserved $ Just 1)
                            ReleasingReference
            let rm = do
                    publish $ diagnostic testId RefCountNotObserved WaitingForFree
                    waitForFree'
                        (refCountTracer publish testId)
                        testPol
                        ref
                        testId
                        $ \n -> do
                            publish
                                $ diagnostic
                                    testId
                                    (RefCountObserved $ Just n)
                                    CheckingCloseSignal
                            n `shouldBe` 0
                            isEmptyMVar closed

            concurrently conn (threadDelay 50_000 >> rm)
                `shouldReturn` ((), False)

    itWithDiagnosticTimeout 60 "waitForFree uses correct id"
        $ \publish _ -> do
            publish $ diagnostic otherId RefCountNotObserved PreparingRefCount
            ref <- newRefCount
            publish
                $ diagnostic
                    otherId
                    RefCountNotObserved
                    HoldingDifferentResource
            withRef ref testId
                $ waitForFree'
                    (refCountTracer publish otherId)
                    testPol
                    ref
                    otherId
                $ withObservedCount publish otherId CheckingCount
                $ flip shouldBe 0

    itWithDiagnosticTimeout 60 "waitForFree times out"
        $ \publish _ -> do
            publish $ diagnostic testId RefCountNotObserved PreparingRefCount
            ref <- newRefCount
            publish
                $ diagnostic
                    testId
                    (RefCountObserved $ Just 1)
                    HoldingReference
            withRef ref testId
                $ waitForFree'
                    (refCountTracer publish testId)
                    quickPol
                    ref
                    testId
                $ withObservedCount publish testId CheckingRetryResult
                $ flip shouldBe 1

data DeleteDiagnostic = DeleteDiagnostic
    { resourceId :: Int
    , observedReferenceCount :: ReferenceCountObservation
    , waitStage :: WaitStage
    }
    deriving (Show)

data ReferenceCountObservation
    = RefCountNotObserved
    | RefCountObserved (Maybe Int)
    deriving (Show)

data WaitStage
    = PreparingRefCount
    | AcquiringReferences
    | HoldingReference
    | HoldingDifferentResource
    | ReleasingReference
    | WaitingForFree
    | PollingReferenceCount
    | CheckingCount
    | CheckingCloseSignal
    | CheckingRetryResult
    deriving (Show)

diagnostic
    :: Int
    -> ReferenceCountObservation
    -> WaitStage
    -> DeleteDiagnostic
diagnostic resourceId observedReferenceCount waitStage =
    DeleteDiagnostic{resourceId, observedReferenceCount, waitStage}

refCountTracer
    :: (DeleteDiagnostic -> IO ())
    -> Int
    -> Tracer IO (Maybe Int)
refCountTracer publish resourceId = mkTracer $ \count ->
    publish
        $ diagnostic
            resourceId
            (RefCountObserved count)
            PollingReferenceCount

withObservedCount
    :: (DeleteDiagnostic -> IO ())
    -> Int
    -> WaitStage
    -> (Int -> IO a)
    -> Int
    -> IO a
withObservedCount publish resourceId waitStage action count = do
    publish
        $ diagnostic resourceId (RefCountObserved $ Just count) waitStage
    action count

testPol :: RetryPolicy
testPol = constantDelay 50_000 <> limitRetries 20

quickPol :: RetryPolicy
quickPol = constantDelay 1_000 <> limitRetries 1
