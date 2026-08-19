{-# LANGUAGE NumericUnderscores #-}

module Cardano.Wallet.Network.ImplementationSpec where

import Cardano.Wallet.Network.Implementation
    ( runBoundedQuery
    )
import Control.Concurrent
    ( newEmptyMVar
    , putMVar
    , takeMVar
    , threadDelay
    )
import Control.Exception
    ( finally
    )
import Control.Monad
    ( forever
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Prelude

spec :: Spec
spec = describe "runBoundedQuery" $ do
    it "cancels an isolated client and query on timeout" $ do
        clientStarted <- newEmptyMVar
        clientStopped <- newEmptyMVar
        queryStarted <- newEmptyMVar
        queryStopped <- newEmptyMVar
        let stalled started stopped =
                (putMVar started () >> forever (threadDelay 1_000_000))
                    `finally` putMVar stopped ()

        result <-
            runBoundedQuery
                (takeMVar clientStarted >> takeMVar queryStarted)
                (stalled clientStarted clientStopped)
                (stalled queryStarted queryStopped :: IO Int)

        result `shouldBe` Nothing
        takeMVar clientStopped
        takeMVar queryStopped

    it "returns the result and cancels its isolated client" $ do
        clientStarted <- newEmptyMVar
        clientStopped <- newEmptyMVar
        let client =
                (putMVar clientStarted () >> forever (threadDelay 1_000_000))
                    `finally` putMVar clientStopped ()

        result <-
            runBoundedQuery
                (threadDelay 1_000_000)
                client
                (takeMVar clientStarted >> pure (42 :: Int))

        result `shouldBe` Just 42
        takeMVar clientStopped
