{-# LANGUAGE NumericUnderscores #-}

module Cardano.Wallet.Shelley.NetworkSpec (spec) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeVersionData )
import Cardano.Wallet.Shelley.Launch
    ( singleNodeParams, withBFTNode, withSystemTempDir )
import Cardano.Wallet.Shelley.Network
    ( withNetworkLayer )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( replicateConcurrently_ )
import Test.Hspec
    ( Spec, describe, it )
import Test.Utils.Trace
    ( withLogging )

{-------------------------------------------------------------------------------
                                      Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec = describe "NetworkLayer regression test #1708" $ do
    it "Parallel local socket connections" $
        withTestNode $ \np sock vData -> withLogging $ \(tr, _getLogs) ->
            replicateConcurrently_ 5 $
                withNetworkLayer tr np sock vData $ \_nl ->
                    threadDelay 1_000_000

withTestNode
    :: (NetworkParameters -> FilePath -> NodeVersionData -> IO a)
    -> IO a
withTestNode action = do
    cfg <- singleNodeParams Error
    withSystemTempDir nullTracer "network-spec" $ \dir ->
        withBFTNode nullTracer dir cfg $ \sock _block0 (np, vData) ->
            action np sock vData
