-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Shelley.NetworkSpec (spec) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeVersionData )
import Cardano.Wallet.Shelley.Launch
    ( ClusterLog, singleNodeParams, withBFTNode, withSystemTempDir )
import Cardano.Wallet.Shelley.Network
    ( withNetworkLayer )
import Control.Concurrent.Async
    ( async, race_, waitAnyCancel )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad
    ( replicateM, void )
import Control.Tracer
    ( Tracer )
import Test.Hspec
    ( Spec, describe, it )

{-------------------------------------------------------------------------------
                                      Spec
-------------------------------------------------------------------------------}

spec :: Spec
spec = describe "NetworkLayer regression test #1708" $ do
    it "Parallel local socket connections" $
        withTestNode nullTracer $ \np sock vData -> do
            tasks <- replicateM 10 $ async $
                withNetworkLayer nullTracer np sock vData $ \nl -> do
                    -- Wait for the first tip result from the node
                    waiter <- newEmptyMVar
                    race_ (watchNodeTip nl (putMVar waiter)) (takeMVar waiter)
            void $ waitAnyCancel tasks

withTestNode
    :: Tracer IO ClusterLog
    -> (NetworkParameters -> FilePath -> NodeVersionData -> IO a)
    -> IO a
withTestNode tr action = do
    cfg <- singleNodeParams Error
    withSystemTempDir tr "network-spec" $ \dir ->
        withBFTNode tr dir cfg $ \sock _block0 (np, vData) ->
            action np sock vData
