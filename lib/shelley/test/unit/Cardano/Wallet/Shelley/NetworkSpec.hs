{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    ( Observer (..), newObserver, withNetworkLayer )
import Control.Concurrent.Async
    ( async, race_, waitAnyCancel )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Monad
    ( replicateM, void )
import Control.Tracer
    ( Tracer )
import Test.Hspec
    ( Spec, beforeAll, describe, it, shouldBe, shouldReturn )

import qualified Data.Map as Map
import qualified Data.Set as Set

{-# ANN module ("HLint: use isNothing" :: String) #-}

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

    describe "Observer" $ do
        describe "(query k) with typical use" $ beforeAll mockObserver $ do
            -- Using monadic-property tests /just/ for the sake of testing with
            -- multiple keys seem worthless.
            let k = ("k"::String)
            let v = length k
            it "returns Nothing before registration"
                $ \(observer, refresh) -> do
                    (query observer k) `shouldReturn` Nothing
                    refresh True
                    (query observer k) `shouldReturn` Nothing

            it "returns v after (startObserving k >> refresh)"
                $ \(observer, refresh) -> do
                    startObserving observer k
                    refresh True
                    v' <- query observer k
                    let expectedValue = length k
                    v' `shouldBe` Just expectedValue

            it "returns the same v after a failed refresh attempt"
                $ \(observer, refresh) -> do
                    refresh False
                    v' <- query observer k
                    v' `shouldBe` Just v

            it "returns Nothing after (stopObserving k >> refresh)"
                $ \(observer, refresh) -> do
                    stopObserving observer k
                    refresh True
                    v' <- query observer k
                    v' `shouldBe` Nothing
  where
    mockObserver
        :: IO ( Observer IO String Int
              , Bool -> IO ()
              )
    mockObserver = newObserver nullTracer fetch
        -- We /could/ use traceInTVarIO, and test that we see the expected
        -- traces.
      where
        fetch True keys = pure
            $ Just
            $ Map.fromList
            $ map (\x -> (x,length x))
            $ Set.toList keys
        fetch False _ = pure $ Nothing

withTestNode
    :: Tracer IO ClusterLog
    -> (NetworkParameters -> FilePath -> NodeVersionData -> IO a)
    -> IO a
withTestNode tr action = do
    cfg <- singleNodeParams Error
    withSystemTempDir tr "network-spec" $ \dir ->
        withBFTNode tr dir cfg $ \sock _block0 (np, vData) ->
            action np sock vData
