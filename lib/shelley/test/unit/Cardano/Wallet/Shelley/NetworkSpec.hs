{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Shelley.NetworkSpec (spec) where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( nullTracer, traceInTVarIO )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeVersionData )
import Cardano.Wallet.Shelley.Launch
    ( ClusterLog, singleNodeParams, withBFTNode, withSystemTempDir )
import Cardano.Wallet.Shelley.Network
    ( Observer (..), ObserverLog (..), newObserver, withNetworkLayer )
import Control.Applicative
    ( (<*) )
import Control.Concurrent.Async
    ( async, race_, waitAnyCancel )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM
    ( atomically )
import Control.Concurrent.STM.TVar
    ( TVar, newTVarIO, readTVar, writeTVar )
import Control.Monad
    ( mapM_, replicateM, unless, void )
import Control.Tracer
    ( Tracer )
import Data.Map
    ( Map )
import Data.Set
    ( Set )
import Fmt
    ( build, fmt, indentF )
import Test.Hspec
    ( Spec, beforeAll, describe, it, shouldBe, shouldReturn )
import Test.QuickCheck
    ( counterexample, property )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monadicIO, monitor, run )

import qualified Data.Map as Map
import qualified Data.Set as Set

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
        it "can fetch all observed keys, but not any other keys"
            $ property $ \keys1 keys2 -> monadicIO $ do
                (observer, refresh, _trVar) <- run mockObserver
                run $ mapM_ (startObserving observer) keys1
                run $ refresh True

                let allNothing = fromKeysWith (const Nothing)
                let unobservedKeys = Set.difference keys2 keys1
                unobservedValues <- run $ queryKeys observer unobservedKeys

                observedValues <- run $ queryKeys observer keys1

                assertEqual "observed keys return expected values"
                    observedValues
                    (fromKeysWith (Just . length) keys1)

                assertEqual "unobserved keys are all Nothing when queried"
                    unobservedValues
                    (allNothing unobservedKeys)

        describe "typical use" $ beforeAll mockObserver $ do
            -- Using monadic-property tests here /just/ for the sake of testing
            -- with multiple keys seem worthless.
            --
            -- State machine tests might be suitable on the other hand...
            --
            -- NOTE: We make sure to test conditions both before and after
            -- calling @refresh@, as it can be called arbitrarily without our
            -- (the observers') knowledge.
            --
            -- NOTE: These tests are stateful.
            -- They also use smaller @it@ blocks, with more @describe@ nesting,
            -- than much of the rest of the wallet tests. This is done for
            -- concise and readable test output.
            let k = ("k"::String)
            let v = length k
            describe "startObserving" $ do
                it "(query k) returns Nothing before startObserving"
                    $ \(observer, refresh, trVar) -> do
                        (query observer k) `shouldReturn` Nothing
                        trVar `shouldHaveTraced` []
                        refresh True
                        (query observer k) `shouldReturn` Nothing
                        trVar `shouldHaveTraced`
                            [ MsgWillFetch Set.empty
                            , MsgDidFetch Map.empty
                            ]

                it "(query k) returns v after (startObserving k >> refresh)"
                    $ \(observer, refresh, _) -> do
                        startObserving observer k
                        refresh True
                        let expectedValue = length k
                        (query observer k)`shouldReturn` Just expectedValue

                -- NOTE: Depends on the @refresh@ call from the previous test.
                it "traced MsgAddedObserver, MsgWillFetch, MsgDidFetch"
                    $ \(_, _, trVar) -> do
                        trVar `shouldHaveTraced`
                            [ MsgAddedObserver k
                            , MsgWillFetch $ Set.singleton k
                            , MsgDidFetch $ Map.singleton k v
                            ]

            describe "calling startObserving a second time" $ do
                it "(query k) is still v"
                    $ \(observer, refresh, trVar) -> do
                        startObserving observer k
                        (query observer k) `shouldReturn` (Just v)
                        refresh True
                        (query observer k) `shouldReturn` (Just v)
                        trVar `shouldHaveTraced`
                            [ MsgWillFetch $ Set.singleton k
                            , MsgDidFetch $ Map.singleton k v
                            ]

            describe "when refresh fails" $ do
                it "(query k) returns the existing v"
                    $ \(observer, refresh, _) -> do
                        refresh False
                        (query observer k) `shouldReturn` Just v

                it "only MsgWillFetch is traced"
                    $ \(_, _, trVar) -> do
                    trVar `shouldHaveTraced`
                        [ MsgWillFetch $ Set.singleton k
                        ]

            describe "stopObserving" $
                it "makes (query k) return Nothing"
                    $ \(observer, refresh, _) -> do
                        stopObserving observer k
                        (query observer k) `shouldReturn` Nothing
                        refresh True
                        (query observer k) `shouldReturn` Nothing
  where
    -- | Expects given messages to have been traced /and/ clears the @TVar@.
    --
    -- NOTE: Reverses the contents in the @TVar@ to get a chronological order.
    shouldHaveTraced :: (Show log, Eq log) => TVar [log] -> [log] -> IO ()
    shouldHaveTraced trVar expected = do
        actual <- atomically ((readTVar trVar) <* (writeTVar trVar []))
        (reverse actual) `shouldBe` expected

    fromKeysWith :: Ord k => (k -> v) -> Set k -> Map k v
    fromKeysWith f =
        Map.fromList
        . map (\k -> (k, f k))
        . Set.toList

    queryKeys :: (Monad m, Ord k) => Observer m k v -> Set k -> m (Map k (Maybe v))
    queryKeys observer keys = Map.fromList <$> mapM
        (\k -> query observer k >>= \v -> return (k, v))
        (Set.toList keys)

    mockObserver
        :: IO ( Observer IO String Int
              , Bool -> IO ()
              , TVar [ObserverLog String Int]
              )
    mockObserver = do
        trVar <- newTVarIO []
        (ob, refresh) <- newObserver (traceInTVarIO trVar) fetch
        return (ob, refresh, trVar)
      where
        fetch True keys = pure
            $ Just
            $ Map.fromList
            $ map (\x -> (x,length x))
            $ Set.toList keys
        fetch False _ = pure Nothing


    -- Assert equiality in monadic properties with nice counterexamples
    --
    -- E.g.
    -- >> observed keys return expected values ✗
    -- >>      fromList [("",Just 0)]
    -- >>      /=
    -- >>      fromList [("",Just 1)]
    assertEqual :: (Eq a, Show a) => String -> a -> a -> PropertyM IO ()
    assertEqual description a b = do
        let condition = a == b
        let flag = if condition then "✓" else "✗"
        monitor (counterexample $ description <> " " <> flag)
        unless condition $ do
            monitor $ counterexample $ fmt $ indentF 4 $ mconcat
                [ build $ show a
                , "\n/=\n"
                , build $ show b
                ]
        assert condition

withTestNode
    :: Tracer IO ClusterLog
    -> (NetworkParameters -> FilePath -> NodeVersionData -> IO a)
    -> IO a
withTestNode tr action = do
    cfg <- singleNodeParams Error Nothing
    withSystemTempDir tr "network-spec" $ \dir ->
        withBFTNode tr dir cfg $ \sock _block0 (np, vData) ->
            action np sock vData
