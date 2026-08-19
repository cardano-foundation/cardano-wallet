{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Cardano.Wallet.Network.Implementation.OuroborosSpec where

import Cardano.Wallet.Network.Implementation.Ouroboros
    ( LSQ (LSQPure)
    , LocalStateQueryCmd (SomeLSQAt)
    , localStateQuery
    )
import Control.Concurrent.Class.MonadSTM
    ( TQueue
    , TVar
    , atomically
    , modifyTVar'
    , newTQueueIO
    , newTVarIO
    , readTVar
    , writeTQueue
    )
import Ouroboros.Network.Block
    ( pattern GenesisPoint
    )
import Ouroboros.Network.Protocol.LocalStateQuery.Client
    ( LocalStateQueryClient (LocalStateQueryClient)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Prelude

spec :: Spec
spec = describe "localStateQuery" $ do
    it "skips cancelled exact-point commands" $ do
        queue <- newTQueueIO :: IO (TQueue IO (LocalStateQueryCmd () IO))
        checks <- newTVarIO (0 :: Int) :: IO (TVar IO Int)
        let cancelled value = do
                atomically $ modifyTVar' checks (+ 1)
                pure value
            command value =
                SomeLSQAt
                    GenesisPoint
                    (LSQPure ())
                    (cancelled value)
                    (const $ pure ())
        atomically $ do
            writeTQueue queue (command True)
            writeTQueue queue (command False)

        let LocalStateQueryClient idle = localStateQuery queue
        _ <- idle

        count <- atomically (readTVar checks) :: IO Int
        count `shouldBe` 2
