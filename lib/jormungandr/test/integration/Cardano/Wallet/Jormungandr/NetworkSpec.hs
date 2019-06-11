{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Wallet.Jormungandr.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..) )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..), Scheme (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( Async, async, cancel )
import Control.Monad
    ( void )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isRight )
import Data.Functor
    ( ($>) )
import System.Directory
    ( removePathForcibly )
import Test.Hspec
    ( Spec, afterAll, beforeAll, describe, it, shouldSatisfy )

import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr

spec :: Spec
spec = do
    describe "Happy Paths" $ beforeAll startNode $ afterAll killNode $ do
        it "get network tip" $ \(_, nw) -> do
            resp <- runExceptT $ networkTip nw
            resp `shouldSatisfy` isRight
            let (Right slot) = slotId . snd <$> resp
            slot `shouldSatisfy` (>= SlotId 0 0)
  where
    second :: Int
    second = 1000000

    startNode :: IO (Async (), NetworkLayer (Jormungandr 'Testnet) IO)
    startNode = do
        removePathForcibly "/tmp/cardano-wallet-jormungandr"
        let dir = "test/data/jormungandr"
        handle <- async $ void $ launch
            [ Command "jormungandr"
                [ "--genesis-block", dir ++ "/block-0.bin"
                , "--config", dir ++ "/config.yaml"
                , "--secret", dir ++ "/secret.yaml"
                ] (return ())
                Inherit
            ]
        let baseUrl = BaseUrl Http "localhost" 8081 "/api"
        nw <- Jormungandr.newNetworkLayer baseUrl
        waitForConnection nw defaultRetryPolicy $> (handle, nw)

    killNode :: (Async (), a) -> IO ()
    killNode (h, _) = do
        cancel h
        threadDelay (1 * second)
