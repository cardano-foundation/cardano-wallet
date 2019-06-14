{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Jormungandr.NetworkSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet.Jormungandr.Api
    ( GetTipId, api )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..), genesis )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..), ErrUnexpectedNetworkFailure (..), Scheme (..) )
import Cardano.Wallet.Network
    ( ErrNetworkTip (..)
    , NetworkLayer (..)
    , defaultRetryPolicy
    , waitForConnection
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( Async, async, cancel )
import Control.Exception
    ( SomeException, bracket, catch )
import Control.Monad
    ( void )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either
    ( isRight )
import Data.Functor
    ( ($>) )
import Data.Proxy
    ( Proxy (..) )
import Servant.Links
    ( safeLink )
import System.Directory
    ( removePathForcibly )
import Test.Hspec
    ( Spec
    , afterAll
    , beforeAll
    , describe
    , it
    , shouldReturn
    , shouldSatisfy
    , shouldThrow
    )

import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr

spec :: Spec
spec = do
    let startNode' = startNode url (`waitForConnection` defaultRetryPolicy)
    describe "Happy Paths" $ beforeAll startNode' $ afterAll killNode $ do
        it "get network tip" $ \(_, nw) -> do
            resp <- runExceptT $ networkTip nw
            resp `shouldSatisfy` isRight
            let (Right slot) = slotId <$> resp
            slot `shouldSatisfy` (>= SlotId 0 0)

    describe "Error paths" $ do
        it "networkTip: ErrNetworkUnreachable" $ do
            nw <- Jormungandr.newNetworkLayer url
            let msg x =
                    "Expected a ErrNetworkUnreachable' failure but got "
                    <> show x
            let action = do
                    res <- runExceptT $ networkTip nw
                    res `shouldSatisfy` \case
                        Left (ErrNetworkTipNetworkUnreachable _) -> True
                        _ -> error (msg res)
            action `shouldReturn` ()

        it "networkTip: throws on invalid url" $ do
            let wrongUrl = BaseUrl Http "localhost" 8081 "/not-valid-prefix"
            let wait nw = waitForConnection nw defaultRetryPolicy
                    `catch` (\(_ :: SomeException) -> return ())
            let test (_, nw) = do
                    let io = void $ runExceptT $ networkTip nw
                    shouldThrow io $ \(ErrUnexpectedNetworkFailure link _) ->
                        show link == show (safeLink api (Proxy @GetTipId))
            bracket (startNode wrongUrl wait) killNode test
  where
    url :: BaseUrl
    url = BaseUrl Http "localhost" 8081 "/api"

    second :: Int
    second = 1000000

    startNode
        :: BaseUrl
        -> (forall n. NetworkLayer n IO -> IO ())
        -> IO (Async (), NetworkLayer (Jormungandr 'Testnet) IO)
    startNode baseUrl wait = do
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
        nw <- Jormungandr.newNetworkLayer baseUrl
        wait nw $> (handle, nw)

    killNode :: (Async (), a) -> IO ()
    killNode (h, _) = do
        cancel h
        threadDelay (1 * second)
