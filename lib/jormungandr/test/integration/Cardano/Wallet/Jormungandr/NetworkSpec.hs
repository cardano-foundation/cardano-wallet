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
import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.Jormungandr.Api
    ( GetTipId, api )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr, Network (..), block0 )
import Cardano.Wallet.Jormungandr.Network
    ( BaseUrl (..), ErrUnexpectedNetworkFailure (..), Scheme (..) )
import Cardano.Wallet.Network
    ( ErrGetBlock (..)
    , ErrNetworkTip (..)
    , NetworkLayer (..)
    , defaultRetryPolicy
    , waitForConnection
    )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), Hash (..), SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( Async, async, cancel )
import Control.DeepSeq
    ( deepseq )
import Control.Exception
    ( SomeException, bracket, catch )
import Control.Monad
    ( void )
import Control.Monad.Trans.Except
    ( runExceptT )
import Control.Retry
    ( limitRetries, retrying )
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
    , shouldBe
    , shouldReturn
    , shouldSatisfy
    , shouldThrow
    )
import Test.QuickCheck
    ( arbitrary, generate, vectorOf )

import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    let startNode' = startNode url (`waitForConnection` defaultRetryPolicy)
    let once = limitRetries 1
    describe "Happy Paths" $ beforeAll startNode' $ afterAll killNode $ do
        it "get network tip" $ \(_, nw) -> do
            resp <- runExceptT $ networkTip nw
            resp `shouldSatisfy` isRight
            let (Right slot) = slotId <$> resp
            slot `shouldSatisfy` (>= SlotId 0 0)

        it "get some blocks from the genesis" $ \(_, nw) -> do
            threadDelay (10 * second)
            resp <- runExceptT $ nextBlocks nw block0
            resp `shouldSatisfy` isRight
            resp `shouldSatisfy` (not . null)

        it "no blocks after the tip" $ \(_, nw) -> do
            let try = do
                    tip <- unsafeRunExceptT $ networkTip nw
                    runExceptT $ nextBlocks nw tip
            -- NOTE Retrying twice since between the moment we fetch the
            -- tip and the moment we get the next blocks, one block may be
            -- inserted.
            -- Nevertheless, this can't happen twice within a slot time.
            resp <- retrying once
                (\_ x -> return $ fmap length x /= Right 0)
                (const try)
            resp `shouldBe` Right []

        it "returns an error when the block header is unknown" $ \(_, nw) -> do
            -- NOTE There's a very little chance of hash clash here. But,
            -- for what it's worth, I didn't bother retrying.
            bytes <- BS.pack <$> generate (vectorOf 32 arbitrary)
            let block = BlockHeader
                    { slotId = SlotId 42 14 -- Anything
                    , prevBlockHash = Hash bytes
                    }
            resp <- runExceptT $ nextBlocks nw block
            resp `shouldBe` Left (ErrGetBlockNotFound (Hash bytes))

    describe "Error paths" $ do
        it "networkTip: ErrNetworkUnreachable" $ do
            nw <- Jormungandr.newNetworkLayer url
            let msg x =
                    "Expected a ErrNetworkUnreachable' failure but got "
                    <> show x
            let action = do
                    res <- runExceptT $ networkTip nw
                    res `shouldSatisfy` \case
                        Left (ErrNetworkTipNetworkUnreachable e) ->
                            show e `deepseq` True
                        _ ->
                            error (msg res)
            action `shouldReturn` ()

        it "nextBlocks: ErrNetworkUnreachable" $ do
            nw <- Jormungandr.newNetworkLayer url
            let msg x =
                    "Expected a ErrNetworkUnreachable' failure but got "
                    <> show x
            let action = do
                    res <- runExceptT $ nextBlocks nw block0
                    res `shouldSatisfy` \case
                        Left (ErrGetBlockNetworkUnreachable e) ->
                            show e `deepseq` True
                        _ ->
                            error (msg res)
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
