{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Network.HttpBridgeSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), launch )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Network.HttpBridge
    ( HttpBridgeError (..) )
import Cardano.Wallet.Primitive.Types
    ( Block (..), BlockHeader (..), Hash (..), SlotId (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( runExceptT )
import Test.Hspec
    ( Spec, afterAll, beforeAll, describe, it, shouldReturn, shouldSatisfy )

import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge

port :: Int
port = 1337

spec :: Spec
spec = do
    describe "Happy paths" $ beforeAll startBridge $ afterAll closeBridge $ do
        it "get from packed epochs" $ \(_, network) -> do
            let blocks = runExceptT $ nextBlocks network (SlotId 14 0)
            (fmap length <$> blocks)
                `shouldReturn` pure 21600
            (fmap (prevBlockHash . header . head) <$> blocks)
                `shouldReturn` pure (Hash
                    "7Z\213\204\SUB\134l\149\&8\191ZO\\0q]\ESCB\CAN\254\f[\RS\
                    \\142\SOH\192K\250^\168\188m")

        it "get from packet epochs and filter by start slot"
                $ \(_, network) -> do
            let blocks = runExceptT $ nextBlocks network (SlotId 14 14000)
            (fmap length <$> blocks)
                `shouldReturn` pure 7600
            (fmap (prevBlockHash . header . head) <$> blocks)
                `shouldReturn` pure (Hash
                    "\186\173\135)\129\248 \214\222\159\161x\EM\214\187\&8\158\
                    \\220\237\245\bd\207\DC4\RS\168\212\143\240g\EOTQ")

        it "get unstable blocks for the unstable epoch" $ \(_, network) -> do
            let action = runExceptT $ do
                    (SlotId ep sl) <- (slotId . snd) <$> networkTip network
                    let sl' = if sl > 2 then sl - 2 else 0
                    blocks <- nextBlocks network (SlotId ep sl')
                    lift $ blocks `shouldSatisfy` (\bs
                        -> length bs >= fromIntegral (sl - sl')
                        && length bs <= fromIntegral (sl - sl' + 1)
                        )
            action `shouldReturn` pure ()

        it "produce no blocks if start is after tip" $ \(_, network) -> do
            let action = runExceptT $ do
                    SlotId ep sl <- (slotId . snd) <$> networkTip network
                    length <$> nextBlocks network (SlotId (ep + 1) sl)
            action `shouldReturn` pure 0

    describe "Error paths" $ beforeAll newNetworkLayer $ do
        it "gets a 'NodeUnavailable' if bridge isn't up" $ \network -> do
            let msg x = "Expected a 'NodeAvailable' failure but got " <> show x
            let action = do
                    res <- runExceptT $ networkTip network
                    res `shouldSatisfy` \case
                        Left (NodeUnavailable _) -> True
                        _ -> error (msg res)
            action `shouldReturn` ()
  where
    newNetworkLayer =
        HttpBridge.newNetworkLayer "testnet" port
    closeBridge (handle, _) = do
        cancel handle
        threadDelay 500000
    startBridge = do
        handle <- async $ launch
            [ Command "cardano-http-bridge"
                [ "start"
                , "--port", show port
                , "--template", "testnet"
                ]
                (return ())
            ]
        network <- newNetworkLayer
        threadDelay 1000000
        return (handle, network)
