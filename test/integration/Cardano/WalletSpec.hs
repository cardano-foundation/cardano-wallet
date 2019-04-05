{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet
    ( NewWallet (..), WalletLayer (..), mkWalletLayer, unsafeRunExceptT )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToBytes, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( currentTip )
import Cardano.Wallet.Primitive.Types
    ( SlotId (..), WalletName (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Test.Hspec
    ( Spec, after, before, it, shouldSatisfy )

import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.Network.HttpBridge as HttpBridge

spec :: Spec
spec = do
    before startBridge $ after closeBridge $ do
        it "A newly created wallet can sync with the chain" $ \(_, wallet) -> do
            bytes <- entropyToBytes <$> genEntropy @(EntropySize 15)
            wid <- unsafeRunExceptT $ createWallet wallet NewWallet
                { seed = Passphrase bytes
                , secondFactor = mempty
                , name = WalletName "My Wallet"
                , passphrase = mempty
                , gap = minBound
                }
            handle <- async (watchWallet wallet wid)
            threadDelay 5000000
            cancel handle
            tip <- currentTip . fst <$> unsafeRunExceptT (readWallet wallet wid)
            tip `shouldSatisfy` (> SlotId 0 0)
  where
    port = 1337
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
                Inherit
            ]
        threadDelay 1000000
        (handle,) <$> (mkWalletLayer
            <$> MVar.newDBLayer
            <*> HttpBridge.newNetworkLayer "testnet" port)
