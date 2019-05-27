{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.Environment.HttpBridge
    ( network )
import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet
    ( WalletLayer (..), newWalletLayer, unsafeRunExceptT )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..), digest, generateKeyFromSeed, publicKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( mkSeqState )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToBytes, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( currentTip )
import Cardano.Wallet.Primitive.Types
    ( SlotId (..), WalletId (..), WalletName (..) )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.Monad
    ( unless )
import Data.Text.Class
    ( toText )
import Test.Hspec
    ( Spec, after, before, expectationFailure, it )

import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge
import qualified Data.Text as T

spec :: Spec
spec = do
    before startBridge $ after closeBridge $ do
        it "A newly created wallet can sync with the chain" $ \(_, wallet) -> do
            bytes <- entropyToBytes <$> genEntropy @(EntropySize 15)
            let xprv = generateKeyFromSeed (Passphrase bytes, mempty) mempty
            wid <- unsafeRunExceptT $ createWallet wallet
                (WalletId $ digest $ publicKey xprv)
                (WalletName "My Wallet")
                (mkSeqState @HttpBridge (xprv, mempty) minBound)
            unsafeRunExceptT $ restoreWallet wallet wid
            threadDelay 2000000
            tip <- currentTip . fst <$> unsafeRunExceptT (readWallet wallet wid)
            unless (tip > (SlotId 0 0)) $
                expectationFailure ("The wallet tip is still " ++ show tip)
  where
    port = 1337
    closeBridge (handle, _) = do
        cancel handle
        threadDelay 1000000
    startBridge = do
        handle <- async $ launch
            [ Command "cardano-http-bridge"
                [ "start"
                , "--port", show port
                , "--template", T.unpack (toText network)
                ]
                (return ())
                Inherit
            ]
        threadDelay 1000000
        db <- MVar.newDBLayer
        nl <- HttpBridge.newNetworkLayer port
        let tl = HttpBridge.newTransactionLayer
        (handle,) <$> (newWalletLayer @_ @HttpBridge db nl tl)
