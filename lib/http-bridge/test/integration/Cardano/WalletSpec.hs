{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Launcher
    ( Command (..), StdStream (..), launch )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, block0, byronBlockchainParameters )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Network
    ( defaultRetryPolicy, waitForConnection )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..), digest, publicKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey, generateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( mkSeqState )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToBytes, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( currentTip )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), WalletId (..), WalletName (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( async, cancel )
import Control.Monad
    ( when )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Data.Either
    ( isLeft )
import Data.Text.Class
    ( toText )
import Test.Hspec
    ( Spec, after, before, expectationFailure, it )
import Test.Utils.Ports
    ( findPort )

import qualified Cardano.Wallet as W
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
            wid <- unsafeRunExceptT $ W.createWallet wallet
                (WalletId $ digest $ publicKey xprv)
                (WalletName "My Wallet")
                (mkSeqState @(HttpBridge 'Testnet) (xprv, mempty) minBound)
            unsafeRunExceptT $ W.restoreWallet wallet wid

            let policy = limitRetriesByCumulativeDelay
                    (60 * second)
                    (constantDelay (1 * second))
            let shouldRetry _ = \case
                    Left _ -> pure True
                    Right _ -> pure False
            let assertion _ = do
                    tip <- slotId . currentTip . fst <$>
                        unsafeRunExceptT (W.readWallet wallet wid)
                    tip <- slotId . currentTip . fst <$>
                        unsafeRunExceptT (W.readWallet wallet wid)
                    return $ if tip > minBound
                        then Right ()
                        else Left ("The wallet tip is still " <> show tip)
            result <- retrying policy shouldRetry assertion
            when (isLeft result) $ expectationFailure (show result)
  where
    second = 1000*1000
    closeBridge (handle, _) = do
        cancel handle
        threadDelay second
    startBridge = do
        port <- findPort
        handle <- async $ launch
            [ Command "cardano-http-bridge"
                [ "start"
                , "--port", show port
                , "--template", T.unpack (toText $ networkVal @'Testnet)
                ]
                (return ())
                Inherit
            ]
        db <- MVar.newDBLayer
        nl <- HttpBridge.newNetworkLayer @'Testnet port
        waitForConnection nl defaultRetryPolicy
        let tl = HttpBridge.newTransactionLayer @'Testnet @SeqKey
        let genesis = (block0, byronBlockchainParameters @'Testnet)
        (handle,) <$>
            (newWalletLayer @(HttpBridge 'Testnet) nullTracer genesis db nl tl)
