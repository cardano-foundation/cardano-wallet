{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( nullTracer )
import Cardano.Launcher
    ( StdStream (..) )
import Cardano.Wallet
    ( newWalletLayer )
import Cardano.Wallet.DB
    ( DBFactory (..) )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, block0, byronBlockchainParameters )
import Cardano.Wallet.HttpBridge.Environment
    ( Network (..) )
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
    ( BlockHeader (..), WalletId (..), WalletName (..), slotMinBound )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Exception
    ( throwIO )
import Control.Monad
    ( when )
import Control.Retry
    ( constantDelay, limitRetriesByCumulativeDelay, retrying )
import Data.Either
    ( isLeft )
import Test.Hspec
    ( Spec, around, expectationFailure, it )

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.HttpBridge.Network as HttpBridge
import qualified Cardano.Wallet.HttpBridge.Transaction as HttpBridge

spec :: Spec
spec = do
    around withWalletLayer $ do
        it "A newly created wallet can sync with the chain" $ \wl -> do
            bytes <- entropyToBytes <$> genEntropy @(EntropySize 15)
            let xprv = generateKeyFromSeed (Passphrase bytes, mempty) mempty
            wid <- unsafeRunExceptT $ W.createWallet wl
                (WalletId $ digest $ publicKey xprv)
                (WalletName "My Wallet")
                (mkSeqState @(HttpBridge 'Testnet) (xprv, mempty) minBound)
            unsafeRunExceptT $ W.restoreWallet wl wid

            let policy = limitRetriesByCumulativeDelay
                    (60 * second)
                    (constantDelay (1 * second))
            let shouldRetry _ = \case
                    Left _ -> pure True
                    Right _ -> pure False
            let assertion _ = do
                    (cp, _, _) <- unsafeRunExceptT $ W.readWallet wl wid
                    let tip = slotId (currentTip cp)
                    return $ if tip > slotMinBound
                        then Right ()
                        else Left ("The wallet tip is still " <> show tip)
            result <- retrying policy shouldRetry assertion
            when (isLeft result) $ expectationFailure (show result)
  where
    second = 1000 * 1000
    withNetworkLayer =
        HttpBridge.withNetworkLayer @'Testnet nullTracer (HttpBridge.Launch cfg)
    withWalletLayer cb = withNetworkLayer $ \case
        Right (_, nl) -> do
            db <- MVar.newDBLayer
            let tl = HttpBridge.newTransactionLayer @'Testnet @SeqKey
            let genesis = (block0, byronBlockchainParameters @'Testnet)
            let dbFactory = DBFactory
                    { withDatabase = const (\with -> with db)
                    , removeDatabase = \_ -> pure ()
                    }
            wl <- newWalletLayer @_ @_ @(HttpBridge 'Testnet)
                nullTracer genesis nl tl dbFactory []
            cb wl
        Left e -> throwIO e
    cfg = HttpBridge.HttpBridgeConfig (Right Testnet) Nothing Nothing [] Inherit
