{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
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
    ( BlockchainParameters (..)
    , WalletLayer (..)
    , newWalletLayer
    , waitForWalletSync
    )
import Cardano.Wallet.Jormungandr.Compatibility
    ( BaseUrl (..), Jormungandr, Scheme (..) )
import Cardano.Wallet.Jormungandr.Environment
    ( Network (..) )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrLayer (..)
    , defaultManagerSettings
    , getInitialBlockchainParameters
    , newManager
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..), digest, generateKeyFromSeed, publicKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( mkSeqState )
import Cardano.Wallet.Primitive.Mnemonic
    ( EntropySize, entropyToBytes, genEntropy )
import Cardano.Wallet.Primitive.Model
    ( currentTip )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , Hash (..)
    , SlotId (..)
    , SlotLength (..)
    , WalletId (..)
    , WalletName (..)
    )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Concurrent
    ( threadDelay )
import Control.Concurrent.Async
    ( Async, async, cancel )
import Control.Monad
    ( unless, void )
import Data.Coerce
    ( coerce )
import Data.Time.Clock
    ( addUTCTime, getCurrentTime )
import System.Directory
    ( removePathForcibly )
import Test.Hspec
    ( Spec, after, before, expectationFailure, it )

import qualified Cardano.Wallet.DB.MVar as MVar
import qualified Cardano.Wallet.Jormungandr.Network as Jormungandr
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    before startNode $ after killNode $ do
        it "A newly created wallet can sync with the chain" $ \(_, wallet) -> do
            bytes <- entropyToBytes <$> genEntropy @(EntropySize 15)
            let xprv = generateKeyFromSeed (Passphrase bytes, mempty) mempty
            wid <- unsafeRunExceptT $ createWallet wallet
                (WalletId $ digest $ publicKey xprv)
                (WalletName "My Wallet")
                (mkSeqState @(Jormungandr 'Testnet) (xprv, mempty) minBound)
            unsafeRunExceptT $ restoreWallet wallet wid
            threadDelay (8 * second)
            tip <- slotId . currentTip . fst <$>
                unsafeRunExceptT (readWallet wallet wid)
            unless (tip > (SlotId 0 0)) $
                expectationFailure ("The wallet tip is still " ++ show tip)

        -- This tests both that the wallet syncs correctly, and that
        -- 'slotIdTime' computes the time of the wallet tip correctly.
        it "time of the wallet tip after syncing is recent" $ \(_, wallet) -> do
            bytes <- entropyToBytes <$> genEntropy @(EntropySize 15)
            let xprv = generateKeyFromSeed (Passphrase bytes, mempty) mempty
            wid <- unsafeRunExceptT $ createWallet wallet
                (WalletId $ digest $ publicKey xprv)
                (WalletName "My Wallet")
                (mkSeqState @(Jormungandr 'Testnet) (xprv, mempty) minBound)
            unsafeRunExceptT $ restoreWallet wallet wid
            waitForWalletSync wallet nullTracer wid
            tip <- slotId . currentTip . fst <$>
                unsafeRunExceptT (readWallet wallet wid)
            let slotLength = getSlotLength' $ getBlockchainParameters wallet
            t <- getCurrentTime
            let tMinted = slotIdTime wallet tip
            -- Check that tMinted ∈ (t - slotLength, t)
            let lowerBound = addUTCTime (-slotLength) t
            let upperBound = t
            if upperBound > tMinted && tMinted > lowerBound
            then return ()
            else expectationFailure $
                "The network tip " ++ show tip ++
                " corresponds to time " ++ show tMinted ++
                "\nIt should be in the interval (" ++ show lowerBound ++
                " , " ++ show upperBound ++ "), but isn't."



  where
    second :: Int
    second = 1000000

    getSlotLength' = toEnum . fromEnum . unSlotLength . getSlotLength

    startNode = do
        removePathForcibly "/tmp/cardano-wallet-jormungandr"
        let dir = "test/data/jormungandr"
        handle <- async $ void $ launch
            [ Command "jormungandr"
                [ "--genesis-block", dir ++ "/block0.bin"
                , "--config", dir ++ "/config.yaml"
                , "--secret", dir ++ "/secret.yaml"
                ] (return ())
                Inherit
            ]
        threadDelay (1 * second)
        db <- MVar.newDBLayer
        (nl, bp) <- newNetworkLayer
        let tl = undefined
        (handle,) <$>
            (newWalletLayer @_ @(Jormungandr 'Testnet) nullTracer bp db nl tl)

    newNetworkLayer = do
            let url = BaseUrl Http "localhost" 8080 "/api"
            mgr <- newManager defaultManagerSettings
            let jor = Jormungandr.mkJormungandrLayer mgr url
            let nl = Jormungandr.mkNetworkLayer @IO @'Testnet jor
            let absent = Hash $ BS.pack $ replicate 32 0
            [block0H] <- unsafeRunExceptT $ getDescendantIds jor absent 1
            blockchainParams <- unsafeRunExceptT $
                getInitialBlockchainParameters jor (coerce block0H)
            return (nl, blockchainParams)


    killNode :: (Async (), a) -> IO ()
    killNode (h, _) = do
        cancel h
        threadDelay (1 * second)
