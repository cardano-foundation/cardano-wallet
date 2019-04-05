{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet
    ( NewWallet (..), WalletLayer (..), mkWalletLayer )
import Cardano.Wallet.DB
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.Wallet.DB.MVar
    ( newDBLayer )
import Cardano.Wallet.Network.HttpBridge
    ( newNetworkLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, SeqState )
import Cardano.Wallet.Primitive.Mnemonic
    ( Entropy
    , EntropySize
    , Mnemonic
    , MnemonicException (..)
    , MnemonicWords
    , ambiguousNatVal
    , entropyToMnemonic
    , mkEntropy
    )
import Cardano.Wallet.Primitive.Types
    ( WalletId (..), WalletName (..) )
import Control.Monad
    ( replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Encoding.BIP39
    ( ValidChecksumSize, ValidEntropySize, ValidMnemonicSentence )
import Crypto.Hash
    ( hash )
import Data.Either
    ( isLeft, isRight )
import Data.Maybe
    ( isJust )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , choose
    , property
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L


spec :: Spec
spec = do
    describe "WalletLayer works as expected" $ do
        it "Wallet upon creation is written down in db"
            (property walletCreationProp)
        it "Wallet cannot be created more than once"
            (property walletDoubleCreationProp)
        it "Wallet after being created can be got using valid wallet Id"
            (property walletGetProp)
        it "Wallet with wrong wallet Id cannot be got"
            (property walletGetWrongIdProp)
        it "Two wallets with same mnemonic have a same public id"
            (property walletIdDeterministic)
        it "Two wallets with different mnemonic have a different public id"
            (property walletIdInjective)


{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

walletCreationProp
    :: NewWallet
    -> Property
walletCreationProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture db _wl walletIds) <- setupFixture newWallet
    resFromDb <- readCheckpoint db (PrimaryKey $ L.head walletIds)
    resFromDb `shouldSatisfy` isJust

walletDoubleCreationProp
    :: NewWallet
    -> Property
walletDoubleCreationProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl _walletIds) <- setupFixture newWallet
    secondTrial <- runExceptT $ createWallet wl newWallet
    secondTrial `shouldSatisfy` isLeft

walletGetProp
    :: NewWallet
    -> Property
walletGetProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl walletIds) <- liftIO $ setupFixture newWallet
    resFromGet <- runExceptT $ readWallet wl (L.head walletIds)
    resFromGet `shouldSatisfy` isRight

walletGetWrongIdProp
    :: (NewWallet, WalletId)
    -> Property
walletGetWrongIdProp (newWallet, corruptedWalletId) = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl _walletIds) <- liftIO $ setupFixture newWallet
    attempt <- runExceptT $ readWallet wl corruptedWalletId
    attempt `shouldSatisfy` isLeft

walletIdDeterministic
    :: NewWallet
    -> Property
walletIdDeterministic newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture _ _ widsA) <- liftIO $ setupFixture newWallet
    (WalletLayerFixture _ _ widsB) <- liftIO $ setupFixture newWallet
    widsA `shouldBe` widsB

walletIdInjective
    :: (NewWallet, NewWallet)
    -> Property
walletIdInjective (walletA, walletB) = monadicIO $ liftIO $ do
    (WalletLayerFixture _ _ widsA) <- liftIO $ setupFixture walletA
    (WalletLayerFixture _ _ widsB) <- liftIO $ setupFixture walletB
    widsA `shouldNotBe` widsB

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

data WalletLayerFixture = WalletLayerFixture {
      _fixtureDBLayer :: DBLayer IO SeqState
    , _fixtureWalletLayer :: WalletLayer SeqState
    , _fixtureWallet :: [WalletId]
    }

setupFixture
    :: NewWallet
    -> IO WalletLayerFixture
setupFixture newWallet = do
    db <- newDBLayer
    network <- newNetworkLayer "testnetwork" 8000
    let wl = mkWalletLayer db network
    res <- runExceptT $ createWallet wl newWallet
    let wal = case res of
            Left _ -> []
            Right walletId -> [walletId]
    pure $ WalletLayerFixture db wl wal

instance Arbitrary NewWallet where
    -- No shrinking
    arbitrary = NewWallet
        <$> arbitrary
        <*> arbitrary
        <*> pure (WalletName "My Wallet")
        <*> arbitrary
        <*> arbitrary

instance
    ( ValidEntropySize n
    , ValidChecksumSize n csz
    ) => Arbitrary (Entropy n) where
    arbitrary =
        let
            size = fromIntegral $ ambiguousNatVal @n
            entropy =
                mkEntropy  @n . B8.pack <$> vectorOf (size `quot` 8) arbitrary
        in
            either (error . show . UnexpectedEntropyError) id <$> entropy

instance
    ( n ~ EntropySize mw
    , mw ~ MnemonicWords n
    , ValidChecksumSize n csz
    , ValidEntropySize n
    , ValidMnemonicSentence mw
    , Arbitrary (Entropy n)
    ) => Arbitrary (Mnemonic mw) where
    arbitrary =
        entropyToMnemonic <$> arbitrary @(Entropy n)

instance Arbitrary (Passphrase goal) where
    shrink (Passphrase "") = []
    shrink (Passphrase _ ) = [Passphrase ""]
    arbitrary = do
        n <- choose (0, 32)
        InfiniteList bytes _ <- arbitrary
        return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

instance Arbitrary AddressPoolGap where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary WalletId where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> replicateM 16 arbitrary
        return $ WalletId (hash bytes)
