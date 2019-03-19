{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.WalletLayerSpec
    ( spec
    ) where


import Prelude

import Cardano.DBLayer
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.DBLayer.MVar
    ( newDBLayer )
import Cardano.NetworkLayer.HttpBridge
    ( newNetworkLayer )
import Cardano.Wallet
    ( WalletId (..), WalletName (..) )
import Cardano.Wallet.AddressDerivation
    ( Passphrase (..) )
import Cardano.Wallet.AddressDiscovery
    ( AddressPoolGap, SeqState )
import Cardano.Wallet.Mnemonic
    ( Entropy
    , EntropySize
    , Mnemonic
    , MnemonicException (..)
    , MnemonicWords
    , ambiguousNatVal
    , entropyToMnemonic
    , mkEntropy
    )
import Cardano.WalletLayer
    ( NewWallet (..), WalletLayer (..), mkWalletLayer )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Encoding.BIP39
    ( ValidChecksumSize, ValidEntropySize, ValidMnemonicSentence )
import Data.Either
    ( isLeft, isRight )
import Data.Maybe
    ( isJust )
import Test.Hspec
    ( Spec, describe, it, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , vectorOf
    )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Text as T


spec :: Spec
spec = do
    describe "WalletLayer works as expected" $ do
        it "Wallet upon creation is written down in db"
            (checkCoverage walletCreationProp)
        it "Wallet cannot be created more than once"
            (checkCoverage walletDoubleCreationProp)
        it "Wallet after being created can be got using valid wallet Id"
            (checkCoverage walletGetProp)
        it "Wallet with wrong wallet Id cannot be got"
            (checkCoverage walletGetWrongIdProp)


{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

walletCreationProp
    :: NewWallet
    -> Property
walletCreationProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture db _wl walletIds) <- setupFixture newWallet

    resFromDb <- readCheckpoints db (PrimaryKey $ L.head walletIds)

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

    resFromGet <- runExceptT $ getWallet wl (L.head walletIds)

    resFromGet `shouldSatisfy` isRight

walletGetWrongIdProp
    :: NewWallet
    -> Property
walletGetWrongIdProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl walletIds) <- liftIO $ setupFixture newWallet

    let (WalletId storedWalletId) = L.head walletIds
    let corruptedWalletId = WalletId $ T.append "@ " storedWalletId
    attempt <- runExceptT $ getWallet wl corruptedWalletId

    attempt `shouldSatisfy` isLeft


{-------------------------------------------------------------------------------
                      Tests machinary, Arbitrary instances
-------------------------------------------------------------------------------}

data WalletLayerFixture = WalletLayerFixture {
      _fixtureDBLayer :: DBLayer IO SeqState
    , _fixtureWalletLayer :: WalletLayer IO SeqState
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

-- | Same remark from 'Arbitrary Entropy' applies here.
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
