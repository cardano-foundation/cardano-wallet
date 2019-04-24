{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.WalletSpec
    ( spec
    ) where

import Prelude

import Cardano.Environment
    ( Network (..) )
import Cardano.Wallet
    ( WalletLayer (..), mkWalletLayer )
import Cardano.Wallet.DB
    ( DBLayer, PrimaryKey (..) )
import Cardano.Wallet.DB.MVar
    ( newDBLayer )
import Cardano.Wallet.Network.HttpBridge
    ( newNetworkLayer )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressScheme (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), IsOurs (..), WalletId (..), WalletName (..) )
import Control.DeepSeq
    ( NFData (..) )
import Control.Monad
    ( replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Hash
    ( hash )
import Data.Either
    ( isLeft, isRight )
import Data.Maybe
    ( isJust )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..), Property, elements, property )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Cardano.Wallet.DB as DB
import qualified Data.ByteString as BS
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
    :: (WalletId, WalletName, DummyState)
    -> Property
walletCreationProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture db _wl walletIds) <- setupFixture newWallet
    resFromDb <- DB.readCheckpoint db (PrimaryKey $ L.head walletIds)
    resFromDb `shouldSatisfy` isJust

walletDoubleCreationProp
    :: (WalletId, WalletName, DummyState)
    -> Property
walletDoubleCreationProp newWallet@(wid, wname, wstate) = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl _walletIds) <- setupFixture newWallet
    secondTrial <- runExceptT $ createWallet wl wid wname wstate
    secondTrial `shouldSatisfy` isLeft

walletGetProp
    :: (WalletId, WalletName, DummyState)
    -> Property
walletGetProp newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl walletIds) <- liftIO $ setupFixture newWallet
    resFromGet <- runExceptT $ readWallet wl (L.head walletIds)
    resFromGet `shouldSatisfy` isRight

walletGetWrongIdProp
    :: ((WalletId, WalletName, DummyState), WalletId)
    -> Property
walletGetWrongIdProp (newWallet, corruptedWalletId) = monadicIO $ liftIO $ do
    (WalletLayerFixture _db wl _walletIds) <- liftIO $ setupFixture newWallet
    attempt <- runExceptT $ readWallet wl corruptedWalletId
    attempt `shouldSatisfy` isLeft

walletIdDeterministic
    :: (WalletId, WalletName, DummyState)
    -> Property
walletIdDeterministic newWallet = monadicIO $ liftIO $ do
    (WalletLayerFixture _ _ widsA) <- liftIO $ setupFixture newWallet
    (WalletLayerFixture _ _ widsB) <- liftIO $ setupFixture newWallet
    widsA `shouldBe` widsB

walletIdInjective
    :: ((WalletId, WalletName, DummyState), (WalletId, WalletName, DummyState))
    -> Property
walletIdInjective (walletA, walletB) = monadicIO $ liftIO $ do
    (WalletLayerFixture _ _ widsA) <- liftIO $ setupFixture walletA
    (WalletLayerFixture _ _ widsB) <- liftIO $ setupFixture walletB
    widsA `shouldNotBe` widsB

{-------------------------------------------------------------------------------
                      Tests machinery, Arbitrary instances
-------------------------------------------------------------------------------}

data WalletLayerFixture = WalletLayerFixture
    { _fixtureDBLayer :: DBLayer IO DummyState
    , _fixtureWalletLayer :: WalletLayer DummyState
    , _fixtureWallet :: [WalletId]
    }

setupFixture
    :: (WalletId, WalletName, DummyState)
    -> IO WalletLayerFixture
setupFixture (wid, wname, wstate) = do
    db <- newDBLayer
    network <- newNetworkLayer Local 8000
    let wl = mkWalletLayer db network
    res <- runExceptT $ createWallet wl wid wname wstate
    let wal = case res of
            Left _ -> []
            Right walletId -> [walletId]
    pure $ WalletLayerFixture db wl wal

data DummyState = DummyState
    deriving (Generic, Show, Eq)

instance NFData DummyState

instance Arbitrary DummyState where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance IsOurs DummyState where
    isOurs _ s = (True, s)

instance AddressScheme DummyState where
    keyFrom _ _ _ = Nothing
    nextChangeAddress s = (Address "dummy", s)

instance Arbitrary WalletId where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> replicateM 16 arbitrary
        return $ WalletId (hash bytes)

instance Arbitrary WalletName where
    shrink _ = []
    arbitrary = elements
        [ WalletName "My Wallet"
        , WalletName mempty
        ]
