{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.DB
    ( DBLayer (..), ErrWalletAlreadyExists (..), PrimaryKey (..) )
import Cardano.Wallet.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.DBSpec
    ( cleanDB )
import Cardano.Wallet.Primitive.AddressDerivation
    ( encryptPassphrase, unsafeGenerateKeyFromSeed )
import Cardano.Wallet.Primitive.Types
    ( WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Text.Class
    ( FromText (..) )
import Data.Time.Clock
    ( getCurrentTime )
import Test.Hspec
    ( Spec, beforeAll, beforeWith, describe, it, shouldReturn )

spec :: Spec
spec = beforeAll (newDBLayer Nothing) $ beforeWith cleanDB $ do
    describe "Wallet table" $ do
        it "create and list works" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk undefined testMetadata
            listWallets db `shouldReturn` [testPk]

        it "create and get meta works" $ \db -> do
            now <- getCurrentTime
            let md = testMetadata { passphraseInfo = Just $ WalletPassphraseInfo now }
            unsafeRunExceptT $ createWallet db testPk undefined md
            readWalletMeta db testPk `shouldReturn` Just md

        it "create twice is handled" $ \db -> do
            let create' = createWallet db testPk undefined testMetadata
            runExceptT create' `shouldReturn` (Right ())
            runExceptT create' `shouldReturn` (Left (ErrWalletAlreadyExists testWid))

        it "create and get private key" $ \db -> do
            unsafeRunExceptT $ createWallet db testPk undefined testMetadata
            readPrivateKey db testPk `shouldReturn` Nothing
            let Right phr = fromText "aaaaaaaaaa"
                k = unsafeGenerateKeyFromSeed (coerce phr, coerce phr) phr
            h <- encryptPassphrase phr
            unsafeRunExceptT (putPrivateKey db testPk (k, h))
            readPrivateKey db testPk `shouldReturn` Just (k, h)

testMetadata :: WalletMetadata
testMetadata = WalletMetadata
    { name = WalletName "test wallet"
    , passphraseInfo = Nothing
    , status = Ready
    , delegation = NotDelegating
    }

testWid :: WalletId
testWid = WalletId (hash ("test" :: ByteString))

testPk :: PrimaryKey WalletId
testPk = PrimaryKey testWid
