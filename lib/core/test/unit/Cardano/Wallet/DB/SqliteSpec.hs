{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

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
import Data.Time.Clock
    ( getCurrentTime )
import Test.Hspec
    ( Spec, describe, it, shouldReturn )

spec :: Spec
spec = do
    describe "Wallet table" $ do
        it "create and list works" $ do
            db <- newDBLayer Nothing
            unsafeRunExceptT (createWallet db testPk undefined testMetadata)
            listWallets db `shouldReturn` [testPk]

        it "create and get meta works" $ do
            db <- newDBLayer Nothing
            now <- getCurrentTime
            let md = testMetadata { passphraseInfo = Just $ WalletPassphraseInfo now }
            unsafeRunExceptT (createWallet db testPk undefined md)
            readWalletMeta db testPk `shouldReturn` Just md

        it "create twice is handled" $ do
            db <- newDBLayer Nothing
            let create' = createWallet db testPk undefined testMetadata
            runExceptT create' `shouldReturn` (Right ())
            runExceptT create' `shouldReturn` (Left (ErrWalletAlreadyExists testWid))

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

deriving instance Show (PrimaryKey WalletId)
