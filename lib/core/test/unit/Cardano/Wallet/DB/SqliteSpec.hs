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
    ( DBLayer (..), PrimaryKey (..) )
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
            now <- getCurrentTime
            let wid = PrimaryKey (WalletId (hash ("test" :: ByteString)))
                md = WalletMetadata
                    { name = WalletName "test wallet"
                    , passphraseInfo = Just $ WalletPassphraseInfo now
                    , status = Ready
                    , delegation = NotDelegating
                    }
            unsafeRunExceptT (createWallet db wid undefined md) `shouldReturn` ()
            listWallets db `shouldReturn` [wid]

        it "create and get meta works" $ do
            db <- newDBLayer Nothing
            now <- getCurrentTime
            let wid = PrimaryKey (WalletId (hash ("test" :: ByteString)))
                md = WalletMetadata
                    { name = WalletName "test wallet"
                    , passphraseInfo = Just $ WalletPassphraseInfo now
                    , status = Ready
                    , delegation = NotDelegating
                    }
            unsafeRunExceptT (createWallet db wid undefined md) `shouldReturn` ()
            readWalletMeta db wid `shouldReturn` Just md

deriving instance Show (PrimaryKey WalletId)
