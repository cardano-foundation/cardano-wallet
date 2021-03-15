{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Auto-generated Sqlite & Persistent machinery via Template-Haskell. This has
-- been moved into a separate file so that we can treat it slightly differently
-- when computing code-coverage.

module Cardano.SharedWallet.DB.Sqlite.TH where

import Prelude

import Cardano.Address.Script
    ( Cosigner, Script )
import Cardano.SharedWallet.Script
    ( CredentialType )
import Cardano.SharedWallet.SharedState
    ( SharedWalletState )
import Cardano.Wallet.DB.Sqlite.Types
    ( BlockId, sqlSettings' )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.Word
    ( Word32, Word8 )
import Database.Persist.Class
    ( AtLeastOneUniqueKey (..), OnlyOneUniqueKey (..) )
import Database.Persist.TH
    ( mkDeleteCascade, mkMigrate, mkPersist, persistLowerCase, share )
import GHC.Generics
    ( Generic (..) )

import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as W
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.ByteString.Char8 as B8

share
    [ mkPersist sqlSettings'
    , mkDeleteCascade sqlSettings'
    , mkMigrate "migrateAll"
    ]
    [persistLowerCase|

-- Shared Wallet
SharedWallet
    sharedWalletWalletId                 W.WalletId                sql=wallet_id
    sharedWalletCreationTime             UTCTime                   sql=creation_time
    sharedWalletUpdateTime               UTCTime Maybe             sql=update_time
    sharedWalletName                     Text                      sql=name
    sharedWalletAccountXPub              B8.ByteString             sql=account_xpub
    sharedWalletAccountIndex             Word32                    sql=account_ix
    sharedWalletScriptGap                W.AddressPoolGap          sql=pool_gap
    sharedWalletPaymentScript            (Script Cosigner)         sql=payment_script
    sharedWalletDelegationScript         (Script Cosigner) Maybe   sql=delegation_script
    sharedWalletState                    SharedWalletState         sql=wallet_state
    sharedWalletGenesisHash              BlockId                   sql=genesis_hash
    sharedWalletGenesisStart             UTCTime                   sql=genesis_start

    Primary sharedWalletWalletId
    deriving Show Generic

CosignerKey
    cosignerKeyWalletId                  W.WalletId                sql=wallet_id
    cosignerKeyCreationTime              UTCTime                   sql=creation_time
    cosignerKeyCredential                CredentialType            sql=credential
    cosignerKeyAccountXPub               B8.ByteString             sql=account_xpub
    cosignerKeyIndex                     Word8                     sql=cosigner_ix

    Primary
        cosignerKeyWalletId
        cosignerKeyCredential
        cosignerKeyIndex
    Foreign SharedWallet fk_shared_wallet_cosigner_key cosignerKeyWalletId ! ON DELETE CASCADE
    deriving Show Generic
|]
