{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2023 IOHK
-- License: Apache-2.0
--
-- Auto-generated Sqlite & Persistent machinery via Template-Haskell. This has
-- been moved into a separate file so that we can treat it slightly differently
-- when computing code-coverage.
--
-- More than 6K lines end-up being generated from the instructions below! As a
-- result, we're going to ignore code-coverage on the following module and, no
-- hand-written functions should be written in this module!

module Cardano.Wallet.DB.Store.Delegations.Migration.Schema where

import Prelude

import Cardano.Pool.Types
    ( PoolId )
import Cardano.Slotting.Slot
    ( SlotNo )
import Cardano.Wallet.DB.Sqlite.Types
    ( sqlSettings' )
import Crypto.Hash
    ( Blake2b_160, Digest, digestFromByteString )
import Data.Aeson
    ( FromJSON (parseJSON), ToJSON (toJSON), Value (String) )
import Data.Aeson.Extra
    ( aesonFromText )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Data
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Database.Persist.Class
    ( PersistField (..) )
import Database.Persist.PersistValue.Extended
    ( fromPersistValueFromText, fromPersistValueRead )
import Database.Persist.Sql
    ( PersistFieldSql (..) )
import Database.Persist.TH
    ( mkPersist, persistLowerCase, share )
import GHC.Generics
    ( Generic (..) )

import qualified Data.Text.Encoding as T

newtype WalletId = WalletId { getWalletId :: Digest Blake2b_160 }
    deriving (Generic, Eq, Ord, Show)

instance FromText WalletId where
    fromText txt = maybe
        (Left $ TextDecodingError msg)
        (Right . WalletId)
        (decodeHex txt >>= digestFromByteString @_ @ByteString)
      where
        msg = "wallet id should be a hex-encoded string of 40 characters"
        decodeHex =
            either (const Nothing) Just . convertFromBase Base16 . T.encodeUtf8

instance ToText WalletId where
    toText = T.decodeUtf8 . convertToBase Base16 . getWalletId

instance PersistField WalletId where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql WalletId where
    sqlType _ = sqlType (Proxy @Text)

instance Read WalletId where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToJSON WalletId where
    toJSON = String . toText

instance FromJSON WalletId where
    parseJSON = aesonFromText "WalletId"

data WStakeKeyCertificate
    = StakeKeyRegistration
    | StakeKeyDeregistration
    deriving (Generic, Show, Read, Eq)

instance PersistField WStakeKeyCertificate where
    toPersistValue = toPersistValue . show
    fromPersistValue = fromPersistValueRead

instance PersistFieldSql WStakeKeyCertificate where
    sqlType _ = sqlType (Proxy @Text)

share
    [ mkPersist sqlSettings'
    ]
    [persistLowerCase|


-- Track whether the wallet's stake key is registered or not.
StakeKeyCertificate
    stakeKeyCertWalletId             WalletId            sql=wallet_id
    stakeKeyCertSlot                 SlotNo                sql=slot
    stakeKeyCertType                 WStakeKeyCertificate sql=type

    Primary stakeKeyCertWalletId stakeKeyCertSlot
    deriving Show Generic

-- Store known delegation certificates for a particular wallet
DelegationCertificate
    certWalletId             WalletId     sql=wallet_id
    certSlot                 SlotNo         sql=slot
    certPoolId               PoolId Maybe sql=delegation

    Primary certWalletId certSlot
    deriving Show Generic

|]
