{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- This module contains instances and types necessary for storing wallets in a
-- SQL database with Persistent.
--
-- It's in a separate module due to the GHC stage restriction.
--
-- The ToJSON/FromJSON and Read instance orphans exist due to class constraints
-- on Persistent functions.

module Cardano.Wallet.DB.Sqlite.Types where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain, Depth (..), Key, deserializeXPub, serializeXPub )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap (..), getAddressPoolGap, mkAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , SlotNo (..)
    , TxStatus (..)
    , WalletId (..)
    , WalletState (..)
    , isValidCoin
    )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), Value (..), withText )
import Data.Aeson.Types
    ( Parser )
import Data.Bifunctor
    ( bimap, first )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..), getPercentage, mkPercentage )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , fromTextMaybe
    , getTextDecodingError
    )
import Data.Word
    ( Word64, Word8 )
import Database.Persist.Sqlite
    ( PersistField (..), PersistFieldSql (..), PersistValue )
import Database.Persist.TH
    ( MkPersistSettings (..), sqlSettings )
import GHC.Generics
    ( Generic )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )
import Web.PathPieces
    ( PathPiece (..) )

import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

----------------------------------------------------------------------------

-- | Settings for generating the Persistent types.
sqlSettings' :: MkPersistSettings
sqlSettings' = sqlSettings { mpsPrefixFields = False }

----------------------------------------------------------------------------
-- Helper functions

-- | 'fromText' but with a simpler error type.
fromText' :: FromText a => Text -> Either Text a
fromText' = first (T.pack . getTextDecodingError) . fromText

-- | Aeson parser defined in terms of 'fromText'
aesonFromText :: FromText a => String -> Value -> Parser a
aesonFromText what = withText what $ either (fail . show) pure . fromText

-- | 'fromPersistValue' defined in terms of 'fromText'
fromPersistValueFromText :: FromText a => PersistValue -> Either Text a
fromPersistValueFromText = fromPersistValue >=> fromTextWithErr
    where fromTextWithErr = first ("not a valid value: " <>) . fromText'

----------------------------------------------------------------------------
-- Direction

instance PersistField Direction where
    toPersistValue = toPersistValue . directionToBool
    fromPersistValue pv = do
        let err = "not a valid value: " <> T.pack (show pv)
        bimap (const err) directionFromBool (fromPersistValue pv)

instance PersistFieldSql Direction where
    sqlType _ = sqlType (Proxy @Bool)

directionToBool :: Direction -> Bool
directionToBool Incoming = True
directionToBool Outgoing = False

directionFromBool :: Bool -> Direction
directionFromBool True = Incoming
directionFromBool False = Outgoing

----------------------------------------------------------------------------
-- WalletId

instance PersistField WalletId where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql WalletId where
    sqlType _ = sqlType (Proxy @Text)

instance Read WalletId where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToHttpApiData WalletId where
    toUrlPiece = toText

instance FromHttpApiData WalletId where
    parseUrlPiece = fromText'

instance ToJSON WalletId where
    toJSON = String . toText

instance FromJSON WalletId where
    parseJSON = aesonFromText "WalletId"

instance PathPiece WalletId where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText

----------------------------------------------------------------------------
-- TxId

-- Wraps Hash "Tx" because the persistent dsl doesn't like (Hash "Tx")
newtype TxId = TxId { getTxId :: Hash "Tx" } deriving (Show, Eq, Ord, Generic)

instance PersistField TxId where
    toPersistValue = toPersistValue . toText . getTxId
    fromPersistValue = fmap TxId <$> fromPersistValueFromText

instance PersistFieldSql TxId where
    sqlType _ = sqlType (Proxy @Text)

instance Read TxId where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToJSON TxId where
    toJSON = String . toText . getTxId

instance FromJSON TxId where
    parseJSON = fmap TxId . aesonFromText "TxId"

instance ToHttpApiData TxId where
    toUrlPiece = toText . getTxId

instance FromHttpApiData TxId where
    parseUrlPiece = fmap TxId . fromText'

instance PathPiece TxId where
    toPathPiece = toText . getTxId
    fromPathPiece = fmap TxId . fromTextMaybe

----------------------------------------------------------------------------
-- BlockId

-- Wraps Hash "BlockHeader" because the persistent dsl doesn't like it raw.
newtype BlockId = BlockId { getBlockId :: Hash "BlockHeader" }
    deriving (Show, Eq, Ord, Generic)

instance PersistField BlockId where
    toPersistValue = toPersistValue . toText . getBlockId
    fromPersistValue = fmap BlockId <$> fromPersistValueFromText

instance PersistFieldSql BlockId where
    sqlType _ = sqlType (Proxy @Text)

instance Read BlockId where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToJSON BlockId where
    toJSON = String . toText . getBlockId

instance FromJSON BlockId where
    parseJSON = fmap BlockId . aesonFromText "BlockId"

instance ToHttpApiData BlockId where
    toUrlPiece = toText . getBlockId

instance FromHttpApiData BlockId where
    parseUrlPiece = fmap BlockId . fromText'

instance PathPiece BlockId where
    toPathPiece = toText . getBlockId
    fromPathPiece = fmap BlockId . fromTextMaybe

----------------------------------------------------------------------------
-- SlotNo

instance PersistFieldSql SlotNo where
    sqlType _ = sqlType (Proxy @Word64)

instance PersistField SlotNo where
    toPersistValue = toPersistValue . unSlotNo
    fromPersistValue = fmap SlotNo . fromPersistValue

instance ToJSON SlotNo where
    toJSON = toJSON . unSlotNo

instance FromJSON SlotNo where
    parseJSON = fmap SlotNo . parseJSON

----------------------------------------------------------------------------
-- WalletState

walletStateNum :: WalletState -> Word8
walletStateNum Ready = 100
walletStateNum (Restoring (Quantity pc)) =
    fromIntegral $ getPercentage pc

walletStateFromNum :: Word8 -> WalletState
walletStateFromNum n | n < 100 = Restoring (Quantity pc)
                     | otherwise = Ready
    where Right pc = mkPercentage n

instance PersistField WalletState where
    toPersistValue = toPersistValue . walletStateNum
    fromPersistValue = fmap walletStateFromNum . fromPersistValue

instance PersistFieldSql WalletState where
    sqlType _ = sqlType (Proxy @Word8)

instance Read WalletState where
    readsPrec _ = error "readsPrec stub needed for persistent"

----------------------------------------------------------------------------
-- TxStatus

instance PersistField TxStatus where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql TxStatus where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- Coin

mkCoin :: Word64 -> Either Text Coin
mkCoin n
    | isValidCoin c = Right c
    | otherwise = Left . T.pack $ "not a valid coin: " <> show n
    where c = Coin n

instance PersistField Coin where
    toPersistValue = toPersistValue . getCoin
    fromPersistValue = fromPersistValue >=> mkCoin

instance PersistFieldSql Coin where
    sqlType _ = sqlType (Proxy @Word64)

----------------------------------------------------------------------------
-- Address

instance PersistField Address where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql Address where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- AddressPoolGap

instance PersistField AddressPoolGap where
    toPersistValue = toPersistValue . getAddressPoolGap
    fromPersistValue pv = fromPersistValue >=> mkAddressPoolGap' $ pv
      where
        mkAddressPoolGap' :: Word8 -> Either Text AddressPoolGap
        mkAddressPoolGap' = first msg . mkAddressPoolGap . fromIntegral
        msg e = T.pack $ "not a valid value: " <> show pv <> ": " <> show e

instance PersistFieldSql AddressPoolGap where
    sqlType _ = sqlType (Proxy @Word8)

----------------------------------------------------------------------------
-- XPub for sequential address discovery

newtype AddressPoolXPub = AddressPoolXPub
    { getAddressPoolXPub :: Key 'AccountK XPub }
    deriving (Show, Eq, Generic)

instance PersistField AddressPoolXPub where
    toPersistValue = toPersistValue . serializeXPub . getAddressPoolXPub
    fromPersistValue pv = fromPersistValue >=> deserializeXPub' $ pv
      where
        deserializeXPub' = bimap msg AddressPoolXPub . deserializeXPub
        msg e = T.pack $ "not a valid XPub: " <> show pv <> ": " <> e

instance PersistFieldSql AddressPoolXPub where
    sqlType _ = sqlType (Proxy @B8.ByteString)

----------------------------------------------------------------------------
-- ChangeChain

instance PersistField ChangeChain where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql ChangeChain where
    sqlType _ = sqlType (Proxy @Text)
