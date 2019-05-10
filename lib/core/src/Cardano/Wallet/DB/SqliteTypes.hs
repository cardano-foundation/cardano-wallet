{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.SqliteTypes where

import Prelude

import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , Hash (..)
    , SlotId (..)
    , TxStatus (..)
    , WalletId (..)
    , WalletState (..)
    , flatSlot
    , fromFlatSlot
    , isValidCoin
    )
import Control.Monad
    ( (>=>) )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , defaultOptions
    , genericParseJSON
    , genericToJSON
    , withText
    )
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
import GHC.Generics
    ( Generic )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )
import Web.PathPieces
    ( PathPiece (..) )

import qualified Data.Text as T

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
-- AddressScheme

data AddressScheme = Sequential | Random | Any deriving (Show, Eq, Generic)

instance PersistField AddressScheme where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql AddressScheme where
    sqlType _ = sqlType (Proxy @Text)

instance FromText AddressScheme where
    fromText txt = case txt of
        "sequential" -> Right Sequential
        "random" -> Right Random
        "any" -> Right Any
        _ ->
            Left . TextDecodingError $ show txt
                <> " is neither \"sequential\", \"random\", nor \"any\""

instance ToText AddressScheme where
    toText Sequential = "sequential"
    toText Random = "random"
    toText Any = "any"

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
    parseJSON = fmap TxId . aesonFromText "WalletId"

instance ToHttpApiData TxId where
    toUrlPiece = toText . getTxId

instance FromHttpApiData TxId where
    parseUrlPiece = fmap TxId . fromText'

instance PathPiece TxId where
    toPathPiece = toText . getTxId
    fromPathPiece = fmap TxId . fromTextMaybe

----------------------------------------------------------------------------
-- SlotId

instance PersistFieldSql SlotId where
    sqlType _ = sqlType (Proxy @Word64)

instance PersistField SlotId where
    toPersistValue = toPersistValue . flatSlot
    fromPersistValue = fmap fromFlatSlot . fromPersistValue

instance ToJSON SlotId where
    toJSON = genericToJSON defaultOptions

instance FromJSON SlotId where
    parseJSON = genericParseJSON defaultOptions

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
    toPersistValue =
        toPersistValue . toText
    fromPersistValue pv = first (const err) (fromPersistValue pv)
        where err = "not a valid value: " <> T.pack (show pv)

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
