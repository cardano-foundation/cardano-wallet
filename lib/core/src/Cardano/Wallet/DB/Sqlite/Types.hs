{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
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

import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..), Passphrase (..), PassphraseScheme (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..), getAddressPoolGap, mkAddressPoolGap )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Direction (..)
    , EpochLength (..)
    , EpochNo (..)
    , FeePolicy
    , Hash (..)
    , PoolId
    , PoolOwner (..)
    , SlotId (..)
    , SlotInEpoch (..)
    , StakeKeyCertificate (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    , StakePoolTicker
    , TxStatus (..)
    , WalletId (..)
    , flatSlot
    , fromFlatSlot
    , isValidCoin
    , unsafeEpochNo
    )
import Control.Arrow
    ( left )
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
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase, convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage )
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
    ( Word32, Word64, Word8 )
import Data.Word.Odd
    ( Word31 )
import Database.Persist.Sqlite
    ( PersistField (..), PersistFieldSql (..), PersistValue )
import Database.Persist.TH
    ( MkPersistSettings (..), sqlSettings )
import GHC.Generics
    ( Generic )
import System.Random
    ( StdGen )
import Text.Read
    ( readMaybe )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )
import Web.PathPieces
    ( PathPiece (..) )

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

-- | 'fromPersistValue' defined in terms of the 'Read' class
fromPersistValueRead :: Read a => PersistValue -> Either Text a
fromPersistValueRead pv = fromPersistValue pv >>= readWithErr
  where
    readWithErr = toEither . readMaybe . T.unpack
    toEither = maybe (Left $ "not a valid value: " <> T.pack (show pv)) Right


----------------------------------------------------------------------------
-- StakeKeyCertificate

instance PersistField StakeKeyCertificate where
    toPersistValue = toPersistValue . show
    fromPersistValue = fromPersistValueRead

instance PersistFieldSql StakeKeyCertificate where
    sqlType _ = sqlType (Proxy @Text)

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
-- Fee Policy

instance PersistField FeePolicy where
    toPersistValue = toPersistValue . toText
    fromPersistValue pv = fromPersistValue pv >>= left (const err) . fromText
        where err = "not a valid value: " <> T.pack (show pv)

instance PersistFieldSql FeePolicy where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- Percentage

instance PersistField Percentage where
    toPersistValue = toPersistValue . toText
    fromPersistValue pv = fromPersistValue pv >>= left (const err) . fromText
        where err = "not a valid percentage: " <> T.pack (show pv)

instance PersistFieldSql Percentage where
    sqlType _ = sqlType (Proxy @Rational)

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
-- SlotId

instance PersistFieldSql SlotId where
    sqlType _ = sqlType (Proxy @Word64)

-- | As a short-to-medium term solution of persisting 'SlotId', we use
-- 'flatSlot' with an artificial epochLength. I.e. /not the same epochLength as
-- the blockchain/. This is just for the sake of storing the epoch number and
-- slot number inside a single 64-bit field.
artificialEpochLength :: EpochLength
artificialEpochLength = EpochLength maxBound

persistSlotId :: SlotId -> PersistValue
persistSlotId = toPersistValue . flatSlot artificialEpochLength

unPersistSlotId :: PersistValue -> Either Text SlotId
unPersistSlotId = fmap (fromFlatSlot artificialEpochLength) . fromPersistValue

instance PersistField SlotId where
    toPersistValue = persistSlotId
    fromPersistValue = unPersistSlotId

instance ToJSON SlotId where
    toJSON = genericToJSON defaultOptions

instance FromJSON SlotId where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON SlotInEpoch where
    toJSON (SlotInEpoch n) = toJSON n

instance FromJSON SlotInEpoch where
    parseJSON = fmap SlotInEpoch . parseJSON

instance ToJSON EpochNo where
    toJSON (EpochNo n) = toJSON (fromIntegral @Word31 @Word32 n)

instance FromJSON EpochNo where
    parseJSON = fmap unsafeEpochNo . parseJSON

instance ToHttpApiData SlotId where
    toUrlPiece = error "toUrlPiece stub needed for persistent"
instance FromHttpApiData SlotId where
    parseUrlPiece = error "parseUrlPiece stub needed for persistent"
instance PathPiece SlotId where
    toPathPiece = error "toPathPiece stub needed for persistent"
    fromPathPiece = error "fromPathPiece stub needed for persistent"

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
-- AccountingStyle

instance PersistField AccountingStyle where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql AccountingStyle where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- StdGen

instance PersistField StdGen where
    toPersistValue = toPersistValue . show
    fromPersistValue = fromPersistValueRead

instance PersistFieldSql StdGen where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- PoolId

instance PersistField PoolId where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql PoolId where
    sqlType _ = sqlType (Proxy @Text)

instance Read PoolId where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance PathPiece PoolId where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText

instance ToJSON PoolId where
    toJSON = String . toText

instance FromJSON PoolId where
    parseJSON = aesonFromText "PoolId"

instance ToHttpApiData PoolId where
    toUrlPiece = error "toUrlPiece stub needed for persistent"

instance FromHttpApiData PoolId where
    parseUrlPiece = error "parseUrlPiece stub needed for persistent"

----------------------------------------------------------------------------
-- PoolOwner

instance PersistField PoolOwner where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql PoolOwner where
    sqlType _ = sqlType (Proxy @Text)

instance Read PoolOwner where
    readsPrec _ = error "readsPrec stub needed for persistent"

----------------------------------------------------------------------------
-- HDPassphrase

newtype HDPassphrase = HDPassphrase (Passphrase "addr-derivation-payload")
    deriving (Generic, Show)

instance PersistField HDPassphrase where
    toPersistValue (HDPassphrase (Passphrase pwd)) =
        toPersistValue (convertToBase @_ @ByteString Base16 pwd)
    fromPersistValue = fromPersistValue >=>
        fmap (HDPassphrase . Passphrase)
        . left T.pack
        . convertFromBase @ByteString Base16

instance PersistFieldSql HDPassphrase where
    sqlType _ = sqlType (Proxy @ByteString)

instance Read HDPassphrase where
    readsPrec _ = error "readsPrec stub needed for persistent"

----------------------------------------------------------------------------
-- PassphraseScheme
--

instance PersistField PassphraseScheme where
    toPersistValue = toPersistValue . show
    fromPersistValue = fromPersistValue >=> pure . read

instance PersistFieldSql PassphraseScheme where
    sqlType _ = sqlType (Proxy @String)

----------------------------------------------------------------------------
-- StakePoolTicker

instance PersistField StakePoolTicker where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql StakePoolTicker where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- StakePoolMetadataHash

instance PersistField StakePoolMetadataHash where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql StakePoolMetadataHash where
    sqlType _ = sqlType (Proxy @Text)

instance Read StakePoolMetadataHash where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToHttpApiData StakePoolMetadataHash where
    toUrlPiece = toText

instance FromHttpApiData StakePoolMetadataHash where
    parseUrlPiece = fromText'

instance ToJSON StakePoolMetadataHash where
    toJSON = String . toText

instance FromJSON StakePoolMetadataHash where
    parseJSON = aesonFromText "StakePoolMetadataHash"

instance PathPiece StakePoolMetadataHash where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText


----------------------------------------------------------------------------
-- StakePoolMetadataUrl


instance PersistField StakePoolMetadataUrl where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql StakePoolMetadataUrl where
    sqlType _ = sqlType (Proxy @Text)

instance Read StakePoolMetadataUrl where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToHttpApiData StakePoolMetadataUrl where
    toUrlPiece = toText

instance FromHttpApiData StakePoolMetadataUrl where
    parseUrlPiece = fromText'

instance ToJSON StakePoolMetadataUrl where
    toJSON = String . toText

instance FromJSON StakePoolMetadataUrl where
    parseJSON = aesonFromText "StakePoolMetadataUrl"

instance PathPiece StakePoolMetadataUrl where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText
