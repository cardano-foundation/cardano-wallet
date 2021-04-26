{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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

import Cardano.Address.Script
    ( Cosigner, KeyHash (..), Script, ScriptHash (..) )
import Cardano.Api
    ( TxMetadataJsonSchema (..)
    , displayError
    , metadataFromJson
    , metadataToJson
    )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..), PassphraseScheme (..), Role (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Script
    ( CredentialType )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPoolGap (..)
    , DerivationPrefix
    , getAddressPoolGap
    , mkAddressPoolGap
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..)
    , FeePolicy
    , PoolId
    , PoolMetadataSource (..)
    , PoolOwner (..)
    , StakeKeyCertificate (..)
    , StakePoolMetadataHash (..)
    , StakePoolMetadataUrl (..)
    , StakePoolTicker
    , WalletId (..)
    , isValidEpochNo
    , unsafeEpochNo
    , unsafeToPMS
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..), isValidCoin )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Direction (..), SealedTx (..), TxMetadata, TxStatus (..) )
import Control.Arrow
    ( left )
import Control.Monad
    ( (<=<), (>=>) )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), Value (..), withText )
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
import Data.Text.Encoding
    ( decodeUtf8, encodeUtf8 )
import Data.Time.Clock.POSIX
    ( POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Data.Time.Format
    ( defaultTimeLocale, formatTime, iso8601DateFormat, parseTimeM )
import Data.Word
    ( Word32, Word64 )
import Data.Word.Odd
    ( Word31 )
import Database.Persist.Sqlite
    ( PersistField (..), PersistFieldSql (..), PersistValue (..) )
import Database.Persist.TH
    ( MkPersistSettings (..), sqlSettings )
import GHC.Generics
    ( Generic )
import Network.URI
    ( parseAbsoluteURI )
import System.Random
    ( StdGen )
import Text.Read
    ( readMaybe )
import Web.HttpApiData
    ( FromHttpApiData (..), ToHttpApiData (..) )
import Web.PathPieces
    ( PathPiece (..) )

import qualified Cardano.Address.Script as CA
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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

-- | Wraps 'Hash "Tx"' because the persistent entity syntax doesn't seem to
-- support parameterized types.
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

--------------------------------------------------------------------------------
-- Tokens
--------------------------------------------------------------------------------

instance PersistField TokenName where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql TokenName where
    sqlType _ = sqlType (Proxy @Text)

instance PersistField TokenPolicyId where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql TokenPolicyId where
    sqlType _ = sqlType (Proxy @Text)

instance PersistField TokenQuantity where
    -- SQLite has no big integer type, so we use a textual representation
    -- instead.
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql TokenQuantity where
    -- SQLite has no big integer type, so we use a textual representation
    -- instead.
    sqlType _ = sqlType (Proxy @Text)

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

instance PersistFieldSql SlotNo where
    sqlType _ = sqlType (Proxy @Word64)

instance Read SlotNo where
    readsPrec _ = error "readsPrec stub needed for persistent"

persistSlotNo :: SlotNo -> PersistValue
persistSlotNo = toPersistValue . unSlotNo

unPersistSlotNo :: PersistValue -> Either Text SlotNo
unPersistSlotNo = fmap SlotNo . fromPersistValue

instance PersistField SlotNo where
    toPersistValue = persistSlotNo
    fromPersistValue = unPersistSlotNo

instance ToHttpApiData SlotNo where
    toUrlPiece = error "toUrlPiece stub needed for persistent"
instance FromHttpApiData SlotNo where
    parseUrlPiece = error "parseUrlPiece stub needed for persistent"
instance PathPiece SlotNo where
    toPathPiece = error "toPathPiece stub needed for persistent"
    fromPathPiece = error "fromPathPiece stub needed for persistent"

----------------------------------------------------------------------------
-- EpochNo

instance PersistFieldSql EpochNo where
    sqlType _ = sqlType (Proxy @Word32)

mkEpochNo :: Word32 -> Either Text EpochNo
mkEpochNo n
    | isValidEpochNo c = Right c
    | otherwise = Left . T.pack $ "not a valid epoch number: " <> show n
    where c = unsafeEpochNo n

persistEpochNo :: EpochNo -> PersistValue
persistEpochNo = toPersistValue . fromIntegral @Word31 @Word32 . unEpochNo

instance PersistField EpochNo where
    toPersistValue = persistEpochNo
    fromPersistValue = fromPersistValue >=> mkEpochNo

instance ToHttpApiData EpochNo where
    toUrlPiece = error "toUrlPiece stub needed for persistent"
instance FromHttpApiData EpochNo where
    parseUrlPiece = error "parseUrlPiece stub needed for persistent"
instance PathPiece EpochNo where
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
-- TxMetadata

instance PersistField TxMetadata where
    toPersistValue =
        toPersistValue .
        decodeUtf8 .
        BL.toStrict .
        Aeson.encode .
        metadataToJson TxMetadataJsonDetailedSchema
    fromPersistValue =
        (left (T.pack . displayError) . metadataFromJsonWithFallback) <=<
        (left T.pack . Aeson.eitherDecode . BL.fromStrict . encodeUtf8) <=<
        fromPersistValue
      where
        -- FIXME
        -- Because of time constraints, we have had two consecutives releases
        -- of cardano-wallet which ended up using different conversions method
        -- for metadata to/from JSON.
        -- As a result, some users' databases contain metadata using the direct
        -- JSON conversion while we now expect the detailed schema variant.
        --
        -- We do therefore fallback when deserializing data do the direct
        -- conversion (which will then be serialized back using the detailed
        -- schema). We can remove that fallback after some time has passed since
        -- release v2020-09-22.
        metadataFromJsonWithFallback json =
            case metadataFromJson TxMetadataJsonDetailedSchema json of
                Right meta -> Right meta
                Left e -> case metadataFromJson TxMetadataJsonNoSchema json of
                    Right meta -> Right meta
                    Left{} -> Left e

instance PersistFieldSql TxMetadata where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- SealedTx - store the serialised tx as a binary blob

instance PersistField SealedTx where
    toPersistValue = toPersistValue . getSealedTx
    fromPersistValue = fmap SealedTx . fromPersistValue

instance PersistFieldSql SealedTx where
    sqlType _ = sqlType (Proxy @ByteString)

----------------------------------------------------------------------------
-- Coin

mkCoin :: Word64 -> Either Text Coin
mkCoin n
    | isValidCoin c = Right c
    | otherwise = Left . T.pack $ "not a valid coin: " <> show n
    where c = Coin n

instance PersistField Coin where
    toPersistValue = toPersistValue . unCoin
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
-- ScriptHash

instance ToText ScriptHash where
    toText (ScriptHash sh) =
        T.decodeUtf8 $ convertToBase Base16 sh

instance FromText ScriptHash where
    fromText = bimap textDecodingError ScriptHash
        . convertFromBase Base16
        . T.encodeUtf8
      where
        textDecodingError = TextDecodingError . show

instance PersistField ScriptHash where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql ScriptHash where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- KeyHash

instance ToText KeyHash where
    toText (KeyHash keyRole sh) =
        T.append keyRoleTxt $ T.decodeUtf8 $ convertToBase Base16 sh
      where
          keyRoleTxt = case keyRole of
              CA.Payment -> "0"
              CA.Delegation -> "2"

instance FromText KeyHash where
    fromText txt = do
        (keyRole, txt') <- case T.uncons txt of
            Just (firstChar, rest) ->case firstChar of
                '0' -> pure (CA.Payment, rest)
                '2' -> pure (CA.Delegation, rest)
                _ -> Left $ TextDecodingError "KeyHash should begin with either '0' or '1'."
            Nothing -> Left $ TextDecodingError "KeyHash should not be empty."
        bimap textDecodingError (KeyHash keyRole)
            $ convertFromBase Base16 $ T.encodeUtf8 txt'
      where
        textDecodingError = TextDecodingError . show

instance PersistField KeyHash where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql KeyHash where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- Script Cosigner

instance PersistField (Script Cosigner) where
    toPersistValue =
        toPersistValue .
        decodeUtf8 .
        BL.toStrict .
        Aeson.encode .
        toJSON
    fromPersistValue =
        (left T.pack . Aeson.eitherDecode . BL.fromStrict . encodeUtf8) <=<
        fromPersistValue

instance PersistFieldSql (Script Cosigner) where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- CredentialType

instance PersistField CredentialType where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql CredentialType where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- AddressPoolGap

instance PersistField AddressPoolGap where
    toPersistValue = toPersistValue . getAddressPoolGap
    fromPersistValue pv = fromPersistValue >=> mkAddressPoolGap' $ pv
      where
        mkAddressPoolGap' :: Word32 -> Either Text AddressPoolGap
        mkAddressPoolGap' = first msg . mkAddressPoolGap . fromIntegral
        msg e = T.pack $ "not a valid value: " <> show pv <> ": " <> show e

instance PersistFieldSql AddressPoolGap where
    sqlType _ = sqlType (Proxy @Word32)

----------------------------------------------------------------------------
-- Role

instance PersistField Role where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql Role where
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

instance FromText [PoolOwner] where
    fromText t = mapM fromText $ T.words t

instance PersistField [PoolOwner] where
    toPersistValue v = toPersistValue $ T.unwords $ toText <$> v
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql [PoolOwner] where
    sqlType _ = sqlType (Proxy @Text)

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

----------------------------------------------------------------------------
-- RewardAccount

instance PersistField RewardAccount where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql RewardAccount where
    sqlType _ = sqlType (Proxy @Text)

instance Read RewardAccount where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance ToHttpApiData RewardAccount where
    toUrlPiece = toText

instance FromHttpApiData RewardAccount where
    parseUrlPiece = fromText'

instance ToJSON RewardAccount where
    toJSON = String . toText

instance FromJSON RewardAccount where
    parseJSON = aesonFromText "RewardAccount"

instance PathPiece RewardAccount where
    fromPathPiece = fromTextMaybe
    toPathPiece = toText

----------------------------------------------------------------------------
-- AddressState

instance PersistField AddressState where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql AddressState where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- PoolMetadataSource


instance PersistField PoolMetadataSource where
    toPersistValue = toPersistValue . toText
    -- be more permissive than fromText here
    fromPersistValue = fromPersistValue
        >=> \case
            "none" -> Right FetchNone
            "direct" -> Right FetchDirect
            uri -> fmap unsafeToPMS
                . maybe (Left "Not an absolute URI") Right
                . parseAbsoluteURI
                $ uri

instance PersistFieldSql PoolMetadataSource where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- DerivationPrefix

instance PersistField DerivationPrefix where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql DerivationPrefix where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- Other

instance PersistField POSIXTime where
    toPersistValue = PersistText
        . T.pack
        . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
        . posixSecondsToUTCTime
    fromPersistValue (PersistText time) =
        utcTimeToPOSIXSeconds <$>
            getEitherText (parseTimeM True defaultTimeLocale
                (iso8601DateFormat (Just "%H:%M:%S")) (T.unpack time))
    fromPersistValue _ = Left
        "Could not parse POSIX time value"

instance PersistFieldSql POSIXTime where
    sqlType _ = sqlType (Proxy @Text)


-- | Newtype to get a MonadFail instance for @Either Text@.
--
-- We need it to use @parseTimeM@.
newtype EitherText a = EitherText { getEitherText :: Either Text a }
    deriving (Functor, Applicative, Monad) via (Either Text)

instance MonadFail EitherText where
    fail = EitherText . Left . T.pack
