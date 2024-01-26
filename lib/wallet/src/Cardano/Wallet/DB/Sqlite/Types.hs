{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2023 IOHK
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
    ( Cosigner
    , Script
    , ScriptHash (..)
    )
import Cardano.Api
    ( TxMetadataJsonSchema (..)
    , metadataFromJson
    , metadataToJson
    )
import Cardano.Pool.Types
    ( PoolId
    )
import Cardano.Slotting.Slot
    ( SlotNo (..)
    )
import Cardano.Wallet.Address.Derivation
    ( Role (..)
    )
import Cardano.Wallet.Address.Discovery
    ( ChangeAddressMode (..)
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap (..)
    , DerivationPrefix
    , getAddressPoolGap
    , mkAddressPoolGap
    )
import Cardano.Wallet.Address.Discovery.Shared
    ( CredentialType
    )
import Cardano.Wallet.DB.Store.UTxOHistory.Model
    ( Pruned (..)
    , Spent (..)
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseScheme (..)
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..)
    , FeePolicy
    , PoolMetadataSource (..)
    , Slot
    , WalletId (..)
    , WithOrigin (..)
    , isValidEpochNo
    , unsafeEpochNo
    , unsafeToPMS
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    , AddressState (..)
    )
import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.SealedTx
    ( SealedTx (..)
    , persistSealedTx
    , unPersistSealedTx
    )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( TxMetadata
    , TxScriptValidity (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..)
    , TxStatus (..)
    )
import Control.Arrow
    ( left
    )
import Control.Lens
    ( (&)
    )
import Control.Monad
    ( (<=<)
    , (>=>)
    )
import Control.Monad.Fail.Extended
    ( ReportFailure (..)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    )
import Data.Aeson.Extra
    ( aesonFromText
    )
import Data.Bifunctor
    ( bimap
    , first
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Maybe
    ( fromMaybe
    , mapMaybe
    )
import Data.Percentage
    ( Percentage
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class.Extended
    ( FromText (..)
    , TextDecodingError (TextDecodingError)
    , ToText (..)
    , fromText'
    , fromTextMaybe
    )
import Data.Text.Encoding
    ( decodeUtf8
    , encodeUtf8
    )
import Data.Time.Clock.POSIX
    ( POSIXTime
    , posixSecondsToUTCTime
    , utcTimeToPOSIXSeconds
    )
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    , parseTimeM
    )
import Data.Word
    ( Word32
    , Word64
    )
import Data.Word.Odd
    ( Word31
    )
import Database.Persist.PersistValue.Extended
    ( fromPersistValueFromText
    )
import Database.Persist.Sqlite
    ( PersistField (..)
    , PersistFieldSql (..)
    , PersistValue (..)
    )
import Database.Persist.TH
    ( MkPersistSettings (..)
    , sqlSettings
    )
import GHC.Generics
    ( Generic
    )
import GHC.Int
    ( Int64
    )
import Network.URI
    ( parseAbsoluteURI
    )
import System.Random.Internal
    ( StdGen (..)
    )
import System.Random.SplitMix
    ( seedSMGen
    , unseedSMGen
    )
import Text.Read
    ( readMaybe
    )
import Web.HttpApiData
    ( FromHttpApiData (..)
    , ToHttpApiData (..)
    )
import Web.PathPieces
    ( PathPiece (..)
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

----------------------------------------------------------------------------

-- | Settings for generating the Persistent types.
sqlSettings' :: MkPersistSettings
sqlSettings' = sqlSettings { mpsPrefixFields = False }

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

instance PersistField AssetName where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql AssetName where
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

-- | Magic value that denotes the hash of the parent of the genesis block
-- (which does not exist). This value is used for serializing
-- the Nothing case of the #parentHeaderHash field.
hashOfNoParent :: Hash "BlockHeader"
hashOfNoParent = Hash . BS.pack $ replicate 32 0

fromMaybeHash :: Maybe (Hash "BlockHeader") -> BlockId
fromMaybeHash = BlockId . fromMaybe hashOfNoParent

toMaybeHash :: BlockId -> Maybe (Hash "BlockHeader")
toMaybeHash (BlockId h) = if h == hashOfNoParent then Nothing else Just h

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
        metadataFromJsonWithFallback <=<
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
                    Left{} -> Left $
                        "fromPersistValue TxMetadata: "
                        <> "Ill-formed JSON in database "
                        <> T.pack (show e)

instance PersistFieldSql TxMetadata where
    sqlType _ = sqlType (Proxy @Text)

----------------------------------------------------------------------------
-- SealedTx - store the serialised tx as a binary blob

instance PersistField SealedTx where
    toPersistValue = toPersistValue . persistSealedTx
    fromPersistValue = fromPersistValue >=> unPersistSealedTx

instance PersistFieldSql SealedTx where
    sqlType _ = sqlType (Proxy @ByteString)

----------------------------------------------------------------------------
-- Coin

instance PersistField Coin where
    toPersistValue = toPersistValue . Coin.unsafeToWord64
    fromPersistValue = fmap Coin.fromWord64 . fromPersistValue

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

instance PersistFieldSql StdGen where
    sqlType _ = sqlType (Proxy @Text)

instance PersistField StdGen where
    toPersistValue = toPersistValue . stdGenToString
    fromPersistValue = fromPersistValue >=> stdGenFromString

-- | In @random < 1.2@ there used to be an @instance Read StdGen@, but no
-- longer.
--
-- The format used to look like this:
-- @
-- 5889121503043413025 17512980752375952679
-- @
stdGenFromString :: String -> Either Text StdGen
stdGenFromString s = case mapMaybe readMaybe (words s) of
    [i, j] -> Right $ StdGen $ seedSMGen i j
    _ -> Left "StdGen should be formatted as two space-separated integers"

-- | Equivalent to the old @random < 1.2@ 'StdGen' 'Show' instance.
stdGenToString :: StdGen -> String
stdGenToString (StdGen (unseedSMGen -> (i, j))) = unwords $ map show [i, j]

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

instance PersistField PassphraseScheme where
    toPersistValue = toPersistValue . show
    fromPersistValue = fromPersistValue >=> pure . read

instance PersistFieldSql PassphraseScheme where
    sqlType _ = sqlType (Proxy @String)

----------------------------------------------------------------------------
-- RewardAccount

instance PersistField RewardAccount where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql RewardAccount where
    sqlType _ = sqlType (Proxy @Text)

instance ToHttpApiData RewardAccount where
    toUrlPiece = toText

instance FromHttpApiData RewardAccount where
    parseUrlPiece = fromText'

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
-- ScriptValidation

instance PersistField TxScriptValidity where
    toPersistValue = \case
        TxScriptValid -> PersistBool True
        TxScriptInvalid -> PersistBool False

    fromPersistValue = \case
        PersistBool True -> Right TxScriptValid
        PersistBool False -> Right TxScriptInvalid
        x -> Left $ T.unwords
            [ "Failed to parse Haskell type `TxScriptValidity`;"
            , "expected null or boolean"
            , "from database, but received:"
            , T.pack (show x)
            ]

instance PersistFieldSql TxScriptValidity where
    sqlType _ = sqlType (Proxy @(Maybe Bool))

----------------------------------------------------------------------------
-- Other

instance PersistField POSIXTime where
    toPersistValue = PersistText
        . T.pack
        . formatTime defaultTimeLocale iso8601DateFormatHMS
        . posixSecondsToUTCTime
    fromPersistValue (PersistText time) =
        utcTimeToPOSIXSeconds <$>
            reportFailure (parseTimeM True defaultTimeLocale
                iso8601DateFormatHMS (T.unpack time)) & left T.pack
    fromPersistValue _ = Left
        "Could not parse POSIX time value"

instance PersistFieldSql POSIXTime where
    sqlType _ = sqlType (Proxy @Text)

data TxSubmissionStatusEnum = InSubmissionE | InLedgerE | ExpiredE
    deriving (Eq, Show, Enum, Generic)

instance PersistField TxSubmissionStatusEnum where
    toPersistValue = toPersistValue . fromEnum
    fromPersistValue = fmap toEnum . fromPersistValue

instance PersistFieldSql TxSubmissionStatusEnum where
    sqlType _ = sqlType (Proxy @Int)

type BlockHeight = Quantity "block" Word32

instance PersistField BlockHeight where
    toPersistValue = toPersistValue . getQuantity
    fromPersistValue = fmap Quantity . fromPersistValue

instance PersistFieldSql BlockHeight where
    sqlType _ = sqlType (Proxy @Word32)

iso8601DateFormatHMS :: String
-- Equivalent to `iso8601DateFormatHMS (Just "%H:%M:%S")`
-- The function `iso8601DateFormatHMS` has been deprecated from the `time` library.
iso8601DateFormatHMS = "%Y-%m-%dT%H:%M:%S"

data DelegationStatusEnum = InactiveE | RegisteredE | ActiveE
    deriving (Eq, Show, Enum, Generic)

instance PersistField DelegationStatusEnum where
    toPersistValue = toPersistValue . \case
        InactiveE -> "inactive" :: Text
        RegisteredE -> "registered"
        ActiveE -> "active"
    fromPersistValue = fromPersistValue >=> readDelegationStatus

readDelegationStatus :: Text -> Either Text DelegationStatusEnum
readDelegationStatus "inactive" = Right InactiveE
readDelegationStatus "registered" = Right RegisteredE
readDelegationStatus "active" = Right ActiveE
readDelegationStatus other = Left $ "Invalid delegation status: " <> other

instance PersistFieldSql DelegationStatusEnum where
    sqlType _ = sqlType (Proxy @Text)

instance PersistField Slot where
    toPersistValue Origin = toPersistValue ((-1) :: Int64)
    toPersistValue (At s) = toPersistValue (fromIntegral $ unSlotNo s :: Int64)
    fromPersistValue v = case fromPersistValue v of
        Right (PersistInt64 x)
            | x < (-1) -> Left "Slot must be positive or -1"
            | x == (-1) -> Right Origin
            | otherwise -> Right . At . SlotNo . fromIntegral $ x
        _ -> Left "Slot must be an Int64"

instance PersistFieldSql Slot where
    sqlType _ = sqlType (Proxy @Int64)

instance PersistField Pruned where
    toPersistValue NotPruned = toPersistValue @(Maybe Word64) Nothing
    toPersistValue (PrunedUpTo h) = toPersistValue (Just h)
    fromPersistValue v = case fromPersistValue @(Maybe Word64) v of
        Right Nothing -> Right NotPruned
        Right (Just x) -> Right $ PrunedUpTo $ fromIntegral x
        _ -> Left "Pruned must be an Int64 or Null"

instance PersistFieldSql Pruned where
    sqlType _ = sqlType (Proxy @(Maybe Word64))

instance PersistField Spent where
    toPersistValue Unspent = toPersistValue @(Maybe Word64) Nothing
    toPersistValue (Spent h) = toPersistValue (Just h)
    fromPersistValue v = case fromPersistValue @(Maybe Word64) v of
        Right Nothing -> Right Unspent
        Right (Just x) -> Right $ Spent $ fromIntegral x
        _ -> Left "Spent must be an Int64 or Null"

instance PersistFieldSql Spent where
    sqlType _ = sqlType (Proxy @(Maybe Word64))

----------------------------------------------------------------------------
-- ChangeAddressMode

instance PersistFieldSql ChangeAddressMode where
    sqlType _ = sqlType (Proxy @Text)

instance PersistField ChangeAddressMode where
    toPersistValue = toPersistValue . textFromChangeAddressMode
    fromPersistValue = changeAddressModeFromText <=< fromPersistValue

changeAddressModeFromText :: Text -> Either Text ChangeAddressMode
changeAddressModeFromText = \case
    "single" -> Right SingleChangeAddress
    "increasing" -> Right IncreasingChangeAddresses
    other -> Left $ "Invalid ChangeAddressMode: " <> other

textFromChangeAddressMode :: ChangeAddressMode -> Text
textFromChangeAddressMode = \case
    SingleChangeAddress -> "single"
    IncreasingChangeAddresses -> "increasing"
