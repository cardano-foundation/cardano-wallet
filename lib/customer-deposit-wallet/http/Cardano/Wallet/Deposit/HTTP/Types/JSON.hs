{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Data types with a JSON schema.
module Cardano.Wallet.Deposit.HTTP.Types.JSON
    ( ApiT (..)

      -- * Re-exports
    , Address
    , Customer
    , CustomerList
    , ChainPoint (..)
    )
where

import Prelude

import Cardano.Wallet.Deposit.HTTP.Types.JSON.Encoding
    ( ViaText (..)
    )
import Cardano.Wallet.Deposit.HTTP.Types.OpenAPI
    ( addressSchema
    , chainPointSchema
    , customerListSchema
    , customerSchema
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    , ChainPoint (..)
    )
import Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMnemonic
    , PostWalletViaXPub
    )
import Control.Applicative
    ( (<|>)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.=)
    )
import Data.Aeson.Types
    ( Parser
    )
import Data.Bifunctor
    ( first
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString.Short
    ( fromShort
    , toShort
    )
import Data.OpenApi
    ( NamedSchema (..)
    , ToSchema (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    , getTextDecodingError
    )
import Servant
    ( FromHttpApiData (..)
    )

import qualified Cardano.Wallet.Read as Read
import qualified Cardano.Wallet.Read.Hash as Hash
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-----------------------------------------------------------------------------
    Additional type definitions
------------------------------------------------------------------------------}

type CustomerList = [(Customer, Address)]

{-----------------------------------------------------------------------------
   ApiT
------------------------------------------------------------------------------}

newtype ApiT a = ApiT {unApiT :: a}
    deriving (Eq, Ord, Show)

{-----------------------------------------------------------------------------
    JSON encodings
------------------------------------------------------------------------------}

-- Address
instance ToText (ApiT Address) where
    toText =
        T.decodeUtf8
            . convertToBase Base16
            . fromShort
            . Read.toShortByteString
            . unApiT

instance FromText (ApiT Address) where
    fromText t = do
        bytes <-
            first textDecodingError
                . convertFromBase Base16
                $ T.encodeUtf8 t
        maybe (Left errInvalidAddress) (Right . ApiT)
            . Read.fromShortByteString
            $ toShort bytes
      where
        errInvalidAddress = TextDecodingError $ "Invalid address: " <> show t
        textDecodingError = TextDecodingError . show

-- FIXME: Bech32 encodings
deriving via ViaText (ApiT Address) instance FromJSON (ApiT Address)
deriving via ViaText (ApiT Address) instance ToJSON (ApiT Address)

instance ToSchema (ApiT Address) where
    declareNamedSchema _ = do
        pure
            $ NamedSchema
                (Just "ApiT Address")
                addressSchema

-- Customer
instance FromHttpApiData (ApiT Customer) where
    parseUrlPiece = fmap (ApiT . toEnum) . fromText'

instance FromJSON (ApiT Customer) where
    parseJSON = fmap (ApiT . toEnum) . parseJSON

instance ToJSON (ApiT Customer) where
    toJSON = toJSON . fromEnum . unApiT

instance ToSchema (ApiT Customer) where
    declareNamedSchema _ = do
        pure
            $ NamedSchema
                (Just "ApiT Customer")
                customerSchema

-- | 'fromText' but with a simpler error type.
fromText' :: FromText a => Text -> Either Text a
fromText' = first (T.pack . getTextDecodingError) . fromText

instance ToJSON (ApiT (Customer, Address)) where
    toJSON (ApiT (c, a)) =
        object
            [ "customer" .= toJSON (ApiT c)
            , "address" .= toJSON (ApiT a)
            ]

instance FromJSON (ApiT (Customer, Address)) where
    parseJSON = withObject "ApiT (Customer, Address)" $ \obj -> do
        customerApiT <- obj .: "customer"
        addressApiT <- obj .: "address"
        pure $ ApiT (unApiT customerApiT, unApiT addressApiT)

instance FromJSON (ApiT CustomerList) where
    parseJSON l = do
        custoList <- (parseJSON l :: Parser [ApiT (Customer, Address)])
        pure $ ApiT (unApiT <$> custoList)

instance ToJSON (ApiT CustomerList) where
    toJSON (ApiT cl) = toJSON (toJSON . ApiT <$> cl)

instance ToSchema (ApiT CustomerList) where
    declareNamedSchema _ = do
        pure
            $ NamedSchema
                (Just "ApiT CustomerList")
                customerListSchema

instance ToJSON (ApiT ChainPoint) where
    toJSON (ApiT Read.GenesisPoint) = "genesis"
    toJSON (ApiT (Read.BlockPoint{slotNo, headerHash})) =
        object
            [ "slot_no"
                .= Read.unSlotNo slotNo
            , "header_hash"
                .= Hash.hashToTextAsHex headerHash
            ]

instance FromJSON (ApiT ChainPoint) where
    parseJSON payload = parseOrigin payload <|> parseSlot payload
      where
        parseOrigin = withText "genesis" $ \txt ->
            if txt == "genesis"
                then pure $ ApiT Read.GenesisPoint
                else fail "'genesis' is expected."
        parseSlot = withObject "slot_no" $ \obj -> do
            slotNo <- Read.SlotNo <$> obj .: "slot_no"
            headerHashText <- obj .: "header_hash"
            headerHash <-
                case Hash.hashFromTextAsHex headerHashText of
                    Nothing -> fail "invalid 'header_hash'"
                    Just hash -> pure hash
            pure $ ApiT Read.BlockPoint{slotNo, headerHash}

instance ToSchema (ApiT ChainPoint) where
    declareNamedSchema _ = do
        pure
            $ NamedSchema
                (Just "ApiT ChainPoint")
                chainPointSchema

instance FromJSON PostWalletViaMnemonic

instance FromJSON PostWalletViaXPub
