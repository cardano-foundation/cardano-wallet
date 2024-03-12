{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Data types with a JSON schema.
--
module Cardano.Wallet.Deposit.HTTP.Types.JSON
    ( ApiT (..)

    -- * Re-exports
    , Address
    , Customer
    , CustomerList
    )
    where

import Prelude

import Cardano.Wallet.Deposit.HTTP.Types.JSON.Encoding
    ( ViaText (..)
    , customOptions
    )
import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , genericParseJSON
    , genericToJSON
    )
import Data.Bifunctor
    ( bimap
    , first
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , getTextDecodingError
    )
import Servant
    ( FromHttpApiData (..)
    )

import qualified Data.Text as T

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
deriving via ViaText Address instance FromJSON (ApiT Address)
deriving via ViaText Address instance ToJSON (ApiT Address)

-- Customer
instance FromHttpApiData (ApiT Customer) where
    parseUrlPiece = fmap ApiT . fromText'

instance FromJSON (ApiT Customer) where
    parseJSON = fmap ApiT . parseJSON

instance ToJSON (ApiT Customer) where
    toJSON = toJSON . unApiT

-- | 'fromText' but with a simpler error type.
fromText' :: FromText a => Text -> Either Text a
fromText' = first (T.pack . getTextDecodingError) . fromText

-- CustomerList
type ApiCustomerList = [(ApiT Customer, ApiT Address)]

toApiCustomerList :: ApiT CustomerList -> ApiCustomerList
toApiCustomerList = fmap (bimap ApiT ApiT) . unApiT

fromApiCustomerList :: ApiCustomerList -> ApiT CustomerList
fromApiCustomerList = ApiT . fmap (bimap unApiT unApiT)

instance FromJSON (ApiT CustomerList) where
    parseJSON = fmap fromApiCustomerList . genericParseJSON customOptions

instance ToJSON (ApiT CustomerList) where
    toJSON = genericToJSON customOptions . toApiCustomerList
