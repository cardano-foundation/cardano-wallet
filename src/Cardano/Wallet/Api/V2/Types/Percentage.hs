{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Api.V2.Types.Percentage
    ( Percentage
    , PercentageError (..)
    , mkPercentage
    ) where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( simpleRecordOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )

newtype Percentage = Percentage
    { getPercentage :: Int
    } deriving (Eq, Generic, Ord, Show)

data PercentageError
    = PercentageOutOfBoundsError
    | PercentageUnitInvalidError
    deriving Show

instance Bounded Percentage where
    minBound = Percentage 0
    maxBound = Percentage 100

instance Enum Percentage where
    toEnum = unsafeMkPercentage
    fromEnum = getPercentage

instance FromJSON Percentage where
    parseJSON x = either (fail . show) pure . validate =<< parseJSON x
instance ToJSON Percentage where
    toJSON = toJSON . unvalidate

mkPercentage :: Integral i => i -> Either PercentageError Percentage
mkPercentage i
    | j < minBound = Left PercentageOutOfBoundsError
    | j > maxBound = Left PercentageOutOfBoundsError
    | otherwise = pure j
  where
    j = Percentage $ fromIntegral i

unsafeMkPercentage :: Integral i => i -> Percentage
unsafeMkPercentage = either (error . show) id . mkPercentage

data UnvalidatedPercentage = UnvalidatedPercentage
    { _quantity :: Int
    , _unit :: Text
    } deriving Generic

instance FromJSON UnvalidatedPercentage where
    parseJSON = genericParseJSON simpleRecordOptions
instance ToJSON UnvalidatedPercentage where
    toJSON = genericToJSON simpleRecordOptions

validate :: UnvalidatedPercentage -> Either PercentageError Percentage
validate = \case
    UnvalidatedPercentage q "percent" -> mkPercentage q
    UnvalidatedPercentage _ _ -> Left PercentageUnitInvalidError

unvalidate :: Percentage -> UnvalidatedPercentage
unvalidate p = UnvalidatedPercentage (getPercentage p) "percent"

