{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Api.V2.Types.Percentage
    ( Percentage
    , PercentageError (..)
    , mkPercentage
    ) where

import Prelude

import Data.Aeson
    ( FromJSON, ToJSON, parseJSON )
import GHC.Generics
    ( Generic )

newtype Percentage = Percentage
    { getPercentage :: Int
    } deriving (Eq, Generic, Ord, Show, ToJSON)

data PercentageError = PercentageOutOfBoundsError
    deriving Show

instance Bounded Percentage where
    minBound = Percentage 0
    maxBound = Percentage 100

instance Enum Percentage where
    toEnum = unsafeMkPercentage
    fromEnum = getPercentage

instance FromJSON Percentage where
    parseJSON x = either (fail . show) pure . parsePercentage =<< parseJSON x

mkPercentage :: Integral i => i -> Either PercentageError Percentage
mkPercentage i
    | j < minBound = Left PercentageOutOfBoundsError
    | j > maxBound = Left PercentageOutOfBoundsError
    | otherwise = pure j
  where
    j = Percentage $ fromIntegral i

parsePercentage :: Int -> Either PercentageError Percentage
parsePercentage = mkPercentage

unsafeMkPercentage :: Integral i => i -> Percentage
unsafeMkPercentage = either (error . show) id . mkPercentage
