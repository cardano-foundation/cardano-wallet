{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Api.Types.WalletAddressPoolGap
    ( WalletAddressPoolGap
    , WalletAddressPoolGapError (..)
    , mkWalletAddressPoolGap
    ) where

import Prelude

import Data.Aeson
    ( FromJSON, ToJSON, parseJSON )
import GHC.Generics
    ( Generic )

newtype WalletAddressPoolGap = WalletAddressPoolGap
    { getWalletAddressPoolGap :: Int
    } deriving (Eq, Generic, Ord, Show, ToJSON)

data WalletAddressPoolGapError
    = WalletAddressPoolGapTooSmallError
    | WalletAddressPoolGapTooLargeError
    deriving Show

instance Bounded WalletAddressPoolGap where
    minBound = WalletAddressPoolGap 10
    maxBound = WalletAddressPoolGap 100

instance Enum WalletAddressPoolGap where
    toEnum = unsafeMkWalletAddressPoolGap
    fromEnum = getWalletAddressPoolGap

instance FromJSON WalletAddressPoolGap where
    parseJSON x =
        either (fail . show) pure . parseWalletAddressPoolGap =<< parseJSON x

mkWalletAddressPoolGap
    :: Integral i => i -> Either WalletAddressPoolGapError WalletAddressPoolGap
mkWalletAddressPoolGap i
    | j < minBound = Left WalletAddressPoolGapTooSmallError
    | j > maxBound = Left WalletAddressPoolGapTooLargeError
    | otherwise = pure j
  where
    j = WalletAddressPoolGap $ fromIntegral i

parseWalletAddressPoolGap
    :: Int -> Either WalletAddressPoolGapError WalletAddressPoolGap
parseWalletAddressPoolGap = mkWalletAddressPoolGap

unsafeMkWalletAddressPoolGap :: Integral i => i -> WalletAddressPoolGap
unsafeMkWalletAddressPoolGap = either (error . show) id . mkWalletAddressPoolGap
