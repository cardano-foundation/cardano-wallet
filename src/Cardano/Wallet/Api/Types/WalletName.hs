{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Api.Types.WalletName
    ( WalletName
    , WalletNameError (..)
    , getWalletName
    , mkWalletName
    , walletNameMinLength
    , walletNameMaxLength
    ) where

import Prelude

import Data.Aeson
    ( FromJSON (..), ToJSON )
import Data.Text
    ( Text )
import GHC.Generics
    ( Generic )

import qualified Data.Text as T

newtype WalletName = WalletName
    { getWalletName :: Text }
    deriving stock (Eq, Generic, Show)
    deriving newtype (ToJSON)

data WalletNameError
    = WalletNameTooShortError
    | WalletNameTooLongError
    deriving Show

instance FromJSON WalletName where
    parseJSON x = either (fail . show) pure . mkWalletName =<< parseJSON x

walletNameMinLength :: Int
walletNameMinLength = 1

walletNameMaxLength :: Int
walletNameMaxLength = 255

mkWalletName :: Text -> Either WalletNameError WalletName
mkWalletName n
    | T.length n < walletNameMinLength = Left WalletNameTooShortError
    | T.length n > walletNameMaxLength = Left WalletNameTooLongError
    | otherwise = Right $ WalletName n
