{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Api.V2.Types.WalletState
    ( WalletState (..)
    , WalletStateError (..)
    ) where

import Prelude

import Cardano.Wallet.Api.V2.JSON
    ( defaultRecordTypeOptions )
import Data.Aeson
    ( FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Api.V2.Types.Percentage as T
import qualified Cardano.Wallet.Api.V2.Types.WalletStateStatus as T

data WalletState
    = Ready
    | Restoring T.Percentage
    deriving (Eq, Generic, Show)

data WalletStateError
    = WalletInReadyStateCannotHaveProgressPercentage
    | WalletInRestoringStateMustHaveProgressPercentage
    deriving Show

data UnvalidatedWalletState = UnvalidatedWalletState
    { _status :: T.WalletStateStatus
    , _progress :: Maybe T.Percentage
    } deriving Generic

instance FromJSON WalletState where
    parseJSON x = either (fail . show) pure . validate =<< parseJSON x
instance ToJSON WalletState where
    toJSON = toJSON . unvalidate

instance FromJSON UnvalidatedWalletState where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON UnvalidatedWalletState where
    toJSON = genericToJSON defaultRecordTypeOptions

validate :: UnvalidatedWalletState -> Either WalletStateError WalletState
validate = \case
    UnvalidatedWalletState T.Ready Nothing ->
        pure Ready
    UnvalidatedWalletState T.Restoring (Just p) ->
        pure $ Restoring p
    UnvalidatedWalletState T.Ready (Just _) ->
        Left WalletInReadyStateCannotHaveProgressPercentage
    UnvalidatedWalletState T.Restoring Nothing ->
        Left WalletInRestoringStateMustHaveProgressPercentage

unvalidate :: WalletState -> UnvalidatedWalletState
unvalidate = \case
    Ready ->
        UnvalidatedWalletState T.Ready Nothing
    Restoring p ->
        UnvalidatedWalletState T.Restoring (Just p)
