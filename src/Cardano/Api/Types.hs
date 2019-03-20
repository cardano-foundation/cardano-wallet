{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Api.Types
    (
    -- * API Types
      Amount(..)
    , CurrencyUnit(..)
    , Percentage(..)
    , Wallet(..)
    , WalletAddressPoolGap(..)
    , WalletBalance(..)
    , WalletDelegation(..)
    , WalletDelegationStatus(..)
    , WalletName(..)
    , WalletId (..)
    , WalletPassphraseInfo(..)
    , WalletState(..)
    , WalletStateStatus(..)
    , mkWalletName
    , walletNameMinLength
    , walletNameMaxLength

    -- * Polymorphic Types
    ) where

import Prelude

import Cardano.Wallet.AddressDiscovery
    ( AddressPoolGap, getAddressPoolGap, mkAddressPoolGap )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , camelTo2
    , constructorTagModifier
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , omitNothingFields
    , tagSingleConstructors
    )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( UTCTime )
import Data.UUID.Types
    ( UUID )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T


{-------------------------------------------------------------------------------
                                  API Types
-------------------------------------------------------------------------------}

data Amount = Amount
    { _quantity :: Natural
    , _unit :: CurrencyUnit
    } deriving (Eq, Generic, Show)

instance FromJSON Amount where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON Amount where
    toJSON = genericToJSON defaultRecordTypeOptions


data CurrencyUnit
    = Lovelace
    deriving (Eq, Generic, Show)

instance FromJSON CurrencyUnit where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON CurrencyUnit where
    toJSON = genericToJSON defaultSumTypeOptions


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
      where
        validate :: UnvalidatedPercentage -> Either PercentageError Percentage
        validate = \case
            UnvalidatedPercentage q "percent" -> mkPercentage q
            UnvalidatedPercentage _ _ -> Left PercentageUnitInvalidError

instance ToJSON Percentage where
    toJSON = toJSON . unvalidate
      where
        unvalidate :: Percentage -> UnvalidatedPercentage
        unvalidate p = UnvalidatedPercentage (getPercentage p) "percent"

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
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON UnvalidatedPercentage where
    toJSON = genericToJSON defaultRecordTypeOptions

data Wallet = Wallet
    { _id :: !WalletId
    , _addressPoolGap :: !WalletAddressPoolGap
    , _balance :: !WalletBalance
    , _delegation :: !WalletDelegation
    , _name :: !WalletName
    , _passphrase :: !WalletPassphraseInfo
    , _state :: !WalletState
    } deriving (Eq, Generic, Show)

instance FromJSON Wallet where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON Wallet where
    toJSON = genericToJSON defaultRecordTypeOptions

newtype WalletAddressPoolGap = WalletAddressPoolGap
    { getWalletAddressPoolGap :: AddressPoolGap }
    deriving stock (Generic, Show)
    deriving newtype (Bounded, Enum, Eq, Ord)

instance FromJSON WalletAddressPoolGap where
    parseJSON x = do
        gap <- parseJSON x
        WalletAddressPoolGap <$> eitherToParser (mkAddressPoolGap gap)
instance ToJSON WalletAddressPoolGap where
    toJSON = toJSON . getAddressPoolGap . getWalletAddressPoolGap


data WalletBalance = WalletBalance
    { _available :: !Amount
    , _total :: !Amount
    } deriving (Eq, Generic, Show)

instance FromJSON WalletBalance where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON WalletBalance where
    toJSON = genericToJSON defaultRecordTypeOptions


newtype WalletDelegation = WalletDelegation
    { _status :: WalletDelegationStatus
    } deriving (Eq, Generic, Show)

instance FromJSON WalletDelegation where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON WalletDelegation where
    toJSON = genericToJSON defaultRecordTypeOptions


data WalletDelegationStatus
    = Delegating
    | NotDelegating
    deriving (Eq, Generic, Show)

instance FromJSON WalletDelegationStatus where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON WalletDelegationStatus where
    toJSON = genericToJSON defaultSumTypeOptions


newtype WalletId = WalletId
    { _uuid :: UUID }
    deriving stock (Eq, Show)
    deriving newtype (FromJSON, ToJSON)


newtype WalletName = WalletName
    { getWalletName :: Text }
    deriving stock (Eq, Generic, Show)
    deriving newtype (ToJSON)

data WalletNameError
    = WalletNameTooShortError
    | WalletNameTooLongError
    deriving Show

instance FromJSON WalletName where
    parseJSON x = eitherToParser . mkWalletName =<< parseJSON x

walletNameMinLength :: Int
walletNameMinLength = 1

walletNameMaxLength :: Int
walletNameMaxLength = 255

mkWalletName :: Text -> Either WalletNameError WalletName
mkWalletName n
    | T.length n < walletNameMinLength = Left WalletNameTooShortError
    | T.length n > walletNameMaxLength = Left WalletNameTooLongError
    | otherwise = Right $ WalletName n


newtype WalletPassphraseInfo = WalletPassphraseInfo
    { _lastUpdatedAt :: UTCTime
    } deriving (Eq, Generic, Show)

instance FromJSON WalletPassphraseInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON WalletPassphraseInfo where
    toJSON = genericToJSON defaultRecordTypeOptions


data WalletState
    = StateReady
    | StateRestoring Percentage
    deriving (Eq, Generic, Show)

instance FromJSON WalletState where
    parseJSON x = either (fail . show) pure . validate =<< parseJSON x
      where
        validate :: UnvalidatedWalletState -> Either WalletStateError WalletState
        validate = \case
            UnvalidatedWalletState Ready Nothing ->
                pure StateReady
            UnvalidatedWalletState Restoring (Just p) ->
                pure $ StateRestoring p
            UnvalidatedWalletState Ready (Just _) ->
                Left WalletInReadyStateCannotHaveProgressPercentage
            UnvalidatedWalletState Restoring Nothing ->
                Left WalletInRestoringStateMustHaveProgressPercentage

instance ToJSON WalletState where
    toJSON = toJSON . unvalidate
      where
        unvalidate :: WalletState -> UnvalidatedWalletState
        unvalidate = \case
            StateReady ->
                UnvalidatedWalletState Ready Nothing
            StateRestoring p ->
                UnvalidatedWalletState Restoring (Just p)

data UnvalidatedWalletState = UnvalidatedWalletState
    { _status :: WalletStateStatus
    , _progress :: Maybe Percentage
    } deriving Generic

data WalletStateError
    = WalletInReadyStateCannotHaveProgressPercentage
    | WalletInRestoringStateMustHaveProgressPercentage
    deriving Show

instance FromJSON UnvalidatedWalletState where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON UnvalidatedWalletState where
    toJSON = genericToJSON defaultRecordTypeOptions

data WalletStateStatus
    = Ready
    | Restoring
    deriving (Eq, Generic, Show)

instance FromJSON WalletStateStatus where
    parseJSON = genericParseJSON defaultSumTypeOptions
instance ToJSON WalletStateStatus where
    toJSON = genericToJSON defaultSumTypeOptions

{-------------------------------------------------------------------------------
                              Polymorphic Types
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------
                                Aeson Options
-------------------------------------------------------------------------------}

defaultSumTypeOptions :: Aeson.Options
defaultSumTypeOptions = Aeson.defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , tagSingleConstructors = True }

defaultRecordTypeOptions :: Aeson.Options
defaultRecordTypeOptions = Aeson.defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 1
    , omitNothingFields = True }


{-------------------------------------------------------------------------------
                                   Helpers
-------------------------------------------------------------------------------}

eitherToParser :: Show s => Either s a -> Aeson.Parser a
eitherToParser = either (fail . show) pure
