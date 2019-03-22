{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Api.Types
    (
    -- * API Types
      Amount (..)
    , Percentage (..)
    , PoolId (..)
    , Wallet (..)
    , WalletBalance (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , Payment (..)

    -- * Re-Exports From Primitive Types
    , AddressPoolGap
    , WalletName (..)
    , Passphrase

    -- * Polymorphic Types
    , ApiT (..)
    , MeasuredIn (..)
    ) where

import Prelude

import Cardano.Wallet
    ( WalletName (..), mkWalletName )
import Cardano.Wallet.AddressDerivation
    ( Passphrase )
import Cardano.Wallet.AddressDiscovery
    ( AddressPoolGap, getAddressPoolGap, mkAddressPoolGap )
import Cardano.Wallet.Primitive
    ( Address, mkAddress )
import Data.Aeson
    ( FromJSON (..)
    , SumEncoding (..)
    , ToJSON (..)
    , Value (String)
    , camelTo2
    , constructorTagModifier
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , object
    , omitNothingFields
    , sumEncoding
    , tagSingleConstructors
    , withObject
    , (.:)
    , (.=)
    )
import Data.Proxy
    ( Proxy (..) )
import Data.Time.Clock
    ( UTCTime )
import Data.UUID.Types
    ( UUID )
import Fmt
    ( build, fmt )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( KnownSymbol, Symbol, symbolVal )
import Numeric.Natural
    ( Natural )

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T

{-------------------------------------------------------------------------------
                                  API Types
-------------------------------------------------------------------------------}

newtype Amount = Amount Natural
    deriving stock (Generic, Show, Ord, Eq)

instance FromJSON Amount where
    parseJSON x = Amount . removeUnit @"lovelace" <$> parseJSON x

instance ToJSON Amount where
    toJSON (Amount x) = toJSON $ addUnit @"lovelace" x

newtype Percentage = Percentage Int
    deriving stock (Eq, Generic, Ord, Show)

data PercentageError
    = PercentageOutOfBoundsError
    deriving (Show)

instance Bounded Percentage where
    minBound = Percentage 0
    maxBound = Percentage 100

instance Enum Percentage where
    fromEnum (Percentage p) = p
    toEnum = either (error . ("toEnum: " <>) . show) id . mkPercentage

instance FromJSON Percentage where
    parseJSON x = eitherToParser
        . mkPercentage @Int . removeUnit @"percent" =<< parseJSON x

instance ToJSON Percentage where
    toJSON (Percentage x) = toJSON $ addUnit @"percent" x

mkPercentage :: Integral i => i -> Either PercentageError Percentage
mkPercentage i
    | j < minBound = Left PercentageOutOfBoundsError
    | j > maxBound = Left PercentageOutOfBoundsError
    | otherwise = pure j
  where
    j = Percentage (fromIntegral i)

data Wallet = Wallet
    { _id :: !WalletId
    , _addressPoolGap :: !(ApiT AddressPoolGap)
    , _balance :: !WalletBalance
    , _delegation :: !WalletDelegation
    , _name :: !(ApiT WalletName)
    , _passphrase :: !WalletPassphraseInfo
    , _state :: !WalletState
    } deriving (Eq, Generic, Show)

instance FromJSON Wallet where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON Wallet where
    toJSON = genericToJSON defaultRecordTypeOptions

instance ToJSON (ApiT WalletName) where
    toJSON = toJSON . getWalletName . getApiT
instance FromJSON (ApiT WalletName) where
    parseJSON x = fmap ApiT . eitherToParser . mkWalletName =<< parseJSON x

deriving newtype instance Bounded (ApiT AddressPoolGap)
deriving newtype instance Enum (ApiT AddressPoolGap)
instance FromJSON (ApiT AddressPoolGap) where
    parseJSON x = do
        gap <- parseJSON x
        ApiT <$> eitherToParser (mkAddressPoolGap gap)
instance ToJSON (ApiT AddressPoolGap) where
    toJSON = toJSON . getAddressPoolGap . getApiT

data WalletBalance = WalletBalance
    { _available :: !Amount
    , _total :: !Amount
    } deriving (Eq, Generic, Show)

instance FromJSON WalletBalance where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON WalletBalance where
    toJSON = genericToJSON defaultRecordTypeOptions

-- | Wallet Delegation representation, can be serialized to and from JSON as
-- follows:
--
-- >>> Aeson.encode NotDelegating
-- {"status":"not_delegating"}
--
-- >>> Aeson.encode $ Delegating poolId
-- {"status":"delegating","target": "27522fe5-262e-42a5-8ccb-cef884ea2ba0"}
data WalletDelegation
    = NotDelegating
    | Delegating !PoolId
    deriving (Eq, Generic, Show)

instance FromJSON WalletDelegation where
    parseJSON = genericParseJSON walletDelegationOptions
instance ToJSON WalletDelegation where
    toJSON = genericToJSON walletDelegationOptions

walletDelegationOptions :: Aeson.Options
walletDelegationOptions = taggedSumTypeOptions $ TaggedObjectOptions
    { _tagFieldName = "status"
    , _contentsFieldName = "target"
    }

newtype PoolId = PoolId
    { _uuid :: UUID }
    deriving stock (Eq, Show)
    deriving newtype (FromJSON, ToJSON)

newtype WalletId = WalletId
    { _uuid :: UUID }
    deriving stock (Eq, Show)
    deriving newtype (FromJSON, ToJSON)

newtype WalletPassphraseInfo = WalletPassphraseInfo
    { _lastUpdatedAt :: UTCTime
    } deriving (Eq, Generic, Show)

instance FromJSON WalletPassphraseInfo where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON WalletPassphraseInfo where
    toJSON = genericToJSON defaultRecordTypeOptions

-- | A Wallet State representation in the API. It can be serialized to and from
-- JSON as follows:
--
-- >>> Aeson.encode Ready
-- {"status":"ready"}
--
-- >>> Aeson.encode $ Restoring (Percentage 14)
-- {"status":"restoring","progress":{"quantity":14,"unit":"percent"}}
data WalletState
    = Ready
    | Restoring !Percentage
    deriving (Eq, Generic, Show)

instance FromJSON WalletState where
    parseJSON = genericParseJSON walletStateOptions
instance ToJSON WalletState where
    toJSON = genericToJSON walletStateOptions

walletStateOptions :: Aeson.Options
walletStateOptions = taggedSumTypeOptions $ TaggedObjectOptions
    { _tagFieldName = "status"
    , _contentsFieldName = "progress"
    }

data Payment = Payment
    { _targets :: ![TargetOutput]
    , _passphrase :: !(ApiT (Passphrase "encryption"))
    } deriving (Generic, Show)

data TargetOutput = TargetOutput
    { _address :: !(ApiT Address)
    , _amount :: !Amount
    } deriving (Generic, Show)

instance ToJSON (ApiT Address) where
    toJSON = toJSON . fmt @T.Text . build . getApiT
instance FromJSON (ApiT Address) where
    parseJSON x = fmap ApiT . eitherToParser . mkAddress =<< parseJSON x

instance FromJSON TargetOutput where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON TargetOutput where
    toJSON = genericToJSON defaultRecordTypeOptions

{-------------------------------------------------------------------------------
                              Polymorphic Types
-------------------------------------------------------------------------------}

-- | Represents a value that has an associated unit of measure, based on some
--   underlying type.
newtype MeasuredIn (u :: Symbol) a = MeasuredIn a
    deriving (Generic, Show, Eq)

-- | Add a unit of measure to a value.
addUnit :: forall u a . a -> MeasuredIn (u :: Symbol) a
addUnit = MeasuredIn

-- | Remove a unit of measure from a value.
removeUnit :: MeasuredIn (u :: Symbol) a -> a
removeUnit (MeasuredIn a) = a

instance (KnownSymbol u, ToJSON a) => ToJSON (MeasuredIn u a) where
    toJSON (MeasuredIn a) = object
        [ "unit"     .= symbolVal (Proxy :: Proxy u)
        , "quantity" .= toJSON a
        ]

instance (KnownSymbol u, FromJSON a) => FromJSON (MeasuredIn u a) where
    parseJSON = withObject "MeasuredIn" $ \o -> do
        verifyUnit =<< o .: "unit"
        MeasuredIn <$> o .: "quantity"
      where
        u = symbolVal (Proxy :: Proxy u)
        verifyUnit = \case
            String u' | u' == T.pack u -> pure ()
            _ -> fail $
                "failed to parse quantified value. Expected value in '" <> u
                <> "' (e.g. { \"unit\": \"" <> u <> "\", \"quantity\": ...})"
                <> " but got something else."

newtype ApiT a = ApiT { getApiT :: a }
    deriving (Generic, Show, Eq)

{-------------------------------------------------------------------------------
                                Aeson Options
-------------------------------------------------------------------------------}

data TaggedObjectOptions = TaggedObjectOptions
    { _tagFieldName :: String
    , _contentsFieldName :: String
    }

defaultSumTypeOptions :: Aeson.Options
defaultSumTypeOptions = Aeson.defaultOptions
    { constructorTagModifier = camelTo2 '_'
    , tagSingleConstructors = True }

defaultRecordTypeOptions :: Aeson.Options
defaultRecordTypeOptions = Aeson.defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop 1
    , omitNothingFields = True }

taggedSumTypeOptions :: TaggedObjectOptions -> Aeson.Options
taggedSumTypeOptions opts = defaultSumTypeOptions
    { sumEncoding = TaggedObject (_tagFieldName opts) (_contentsFieldName opts)
    }

{-------------------------------------------------------------------------------
                                   Helpers
-------------------------------------------------------------------------------}

eitherToParser :: Show s => Either s a -> Aeson.Parser a
eitherToParser = either (fail . show) pure
