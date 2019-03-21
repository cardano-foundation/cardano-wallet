{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Api.Types
    (
    -- * API Types
      PoolId (..)
    , Wallet (..)
    , WalletBalance (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)

    -- * Re-Exports From Primitive Types
    , AddressPoolGap
    , WalletName (..)

    -- * Polymorphic Types
    , ApiT (..)
    ) where

import Prelude

import Cardano.Wallet
    ( WalletName (..), mkWalletName )
import Cardano.Wallet.AddressDiscovery
    ( AddressPoolGap, getAddressPoolGap, mkAddressPoolGap )
import Data.Aeson
    ( FromJSON (..)
    , SumEncoding (..)
    , ToJSON (..)
    , camelTo2
    , constructorTagModifier
    , fieldLabelModifier
    , genericParseJSON
    , genericToJSON
    , omitNothingFields
    , sumEncoding
    , tagSingleConstructors
    )
import Data.Quantity
    ( Quantity (..) )
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


{-------------------------------------------------------------------------------
                                  API Types
-------------------------------------------------------------------------------}

data Wallet = Wallet
    { _id :: !WalletId
    , _addressPoolGap :: !(ApiT AddressPoolGap)
    , _balance :: !WalletBalance
    , _delegation :: !WalletDelegation
    , _name :: !(ApiT WalletName)
    , _passphrase :: !WalletPassphraseInfo
    , _state :: !WalletState
    } deriving (Eq, Generic, Show)

data WalletBalance = WalletBalance
    { _available :: !(Quantity "lovelace" Natural)
    , _total :: !(Quantity "lovelace" Natural)
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
-- >>> Aeson.encode $ Restoring (Quantity 14)
-- {"status":"restoring","progress":{"quantity":14,"unit":"percent"}}
data WalletState
    = Ready
    | Restoring !(Quantity "percent" Word)
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

{-------------------------------------------------------------------------------
                              Polymorphic Types
-------------------------------------------------------------------------------}

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
