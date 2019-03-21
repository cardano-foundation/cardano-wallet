{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Api.Types
    (
    -- * API Types
      Wallet (..)
    , WalletBalance (..)

    -- * Re-Export From Primitives
    , PoolId (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , AddressPoolGap

    -- * Polymorphic Types
    , ApiT (..)
    ) where

import Prelude

import Cardano.Wallet
    ( PoolId (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , mkWalletName
    )
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
    { _id :: !(ApiT WalletId)
    , _addressPoolGap :: !(ApiT AddressPoolGap)
    , _balance :: !(ApiT WalletBalance)
    , _delegation :: !(ApiT (WalletDelegation (ApiT PoolId)))
    , _name :: !(ApiT WalletName)
    , _passphrase :: !(ApiT WalletPassphraseInfo)
    , _state :: !(ApiT WalletState)
    } deriving (Eq, Generic, Show)

data WalletBalance = WalletBalance
    { _available :: !(Quantity "lovelace" Natural)
    , _total :: !(Quantity "lovelace" Natural)
    } deriving (Eq, Generic, Show)

instance FromJSON Wallet where
    parseJSON = genericParseJSON defaultRecordTypeOptions
instance ToJSON Wallet where
    toJSON = genericToJSON defaultRecordTypeOptions

instance FromJSON (ApiT WalletId) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT WalletId) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

instance FromJSON (ApiT AddressPoolGap) where
    parseJSON x = do
        gap <- parseJSON x
        ApiT <$> eitherToParser (mkAddressPoolGap gap)
instance ToJSON (ApiT AddressPoolGap) where
    toJSON = toJSON . getAddressPoolGap . getApiT

instance FromJSON (ApiT WalletBalance) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT WalletBalance) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

instance FromJSON (ApiT (WalletDelegation (ApiT PoolId))) where
    parseJSON = fmap ApiT . genericParseJSON walletDelegationOptions
instance ToJSON (ApiT (WalletDelegation (ApiT PoolId))) where
    toJSON = genericToJSON walletDelegationOptions . getApiT

instance FromJSON (ApiT WalletName) where
    parseJSON x = fmap ApiT . eitherToParser . mkWalletName =<< parseJSON x
instance ToJSON (ApiT WalletName) where
    toJSON = toJSON . getWalletName . getApiT

instance FromJSON (ApiT WalletPassphraseInfo) where
    parseJSON = fmap ApiT . genericParseJSON defaultRecordTypeOptions
instance ToJSON (ApiT WalletPassphraseInfo) where
    toJSON = genericToJSON defaultRecordTypeOptions . getApiT

instance FromJSON (ApiT WalletState) where
    parseJSON = fmap ApiT . genericParseJSON walletStateOptions
instance ToJSON (ApiT WalletState) where
    toJSON = genericToJSON walletStateOptions . getApiT

instance FromJSON (ApiT PoolId) where
    parseJSON = fmap (ApiT . PoolId) . parseJSON
instance ToJSON (ApiT PoolId) where
    toJSON = toJSON . getPoolId . getApiT

-- | Options for encoding wallet delegation settings. It can be serialized to
-- and from JSON as follows:
--
-- >>> Aeson.encode NotDelegating
-- {"status":"not_delegating"}
--
-- >>> Aeson.encode $ Delegating poolId
-- {"status":"delegating","target": "27522fe5-262e-42a5-8ccb-cef884ea2ba0"}
walletDelegationOptions :: Aeson.Options
walletDelegationOptions = taggedSumTypeOptions $ TaggedObjectOptions
    { _tagFieldName = "status"
    , _contentsFieldName = "target"
    }

-- | Options for encoding a wallet state. It can be serialized to and from JSON
-- as follows:
--
-- >>> Aeson.encode Ready
-- {"status":"ready"}
--
-- >>> Aeson.encode $ Restoring (Quantity 14)
-- {"status":"restoring","progress":{"quantity":14,"unit":"percent"}}

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
    { fieldLabelModifier = camelTo2 '_' . dropWhile (== '_')
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
