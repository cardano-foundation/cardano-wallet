{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Pool.Types
    ( StakePoolsSummary (..)
    , PoolId(..)
    , PoolOwner(..)
    , poolIdBytesLength
    , decodePoolIdBech32
    , encodePoolIdBech32
    , StakePoolTicker (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Pool
    ( PoolId (..)
    , PoolOwner (..)
    , decodePoolIdBech32
    , encodePoolIdBech32
    , poolIdBytesLength
    )
import Cardano.Wallet.Primitive.Types.StakePoolSummary
    ( StakePoolsSummary (..)
    )
import Cardano.Wallet.Util
    ( ShowFmt (..)
    )
import Control.Monad
    ( (>=>)
    )
import Data.Aeson
    ( FromJSON (parseJSON)
    , ToJSON (toJSON)
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (TextDecodingError)
    , ToText (..)
    )
import Database.Persist.Class.PersistField
    ( PersistField (..)
    )
import Database.Persist.PersistValue.Extended
    ( fromPersistValueFromText
    )
import Database.Persist.Sqlite
    ( PersistFieldSql (..)
    )
import GHC.Generics
    ( Generic
    )

import qualified Data.Text as T

-- | Very short name for a stake pool.
newtype StakePoolTicker = StakePoolTicker { unStakePoolTicker :: Text }
    deriving stock (Generic, Show, Eq, Ord)
    deriving newtype (ToText)

instance FromText StakePoolTicker where
    fromText t
        | T.length t >= 3 && T.length t <= 5
            = Right $ StakePoolTicker t
        | otherwise
            = Left . TextDecodingError $
                "stake pool ticker length must be 3-5 characters"

-- Here to avoid needless orphan instances in the API types.
instance FromJSON StakePoolTicker where
    parseJSON = parseJSON >=> either (fail . show . ShowFmt) pure . fromText

instance ToJSON StakePoolTicker where
    toJSON = toJSON . toText

instance PersistField StakePoolTicker where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql StakePoolTicker where
    sqlType _ = sqlType (Proxy @Text)

instance FromJSON PoolOwner where
    parseJSON = parseJSON >=> either (fail . show . ShowFmt) pure . fromText

instance ToJSON PoolOwner where
    toJSON = toJSON . toText

instance PersistField PoolOwner where
    toPersistValue = toPersistValue . toText
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql PoolOwner where
    sqlType _ = sqlType (Proxy @Text)

instance Read PoolOwner where
    readsPrec _ = error "readsPrec stub needed for persistent"

instance FromText [PoolOwner] where
    fromText t = mapM fromText $ T.words t

instance PersistField [PoolOwner] where
    toPersistValue v = toPersistValue $ T.unwords $ toText <$> v
    fromPersistValue = fromPersistValueFromText

instance PersistFieldSql [PoolOwner] where
    sqlType _ = sqlType (Proxy @Text)
