{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright: © 2018-2022 IOHK, 2023 Cardano Foundation
-- License: Apache-2.0

module Cardano.Wallet.Api.Lib.ApiT
    ( ApiT (..)
    , fromTextApiT
    , toTextApiT
    )
    where

import Prelude

import Cardano.Wallet.Api.Aeson
    ( fromTextJSON
    , toTextJSON
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Aeson
    ( Value
    )
import Data.Aeson.Types
    ( Parser
    )
import Data.Hashable
    ( Hashable
    )
import Data.Text.Class
    ( FromText
    , ToText
    )
import GHC.Generics
    ( Generic
    )
import Quiet
    ( Quiet (Quiet)
    )

-- | Polymorphic wrapper type to put around primitive types and, 3rd party lib
-- types to avoid defining orphan instances and/or, undesirable instances on
-- primitive types. It helps to keep a nice separation of concerns between the
-- API layer and other modules.
newtype ApiT a =
    ApiT { getApiT :: a }
    deriving (Generic, Eq, Functor)
    deriving newtype (Semigroup, Monoid, Hashable)
    deriving anyclass NFData
    deriving Show via (Quiet (ApiT a))
deriving instance Ord a => Ord (ApiT a)

fromTextApiT :: FromText a => String -> Value -> Parser (ApiT a)
fromTextApiT t = fmap ApiT . fromTextJSON t

toTextApiT :: ToText a => ApiT a -> Value
toTextApiT = toTextJSON . getApiT
