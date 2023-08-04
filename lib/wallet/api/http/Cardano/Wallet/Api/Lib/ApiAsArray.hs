{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Api.Lib.ApiAsArray (ApiAsArray (..)) where

import Control.DeepSeq
  ( NFData
  )
import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  )
import Data.Maybe
  ( maybeToList
  )
import Data.Typeable
  ( Proxy (..)
  , Typeable
  )
import GHC.Base
  ( Symbol
  )
import GHC.Generics
  ( Generic
  )
import GHC.TypeLits
  ( KnownSymbol
  , symbolVal
  )
import Prelude

-- | A wrapper that allows any type to be serialized as a JSON array.
--
-- The number of items permitted in the array is dependent on the wrapped type.
newtype ApiAsArray (s :: Symbol) a = ApiAsArray a
  deriving (Eq, Generic, Show, Typeable)
  deriving newtype (Monoid, Semigroup)
  deriving anyclass (NFData)

instance (KnownSymbol s, FromJSON a) => FromJSON (ApiAsArray s (Maybe a)) where
  parseJSON json =
    parseJSON @[a] json >>= \case
      [a] ->
        pure $ ApiAsArray $ Just a
      [] ->
        pure $ ApiAsArray Nothing
      _ ->
        fail
          $ mconcat
            [ "Expected at most one item for "
            , show $ symbolVal $ Proxy @s
            , "."
            ]

instance ToJSON a => ToJSON (ApiAsArray s (Maybe a)) where
  toJSON (ApiAsArray m) = toJSON (maybeToList m)
