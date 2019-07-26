{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Use the types from "Cardano.Wallet.Primitive.AddressDerivation" or the key
-- generation or derivation functions from
-- "Cardano.Wallet.Primitive.AddressDerivation.Sequential" or
-- "Cardano.Wallet.Primitive.AddressDerivation.Random" instead of this module.

module Cardano.Wallet.Primitive.AddressDerivation.Common
    ( Key(..)
    , Depth (..)
    ) where

import Prelude

import Control.DeepSeq
    ( NFData )
import GHC.Generics
    ( Generic )

{-------------------------------------------------------------------------------
                        Polymorphic / General Purpose Types
-------------------------------------------------------------------------------}

-- | A cryptographic key, with phantom-types to disambiguate key types.
--
-- @
-- let rootPrivateKey = Key 'RootK XPrv
-- let accountPubKey = Key 'AccountK XPub
-- let addressPubKey = Key 'AddressK XPub
-- @
newtype Key (level :: Depth) key = Key { getKey :: key }
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (Key level key)

-- | Key Depth in the derivation path, according to BIP-0039 / BIP-0044
--
-- @m | purpose' | cointype' | account' | change | address@
--
-- We do not manipulate purpose, cointype and change paths directly, so they are
-- left out of the sum type.
data Depth = RootK | AccountK | AddressK
