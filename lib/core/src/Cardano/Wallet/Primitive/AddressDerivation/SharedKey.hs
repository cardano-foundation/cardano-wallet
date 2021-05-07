{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Definition of 'Shared' Keys.

module Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( -- * Types
      SharedKey(..)

    , purposeCIP1854
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..) )
import Control.DeepSeq
    ( NFData (..) )
import GHC.Generics
    ( Generic )

-- | Purpose for shared wallets is a constant set to 1854' (or 0x8000073E) following the original
-- CIP-1854 Multi-signature Wallets.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeCIP1854 :: Index 'Hardened 'PurposeK
purposeCIP1854 = toEnum 0x8000073E

-- | A cryptographic key for Shared address derivation, with phantom-types to
-- disambiguate derivation paths
--
-- @
-- let rootPrivateKey = SharedKey 'RootK XPrv
-- let accountPubKey = SharedKey 'AccountK XPub
-- let addressPubKey = SharedKey 'AddressK XPub
-- @
newtype SharedKey (depth :: Depth) key =
    SharedKey { getKey :: key }
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (SharedKey depth key)
