{-# LANGUAGE DataKinds #-}
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
-- Implementation of address derivation for the random scheme, as
-- implemented by the legacy Cardano wallets.

module Cardano.Wallet.Primitive.AddressDerivation.Random
    ( deriveAccountPrivateKey
    , deriveAddressPrivateKey
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..), Key (..), Passphrase (..) )

deriveAccountPrivateKey
    :: Passphrase "encryption"
    -> Key 'RootK XPrv
    -> Index 'Hardened 'AccountK
    -> Key 'AccountK XPrv
deriveAccountPrivateKey (Passphrase _pwd) (Key _rootXPrv) (Index _accIx) =
    error "AddressDerivation.Random unimplemented"

deriveAddressPrivateKey
    :: Passphrase "encryption"
    -> Key 'AccountK XPrv
    -> Index 'Soft 'AddressK
    -> Key 'AddressK XPrv
deriveAddressPrivateKey (Passphrase _pwd) (Key _accXPrv) (Index _addrIx) =
    error "AddressDerivation.Random unimplemented"
