-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Contains various implementation decision that are specific to a particular
-- network / protocol. This allows us to easily select a particular backend
-- (Byron, Shelley-Rust, Shelley-Haskell) and isolate the bits that vary between
-- those backends.

module Cardano.Wallet.Compatibility.Jormungandr
    ( -- * Target
      Jormungandr
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( KeyToAddress (..) )
import Cardano.Wallet.Primitive.Types
    ( TxId (..) )

-- | A type representing the Jormungandr as a network target. This has an
-- influence on binary serializer & network primitives. See also 'TxId'
data Jormungandr

instance TxId Jormungandr where
    txId = undefined

instance KeyToAddress Jormungandr where
    keyToAddress = undefined
