{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Era-indexed address types.
-}
module Cardano.Read.Ledger.Address
    ( -- * Compact address type
      CompactAddrType
    , CompactAddr (..)

      -- * Conversions
    , translateCompactAddrShelleyFromByron
    )
where

import Prelude

import Cardano.Chain.Common qualified as BY
import Cardano.Ledger.Address qualified as SH
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )

{-----------------------------------------------------------------------------
    Compact address
------------------------------------------------------------------------------}

-- | Era-specific compact address type.
type family CompactAddrType era where
    CompactAddrType Byron = BY.CompactAddress
    CompactAddrType Shelley = SH.CompactAddr
    CompactAddrType Allegra = SH.CompactAddr
    CompactAddrType Mary = SH.CompactAddr
    CompactAddrType Alonzo = SH.CompactAddr
    CompactAddrType Babbage = SH.CompactAddr
    CompactAddrType Conway = SH.CompactAddr

-- | Era-indexed compact address wrapper.
newtype CompactAddr era = CompactAddr (CompactAddrType era)

deriving instance Show (CompactAddrType era) => Show (CompactAddr era)
deriving instance Eq (CompactAddrType era) => Eq (CompactAddr era)

-- | Convert a Byron compact address to a Shelley compact address.
translateCompactAddrShelleyFromByron
    :: CompactAddrType Byron -> CompactAddrType Shelley
translateCompactAddrShelleyFromByron = SH.fromBoostrapCompactAddress
