{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Re-exports functionality provided by module 'Cardano.Ledger.Credential',
-- but with a safer interface.
--
module Cardano.Ledger.Credential.Safe
    (
      -- * Safe 'Ptr' interface
      Ptr

      -- * Conversions to and from 'Slot32'
    , SlotNo32 (..)
    , toSlotNo32
    , fromSlotNo32
    )
    where

import Prelude

import Cardano.Api
    ( SlotNo (..)
    )
import Cardano.Ledger.Credential
    ( Ptr (..)
    , SlotNo32 (..)
    )
import Data.IntCast
    ( intCast
    , intCastMaybe
    )
import Data.Word
    ( Word32
    , Word64
    )

--------------------------------------------------------------------------------
-- Safe public interface
--------------------------------------------------------------------------------

-- | Converts an ordinary 'SlotNo' into a 'SlotNo32'.
--
-- Returns 'Nothing' if the slot number could not be converted safely.
--
toSlotNo32 :: SlotNo -> Maybe SlotNo32
toSlotNo32 (SlotNo n) = SlotNo32 <$> intCastMaybe @Word64 @Word32 n

-- | Converts a 'SlotNo32' into an ordinary 'SlotNo'.
--
fromSlotNo32 :: SlotNo32 -> SlotNo
fromSlotNo32 (SlotNo32 n) = SlotNo (intCast @Word32 @Word64 n)
