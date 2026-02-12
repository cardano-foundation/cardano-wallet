{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Re-exports functionality provided by module 'Cardano.Ledger.Credential',
-- but with a safer interface.
module Cardano.Ledger.Credential.Safe
    ( -- * Safe 'Ptr' interface
      Ptr
    , safePtr
    , safeUnwrapPtr

      -- * Conversions to and from 'Slot32'
    , SlotNo32 (..)
    , toSlotNo32
    , fromSlotNo32
    )
where

import Cardano.Api
    ( SlotNo (..)
    )
import Cardano.Ledger.BaseTypes
    ( CertIx
    , TxIx
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
import Prelude

--------------------------------------------------------------------------------
-- Safe public interface
--------------------------------------------------------------------------------

-- | Safely constructs a 'Ptr' without silent truncation of slot numbers.
--
-- Use 'toSlotNo32' to convert an ordinary 'SlotNo' to a 'SlotNo32'.
--
-- This function should satisfy the following property:
--
-- prop> safeUnwrapPtr (safePtr s t c) == (s, t, c)
safePtr :: SlotNo32 -> TxIx -> CertIx -> Ptr
safePtr = Ptr

-- | Safely deconstructs a 'Ptr'.
--
-- Use 'fromSlotNo32' to convert the returned slot number to a 'SlotNo'.
--
-- This function should satisfy the following property:
--
-- prop> safeUnwrapPtr (safePtr s t c) == (s, t, c)
safeUnwrapPtr :: Ptr -> (SlotNo32, TxIx, CertIx)
safeUnwrapPtr (Ptr s t c) = (s, t, c)

-- | Converts an ordinary 'SlotNo' into a 'SlotNo32'.
--
-- Returns 'Nothing' if the slot number could not be converted safely.
toSlotNo32 :: SlotNo -> Maybe SlotNo32
toSlotNo32 (SlotNo n) = SlotNo32 <$> intCastMaybe @Word64 @Word32 n

-- | Converts a 'SlotNo32' into an ordinary 'SlotNo'.
fromSlotNo32 :: SlotNo32 -> SlotNo
fromSlotNo32 (SlotNo32 n) = SlotNo (intCast @Word32 @Word64 n)
