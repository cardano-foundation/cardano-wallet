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
import Data.Maybe
    ( fromMaybe
    )
import Data.Word
    ( Word32
    , Word64
    )
import GHC.Stack
    ( HasCallStack
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

--------------------------------------------------------------------------------
-- Unsafe internal interface
--------------------------------------------------------------------------------

unsafeToSlotNo32 :: HasCallStack => SlotNo -> SlotNo32
unsafeToSlotNo32 = fromMaybe reportFailure . toSlotNo32
  where
    reportFailure = error
        "unsafeToSlotNo32: unable to convert SlotNo to SlotNo32"
