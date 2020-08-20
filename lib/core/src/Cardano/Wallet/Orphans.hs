{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Module for orphans which would be too inconvenient to avoid.

module Cardano.Wallet.Orphans where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Fmt
    ( Buildable (..) )

instance Buildable SlotNo where
    build (SlotNo n) = build (show n)
