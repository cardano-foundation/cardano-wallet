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
    ( Buildable (..), (+|), (+||), (|+), (||+) )

import qualified Ouroboros.Consensus.HardFork.History.Qry as HF

instance Buildable SlotNo where
    build (SlotNo n) = build (show n)

instance Buildable (HF.Qry a) where
    build = \case
        HF.QPure _ -> "QPure"
        HF.QBind q _ -> "QBind "+|q|+""
        HF.QAbsToRelTime t -> "QAbsToRelTime "+||t||+""
        HF.QAbsToRelSlot sl -> "QAbsToRelSlot "+||sl||+""
        HF.QAbsToRelEpoch ep -> "QAbsToRelEpoch "+||ep||+""
        HF.QSlotLength sl -> "QSlotLength "+||sl||+""
        HF.QEpochSize ep -> "QEpochSize "+||ep||+""
        -- Necessary constructors or show instances are not exported -- we can't
        -- show these values.
        HF.QRelToAbsTime _t -> "QRelToAbsTime ?"
        HF.QRelToAbsSlot _slt -> "QRelToAbsSlot ?"
        HF.QRelToAbsEpoch _epe -> "QRelToAbsEpoch ?"
        HF.QRelTimeToSlot _t -> "QRelTimeToSlot ?"
        HF.QRelSlotToTime _sl -> "QRelSlotToTime ?"
        HF.QRelSlotToEpoch _sl -> "QRelSlotToEpoch ?"
        HF.QRelEpochToSlot _ep -> "QRelEpochToSlot ?"
