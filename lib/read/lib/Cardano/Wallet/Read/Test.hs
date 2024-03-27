{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -O #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Test where

import Prelude

newtype K a era = K {unK :: a}

{-----------------------------------------------------------------------------
    EraFun
------------------------------------------------------------------------------}
data Byron
data Shelley

data Era era where
    Byron :: Era Byron
    Shelley :: Era Shelley

class IsEra era where
    theEra :: Era era

instance IsEra Byron where theEra = Byron
instance IsEra Shelley where theEra = Shelley

data EraFun f g = EraFun
    { byronFun :: f Byron -> g Byron
    , shelleyFun :: f Shelley -> g Shelley
    }

{-# INLINEABLE applyOnEra #-}
applyOnEra
    :: forall era f g. IsEra era
    => EraFun f g -> f era -> g era
applyOnEra f = case theEra :: Era era of
    Byron -> byronFun f
    Shelley -> shelleyFun f

{-----------------------------------------------------------------------------
    Block
------------------------------------------------------------------------------}
type SlotNo = Integer

data BlockByron = BlockByron
    { slotNo :: Integer
    , isEpochBoundaryBlock :: Bool
    }

data BlockShelley = BlockShelley
    { slotNo :: Integer
    , headerHash :: String
    }

type family BlockT era where
    BlockT Byron = BlockByron
    BlockT Shelley = BlockShelley

newtype Block era = Block {unBlock :: BlockT era}

{-----------------------------------------------------------------------------
    Block features
------------------------------------------------------------------------------}
getEraSlotNo :: EraFun Block (K SlotNo)
getEraSlotNo = EraFun
    { byronFun = \(Block BlockByron{slotNo}) -> K slotNo
    , shelleyFun = \(Block BlockShelley{slotNo}) -> K slotNo
    }

type family HeaderHashT era where
    HeaderHashT Byron = ()
    HeaderHashT Shelley = String

newtype HeaderHash era = HeaderHash {unHeaderHash :: HeaderHashT era}

getEraHeaderHash :: EraFun Block HeaderHash
getEraHeaderHash = EraFun
    { byronFun = \(Block{}) -> HeaderHash ()
    , shelleyFun = \(Block BlockShelley{headerHash}) -> HeaderHash headerHash
    }

{-----------------------------------------------------------------------------
    Block
------------------------------------------------------------------------------}

{-# INLINEABLE getSlotNo #-}
getSlotNo :: IsEra era => Block era -> SlotNo
getSlotNo = unK . applyOnEra getEraSlotNo

{-# INLINEABLE getHeaderHash #-}
getHeaderHash :: IsEra era => Block era -> HeaderHash era
getHeaderHash = applyOnEra getEraHeaderHash

{-# INLINEABLE getLength #-}
getLength :: forall era. IsEra era => HeaderHash era -> Int
getLength (HeaderHash hash) = case theEra :: Era era of
    Byron -> 0
    Shelley -> length hash

{-# INLINEABLE getHeaderHashLength #-}
getHeaderHashLength :: forall era. IsEra era => Block era -> Int
getHeaderHashLength = getLength . getHeaderHash

specializeShelley :: Block Shelley -> (SlotNo, Int)
specializeShelley block = (getSlotNo block, getHeaderHashLength block)
