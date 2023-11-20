{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Primitive.Types.Block
    ( Block (..)
    , BlockHeader (..)
    , ChainPoint (..)
    , Slot
    , isGenesisBlockHeader
    , compareSlot
    , chainPointFromBlockHeader
    , toSlot
    )

where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo
    , WithOrigin (..)
    )
import Cardano.Wallet.Primitive.Types.DelegationCertificate
    ( DelegationCertificate
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (getHash)
    )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( Tx
    )
import Control.DeepSeq
    ( NFData
    )
import Data.Quantity
    ( Quantity (getQuantity)
    )
import Data.Word
    ( Word32
    )
import Fmt
    ( Buildable (..)
    , blockListF
    , indentF
    , prefixF
    , pretty
    )
import GHC.Generics
    ( Generic
    )
import NoThunks.Class
    ( NoThunks
    )

import Control.Lens
    ( view
    )
import Data.ByteArray.Encoding
    ( Base (Base16)
    , convertToBase
    )
import Data.Maybe
    ( isNothing
    )
import qualified Data.Text.Encoding as T

data Block = Block
    { header
        :: !BlockHeader
    , transactions
        :: ![Tx]
    , delegations
        :: ![DelegationCertificate]
    } deriving (Show, Eq, Ord, Generic)

instance NFData Block

instance Buildable (Block) where
    build (Block h txs _) = mempty
        <> build h
        <> if null txs then " ∅" else "\n" <> indentF 4 (blockListF txs)

data BlockHeader = BlockHeader
    { slotNo
        :: SlotNo
    , blockHeight
        :: Quantity "block" Word32
    , headerHash
        :: !(Hash "BlockHeader")
    , parentHeaderHash
        :: !(Maybe (Hash "BlockHeader"))
    } deriving (Show, Eq, Ord, Generic)

-- | Check whether a block with a given 'BlockHeader' is the genesis block.
isGenesisBlockHeader :: BlockHeader -> Bool
isGenesisBlockHeader = isNothing . view #parentHeaderHash

instance NFData BlockHeader

instance Buildable BlockHeader where
    build BlockHeader{..} =
        previous
        <> "["
        <> current
        <> "-"
        <> build slotNo
        <> "#" <> (build . show . getQuantity) blockHeight
        <> "]"
      where
        toHex = T.decodeUtf8 . convertToBase Base16
        current = prefixF 8 $ build $ toHex $ getHash headerHash
        previous = case parentHeaderHash of
            Nothing -> ""
            Just h  -> prefixF 8 (build $ toHex $ getHash h) <> "<-"

-- | A point on the blockchain
-- is either the genesis block, or a block with a hash that was
-- created at a particular 'SlotNo'.
--
-- TODO:
--
-- * This type is essentially a copy of the 'Cardano.Api.Block.ChainPoint'
-- type. We want to import it from there when overhauling our types.
-- * That said, using 'WithOrigin' would not be bad.
-- * 'BlockHeader' is also a good type for rerencing points on the chain,
-- but it's less compatible with the types in ouroboros-network.
data ChainPoint
    = ChainPointAtGenesis
    | ChainPoint !SlotNo !(Hash "BlockHeader")
    deriving (Eq, Ord, Show, Generic)

-- | Compare the slot numbers of two 'ChainPoint's,
-- but where the 'ChainPointAtGenesis' comes before all other slot numbers.
compareSlot :: ChainPoint -> ChainPoint -> Ordering
compareSlot pt1 pt2 = compare (toSlot pt1) (toSlot pt2)

-- | Convert a 'BlockHeader' into a 'ChainPoint'.
chainPointFromBlockHeader :: BlockHeader -> ChainPoint
chainPointFromBlockHeader header@(BlockHeader sl _ hash _)
    | isGenesisBlockHeader header = ChainPointAtGenesis
    | otherwise                   = ChainPoint sl hash

instance NFData ChainPoint

instance NoThunks ChainPoint

instance Buildable ChainPoint where
    build ChainPointAtGenesis    = "[point genesis]"
    build (ChainPoint slot hash) =
        "[point " <> hashF <> " at slot " <> pretty slot <> "]"
      where
        hashF = prefixF 8 $ T.decodeUtf8 $ convertToBase Base16 $ getHash hash

-- | A point in (slot) time, which is either genesis ('Origin')
-- or has a slot number ('At').
--
-- In contrast to 'ChainPoint', the type 'Slot' does not refer
-- to a point on an actual chain with valid block hashes,
-- but merely to a timeslot which can hold a single block.
-- This implies:
--
-- * 'Slot' has a linear ordering implemented in the 'Ord' class
--   (where @Origin < At slot@).
-- * Using 'Slot' in QuickCheck testing requires less context
-- (such as an actual simulated chain.)
type Slot = WithOrigin SlotNo

-- | Retrieve the slot of a 'ChainPoint'.
toSlot :: ChainPoint -> Slot
toSlot ChainPointAtGenesis = Origin
toSlot (ChainPoint slot _) = At slot

instance Buildable Slot where
    build Origin    = "[genesis]"
    build (At slot) = "[at slot " <> pretty slot <> "]"
