{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- DRepLayer: service facade that merges live ledger state with cached
-- off-chain metadata, analogous to StakePoolLayer.
module Cardano.Wallet.DRep.Layer
    ( DRepLayer (..)
    , DRepInfo (..)
    , newDRepLayer
    ) where

import Cardano.Pool.DB
    ( DBLayer (..)
    )
import Cardano.Wallet.Network
    ( NetworkLayer
    , listDReps
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRepAnchor (..)
    , DRepMetadata
    , DRepRegistration (..)
    )
import Data.Map.Strict
    ( Map
    )
import Data.Text
    ( Text
    )
import Prelude

import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE

-- | A DRep registration enriched with cached off-chain metadata.
data DRepInfo = DRepInfo
    { drepInfoReg      :: !DRepRegistration
    , drepInfoMetadata :: !(Maybe DRepMetadata)
    } deriving (Eq, Show)

-- | Service facade for DRep data. Merges live LSQ data with cached metadata.
newtype DRepLayer m = DRepLayer
    { listDRepInfos :: m [DRepInfo]
    }

-- | Build a 'DRepLayer' from a 'NetworkLayer' (live LSQ) and a 'DBLayer'
-- (cached metadata).
newDRepLayer :: NetworkLayer IO block -> DBLayer IO -> DRepLayer IO
newDRepLayer netLayer db =
    DRepLayer { listDRepInfos = fetchAndMerge }
  where
    fetchAndMerge = case db of
        DBLayer { atomically, getAllDRepMetadata } -> do
            mRegs  <- listDReps netLayer
            cached <- atomically getAllDRepMetadata
            pure $ case mRegs of
                Nothing   -> []
                Just regs -> map (enrich cached) regs

    enrich :: Map Text DRepMetadata -> DRepRegistration -> DRepInfo
    enrich cached reg =
        let meta = do
                anchor <- drepRegAnchor reg
                Map.lookup (hexBS (drepAnchorHash anchor)) cached
        in  DRepInfo { drepInfoReg = reg, drepInfoMetadata = meta }

hexBS :: B8.ByteString -> Text
hexBS = TE.decodeUtf8 . BA.convertToBase BA.Base16
