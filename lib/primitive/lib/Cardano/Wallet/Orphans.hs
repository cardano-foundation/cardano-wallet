{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Module for orphans which would be too inconvenient to avoid.
module Cardano.Wallet.Orphans where

import Cardano.Api
  ( TxMetadata (..)
  , TxMetadataValue (..)
  )
import Cardano.Slotting.Slot
  ( SlotNo (..)
  )
import Control.DeepSeq
  ( NFData (..)
  )
import Data.Map qualified as Map
import Data.Ord
  ( comparing
  )
import Fmt
  ( Buildable (..)
  , blockListF
  , hexF
  , nameF
  , unlinesF
  )
import Ouroboros.Consensus.HardFork.History.Qry
  ( PastHorizonException
  )
import UnliftIO.Exception
  ( displayException
  )
import Prelude

instance Buildable SlotNo where
  build (SlotNo n) = build (show n)

-- Compare metadatas by their string representation.
-- Defined here so other types which use TxMetadata can have Ord.
instance Ord TxMetadata where
  compare = comparing show

instance Buildable TxMetadata where
  build (TxMetadata m) =
    unlinesF (map buildElem (Map.toList m))
    where
      buildElem (n, d) = nameF ("element " <> build n) $ buildDatum d
      buildDatum = \case
        TxMetaMap as ->
          blockListF
            $ mconcat
              [ [nameF "key" (buildDatum k), nameF "val" (buildDatum v)]
              | (k, v) <- as
              ]
        TxMetaList xs -> nameF "list" $ blockListF (map buildDatum xs)
        TxMetaNumber i -> build i
        TxMetaBytes bs -> hexF bs
        TxMetaText s -> build (show s)

instance NFData TxMetadata where
  rnf (TxMetadata md) = rnf md

instance NFData TxMetadataValue where
  rnf (TxMetaMap x) = rnf x
  rnf (TxMetaList x) = rnf x
  rnf (TxMetaNumber x) = rnf x
  rnf (TxMetaBytes x) = rnf x
  rnf (TxMetaText x) = rnf x

-- Compare PastHorizonException based on their error messages being the same.
-- Defined here so that other types with use PastHorizonException can have Eq.
instance Eq PastHorizonException where
  a == b = displayException a == displayException b
