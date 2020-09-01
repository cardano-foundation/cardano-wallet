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

import Cardano.Api.Typed
    ( TxMetadata (..) )
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.Ord
    ( comparing )
import Fmt
    ( Buildable (..), blockListF, hexF, nameF, unlinesF )
import Shelley.Spec.Ledger.MetaData
    ( MetaData (..), MetaDatum (..) )

import qualified Data.Map as Map

instance Buildable SlotNo where
    build (SlotNo n) = build (show n)

-- Compare metadatas by their string representation.
-- Defined here so other types which use TxMetadata can have Ord.
instance Ord TxMetadata where
    compare = comparing show

instance Buildable TxMetadata where
    build (TxMetadata (MetaData m)) =
        unlinesF (map buildElem (Map.toList m))
      where
        buildElem (n, d) = nameF ("element " <> build n) $ buildDatum d
        buildDatum = \case
            Map as -> blockListF $ mconcat
                [ [ nameF "key" (buildDatum k), nameF "val" (buildDatum v) ]
                | (k, v) <- as ]
            List xs -> nameF "list" $ blockListF (map buildDatum xs)
            I i -> build i
            B bs -> hexF bs
            S s -> build (show s)

instance NFData MetaDatum

instance NFData MetaData

instance NFData TxMetadata where
    rnf (TxMetadata md) = rnf md
