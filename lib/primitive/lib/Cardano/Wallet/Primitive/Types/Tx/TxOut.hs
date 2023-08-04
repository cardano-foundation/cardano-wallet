{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
--
-- This module defines the 'TxOut' type.
module Cardano.Wallet.Primitive.Types.Tx.TxOut
  ( -- * Type
    TxOut (..)

    -- * Queries
  , assetIds
  , coin

    -- * Modifiers
  , addCoin
  , mapAssetIds
  , removeAssetId
  , subtractCoin
  )
where

import Cardano.Wallet.Primitive.Types.Address
  ( Address (..)
  )
import Cardano.Wallet.Primitive.Types.Coin
  ( Coin (..)
  )
import Cardano.Wallet.Primitive.Types.Coin qualified as Coin
import Cardano.Wallet.Primitive.Types.TokenBundle
  ( TokenBundle
  )
import Cardano.Wallet.Primitive.Types.TokenBundle qualified as TokenBundle
import Cardano.Wallet.Primitive.Types.TokenMap
  ( AssetId
  , Lexicographic (..)
  )
import Cardano.Wallet.Primitive.Types.TokenMap qualified as TokenMap
import Control.DeepSeq
  ( NFData (..)
  )
import Data.Bifunctor
  ( first
  )
import Data.Generics.Internal.VL.Lens
  ( over
  , view
  )
import Data.Generics.Labels
  (
  )
import Data.Ord
  ( comparing
  )
import Data.Set
  ( Set
  )
import Fmt
  ( Buildable (..)
  , blockMapF
  , prefixF
  , suffixF
  )
import GHC.Generics
  ( Generic
  )
import Prelude

--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

data TxOut = TxOut
  { address
      :: !Address
  , tokens
      :: !TokenBundle
  }
  deriving (Read, Show, Generic, Eq)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

-- Since the 'TokenBundle' type deliberately does not provide an 'Ord' instance
-- (as that would lead to arithmetically invalid orderings), this means we can't
-- automatically derive an 'Ord' instance for the 'TxOut' type.
--
-- Instead, we define an 'Ord' instance that makes comparisons based on
-- lexicographic ordering of 'TokenBundle' values.
--
instance Ord TxOut where
  compare = comparing projection
    where
      projection (TxOut address bundle) = (address, Lexicographic bundle)

instance NFData TxOut

instance Buildable TxOut where
  build txOut =
    buildMap
      [
        ( "address"
        , addressShort
        )
      ,
        ( "coin"
        , build (coin txOut)
        )
      ,
        ( "tokens"
        , build (TokenMap.Nested $ view (#tokens . #tokens) txOut)
        )
      ]
    where
      addressShort =
        mempty
          <> prefixF 8 addressFull
          <> "..."
          <> suffixF 8 addressFull
      addressFull = build $ view #address txOut
      buildMap = blockMapF . fmap (first $ id @String)

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Gets the current set of asset identifiers from a transaction output.
assetIds :: TxOut -> Set AssetId
assetIds (TxOut _ bundle) = TokenBundle.getAssets bundle

-- | Gets the current 'Coin' value from a transaction output.
--
-- 'Coin' values correspond to the ada asset.
coin :: TxOut -> Coin
coin = TokenBundle.getCoin . view #tokens

--------------------------------------------------------------------------------
-- Modifiers
--------------------------------------------------------------------------------

-- | Increments the 'Coin' value of a 'TxOut'.
--
-- Satisfies the following property for all values of 'c':
--
-- >>> subtractCoin c . addCoin c == id
addCoin :: Coin -> TxOut -> TxOut
addCoin val TxOut {address, tokens} =
  TxOut address (tokens <> TokenBundle.fromCoin val)

-- | Applies the given function to all asset identifiers in a 'TxOut'.
mapAssetIds :: (AssetId -> AssetId) -> TxOut -> TxOut
mapAssetIds f (TxOut address bundle) =
  TxOut address (TokenBundle.mapAssetIds f bundle)

-- | Removes the asset corresponding to the given 'AssetId' from a 'TxOut'.
removeAssetId :: TxOut -> AssetId -> TxOut
removeAssetId (TxOut address bundle) asset =
  TxOut address (TokenBundle.setQuantity bundle asset mempty)

-- | Decrements the 'Coin' value of a 'TxOut'.
--
-- Satisfies the following property for all values of 'c':
--
-- >>> subtractCoin c . addCoin c == id
--
-- If the given 'Coin' is greater than the 'Coin' value of the given 'TxOut',
-- the resulting 'TxOut' will have a 'Coin' value of zero.
subtractCoin :: Coin -> TxOut -> TxOut
subtractCoin toSubtract =
  over (#tokens . #coin) (`Coin.difference` toSubtract)
