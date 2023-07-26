{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
--
-- This module defines the 'TxOut' type.
--
module Cardano.Wallet.Primitive.Types.Tx.TxOut
    (
    -- * Type
      TxOut (..)

    -- * Queries
    , assetIds
    , coin

    -- * Modifiers
    , addCoin
    , mapAssetIds
    , removeAssetId
    , subtractCoin

    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash, Script (..), keyHashToText )
import Cardano.Address.Script.Parser
    ( requireSignatureOfParser, scriptParser )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId, Lexicographic (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.Bifunctor
    ( first )
import Data.Generics.Internal.VL.Lens
    ( over, view )
import Data.Generics.Labels
    ()
import Data.Ord
    ( comparing )
import Data.Set
    ( Set )
import Fmt
    ( Buildable (..), blockMapF, prefixF, suffixF )
import GHC.Generics
    ( Generic )

import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Text as T
import qualified Text.ParserCombinators.ReadP as Parse


--------------------------------------------------------------------------------
-- Type
--------------------------------------------------------------------------------

data TxOut = TxOut
    { address
        :: !Address
    , tokens
        :: !TokenBundle
    , script
        :: !(Maybe (Script KeyHash))
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
        projection (TxOut address bundle _) = (address, Lexicographic bundle)

instance NFData TxOut

instance Buildable TxOut where
    build txOut = buildMap
        [ ("address"
          , addressShort)
        , ("coin"
          , build (coin txOut))
        , ("tokens"
          , build (TokenMap.Nested $ view (#tokens . #tokens) txOut))
        , ("script"
          , build (T.pack . show $ script txOut))
        ]
      where
        addressShort = mempty
            <> prefixF 8 addressFull
            <> "..."
            <> suffixF 8 addressFull
        addressFull = build $ view #address txOut
        buildMap = blockMapF . fmap (first $ id @String)

instance Read (Script KeyHash) where
    readsPrec _ = Parse.readP_to_S (scriptParser requireSignatureOfParser)

instance {-# OVERLAPPING #-} Show (Maybe (Script KeyHash)) where
    show scriptM = case scriptM of
        Nothing -> "not present"
        Just script -> T.unpack . scriptToText $ script
      where
          scriptToText :: Script KeyHash -> T.Text
          scriptToText (RequireSignatureOf keyhash) =
              keyHashToText keyhash
          scriptToText (RequireAllOf contents) =
              "all [" <>  T.intercalate "," (map scriptToText contents) <> "]"
          scriptToText (RequireAnyOf contents) =
              "any [" <>  T.intercalate "," (map scriptToText contents) <> "]"
          scriptToText (RequireSomeOf m contents) =
              "at_least "<> T.pack (show m) <>
              " [" <>  T.intercalate "," (map scriptToText contents) <> "]"
          scriptToText (ActiveFromSlot s) =
              "active_from " <> T.pack (show s)
          scriptToText (ActiveUntilSlot s) =
              "active_until " <> T.pack (show s)

--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

-- | Gets the current set of asset identifiers from a transaction output.
--
assetIds :: TxOut -> Set AssetId
assetIds (TxOut _ bundle _) = TokenBundle.getAssets bundle

-- | Gets the current 'Coin' value from a transaction output.
--
-- 'Coin' values correspond to the ada asset.
--
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
--
addCoin :: Coin -> TxOut -> TxOut
addCoin val TxOut {address, tokens, script} =
    TxOut address (tokens <> TokenBundle.fromCoin val) script

-- | Applies the given function to all asset identifiers in a 'TxOut'.
--
mapAssetIds :: (AssetId -> AssetId) -> TxOut -> TxOut
mapAssetIds f (TxOut address bundle script) =
    TxOut address (TokenBundle.mapAssetIds f bundle) script

-- | Removes the asset corresponding to the given 'AssetId' from a 'TxOut'.
--
removeAssetId :: TxOut -> AssetId -> TxOut
removeAssetId (TxOut address bundle script) asset =
    TxOut address (TokenBundle.setQuantity bundle asset mempty) script

-- | Decrements the 'Coin' value of a 'TxOut'.
--
-- Satisfies the following property for all values of 'c':
--
-- >>> subtractCoin c . addCoin c == id
--
-- If the given 'Coin' is greater than the 'Coin' value of the given 'TxOut',
-- the resulting 'TxOut' will have a 'Coin' value of zero.
--
subtractCoin :: Coin -> TxOut -> TxOut
subtractCoin toSubtract =
    over (#tokens . #coin) (`Coin.difference` toSubtract)
