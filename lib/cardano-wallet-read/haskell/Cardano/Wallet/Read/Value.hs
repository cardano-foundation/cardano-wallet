{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- 'Value' — ADA and native assets.
module Cardano.Wallet.Read.Value
    ( -- * Coin
      Coin (CoinC, unCoin)

      -- * MultiAsset
    , MultiAsset
    , AssetName
    , PolicyID
    , AssetID (..)
    , Quantity

      -- * Value
    , Value (ValueC, getCoin, getAssets)
    , lookupAssetID
    , injectCoin
    , valueFromList
    , add
    , subtract
    , lessOrEqual
    , largerOrEqual

    -- * Internal
    , fromEraValue
    , toMaryValue
    ) where

import Prelude hiding
    ( subtract
    )

import Cardano.Ledger.Coin
    ( Coin (Coin)
    )
import Cardano.Ledger.Val
    ( pointwise
    , (<->)
    )
import Cardano.Read.Ledger.Eras
    ( Era (..)
    , IsEra (..)
    )

import qualified Cardano.Ledger.BaseTypes as SH
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Mary.Value as MA
import qualified Cardano.Read.Ledger.Value as L

{-----------------------------------------------------------------------------
    Coin
------------------------------------------------------------------------------}
{-# COMPLETE CoinC #-}
pattern CoinC :: Integer -> Coin
pattern CoinC{unCoin} = Coin unCoin

{-----------------------------------------------------------------------------
    MultiAssets
------------------------------------------------------------------------------}

type AssetName = MA.AssetName

type PolicyID = MA.PolicyID

type Quantity = Integer

-- | Identifier for an asset.
data AssetID
    = AdaID
    | Asset PolicyID AssetName

deriving instance Eq AssetID
deriving instance Ord AssetID

type MultiAsset = MA.MultiAsset

{-----------------------------------------------------------------------------
    Value
------------------------------------------------------------------------------}

-- | Monetary values, representing both ADA and native assets/tokens.
newtype Value = Value MA.MaryValue

-- | Internal: Convert from ledger 'MaryValue'.
fromMaryValue :: MA.MaryValue -> Value
fromMaryValue = Value

-- | Internal: Convert to ledger 'MaryValue'.
toMaryValue :: Value -> MA.MaryValue
toMaryValue (Value v) = v

instance Eq Value where
    (Value x) == (Value y) = x == y

instance Show Value where
    show (Value x) = show x

{-# COMPLETE ValueC #-}
pattern ValueC :: Coin -> MultiAsset -> Value
pattern ValueC{getCoin, getAssets} = Value (MA.MaryValue getCoin getAssets)

-- | Internal: Convert from era-indexed 'L.Value'.
fromEraValue :: forall era. IsEra era => L.Value era -> Value
fromEraValue = fromMaryValue . case theEra :: Era era of
    Byron -> onValue L.maryValueFromByronValue
    Shelley -> onValue L.maryValueFromShelleyValue
    Allegra -> onValue L.maryValueFromShelleyValue
    Mary -> onValue id
    Alonzo -> onValue id
    Babbage -> onValue id
    Conway -> onValue id

-- Helper function for type inference.
onValue :: (L.ValueType era -> t) -> L.Value era -> t
onValue f (L.Value x) = f x

-- | Look up the quantity corresponding to an 'AssetID'.
lookupAssetID :: AssetID -> Value -> Quantity
lookupAssetID AdaID value = unCoin $ getCoin value
lookupAssetID (Asset policyId assetName) (Value value) =
    MA.lookupMultiAsset policyId assetName value

-- | Turn a 'Coin' into a 'Value', @inject@ from the specification.
injectCoin :: Coin -> Value
injectCoin = Value . SH.inject

-- | Construct a 'Value' from a 'Coin' and a list of assets.
valueFromList :: Coin -> [(PolicyID, AssetName, Quantity)] -> Value
valueFromList coin = Value . MA.valueFromList coin

-- | '(<>)' adds monetary values.
instance Semigroup Value where
    (Value x) <> (Value y) = Value (x <> y)

instance Monoid Value where
    mempty = Value mempty

-- | Add all quantities in the second argument to the first argument.
-- Synonym of '(<>)'.
--
-- > ∀ a. lookupAssetID a (x `add` y)
-- >      = lookupAssetID a x + lookupAssetID a y
add :: Value -> Value -> Value
add = (<>)

-- | Subtract the quantities in the second argument from the first argument.
--
-- > ∀ a. lookupAssetID a (x `subtract` y)
-- >      = lookupAssetID a x - lookupAssetID a y
subtract :: Value -> Value -> Value
subtract (Value x) (Value y) = Value (x <-> y)

-- | Check whether all assets in the first argument
-- are present in less or equal quantity
-- than the assets in the second argument.
lessOrEqual :: Value -> Value -> Bool
lessOrEqual (Value value1) (Value value2) =
    pointwise (<=) value1 value2

-- | Check whether all assets in the first argument
-- are present in larger or equal quantity
-- than the assets in the second argument.
largerOrEqual :: Value -> Value -> Bool
largerOrEqual (Value value1) (Value value2) =
    pointwise (>=) value1 value2
