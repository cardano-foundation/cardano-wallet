{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

'Value' — ADA and native assets.
-}
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
    , Value (ValueC,getCoin,getAssets)
    , lookupAssetID
    , injectCoin
    , valueFromList
    , add
    , subtract
    , lessOrEqual

    -- * Internal
    , fromMaryValue
    , toMaryValue
    ) where

import Prelude hiding
    ( subtract
    )

import Cardano.Ledger.Api
    ( StandardCrypto
    )
import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Ledger.Val
    ( pointwise
    , (<->)
    )

import qualified Cardano.Ledger.BaseTypes as SH
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Mary.Value as MA

{-----------------------------------------------------------------------------
    Coin
------------------------------------------------------------------------------}
{-# COMPLETE CoinC #-}
pattern CoinC :: Integer -> Coin
pattern CoinC{unCoin} = L.Coin unCoin

{-----------------------------------------------------------------------------
    MultiAssets
------------------------------------------------------------------------------}

type AssetName = MA.AssetName

type PolicyID = MA.PolicyID StandardCrypto

type Quantity = Integer

-- | Identifier for an asset.
data AssetID
    = AdaID
    | Asset PolicyID AssetName

deriving instance Eq AssetID
deriving instance Ord AssetID

type MultiAsset = MA.MultiAsset StandardCrypto

{-----------------------------------------------------------------------------
    Value
------------------------------------------------------------------------------}
-- | Monetary values, representing both ADA and native assets/tokens.
newtype Value = Value (MA.MaryValue StandardCrypto)

-- | Internal: Convert from ledger 'MaryValue'.
fromMaryValue :: MA.MaryValue StandardCrypto -> Value
fromMaryValue = Value

-- | Internal: Convert to ledger 'MaryValue'.
toMaryValue :: Value -> MA.MaryValue StandardCrypto
toMaryValue (Value v) = v

instance Eq Value where
    (Value x) == (Value y) = x == y

instance Show Value where
    show (Value x) = show x

{-# COMPLETE ValueC #-}
pattern ValueC :: Coin -> MultiAsset -> Value
pattern ValueC{getCoin,getAssets} = Value (MA.MaryValue getCoin getAssets)

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
