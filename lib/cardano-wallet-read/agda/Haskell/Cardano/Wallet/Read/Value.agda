{-# OPTIONS --erasure #-}

-- Synchronized manually with the corresponding Haskell module.
module Haskell.Cardano.Wallet.Read.Value where

open import Haskell.Law
open import Haskell.Prelude

{-----------------------------------------------------------------------------
    Coin
------------------------------------------------------------------------------}

-- imported, not transpiled
record Coin : Set where
  constructor CoinC
  field
    unCoin : Integer

open Coin public

mkCoin : Integer → Coin
mkCoin = CoinC

instance
  iEqCoin : Eq Coin
  iEqCoin ._==_ x y =
    unCoin x == unCoin y

{-----------------------------------------------------------------------------
    MultiAsset
------------------------------------------------------------------------------}

postulate
  MultiAsset : Set
  AssetName : Set
  PolicyID : Set

postulate instance
  iEqMultiAsset : Eq MultiAsset
  iEqAssetName : Eq AssetName
  iEqPolicyID : Eq PolicyID

data AssetID : Set where
  AdaID : AssetID
  Asset : PolicyID → AssetName → AssetID

instance
  iEqAssetID : Eq AssetID
  iEqAssetID ._==_ AdaID AdaID = True
  iEqAssetID ._==_ (Asset a1 b1) (Asset a2 b2) = a1 == a2 && b1 == b2
  iEqAssetID ._==_ _ _ = False

postulate instance
  iOrdAssetID : Ord AssetID

Quantity = Integer

{-----------------------------------------------------------------------------
    Value
------------------------------------------------------------------------------}

-- imported, not transpiled
record Value : Set where
  constructor ValueC
  field
    getCoin : Coin
    getAssets : MultiAsset

open Value public

postulate
  lookupAssetID : AssetID → Value → Quantity
  injectCoin : Coin → Value
  valueFromList : Coin → List (PolicyID × AssetName × Quantity) → Value
  add : Value → Value → Value
  subtract : Value → Value → Value
  lessOrEqual : Value → Value → Bool
  largerOrEqual : Value → Value → Bool

instance
  iEqValue : Eq Value
  iEqValue ._==_ x y =
    getCoin x == getCoin y && getAssets x == getAssets y

  iSemigroupValue : Semigroup Value
  _<>_ {{iSemigroupValue}} = add

  iMonoidValue : Monoid Value
  iMonoidValue =
    record {DefaultMonoid (λ where
      .DefaultMonoid.mempty → injectCoin (mkCoin 0)
    )}

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
postulate
  prop-coin-inject
    : ∀ (c : Coin)
    → getCoin (injectCoin c) ≡ c

  instance
    iIsLawfulSemigroupValue : IsLawfulSemigroup Value
    iIsLawfulMonoidValue : IsLawfulMonoid Value

  prop-Value-<>-sym
    : ∀ (x y : Value)
    → x <> y ≡ y <> x
  
  prop-largerOrEqual-lessOrEqual
    : ∀ (x y : Value)
    → largerOrEqual x y ≡ lessOrEqual y x

  prop-add-monotone
    : ∀ (x y z : Value)
    → largerOrEqual (add x z) (add y z) ≡ largerOrEqual x y
