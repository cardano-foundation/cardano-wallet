{-# OPTIONS --erasure #-}

module Haskell.Data.ByteString.Short
    {-
    ; ByteString
    -}
    where

open import Haskell.Prelude hiding (lookup; null; map)
open import Haskell.Law

open import Haskell.Data.Word using
    ( Word8
    )

import Haskell.Data.ByteString as BS

{-----------------------------------------------------------------------------
    ByteString
------------------------------------------------------------------------------}

postulate
  ShortByteString : Set

postulate
  pack   : List Word8 → ShortByteString
  unpack : ShortByteString → List Word8

  instance
    iEqShortByteString  : Eq ShortByteString
  
  iOrdShortByteString₀ : Ord ShortByteString

instance
  iOrdShortByteString : Ord ShortByteString
  iOrdShortByteString =
      record iOrdShortByteString₀ { super = iEqShortByteString }

empty : ShortByteString
empty = pack []

singleton : Word8 → ShortByteString
singleton x = pack (x ∷ [])

fromShort : ShortByteString → BS.ByteString
fromShort = BS.pack ∘ unpack

toShort : BS.ByteString → ShortByteString
toShort = pack ∘ BS.unpack

append : ShortByteString → ShortByteString → ShortByteString
append x y = pack (unpack x ++ unpack y)

instance
  iSemigroupShortByteString : Semigroup ShortByteString
  iSemigroupShortByteString = record { _<>_ = append }

instance
  iMonoidShortByteString : Monoid ShortByteString
  iMonoidShortByteString =
    record {DefaultMonoid (λ where .DefaultMonoid.mempty → empty)}

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}

postulate
  prop-pack-∘-unpack
    : ∀ (x : ShortByteString)
    → pack (unpack x) ≡ x

  prop-unpack-∘-pack
    : ∀ (x : List Word8)
    → unpack (pack x) ≡ x
  
  instance
    iLawfulEqShortByteString : IsLawfulEq ShortByteString

{-----------------------------------------------------------------------------
    Properties
    Injectivity
------------------------------------------------------------------------------}

--
prop-pack-injective
  : ∀ (x y : List Word8)
  → pack x ≡ pack y
  → x ≡ y
--
prop-pack-injective x y eq =
  begin
    x
  ≡⟨ sym (prop-unpack-∘-pack x) ⟩
    unpack (pack x)
  ≡⟨ cong unpack eq ⟩
    unpack (pack y)
  ≡⟨ prop-unpack-∘-pack y ⟩
    y
  ∎

--
prop-unpack-injective
  : ∀ (x y : ShortByteString)
  → unpack x ≡ unpack y
  → x ≡ y
--
prop-unpack-injective x y eq =
  begin
    x
  ≡⟨ sym (prop-pack-∘-unpack x) ⟩
    pack (unpack x)
  ≡⟨ cong pack eq ⟩
    pack (unpack y)
  ≡⟨ prop-pack-∘-unpack y ⟩
    y
  ∎

--
prop-fromShort-injective
  : ∀ (x y : ShortByteString)
  → fromShort x ≡ fromShort y
  → x ≡ y
--
prop-fromShort-injective x y =
  prop-unpack-injective _ _
  ∘ BS.prop-pack-injective _ _

--
prop-toShort-injective
  : ∀ (x y : BS.ByteString)
  → toShort x ≡ toShort y
  → x ≡ y
--
prop-toShort-injective x y =
  BS.prop-unpack-injective _ _
  ∘ prop-pack-injective _ _

{-----------------------------------------------------------------------------
    Properties
    Semigroup morphisms
------------------------------------------------------------------------------}

--
prop-pack-morphism
  : ∀ (x y : List Word8)
  → pack x <> pack y ≡ pack (x ++ y)
--
prop-pack-morphism x y =
  begin
    pack x <> pack y
  ≡⟨⟩
    pack (unpack (pack x) ++ unpack (pack y))
  ≡⟨ cong (λ X → pack (X ++ unpack (pack y))) (prop-unpack-∘-pack x) ⟩
    pack (x ++ unpack (pack y))
  ≡⟨ cong (λ Y → pack (x ++ Y)) (prop-unpack-∘-pack y) ⟩
    pack (x ++ y)
  ∎

--
prop-unpack-morphism
  : ∀ (x y : ShortByteString)
  → unpack (x <> y) ≡ unpack x ++ unpack y
--
prop-unpack-morphism x y =
  begin
    unpack (x <> y)
  ≡⟨ cong unpack refl ⟩
    unpack (pack (unpack x ++ unpack y))
  ≡⟨ prop-unpack-∘-pack _ ⟩
    unpack x ++ unpack y
  ∎

--
prop-<>-cancel-left
  : ∀ (x y z : ShortByteString)
  → x <> y ≡ x <> z
  → y ≡ z
--
prop-<>-cancel-left x y z =
  prop-unpack-injective _ _
  ∘ ++-cancel-left (unpack x) (unpack y)
  ∘ prop-pack-injective _ _

--
prop-singleton-<>-injective
  : ∀ (x y : Word8) (xs ys : ShortByteString)
  → singleton x <> xs ≡ singleton y <> ys
  → (x ≡ y × xs ≡ ys)
--
prop-singleton-<>-injective x y xs ys eq =
    ( ∷-injective-left lem2
    , prop-unpack-injective _ _ (∷-injective-right lem2)
    )
  where
    lem1
      : ∀ (z : Word8) (zs : ShortByteString)
      → z ∷ unpack zs ≡ unpack (singleton z <> zs)
    lem1 z zs =
      begin
        z ∷ unpack zs
      ≡⟨⟩
        (z ∷ []) ++ unpack zs
      ≡⟨ cong (λ o → o ++ unpack zs) (sym (prop-unpack-∘-pack (z ∷ []))) ⟩
        unpack (pack (z ∷ [])) ++ unpack zs
      ≡⟨ sym (prop-unpack-morphism (pack (z ∷ [])) zs) ⟩
        unpack (singleton z <> zs)
      ∎

    lem2 =
      begin
        x ∷ unpack xs
      ≡⟨ lem1 x xs ⟩
        unpack (singleton x <> xs)
      ≡⟨ cong unpack eq ⟩
        unpack (singleton y <> ys)
      ≡⟨ sym (lem1 y ys) ⟩
        y ∷ unpack ys
      ∎
