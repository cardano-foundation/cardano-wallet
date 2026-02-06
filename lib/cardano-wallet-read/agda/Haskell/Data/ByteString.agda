{-# OPTIONS --erasure #-}

module Haskell.Data.ByteString
    {-
    ; ByteString
    -}
    where

open import Haskell.Law
open import Haskell.Prelude hiding (lookup; null; map)

open import Haskell.Data.Word using
    ( Word8
    )

{-----------------------------------------------------------------------------
    ByteString
------------------------------------------------------------------------------}

postulate
  ByteString : Set

postulate
  pack   : List Word8 → ByteString
  unpack : ByteString → List Word8

  instance
    iEqByteString  : Eq ByteString
  
  iOrdByteString₀ : Ord ByteString

instance
  iOrdByteString : Ord ByteString
  iOrdByteString = record iOrdByteString₀ { super = iEqByteString }

empty : ByteString
empty = pack []

append : ByteString → ByteString → ByteString
append x y = pack (unpack x ++ unpack y)

singleton : Word8 → ByteString
singleton x = pack (x ∷ [])

instance
  iSemigroupByteString : Semigroup ByteString
  iSemigroupByteString = record { _<>_ = append }

instance
  iMonoidByteString : Monoid ByteString
  iMonoidByteString =
    record {DefaultMonoid (λ where .DefaultMonoid.mempty → empty)}

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}

postulate
  prop-pack-∘-unpack
    : ∀ (x : ByteString)
    → pack (unpack x) ≡ x

  prop-unpack-∘-pack
    : ∀ (x : List Word8)
    → unpack (pack x) ≡ x
  
  instance
    iLawfulEqByteString : IsLawfulEq ByteString

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
  : ∀ (x y : ByteString)
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
  : ∀ (x y : ByteString)
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
  : ∀ (x y z : ByteString)
  → x <> y ≡ x <> z
  → y ≡ z
--
prop-<>-cancel-left x y z =
  prop-unpack-injective _ _
  ∘ ++-cancel-left (unpack x) (unpack y)
  ∘ prop-pack-injective _ _
