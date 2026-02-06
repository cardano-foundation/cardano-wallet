{-# OPTIONS --erasure #-}

-- Synchronized manually with the corresponding Haskell module.
module Haskell.Cardano.Wallet.Read.Address
    {-
    ; CompactAddr
      ; toShortByteString
      ; fromShortByteString
      ; isBootstrapCompactAddr
    -}
    where

open import Haskell.Prelude
open import Haskell.Law

open import Haskell.Data.ByteString.Short using
    ( ShortByteString
    )
open import Haskell.Data.Maybe using
    ( isJust
    )

prop-Just-injective
  : ∀ {a : Set} (x y : a)
  → Just x ≡ Just y
  → x ≡ y
prop-Just-injective _ _ refl = refl

{-----------------------------------------------------------------------------
    Address
------------------------------------------------------------------------------}
postulate
  CompactAddr : Set

  instance
    iEqCompactAddr : Eq CompactAddr
    iIsLawfulEqCompactAddr : IsLawfulEq CompactAddr

  iOrdCompactAddr₀ : Ord CompactAddr

  toShortByteString : CompactAddr → ShortByteString
  fromShortByteString : ShortByteString → Maybe CompactAddr

  isBootstrapCompactAddr : CompactAddr → Bool

instance
  iOrdCompactAddr : Ord CompactAddr
  iOrdCompactAddr = record iOrdCompactAddr₀ { super = iEqCompactAddr }

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}

postulate

  prop-from-∘-to
    : ∀ (x : CompactAddr)
    → fromShortByteString (toShortByteString x) ≡ Just x

  prop-to-∘-from-Just
    : ∀ (s : ShortByteString)
        (x : CompactAddr)
    → fromShortByteString s ≡ Just x
    → toShortByteString x ≡ s

prop-toShortByteString-injective
  : ∀ (x y : CompactAddr)
  → toShortByteString x ≡ toShortByteString y
  → x ≡ y
prop-toShortByteString-injective x y eq =
    prop-Just-injective _ _ lem1
  where
    lem1 : _
    lem1 =
      begin
        Just x
      ≡⟨ sym (prop-from-∘-to x) ⟩
        fromShortByteString (toShortByteString x)
      ≡⟨ cong fromShortByteString eq ⟩
        fromShortByteString (toShortByteString y)
      ≡⟨ prop-from-∘-to y ⟩
        Just y
      ∎

@0 prop-fromShortByteString-partially-injective
  : ∀ (sx sy : ShortByteString)
  → @0 (isJust (fromShortByteString sx) ≡ True)
  → fromShortByteString sx ≡ fromShortByteString sy
  → sx ≡ sy
prop-fromShortByteString-partially-injective sx sy eqJust eq =
  case fromShortByteString sx of λ
    { Nothing {{eqNothingx}} →
        case (trans (sym eqJust) (cong isJust eqNothingx)) of λ ()
    ; (Just x) {{eqJustx}} →
       case fromShortByteString sy of λ 
       { Nothing {{eqNothingy}} →
            case (trans (trans (sym eqJustx) eq) eqNothingy) of λ ()
       ; (Just y) {{eqJusty}} →
        begin
          sx
        ≡⟨ sym (prop-to-∘-from-Just sx y (trans eq eqJusty)) ⟩
          toShortByteString y
        ≡⟨ prop-to-∘-from-Just sy y eqJusty ⟩
          sy
        ∎
       }
    }
 