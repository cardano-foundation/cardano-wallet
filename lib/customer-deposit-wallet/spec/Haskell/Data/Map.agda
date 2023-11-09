{-# OPTIONS --erasure #-}

-- Formalization of Data.Map
module Haskell.Data.Map where

open import Haskell.Prelude hiding (lookup; null; map)
import Haskell.Prelude as L using (map)

-- These lemmas are obvious substitutions,
-- but substitution in a subterm is sometimes cumbersome
-- with equational reasoning.
lemma-if-True
  : ∀ {A B : Set} {{_ : Eq A}} (x x' : A) {t f : B}
  → (x == x') ≡ True
  → (if (x == x') then t else f) ≡ t
lemma-if-True _ _ eq1 rewrite eq1 = refl

lemma-if-False
  : ∀ {A B : Set} {{_ : Eq A}} (x x' : A) {t f : B}
  → (x == x') ≡ False
  → (if (x == x') then t else f) ≡ f
lemma-if-False _ _ eq1 rewrite eq1 = refl

-- Data.Map

postulate
  Map : ∀ (k : Set) {{iOrd : Ord k}} → Set → Set

module
    OperationsAndProperties
      {k a : Set}
      {{iOrd : Ord k}}
  where
  postulate
    lookup : k → Map k a → Maybe a
    null      : Map k a → Bool

    empty     : Map k a
    insert    : k → a → Map k a → Map k a
    delete    : k → Map k a → Map k a
    toAscList : Map k a → List (k × a)
    fromList  : List (k × a) → Map k a
    fromListWith : (a → a → a) → List (k × a) → Map k a
    unionWith    : (a → a → a) → Map k a → Map k a → Map k a

    instance
      iMapFunctor : Functor (Map k)

    prop-lookup-empty
      : ∀ (key : k)
      → lookup key empty ≡ Nothing

    prop-lookup-insert
      : ∀ (key keyi : k) (x : a) (m : Map k a)
      → lookup key (insert keyi x m)
        ≡ (if (key == keyi) then Just x else lookup key m)

    prop-lookup-delete
      : ∀ (key keyi : k) (m : Map k a)
      → lookup key (delete keyi m)
        ≡ (if (key == keyi) then Nothing else lookup key m)

    prop-lookup-toAscList-Just
      : ∀ (key : k) (x : a) (m : Map k a)
      → lookup key m ≡ Just x
      → (elem key ∘ L.map fst ∘ toAscList) m ≡ True

    prop-lookup-toAscList-Nothing
      : ∀ (key : k) (x : a) (m : Map k a)
      → lookup key m ≡ Nothing
      → (elem key ∘ L.map fst ∘ toAscList) m ≡ False


  map : ∀ {b : Set} → (a → b) → Map k a → Map k b
  map = fmap

  singleton : k → a → Map k a
  singleton = λ k x → insert k x empty

  prop-lookup-singleton
    : ∀ (key keyi : k) (x : a)
    → lookup key (singleton keyi x)
      ≡ (if (key == keyi) then Just x else Nothing)
  prop-lookup-singleton key keyi x =
    begin
      lookup key (singleton keyi x)
    ≡⟨⟩
      lookup key (insert keyi x empty)
    ≡⟨ prop-lookup-insert key keyi x empty ⟩
      (if (key == keyi) then Just x else lookup key empty)
    ≡⟨ cong (λ f → if (key == keyi) then Just x else f) (prop-lookup-empty key) ⟩
      (if (key == keyi) then Just x else Nothing)
    ∎

open OperationsAndProperties public
