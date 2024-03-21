{-# OPTIONS --erasure #-}

module Haskell.Data.Maybe where

open import Haskell.Prelude

isNothing : ∀ {a : Set} → Maybe a → Bool
isNothing (Just _) = False
isNothing Nothing = True

isJust : ∀ {a : Set} → Maybe a → Bool
isJust (Just _) = True
isJust Nothing = False

catMaybes : ∀ {a : Set} → List (Maybe a) → List a
catMaybes [] = []
catMaybes (Nothing ∷ xs) = catMaybes xs
catMaybes (Just x ∷ xs) = x ∷ catMaybes xs
