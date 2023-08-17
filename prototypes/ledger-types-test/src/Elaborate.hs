module Elaborate
    ( elaborates
    ) where

import Prelude

import Typ
    ( Typ (..), OpBinary (..), OpUnary (..) )

{-----------------------------------------------------------------------------
    Elaboration
------------------------------------------------------------------------------}
-- | Type A elaborates type B if it adds more specific information,
-- such as field or constructor names, while representing the same structure.
--
-- Specifically,
--
-- * Every type elaborates 'Abstract'.
-- * A record type with field names elaborates a product.
-- * A sum type with constructor names elaborates a disjoint sum.
elaborates :: Typ -> Typ -> Bool
elaborates _ Abstract = True
elaborates (Var name1) (Var name2) = name1 == name2
elaborates (Unary op1 a1) (Unary op2 a2)
    = op1 == op2 && elaborates a1 a2
elaborates (Binary op1 a1 b1) (Binary op2 a2 b2)
    = op1 == op2 && elaborates a1 a2 && elaborates b1 b2
elaborates (Record fields) a
    | Just components <- matchProduct a =
        elaboratesSeq (map snd fields) components
    | otherwise = False
elaborates (Union fields) a
    | Just summands <- matchSum a =
        elaboratesSeq (map snd fields) summands
    | otherwise = False
elaborates _ _ = False

-- | Check whether a sequence of types elaborates another sequence.
elaboratesSeq:: [Typ] -> [Typ] -> Bool
elaboratesSeq [] [] = True
elaboratesSeq (x:xs) (y:ys) = elaborates x y && elaboratesSeq xs ys
elaboratesSeq _ _ = False

-- | Match a type @X@ against a product @A1 × A2 × … × An@
-- with at least two components.
--
-- Association does not matter, i.e. @(A × B) × C = A × (B × C)@ have
-- the same components.
matchProduct :: Typ -> Maybe [Typ]
matchProduct typ = case typ of
    e@(Binary Product _ _) -> Just $ match e
    _ -> Nothing
  where
    match (Binary Product a b) = match a <> match b
    match e = [e]

-- | Match a type @X@ against a sum @A1 ⊎ A2 ⊎ … ⊎ An@
-- with at least two summands.
--
-- Association does not matter, i.e. @(A ⊎ B) ⊎ C = A ⊎ (B ⊎ C)@ have
-- the same summands.
matchSum :: Typ -> Maybe [Typ]
matchSum typ = case typ of
    e@(Binary Sum _ _) -> Just $ match e
    _ -> Nothing
  where
    match (Binary Sum a b) = match a <> match b
    match e = [e]
