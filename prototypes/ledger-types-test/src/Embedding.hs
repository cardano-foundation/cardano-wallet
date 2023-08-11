module Embedding where

import Prelude

import Typ
import Value

import qualified Data.Map as Map
import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Embedding
------------------------------------------------------------------------------}
-- | An 'Embedding' from a type A into a type B
-- is a many-to-one correspondence from the second type to the first.
--
-- > from . to = id
--
-- See Wadler's introduction to Agda
--  https://plfa.github.io/20.07/Isomorphism/#embedding
data Embedding a b = Embedding
    { to :: a -> b
    , from :: b -> a
    }

-- | An 'EmbeddingV' is a dynamically typed embedding of 'Value' into 'Value'.
--
-- Specifically, if @a `hasTyp` ta@ and @typecheck e ta == Just tb@,
-- then @to e a == b@ and @b `hasTyp tab@.
--
-- The result of @to e a@ can be 'undefined' if the value @a@ does
-- not have the expected 'Typ', that is @typecheck e ta == Nothing tb@.
data EmbeddingTyp = EmbeddingTyp
    { embed :: Embedding Value Value
        -- ^ Embedding of 'Value's from one 'Typ' into the other.
    , typecheck :: Typ -> Maybe Typ
        -- ^ Check whether the 'Embedding' works on the given 'Typ'.
    }

-- Design Question: Return the embedding as part of the type check?

{-----------------------------------------------------------------------------
    Functorial operations
------------------------------------------------------------------------------}
-- | Composition of embeddings. Right-to-left.
--
-- > to (embed (ebc <> eab)) = to (embed ebc) . to (embed eab)
instance Semigroup EmbeddingTyp where
    ebc <> eab = EmbeddingTyp
        { embed = Embedding
            { to = to (embed ebc) . to (embed eab)
            , from = from (embed eab) . from (embed ebc)
            }
        , typecheck = \t -> typecheck eab t >>= typecheck ebc
        }

instance Monoid EmbeddingTyp where
    mempty = EmbeddingTyp
        { embed = Embedding { to = id, from = id }
        , typecheck = Just
        }

-- | Operate on the argument of an unary operation.
map1 :: EmbeddingTyp -> EmbeddingTyp
map1 ee = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            One x -> One (fmap' (to e) x)
            _ -> error "Typ(e) error in: map1, to"
        , from = \b -> case b of
            One x -> One (fmap' (from e) x)
            _ -> error "Typ(e) error in: map1, from"
        }
    , typecheck = \t -> case t of
        Unary op a -> Unary op <$> typecheck ee a
        _ -> Nothing
    }
  where
    e = embed ee

    fmap' :: (Value -> Value) -> OneF Value -> OneF Value
    fmap' f (OptionV a) = OptionV (f <$> a)
    fmap' f (SequenceV a) = SequenceV (f <$> a)
    fmap' f (PowerSetV a) = PowerSetV (Set.map f a)


-- | Operate on the first argument of a 'Product' or 'Sum'.
first' :: EmbeddingTyp -> EmbeddingTyp
first' ee = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            ProductV [x,y] -> ProductV [to e x, y]
            SumV 0 x -> SumV 0 $ to e x
            SumV 1 _ -> a
            _ -> error "Typ(e) error in: first', to"
        , from = \b -> case b of
            ProductV [x2,y2] -> ProductV [from e x2, y2]
            SumV 0 x2 -> SumV 0 $ from e x2
            SumV 1 _ -> b
            _ -> error "Typ(e) error in: first', from"
        }
    , typecheck = \t -> case t of
        Binary fun a0 b
            | fun == Sum || fun == Product
                -> (\a1 -> Binary fun a1 b) <$> typecheck ee a0
        _ -> Nothing
    }
  where
    e = embed ee

-- | Operate on the first argument of a 'Product' or 'Sum'.
second' :: EmbeddingTyp -> EmbeddingTyp
second' ee = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            ProductV [x,y] -> ProductV [x, to e y]
            SumV 0 _ -> a
            SumV 1 y -> SumV 1 $ to e y
            _ -> error "Typ(e) error in: second', to"
        , from = \b -> case b of
            ProductV [x2, y2] -> ProductV [x2, from e y2]
            SumV 0 _ -> b
            SumV 1 y2 -> SumV 1 $ from e y2
            _ -> error "Typ(e) error in: second', from"
        }
    , typecheck = \t -> case t of
        Binary fun a b0
            | fun == Sum || fun == Product
                -> (\b1 -> Binary fun a b1) <$> typecheck ee b0
        _ -> Nothing
    }
  where
    e = embed ee

-- | Associative law for products.
--
-- > (A × B) × C  =>  A × (B × C)
assocR :: EmbeddingTyp
assocR = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            ProductV [ProductV [a,b],c] -> ProductV [a, ProductV [b,c]]
            _ -> error "Typ(e) error in: assocR, to"
        , from = \b -> case b of
            ProductV [a, ProductV [b,c]] -> ProductV [ProductV [a,b],c]
            _ -> error "Typ(e) error in: assocR, from"
        }
    , typecheck = \t -> case t of
        Binary Product (Binary Product a b) c
            -> Just $ Binary Product a (Binary Product b c)
        _ -> Nothing
    }

{-----------------------------------------------------------------------------
    Basic algebra
------------------------------------------------------------------------------}
-- | Unit law for a monoid.
--
-- > () ↦0 A  =>  A
unit0 :: Value -> EmbeddingTyp
unit0 zero = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            Two (FiniteMapV m)
                -> Map.findWithDefault zero (Zero UnitV) m
            _ -> error "Typ(e) error in: unit0, to"
        , from = \b ->
            Two . FiniteMapV $ Map.singleton (Zero UnitV) b
        }
    , typecheck = \t -> case t of
        Binary FiniteSupport (Var "Unit") s -> Just s
        _ -> Nothing
    }

-- | Exponential law(s)
-- 
-- > (A ⊎ B) ↦0 C  =>  (A ↦0 C) × (B ↦0 C)
-- > (A ⊎ B) ↦  C  =>  (A ↦ C)  × (B ↦ C)
exponential :: EmbeddingTyp
exponential = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            Two (FiniteMapV m) ->
                ProductV
                    [ Two $ FiniteMapV (left m)
                    , Two $ FiniteMapV (right m)
                    ]
            _ -> error "Typ(e) error in: exponential, to"
        , from = \b -> case b of
            ProductV
                [ Two (FiniteMapV ml)
                , Two (FiniteMapV mr)
                ]
                -> Two $ FiniteMapV (plus ml mr)
            _ -> error "Typ(e) error in: exponential, from"
        }
    , typecheck = \t -> case t of
        Binary fun (Binary Sum a b) c
            | fun == FiniteSupport || fun == PartialFunction
            -> Just $ Binary Product (Binary fun a c) (Binary fun b c)
        _ -> Nothing
    }
  where
    plus ml mr
        = Map.mapKeys (SumV 0) ml <> Map.mapKeys (SumV 1) mr

    left = withKeys matchLeft
    right = withKeys matchRight

    withKeys f
        = Map.mapKeys (\(SumV _ x) -> x)
        . Map.mapMaybeWithKey (\k v -> f k v)

    matchLeft (SumV 0 _) v = Just v
    matchLeft _ _ = Nothing

    matchRight (SumV 1 _) v = Just v
    matchRight _ _ = Nothing

{-----------------------------------------------------------------------------
    Conversions
------------------------------------------------------------------------------}
-- | Representation of finite maps as sequences of pairs.
--
-- > A ↦ B   =>  (A × B)*
-- > A ↦0 B   =>  (A × B)*
representMap :: EmbeddingTyp
representMap = EmbeddingTyp
    { embed = Embedding
        { to = \a -> case a of
            Two (FiniteMapV m) -> valueFromList (Map.toList m)
            _ -> error "Typ(e) error in: representMap, to"
        , from = \b -> case b of
            One (SequenceV xs) ->
                Two $ FiniteMapV
                    $ Map.fromList [ (a,b) | ProductV [a,b] <- xs ]
            _ -> error "Typ(e) error in: representMap, from"
        }
    , typecheck = \t -> case t of
        Binary PartialFunction a b
            -> Just $ Unary Sequence (Binary Product a b)
        Binary FiniteSupport a b
            -> Just $ Unary Sequence (Binary Product a b)
        _ -> Nothing
    }
  where
    valueFromList = One . SequenceV . map (\(a,b) -> ProductV [a,b])
