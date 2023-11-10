{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- A datatype that represents a vector of functions covering all known eras
-- The functions are supposed to map values from and to the same era.
--
-- We removed the cached encoding at the price of 'toEraFun' and 'fromEraFun'
-- during all compositions, we are not 100% it's not relevant for performance
-- If the computed functions after record compositions are the same then we can
-- avoid that layer
--
-- Note composition is anyway expansive, do not recompose,
-- just cache and reuse the compositions
module Cardano.Wallet.Read.Eras.EraFun
    ( -- * Types.
      EraFun
        ( EraFun
        , byronFun
        , shelleyFun
        , allegraFun
        , maryFun
        , alonzoFun
        , babbageFun
        , conwayFun
        )

      -- * Composition.
    , (*.**)
    , (*&&&*)

      -- * Application.
    , applyEraFun

      -- * Constant era 'EraFun'
    , EraFunK (..)

      -- * higher order record encoding
    , runAllEraValue
    , AllEraValue
    , EraFunSel
    )
where

import Prelude hiding
    ( id
    , (.)
    )

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , ConwayEra
    , MaryEra
    , ShelleyEra
    )
import Cardano.Wallet.Read.Eras.EraValue
    ( EraValue (..)
    , MkEraValue (..)
    , inject
    , prisms
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( KnownEras
    )
import Control.Category
    ( Category (..)
    )
import Generics.SOP
    ( (:.:) (..)
    , K (..)
    , NP
    , unComp
    , unK
    )
import Generics.SOP.Classes
import Generics.SOP.NP
    ( NP (..)
    , collapse_NP
    , map_NP
    , pure_NP
    , zipWith_NP
    )
import Generics.SOP.NS
    ( ap_NS
    )
import GHC.Generics
    ( (:*:) (..)
    )

-- | A function that selects a field from any 'EraFun'.
type EraFunSel era = forall f g. EraFun f g -> f era -> g era

-- | A function where domain and codomain are indexed by an era type.
type Fun f g era = f era -> g era

-- | A product of functions indexed by KnownEras.
type EraFunI f g = NP (f -.-> g) KnownEras

-- | A product of functions indexed by KnownEras expressed as record
pattern EraFun
    :: Fun f g ByronEra
    -- ^ Byron era function
    -> Fun f g ShelleyEra
    -- ^ Shelley era function
    -> Fun f g AllegraEra
    -- ^ Allegra era function
    -> Fun f g MaryEra
    -- ^ Mary era function
    -> Fun f g AlonzoEra
    -- ^ Alonzo era function
    -> Fun f g BabbageEra
    -- ^ Babbage era function
    -> Fun f g ConwayEra
    -- ^ Conway era function
    -> EraFun f g
pattern EraFun
    { byronFun
    , shelleyFun
    , allegraFun
    , maryFun
    , alonzoFun
    , babbageFun
    , conwayFun
    } =
    MkEraFun
        ( Fn byronFun
                :* Fn shelleyFun
                :* Fn allegraFun
                :* Fn maryFun
                :* Fn alonzoFun
                :* Fn babbageFun
                :* Fn conwayFun
                :* Nil
            )

-- | Type of vector functions that cover all eras.
newtype EraFun f g = MkEraFun {fromEraFun :: EraFunI f g}

-- | Apply an 'EraFun' to an 'EraValue'.
-- Because EraValue is a value in a specific era, the application will choose
-- the correct function from the vector.
-- In case of repeated application use this function curried on the 'EraFun'
-- argument, this will avoid the recomputation of the core
applyEraFun :: EraFun f g -> EraValue f -> EraValue g
applyEraFun (MkEraFun f) (EraValue v) = EraValue $ ap_NS f v

instance Category EraFun where
    id = MkEraFun $ pure_NP $ Fn id
    f . g =
        MkEraFun
            $ zipWith_NP
                (\(Fn f') (Fn g') -> Fn $ f' . g')
                (fromEraFun f)
                (fromEraFun g)

infixr 9 *.**

-- | Compose 2 EraFunI as a category, jumping the outer functorial layer in the
-- output of the first one.
(*.**) :: Functor w => EraFun g h -> EraFun f (w :.: g) -> EraFun f (w :.: h)
f *.** g =
    MkEraFun
        $ composeEraFunWith
            (\f' g' -> Comp . fmap f' . unComp . g')
            (fromEraFun f)
            (fromEraFun g)

-- | Compose 2 EraFunI as a category, keeping the outer layer in the
-- output of the first one.
composeEraFunWith
    :: (forall a. (g a -> h a) -> (f a -> w g a) -> f a -> w h a)
    -> EraFunI g h
    -> EraFunI f (w g)
    -> EraFunI f (w h)
composeEraFunWith q = zipWith_NP (\(Fn f') (Fn g') -> Fn $ q f' g')

infixr 9 *&&&*

-- | Compose 2 EraFunI as parallel application using '(:*:)'.
(*&&&*) :: EraFun f g -> EraFun f h -> EraFun f (g :*: h)
f *&&&* g = MkEraFun $ zipWith_NP r (fromEraFun f) (fromEraFun g)
  where
    r (Fn f') (Fn g') = Fn $ \x -> f' x :*: g' x

newtype EraFunK src ft = EraFunK {fromEraFunK :: EraFun src (K ft)}

instance Functor (EraFunK src) where
    fmap :: forall a b. (a -> b) -> EraFunK src a -> EraFunK src b
    fmap f (EraFunK g) =
        EraFunK (MkEraFun $ map_NP q $ fromEraFun g)
      where
        q :: (-.->) src (K a) era -> (-.->) src (K b) era
        q (Fn h) = Fn $ \x -> K . f $ unK $ h x

instance Applicative (EraFunK src) where
    pure x = EraFunK $ MkEraFun $ pure_NP $ Fn $ \_ -> K x
    EraFunK f <*> EraFunK g =
        EraFunK $ MkEraFun $ zipWith_NP q (fromEraFun f) (fromEraFun g)
      where
        q (Fn h) (Fn j) = Fn $ \src -> K $ unK (h src) $ unK $ j src

type AllEraValue f = EraFun (K ()) f

-- | Collapse an 'AllEraValue' into a list of 'EraValue'.
runAllEraValue :: AllEraValue f -> [EraValue f]
runAllEraValue v = collapse_NP $ zipWith_NP q prisms (fromEraFun v)
  where
    q :: MkEraValue f era -> (K () -.-> f) era -> K (EraValue f) era
    q p (Fn f) = K $ inject p $ f (K ())
