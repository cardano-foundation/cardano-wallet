{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
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
--

module Cardano.Wallet.Read.Eras.EraFun
  ( -- * Types.
  EraFun (..)
  -- * Composition.
  , (*.**)
  , (*&&&*)
  -- * Application.
  , applyEraFun
  -- * Constant era 'EraFun'
  , EraFunK (..)
  )
  where

import Prelude hiding
    ( id, (.) )

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, ConwayEra, MaryEra, ShelleyEra )
import Cardano.Wallet.Read.Eras.EraValue
    ( EraValue (..) )
import Cardano.Wallet.Read.Eras.KnownEras
    ( KnownEras )
import Control.Category
    ( Category (..) )
import Generics.SOP
    ( (:.:) (..)
    , I (..)
    , K (..)
    , NP
    , Proxy (Proxy)
    , productTypeFrom
    , productTypeTo
    , unComp
    , unK
    )
import Generics.SOP.Classes
import Generics.SOP.NP
    ( map_NP, pure_NP, trans_NP, zipWith_NP )
import Generics.SOP.NS
    ( ap_NS )
import Generics.SOP.TH
    ( deriveGeneric )
import GHC.Generics
    ( (:*:) (..) )

-- | A record of functions indexed by all known eras. This is the natural way
-- of defining the vector.
data EraFun f g  = EraFun
  { byronFun :: f ByronEra -> g ByronEra
  , shelleyFun :: f ShelleyEra -> g ShelleyEra
  , allegraFun :: f AllegraEra -> g AllegraEra
  , maryFun :: f MaryEra -> g MaryEra
  , alonzoFun :: f AlonzoEra -> g AlonzoEra
  , babbageFun :: f BabbageEra -> g BabbageEra
  , conwayFun :: f ConwayEra -> g ConwayEra
  }

deriveGeneric ''EraFun
-- | A product of functions indexed by KnownEras.
type EraFunI f g = NP (f -.-> g) KnownEras

-- | Apply an 'EraFun' to an 'EraValue'.
-- Because EraValue is a value in a specific era, the application will choose
-- the correct function from the vector.
-- In case of repeated application use this function curried on the 'EraFun'
-- argument, this will avoid the recomputation of the core
applyEraFun :: EraFun f g -> EraValue f -> EraValue g
applyEraFun f = let
  g = fromEraFun f  -- curry friendly
  in \(EraValue v) -> EraValue $ ap_NS g v

class CR f g x y where
  unC :: I x -> (f -.-> g)  y
instance CR f g (f era -> g era) era where
  unC (I f) = Fn f

-- Promote an 'EraFun'.
fromEraFun :: forall f g . EraFun f g ->  EraFunI f g
fromEraFun = trans_NP (Proxy @(CR f g)) unC  . productTypeFrom

class DR f g x y where
  unD :: (f -.-> g) x -> I y
instance DR f g era (f era -> g era)  where
  unD (Fn f) = I f

-- Project out to an 'EraFun'.
toEraFun :: forall f g. EraFunI f g -> EraFun f g
toEraFun = productTypeTo . trans_NP (Proxy @(DR f g)) unD

instance Category EraFun where
  id = toEraFun $ pure_NP $ Fn id
  f . g = toEraFun
      $ zipWith_NP (\(Fn f') (Fn g') -> Fn $ f' . g')
        (fromEraFun f) (fromEraFun g)

infixr 9 *.**

-- | Compose 2 EraFunI as a category, jumping the outer functorial layer in the
-- output of the first one.
(*.**) :: Functor w => EraFun g h -> EraFun f (w :.: g) -> EraFun f (w :.: h)
f *.** g
  = toEraFun
  $ composeEraFunWith
      (\f' g' -> Comp . fmap f' . unComp . g')
      (fromEraFun f)
      (fromEraFun g)

-- | Compose 2 EraFunI as a category, keeping the outer layer in the
-- output of the first one.
composeEraFunWith
  :: (forall a . (g a -> h a) -> (f a -> w g a) -> f a -> w h a)
  -> EraFunI g h
  -> EraFunI f (w g)
  -> EraFunI f (w h)
composeEraFunWith q = zipWith_NP (\(Fn f') (Fn g') -> Fn $ q f' g')

infixr 9 *&&&*

-- | Compose 2 EraFunI as parallel application using '(:*:)'.
(*&&&*) :: EraFun f g -> EraFun f h -> EraFun f (g :*: h)
f *&&&* g = toEraFun $ zipWith_NP r (fromEraFun f) (fromEraFun g)
  where
    r (Fn f') (Fn g') = Fn $ \x -> f' x :*: g' x

newtype EraFunK src ft = EraFunK { fromEraFunK :: EraFun src (K ft) }

instance Functor (EraFunK src) where
    fmap :: forall a b . (a -> b) -> EraFunK src a -> EraFunK src b
    fmap f (EraFunK g)
        = EraFunK (toEraFun $ map_NP q $ fromEraFun g )
        where
        q :: (-.->) src (K a) era -> (-.->) src (K b) era
        q (Fn h) = Fn $ \x -> K . f $  unK $  h x

instance Applicative (EraFunK src) where
    pure x = EraFunK $ toEraFun $ pure_NP $ Fn $ \_ -> K x
    EraFunK f <*> EraFunK g =
        EraFunK $ toEraFun $ zipWith_NP q (fromEraFun f) (fromEraFun g)
        where
        q (Fn h) (Fn j) = Fn $ \src -> K $ unK (h src) $ unK $ j src
