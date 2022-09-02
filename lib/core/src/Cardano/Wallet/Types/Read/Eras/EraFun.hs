{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Types.Read.Eras.EraFun
  (
  EraFun (..)
  , EraFunR(..)
  , fromEraFunR
  , toEraFunR
  , eraFunR
  -- * categorical composition
  , (*.*)
  , (*.**)
  -- * application
  , applyEraFun
  -- applicative composition
  , (*-*)
  ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Wallet.Types.Read.Eras.EraValue
    ( EraValue (..) )
import Cardano.Wallet.Types.Read.Eras.KnownEras
    ( KnownEras )
import Data.Generics.Internal.VL
    ( Iso', iso )
import Generics.SOP
    ( (:.:) (..)
    , I (..)
    
    , NP
    , Proxy (Proxy)
    , productTypeFrom
    , productTypeTo
    , unComp
    
    )
import Generics.SOP.Classes
import Generics.SOP.NP
    ( trans_NP, zipWith_NP )
import Generics.SOP.NS
    ( ap_NS )
import Generics.SOP.TH
    ( deriveGeneric )
import GHC.Generics (type (:*:) (..))

-- | A product of functions indexed by KnownEras
newtype EraFun f g = EraFun (NP (f -.-> g) KnownEras)

-- | apply an EraFun to an EraValue
applyEraFun :: EraFun f g -> EraValue f -> EraValue g
applyEraFun (EraFun f) (EraValue v) = EraValue $ ap_NS f v

-- | A record of functions indexed by all known eras
data EraFunR f g  = EraFunR
  { byronFun :: f ByronEra -> g ByronEra
  , shelleyFun :: f ShelleyEra -> g ShelleyEra
  , allegraFun :: f AllegraEra -> g AllegraEra
  , maryFun :: f MaryEra -> g MaryEra
  , alonzoFun :: f AlonzoEra -> g AlonzoEra
  , babbageFun :: f BabbageEra -> g BabbageEra
  }

deriveGeneric ''EraFunR

class CR f g x y where
  unC :: I x -> (f -.-> g)  y
instance CR f g (f era -> g era) era where
  unC (I f) = Fn f

fromEraFunR :: forall f g . EraFunR f g ->  EraFun f g
fromEraFunR = EraFun . trans_NP (Proxy @(CR f g)) unC  . productTypeFrom

class DR f g x y where
  unD :: (f -.-> g) x -> I y
instance DR f g era (f era -> g era)  where
  unD (Fn f) = I f

toEraFunR :: forall f g. EraFun f g -> EraFunR f g
toEraFunR (EraFun f) = productTypeTo . trans_NP (Proxy @(DR f g)) unD $ f

-- | The isomorphism between EraFun and EraFunR
eraFunR :: Iso' (EraFunR f g) (EraFun f g)
eraFunR = iso fromEraFunR toEraFunR

infixr 8 *.*

-- | compose 2 EraFun as a category
(*.*) :: EraFun g h -> EraFun f g -> EraFun f h
EraFun f *.* EraFun g = EraFun
  $ zipWith_NP (\(Fn f') (Fn g') -> Fn $ f' . g') f g

infixr 9 *.**

(*.**) :: Functor w => EraFun g h -> EraFun f (w :.: g) -> EraFun f (w :.: h)
(*.**) = composeEraFunWith $ \f' g' -> Comp . fmap f' . unComp . g'

composeEraFunWith
  :: (forall a . (g a -> h a) -> (f a -> w g a) -> f a -> w h a)
  -> EraFun g h
  -> EraFun f (w g)
  -> EraFun f (w h)
composeEraFunWith q (EraFun f) (EraFun g) = EraFun
  $ zipWith_NP (\(Fn f') (Fn g') -> Fn $ q f' g') f g

infixr 9 *-*

-- | compose 2 EraFun as applicative
(*-*) :: EraFun f g -> EraFun f h -> EraFun f (g :*: h)
EraFun f *-* EraFun g = EraFun $ zipWith_NP r f g
  where
    r (Fn f') (Fn g') = Fn $ \x -> f' x :*: g' x
