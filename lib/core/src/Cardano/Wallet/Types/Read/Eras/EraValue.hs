{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Types.Read.Eras.EraValue
  ( -- * era bounded values
  EraValue (..)
  , eraValueSerialize
  , extractEraValue
  -- * era specific prisms
  , MkEraValue (..)
  , byron
  , shelley
  , allegra
  , mary
  , alonzo
  , babbage
  , inject
  , eject
  -- * sum type encoding
  , EraValueS (..)
  , eraValueS
  , fromEraValueS
  , toEraValueS
  -- * specials
  , sequenceEraValue
  )
  where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Wallet.Types.Read.Eras.KnownEras
    ( KnownEras )
import Control.DeepSeq
    ( NFData )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Generics.Internal.VL
    ( Iso', Prism', build, iso, match, prism )
import Generics.SOP
    ( (:.:)
    , All
    , Compose
    , Generic (from, to)
    , I (..)
    , K (..)
    , NP (..)
    , NS
    , Proxy (Proxy)
    , SOP (SOP)
    , ejections
    , hd
    , injections
    , unComp
    , unI
    , unK
    , unSOP
    )
import Generics.SOP.Classes
import Generics.SOP.NP
    ( zipWith_NP )
import Generics.SOP.NS
    ( collapse_NS, index_NS, sequence'_NS, trans_NS )
import Generics.SOP.TH
    ( deriveGeneric )

-- | A value which is in one era
newtype EraValue f = EraValue (NS f KnownEras)

deriving instance (All (Compose Show f) KnownEras) => Show (EraValue f)
deriving instance (All (Compose Eq f) KnownEras) => Eq (EraValue f)
deriving instance (All (Compose Ord f) KnownEras) => Ord (EraValue f)
deriving instance (All (Compose NFData f) KnownEras) => NFData (EraValue f)

-- extract an era indipendent value
extractEraValue :: EraValue (K a) -> a
extractEraValue (EraValue v) = collapse_NS v

-- support for serializing
indexEraValue :: EraValue f -> Int
indexEraValue (EraValue v) = index_NS v

-- | sequence one functor level out
sequenceEraValue :: Applicative f => EraValue (f :.: g) -> f (EraValue g)
sequenceEraValue (EraValue v) = EraValue <$> sequence'_NS v

-- | A sum of values indexed by all known eras
data EraValueS f
  = ByronValue (f ByronEra)
    | ShelleyValue (f ShelleyEra)
    | AllegraValue (f AllegraEra)
    | MaryValue (f MaryEra)
    | AlonzoValue (f AlonzoEra)
    | BabbageValue (f BabbageEra)

deriveGeneric ''EraValueS
class CS f x y where
  unCS :: NP I x -> f y
instance CS f  ('[f era]) era where
  unCS x  = unI $ hd x

fromEraValueS :: forall f . EraValueS f ->  EraValue f
fromEraValueS = EraValue . trans_NS (Proxy @(CS f)) unCS . unSOP . from

class DS f x y where
  unDS :: f x -> NP I y
instance DS f era ('[f era]) where
  unDS x  = I x :* Nil

toEraValueS :: forall f . EraValue f ->  EraValueS f
toEraValueS (EraValue v) = to . SOP . trans_NS (Proxy @(DS f)) unDS $ v

-- | The isomorphism between EraValueS representation and EraValue, useful to
-- extract by pattern matching
eraValueS :: Iso' (EraValue f) (EraValueS f)
eraValueS = iso toEraValueS fromEraValueS

--- era dependent api

-- | A prism for one era that can project `f era` into `EraValue f`
-- it's a prism because extracting the `f era` is potentially impossible
-- as the value could not be in the requested era
newtype MkEraValue f era = MkEraValue (Prism' (EraValue f) (f era))

-- | byron era prism
byron :: MkEraValue f ByronEra

-- | byron era prism
shelley :: MkEraValue f ShelleyEra

-- | shelley era prism
allegra :: MkEraValue f AllegraEra

-- | mary era prism
mary :: MkEraValue f MaryEra

-- | alonzo era prism
alonzo :: MkEraValue f AlonzoEra

-- | babbage era prism
babbage :: MkEraValue f BabbageEra

byron :* shelley :* allegra :* mary :* alonzo :* babbage :* Nil
  = zipWith_NP g injections ejections
      where
        g i e = MkEraValue $ prism (inject' i) (eject' e)
        inject' f =  EraValue . unK . apFn f
        eject' e vb@(EraValue v) = case unComp $ apFn e (K v) of
          Nothing -> Left vb
          Just r -> Right r

inject :: MkEraValue f era -> f era -> EraValue f
inject (MkEraValue p) = build p

eject :: MkEraValue f era -> EraValue f -> Maybe (f era)
eject (MkEraValue p) = eitherToMaybe . match p

-- serailization

parseEraValue
  :: forall a n
  . (Eq n, Num n)
  => (a, n)
  -> Either (a, n) (EraValue (K a))
parseEraValue (x, era) = case era  of
    0 -> r byron
    1 -> r shelley
    2 -> r allegra
    3 -> r mary
    4 -> r alonzo
    5 -> r babbage
    _ -> Left (x, era)
    where
      r :: MkEraValue (K a) era  -> Either (a, n) (EraValue (K a))
      r e = Right $ inject e (K x)

renderEraValue :: EraValue (K b) -> (b, Int)
renderEraValue e = (extractEraValue e, indexEraValue e)

-- | The prism to serialize era independent EraValues into the value ant the
-- era expressed as Int, starting from 0, see 'KnownEras'

eraValueSerialize :: Prism' (a, Int) (EraValue (K a))
eraValueSerialize = prism renderEraValue parseEraValue
