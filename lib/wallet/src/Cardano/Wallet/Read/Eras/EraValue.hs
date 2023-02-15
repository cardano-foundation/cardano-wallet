{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- A datatype that represents values that can be different in any known eras.
--

module Cardano.Wallet.Read.Eras.EraValue
  ( -- * Era bounded values
    EraValue (..)
  , eraValueSerialize
  , extractEraValue

  -- * Era specific prisms
  , MkEraValue (..)
  , byron
  , conway
  , shelley
  , allegra
  , mary
  , alonzo
  , babbage
  , inject
  , project

  -- * Specials
  , sequenceEraValue
  , witnessEra
  , hoistEraValue

  -- * Internals
  , cardanoEras
  )
  where

import Prelude

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , AnyCardanoEra (..)
    , BabbageEra
    , ByronEra
    , CardanoEra (..)
    , ConwayEra
    , IsCardanoEra
    , MaryEra
    , ShelleyEra
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( KnownEras )
import Control.DeepSeq
    ( NFData )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Generics.Internal.VL
    ( Prism', build, match, prism )
import Generics.SOP
    ( (:.:)
    , All
    , Compose
    , K (..)
    , NP (..)
    , NS
    , Proxy (..)
    , ejections
    , injections
    , unComp
    , unK
    )
import Generics.SOP.Classes
import Generics.SOP.NP
    ( cmap_NP, pure_NP, zipWith_NP )
import Generics.SOP.NS
    ( ap_NS, collapse_NS, index_NS, sequence'_NS )

import qualified GHC.Generics as GHC

-- | A value which is in one era.
newtype EraValue f = EraValue (NS f KnownEras)
    deriving GHC.Generic

deriving instance (All (Compose Show f) KnownEras) => Show (EraValue f)
deriving instance (All (Compose Eq f) KnownEras) => Eq (EraValue f)
deriving instance (All (Compose Ord f) KnownEras) => Ord (EraValue f)
deriving instance (All (Compose NFData f) KnownEras) => NFData (EraValue f)

-- | Internal product of 'CardanoEra'
cardanoEras :: NP CardanoEra KnownEras
cardanoEras =
    ByronEra
        :* ShelleyEra
        :* AllegraEra
        :* MaryEra
        :* AlonzoEra
        :* BabbageEra
        :* ConwayEra
        :* Nil

-- | Add an era witness to an era independent EraValue.
witnessEra :: EraValue (K b) -> EraValue (K (AnyCardanoEra, b))
witnessEra (EraValue v) = EraValue $ ap_NS
    (cmap_NP (Proxy @IsCardanoEra)
        (Fn . (\x (K y) -> K (AnyCardanoEra x, y))) cardanoEras
    )
    v

-- | Extract an era indipendent value.
extractEraValue :: EraValue (K a) -> a
extractEraValue (EraValue v) = collapse_NS v

indexEraValue :: EraValue f -> Int
indexEraValue (EraValue v) = index_NS v

-- | Sequence one applicative functor level out.
sequenceEraValue :: Applicative f => EraValue (f :.: g) -> f (EraValue g)
sequenceEraValue (EraValue v) = EraValue <$> sequence'_NS v

-- | A prism for one era that can project `f era` into `EraValue f`
-- it's a prism because extracting the `f era` is potentially impossible
-- as the value could not be in the requested era.
newtype MkEraValue f era = MkEraValue (Prism' (EraValue f) (f era))

-- | Byron era prism.
byron :: MkEraValue f ByronEra

-- | Shelley era prism.
shelley :: MkEraValue f ShelleyEra

-- | Allegra era prism.
allegra :: MkEraValue f AllegraEra

-- | Mary era prism.
mary :: MkEraValue f MaryEra

-- | Alonzo era prism.
alonzo :: MkEraValue f AlonzoEra

-- | Babbage era prism.
babbage :: MkEraValue f BabbageEra

-- | Conway era prism.
conway :: MkEraValue f ConwayEra

byron :* shelley :* allegra :* mary :* alonzo :* babbage :* conway :* Nil
  = zipWith_NP g injections ejections
      where
        g i e = MkEraValue $ prism (inject' i) (project' e)
        inject' f =  EraValue . unK . apFn f
        project' e vb@(EraValue v) = case unComp $ apFn e (K v) of
          Nothing -> Left vb
          Just r -> Right r

-- | Inject a value into its era position.
inject :: MkEraValue f era -> f era -> EraValue f
inject (MkEraValue p) = build p

-- | Try to project a value from its era position.
project :: MkEraValue f era -> EraValue f -> Maybe (f era)
project (MkEraValue p) = eitherToMaybe . match p

{-----------------------------------------------------------------------------
    Serialization
------------------------------------------------------------------------------}
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
-- era expressed as Int, starting from 0, see 'KnownEras'.
eraValueSerialize :: Prism' (a, Int) (EraValue (K a))
eraValueSerialize = prism renderEraValue parseEraValue

-- | change unconditionally the functor
hoistEraValue :: (forall a. f a -> g a)  -> EraValue f -> EraValue g
hoistEraValue f (EraValue ns) = EraValue $ ap_NS (pure_NP $ Fn f) ns
