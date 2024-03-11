{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- A datatype that represents a vector of functions covering all known eras
-- The functions are supposed to map values from and to the same era.
--
-- We removed the cached encoding at the price of 'MkEraFun' and 'fromEraFun'
-- during all compositions, we are not 100% it's not relevant for performance
-- If the computed functions after record compositions are the same then we can
-- avoid that layer
--
-- Note:
-- composition is anyway expansive, do not recompose,
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
    , mkEraFun
    , runEraFun
    , EraFunSel

      -- * Composition.
    , (*.**)
    , (*&&&*)

      -- * Application.
    , applyEraFun

      -- * higher order record encoding
    , runAllEraValue
    , AllEraValue
        ( AllEraValue
        , AllEraValueP
        , byronVal
        , shelleyVal
        , allegraVal
        , maryVal
        , alonzoVal
        , babbageVal
        , conwayVal
        )
    , runEraFunK
    , mkEraFunK
    )
where

import Cardano.Wallet.Read.Eras.EraValue
    ( EraValue (..)
    , MkEraValue (..)
    , inject
    , prisms
    )
import Cardano.Wallet.Read.Eras.KnownEras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , KnownEras
    , Mary
    , Shelley
    )
import Control.Category
    ( Category (..)
    )
import Generics.SOP
    ( K (..)
    , NP
    , unComp
    , (:.:) (..), unK
    )
import Generics.SOP.Classes
import Generics.SOP.NP
    ( NP (..)
    , collapse_NP
    , zipWith_NP
    )
import Generics.SOP.NS
    ( ap_NS
    )
import GHC.Generics
    ( (:*:) (..)
    )
import Prelude hiding
    ( id
    , (.)
    )

-- | A function that selects a field from any 'EraFun'.
type EraFunSel era = forall f g. EraFun f g -> f era -> g era

-- | A function where domain and codomain are indexed by an era type.
type Fun f g era = f era -> g era

-- | A product of functions indexed by KnownEras.
type EraFunI f g = NP (f -.-> g) KnownEras

-- | A product of functions indexed by KnownEras expressed as record
pattern EraFun
    :: Fun f g Byron
    -- ^ Byron era function
    -> Fun f g Shelley
    -- ^ Shelley era function
    -> Fun f g Allegra
    -- ^ Allegra era function
    -> Fun f g Mary
    -- ^ Mary era function
    -> Fun f g Alonzo
    -- ^ Alonzo era function
    -> Fun f g Babbage
    -- ^ Babbage era function
    -> Fun f g Conway
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
    } <-
        (fromEraFun ->
            ( Fn byronFun
                :* Fn shelleyFun
                :* Fn allegraFun
                :* Fn maryFun
                :* Fn alonzoFun
                :* Fn babbageFun
                :* Fn conwayFun
                :* Nil
            )
        )
  where
    EraFun f1 f2 f3 f4 f5 f6 f7
        = fromEraFunI
            ( Fn f1
                :* Fn f2
                :* Fn f3
                :* Fn f4
                :* Fn f5
                :* Fn f6
                :* Fn f7
                :* Nil
            )

fromEraFun :: EraFun f g -> EraFunI f g
fromEraFun (EraFunCon f) =
    Fn f
        :* Fn f
        :* Fn f
        :* Fn f
        :* Fn f
        :* Fn f
        :* Fn f
        :* Nil

fromEraFunI :: forall f g. EraFunI f g -> EraFun f g
fromEraFunI
    (Fn f1
        :* Fn f2
        :* Fn f3
        :* Fn f4
        :* Fn f5
        :* Fn f6
        :* Fn f7
        :* Nil
    )
  = EraFunCon match
  where
    match :: forall era. IsEra era => f era -> g era
    match = case theEra :: Era era of
        Byron -> f1
        Shelley -> f2
        Allegra -> f3
        Mary -> f4
        Alonzo -> f5
        Babbage -> f6
        Conway -> f7

-- | Function that maps an era-indexed type.
newtype EraFun f g = EraFunCon
    {runEraFun :: forall era. IsEra era => f era -> g era}

-- | Smart constructor.
mkEraFun :: (forall era. IsEra era => f era -> g era) -> EraFun f g
mkEraFun = EraFunCon

-- | Apply an 'EraFun' to an 'EraValue'.
-- Because EraValue is a value in a specific era, the application will choose
-- the correct function from the vector.
-- In case of repeated application use this function curried on the 'EraFun'
-- argument, this will avoid the recomputation of the core
applyEraFun :: EraFun f g -> EraValue f -> EraValue g
applyEraFun (fromEraFun -> f) (EraValue v) = EraValue $ ap_NS f v

instance Category EraFun where
    id = EraFunCon id
    (EraFunCon f) . (EraFunCon g) = EraFunCon (f . g)

infixr 9 *.**

-- | Compose 2 EraFunI as a category, jumping the outer functorial layer in the
-- output of the first one.
(*.**) :: Functor w => EraFun g h -> EraFun f (w :.: g) -> EraFun f (w :.: h)
(EraFunCon a) *.** (EraFunCon b) =
    EraFunCon (Comp . fmap a . unComp . b)

infixr 8 *&&&*

-- | Compose 2 'EraFun' as parallel application using '(:*:)'.
(*&&&*) :: EraFun f g -> EraFun f h -> EraFun f (g :*: h)
(EraFunCon f) *&&&* (EraFunCon g) = EraFunCon (\x -> f x :*: g x)

mkEraFunK :: (forall era. IsEra era => f era -> g ) -> EraFun f (K g)
mkEraFunK f = EraFunCon (K . f)

runEraFunK :: IsEra era => EraFun f (K c) -> f era -> c
runEraFunK f = unK . runEraFun f

-- | A constant era 'EraFun' wrapped to expose the semigroup instance
newtype AllEraValue f = AllEraValue {_unAllEraValue :: EraFunI (K ()) f}

-- | A pattern to construct/deconstruct an 'AllEraValue'
pattern AllEraValueP
    :: f Byron
    -> f Shelley
    -> f Allegra
    -> f Mary
    -> f Alonzo
    -> f Babbage
    -> f Conway
    -> AllEraValue f
pattern AllEraValueP
    { byronVal
    , shelleyVal
    , allegraVal
    , maryVal
    , alonzoVal
    , babbageVal
    , conwayVal
    } <-
    AllEraValue
        ( Fn (mkConst -> byronVal)
                :* Fn (mkConst -> shelleyVal)
                :* Fn (mkConst -> allegraVal)
                :* Fn (mkConst -> maryVal)
                :* Fn (mkConst -> alonzoVal)
                :* Fn (mkConst -> babbageVal)
                :* Fn (mkConst -> conwayVal)
                :* Nil
            )
    where
        AllEraValueP
            byronVal'
            shelleyVal'
            allegraVal'
            maryVal'
            alonzoVal'
            babbageVal'
            conwayVal' =
                AllEraValue
                    ( Fn (const byronVal')
                        :* Fn (const shelleyVal')
                        :* Fn (const allegraVal')
                        :* Fn (const maryVal')
                        :* Fn (const alonzoVal')
                        :* Fn (const babbageVal')
                        :* Fn (const conwayVal')
                        :* Nil
                    )

mkConst :: (K () x -> f x) -> f x
mkConst = ($ K ())

-- | Collapse an 'AllEraValue' into a list of 'EraValue'.
runAllEraValue :: AllEraValue f -> [EraValue f]
runAllEraValue (AllEraValue v) = collapse_NP $ zipWith_NP q prisms v
  where
    q :: MkEraValue f era -> (K () -.-> f) era -> K (EraValue f) era
    q p (Fn f) = K $ inject p $ f (K ())
