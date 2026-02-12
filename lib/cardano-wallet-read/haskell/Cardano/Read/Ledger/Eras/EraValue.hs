{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- A datatype that represents values that can be different in any known eras.
module Cardano.Read.Ledger.Eras.EraValue
    ( -- * Era-existential values
      EraValue (..)
    , getEra
    , knownEras

      -- * Transformations
    , applyEraFun
    , applyEraFunValue
    , extractEraValue
    , sequenceEraValue

      -- * Serialization
    , parseEraIndex
    , eraValueSerialize
    )
where

import Cardano.Read.Ledger.Eras.KnownEras
    ( Era (..)
    , IsEra (..)
    , KnownEras
    , indexOfEra
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Control.Lens
    ( Prism'
    , prism
    )
import Data.Kind
    ( Type
    )
import Generics.SOP
    ( All
    , Compose
    , K (..)
    , (:.:) (..)
    )
import Prelude

-- |
-- A value which is in one particular 'Era'.
--
-- This existential type wraps an era-indexed value @f era@ while hiding
-- the specific era. Pattern matching on 'EraValue' brings the 'IsEra'
-- constraint into scope, allowing era-specific operations.
data EraValue f = forall (era :: Type). IsEra era => EraValue (f era)

{-# INLINEABLE getEra #-}

-- | Get the 'Era' that the value is in.
getEra :: EraValue f -> EraValue Era
getEra (EraValue (_ :: f era)) = EraValue (theEra :: Era era)

-- | Value-level list of known 'Era'.
knownEras :: [EraValue Era]
knownEras =
    [ EraValue Byron
    , EraValue Shelley
    , EraValue Allegra
    , EraValue Mary
    , EraValue Alonzo
    , EraValue Babbage
    , EraValue Conway
    ]

instance (All (Compose Show f) KnownEras) => Show (EraValue f) where
    show (EraValue (x :: f era)) = case theEra @era of
        Byron -> "InByron " ++ show x
        Shelley -> "InShelley " ++ show x
        Allegra -> "InAllegra " ++ show x
        Mary -> "InMary " ++ show x
        Alonzo -> "InAlonzo " ++ show x
        Babbage -> "InBabbage " ++ show x
        Conway -> "InConway " ++ show x

instance (All (Compose Eq f) KnownEras) => Eq (EraValue f) where
    {-# INLINEABLE (==) #-}
    (==) (EraValue (v :: f erax)) (EraValue (w :: f eray)) =
        case (theEra @erax, theEra @eray) of
            (Byron, Byron) -> v == w
            (Shelley, Shelley) -> v == w
            (Allegra, Allegra) -> v == w
            (Mary, Mary) -> v == w
            (Alonzo, Alonzo) -> v == w
            (Babbage, Babbage) -> v == w
            (Conway, Conway) -> v == w
            (_, _) -> False

instance (All (Compose Ord f) KnownEras) => Ord (EraValue f) where
    {-# INLINEABLE compare #-}
    compare ex@(EraValue (x :: f erax)) ey@(EraValue (y :: f eray)) =
        case (theEra @erax, theEra @eray) of
            (Byron, Byron) -> compare x y
            (Shelley, Shelley) -> compare x y
            (Allegra, Allegra) -> compare x y
            (Mary, Mary) -> compare x y
            (Alonzo, Alonzo) -> compare x y
            (Babbage, Babbage) -> compare x y
            (Conway, Conway) -> compare x y
            (_, _) -> compare (indexEraValue ex) (indexEraValue ey)

instance (All (Compose NFData f) KnownEras) => NFData (EraValue f) where
    {-# INLINEABLE rnf #-}
    rnf (EraValue (x :: f era)) = case theEra @era of
        Byron -> rnf x
        Shelley -> rnf x
        Allegra -> rnf x
        Mary -> rnf x
        Alonzo -> rnf x
        Babbage -> rnf x
        Conway -> rnf x

-- |
-- Apply an era-polymorphic function to an 'EraValue', producing
-- an era-independent result.
applyEraFun
    :: (forall era. IsEra era => f era -> g) -> EraValue f -> g
applyEraFun f (EraValue x) = f x

-- |
-- Apply an era-polymorphic function to an 'EraValue', producing
-- another 'EraValue' with a different type constructor.
applyEraFunValue
    :: (forall era. IsEra era => f era -> g era)
    -> EraValue f
    -> EraValue g
applyEraFunValue f (EraValue x) = EraValue (f x)

-- | Extract an era-independent value from a constant 'EraValue'.
extractEraValue :: EraValue (K a) -> a
extractEraValue (EraValue (K x)) = x

{-# INLINEABLE parseEraIndex #-}

-- |
-- Parse an era index (0-6) into an 'EraValue' containing the 'Era' singleton.
-- Returns 'Nothing' for invalid indices.
parseEraIndex :: Int -> Maybe (EraValue Era)
parseEraIndex ix = case ix of
    0 -> Just $ EraValue Byron
    1 -> Just $ EraValue Shelley
    2 -> Just $ EraValue Allegra
    3 -> Just $ EraValue Mary
    4 -> Just $ EraValue Alonzo
    5 -> Just $ EraValue Babbage
    6 -> Just $ EraValue Conway
    _ -> Nothing

-- | Sequence one applicative functor level out.
sequenceEraValue
    :: Applicative f => EraValue (f :.: g) -> f (EraValue g)
sequenceEraValue (EraValue (Comp fg)) = EraValue <$> fg

{-----------------------------------------------------------------------------
    Serialization
------------------------------------------------------------------------------}

parseEraValue
    :: forall a. (a, Int) -> Either (a, Int) (EraValue (K a))
parseEraValue (x, ix) =
    case parseEraIndex ix of
        Just (EraValue (_ :: Era era)) -> Right $ EraValue (K x :: K a era)
        Nothing -> Left (x, ix)

{-# INLINEABLE indexEraValue #-}
indexEraValue :: EraValue f -> Int
indexEraValue = applyEraFun indexOfEra . getEra

renderEraValue :: EraValue (K a) -> (a, Int)
renderEraValue e = (extractEraValue e, indexEraValue e)

-- | The prism to serialize EraValues into the value at the
-- era expressed as Int, starting from 0, see 'KnownEras'.
eraValueSerialize :: Prism' (a, Int) (EraValue (K a))
eraValueSerialize = prism renderEraValue parseEraValue
