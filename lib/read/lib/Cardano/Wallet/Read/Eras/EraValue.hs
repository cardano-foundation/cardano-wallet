{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- A datatype that represents values that can be different in any known eras.
module Cardano.Wallet.Read.Eras.EraValue
    ( -- * Era bounded values
      EraValue (..)
    , eraValueSerialize
    , extractEraValue
    , sequenceEraValue
    , eraValue
    )
where

import Prelude

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
import Control.DeepSeq
    ( NFData (..)
    )
import Control.Lens
    ( Prism'
    , prism
    )
import Generics.SOP
    ( All
    , Compose
    , K (..)
    , (:.:) (..)
    )

-- | A value which is in one era.
data EraValue f = forall era. IsEra era => EraValue (f era)

eraValue :: IsEra era => f era -> EraValue f
eraValue = EraValue

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
    {-# INLINABLE (==) #-}
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
    {-# INLINABLE compare #-}
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
    {-# INLINABLE rnf #-}
    rnf (EraValue (x :: f era)) = case theEra @era of
        Byron -> rnf x
        Shelley -> rnf x
        Allegra -> rnf x
        Mary -> rnf x
        Alonzo -> rnf x
        Babbage -> rnf x
        Conway -> rnf x

-- | Extract an era indipendent value.
extractEraValue :: EraValue (K a) -> a
extractEraValue (EraValue (K x)) = x

{-# INLINABLE indexEraValue #-}
indexEraValue :: EraValue f -> Int
indexEraValue (EraValue (_ :: f era)) = case theEra @era of
    Byron -> 0
    Shelley -> 1
    Allegra -> 2
    Mary -> 3
    Alonzo -> 4
    Babbage -> 5
    Conway -> 6

-- | Sequence one applicative functor level out.
sequenceEraValue :: Applicative f => EraValue (f :.: g) -> f (EraValue g)
sequenceEraValue (EraValue (Comp fg)) = EraValue <$> fg

{-----------------------------------------------------------------------------
    Serialization
------------------------------------------------------------------------------}

parseEraValue
    :: forall a n
     . (Eq n, Num n)
    => (a, n)
    -> Either (a, n) (EraValue (K a))
parseEraValue (x, era) = case era of
    0 -> Right $ EraValue (K x :: K a Byron)
    1 -> Right $ EraValue (K x :: K a Shelley)
    2 -> Right $ EraValue (K x :: K a Allegra)
    3 -> Right $ EraValue (K x :: K a Mary)
    4 -> Right $ EraValue (K x :: K a Alonzo)
    5 -> Right $ EraValue (K x :: K a Babbage)
    6 -> Right $ EraValue (K x :: K a Conway)
    _ -> Left (x, era)

renderEraValue :: EraValue (K a) -> (a, Int)
renderEraValue e = (extractEraValue e, indexEraValue e)

-- | The prism to serialize EraValues into the value at the
-- era expressed as Int, starting from 0, see 'KnownEras'.
eraValueSerialize :: Prism' (a, Int) (EraValue (K a))
eraValueSerialize = prism renderEraValue parseEraValue
