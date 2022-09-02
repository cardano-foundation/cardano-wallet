module Cardano.Wallet.Types.Read.Eras.EraFunK (EraFunK (..)) where

import Prelude

import Cardano.Wallet.Types.Read.Eras.EraFun
    ( EraFun (..) )
import Generics.SOP
import Generics.SOP.NP
    ( map_NP, pure_NP, zipWith_NP )


newtype EraFunK src ft = EraFunK (EraFun src (K ft))

instance Functor (EraFunK src) where
    fmap f (EraFunK (EraFun g))
        = EraFunK (EraFun $ map_NP q g )
        where
        q (Fn h)= Fn $ \x -> K . f $  unK $  h x

instance Applicative (EraFunK src) where
    pure x = EraFunK $ EraFun $ pure_NP $ Fn $ \_ -> K x
    EraFunK (EraFun f) <*> EraFunK (EraFun g) =
        EraFunK $ EraFun $ zipWith_NP q f g
        where
        q (Fn h) (Fn j) = Fn $ \src -> K $ unK (h src) $ unK $ j src
