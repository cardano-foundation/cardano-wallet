{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Types.Read.Tx.Eras
    ( UseTxEra (..)
    , (:*-*) (..)
    , (*-*)
    , UseTxAllEra
    , useTx
    , useTxCached
    )
    where

import Prelude

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , CardanoEra (..)
    , InAnyCardanoEra (InAnyCardanoEra)
    , MaryEra
    , ShelleyEra
    )
import Cardano.Wallet.Types.Read.Tx
    ( FindNP (find), IsKnownEra, KnownEras, Tx (..), TxEra )
import Generics.SOP
    ( All, NP, Top )
import Generics.SOP.NP
    ( zipWith_NP )

--- | higher type tuple tupling, to serve zipping
data (:*-*) f g a = (:*-*) (f a) (g a)

infixr 9 :*-*

deriving instance (Show (f era), Show (g era)) => Show ((:*-*) f g era)

deriving instance (Eq (f era), Eq (g era)) => Eq ((:*-*) f g era)

-- | using `Tx era`, promoted to bw used as unsaturated type parameter
newtype UseTxEra f era = UseTxEra (TxEra era -> f era)

-- | zipping NS UseTxEra
(*-*)
    :: All Top xs
    => NP (UseTxEra f) xs
    -> NP (UseTxEra g) xs
    -> NP (UseTxEra (f :*-* g)) xs
x *-* y = zipWith_NP z x y
    where
    z :: UseTxEra f a -> UseTxEra g a -> UseTxEra (f :*-* g) a
    z (UseTxEra f) (UseTxEra g) = UseTxEra $ \tx -> f tx :*-* g tx

infixr 9 *-*

type UseTxAllEra f = NP (UseTxEra f) KnownEras

-- | A useTx version which select the era in linear time
useTx :: UseTxAllEra f -> Tx -> InAnyCardanoEra f
useTx f (Tx era tx) = InAnyCardanoEra era $ case find f of
    UseTxEra v -> v tx

-- internal ?
useTxEra :: forall era f . IsKnownEra era => UseTxAllEra f -> TxEra era -> f era
useTxEra f = case find f of UseTxEra v -> v

-- | A useTx version which select the era in constant time
-- from the second search
useTxCached :: UseTxAllEra f -> Tx -> InAnyCardanoEra f
useTxCached f = let
    byronUseTx = useTxEra @ByronEra f
    shelleyUseTx = useTxEra @ShelleyEra f
    allegraUseTx = useTxEra @AllegraEra f
    maryUseTx = useTxEra @MaryEra f
    alonzoUseTx = useTxEra @AlonzoEra f
    babbageUseTx = useTxEra @BabbageEra f
    in \(Tx era tx) -> case era of
        ByronEra -> InAnyCardanoEra ByronEra $ byronUseTx tx
        ShelleyEra -> InAnyCardanoEra ShelleyEra $ shelleyUseTx tx
        AllegraEra -> InAnyCardanoEra AllegraEra $ allegraUseTx tx
        MaryEra -> InAnyCardanoEra MaryEra $ maryUseTx tx
        AlonzoEra -> InAnyCardanoEra AlonzoEra $ alonzoUseTx tx
        BabbageEra -> InAnyCardanoEra BabbageEra $ babbageUseTx tx
