{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Module mainly for containing 'RecentEra' and related functionality.
module Cardano.Wallet.Write.Era
    (
    -- ** RecentEra
      RecentEra (..)
    , IsRecentEra (..)
    , toRecentEra
    , fromRecentEra

    , LatestLedgerEra
    , LatestEra

    , cardanoEra
    , shelleyBasedEra
    , ShelleyLedgerEra
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra

    -- ** InAnyRecentEra
    , InAnyRecentEra (..)
    , withInAnyRecentEra

    -- ** AnyRecentEra
    , AnyRecentEra (..)
    , asAnyRecentEra
    , toAnyCardanoEra
    , fromAnyCardanoEra
    , withRecentEra

    -- ** Misc
    , StandardCrypto
    , StandardBabbage
    , StandardConway
    )

where

import Prelude

import Cardano.Api
    ( BabbageEra, ConwayEra )
import Cardano.Api.Shelley
    ( ShelleyLedgerEra )
import Data.Type.Equality
    ( (:~:) (Refl), TestEquality (testEquality) )
import Data.Typeable
    ( Typeable )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardBabbage, StandardConway, StandardCrypto )

import qualified Cardano.Api as Cardano
import Data.Maybe
    ( fromMaybe, isJust )

--------------------------------------------------------------------------------
-- Eras
--------------------------------------------------------------------------------

type LatestEra = ConwayEra

type LatestLedgerEra = StandardConway

--------------------------------------------------------------------------------
-- RecentEra
--------------------------------------------------------------------------------

-- | 'RecentEra' respresents the eras we care about constructing transactions
-- for.
--
-- To have the same software constructing transactions just before and just
-- after a hard-fork, we need to, at that time, support the two latest eras. We
-- could get away with just supporting one era at other times, but for
-- simplicity we stick with always supporting the two latest eras for now.
--
-- NOTE: We /could/ let 'era' refer to eras from the ledger rather than from
-- cardano-api.
data RecentEra era where
    RecentEraBabbage :: RecentEra BabbageEra
    RecentEraConway :: RecentEra ConwayEra

deriving instance Eq (RecentEra era)
deriving instance Show (RecentEra era)

instance TestEquality RecentEra where
    testEquality RecentEraBabbage RecentEraBabbage = Just Refl
    testEquality RecentEraConway RecentEraConway = Just Refl
    testEquality RecentEraBabbage RecentEraConway = Nothing
    testEquality RecentEraConway RecentEraBabbage = Nothing

class
    ( Cardano.IsShelleyBasedEra era
    , Typeable era
    ) => IsRecentEra era where
    recentEra :: RecentEra era

-- | Return a proof that the wallet can create txs in this era, or @Nothing@.
toRecentEra :: Cardano.CardanoEra era -> Maybe (RecentEra era)
toRecentEra = \case
    Cardano.ConwayEra  -> Just RecentEraConway
    Cardano.BabbageEra -> Just RecentEraBabbage
    Cardano.AlonzoEra  -> Nothing
    Cardano.MaryEra    -> Nothing
    Cardano.AllegraEra -> Nothing
    Cardano.ShelleyEra -> Nothing
    Cardano.ByronEra   -> Nothing

fromRecentEra :: RecentEra era -> Cardano.CardanoEra era
fromRecentEra = \case
  RecentEraConway -> Cardano.ConwayEra
  RecentEraBabbage -> Cardano.BabbageEra

instance IsRecentEra BabbageEra where
    recentEra = RecentEraBabbage

instance IsRecentEra ConwayEra where
    recentEra = RecentEraConway

cardanoEraFromRecentEra :: RecentEra era -> Cardano.CardanoEra era
cardanoEraFromRecentEra =
    Cardano.shelleyBasedToCardanoEra
    . shelleyBasedEraFromRecentEra

shelleyBasedEraFromRecentEra :: RecentEra era -> Cardano.ShelleyBasedEra era
shelleyBasedEraFromRecentEra = \case
    RecentEraConway -> Cardano.ShelleyBasedEraConway
    RecentEraBabbage -> Cardano.ShelleyBasedEraBabbage

-- | For convenience working with 'IsRecentEra'. Similar to 'Cardano.cardanoEra,
-- but with a 'IsRecentEra era' constraint instead of 'Cardano.IsCardanoEra.
cardanoEra :: forall era. IsRecentEra era => Cardano.CardanoEra era
cardanoEra = cardanoEraFromRecentEra $ recentEra @era

-- | For convenience working with 'IsRecentEra'. Similar to
-- 'Cardano.shelleyBasedEra, but with a 'IsRecentEra era' constraint instead of
-- 'Cardano.IsShelleyBasedEra'.
shelleyBasedEra :: forall era. IsRecentEra era => Cardano.ShelleyBasedEra era
shelleyBasedEra = shelleyBasedEraFromRecentEra $ recentEra @era

--------------------------------------------------------------------------------
-- InAnyRecentEra
--------------------------------------------------------------------------------

data InAnyRecentEra thing where
     InAnyRecentEra
         :: IsRecentEra era -- Provide class constraint
         => RecentEra era   -- and explicit value.
         -> thing era
         -> InAnyRecentEra thing

withInAnyRecentEra
    :: InAnyRecentEra thing
    -> (forall era. IsRecentEra era => thing era -> a)
    -> a
withInAnyRecentEra (InAnyRecentEra _era tx) f = f tx

-- | "Downcast" something existentially wrapped in 'Cardano.InAnyCardanoEra'.
asAnyRecentEra
    :: Cardano.InAnyCardanoEra a
    -> Maybe (InAnyRecentEra a)
asAnyRecentEra = \case
    Cardano.InAnyCardanoEra Cardano.ByronEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.ShelleyEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.AllegraEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.MaryEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.AlonzoEra _ ->
        Nothing
    Cardano.InAnyCardanoEra Cardano.BabbageEra a ->
        Just $ InAnyRecentEra RecentEraBabbage a
    Cardano.InAnyCardanoEra Cardano.ConwayEra a ->
        Just $ InAnyRecentEra RecentEraConway a

--------------------------------------------------------------------------------
-- AnyRecentEra
--------------------------------------------------------------------------------

-- | An existential type like 'AnyCardanoEra', but for 'RecentEra'.
data AnyRecentEra where
     AnyRecentEra :: IsRecentEra era -- Provide class constraint
                  => RecentEra era   -- and explicit value.
                  -> AnyRecentEra    -- and that's it.

instance Enum AnyRecentEra where
    -- NOTE: We're not starting at 0! 0 would be Byron, which is not a recent
    -- era.
    fromEnum = fromEnum . toAnyCardanoEra
    toEnum n = fromMaybe err . fromAnyCardanoEra $ toEnum n
      where
        err = error $ unwords
            [ "AnyRecentEra.toEnum:", show n
            , "doesn't correspond to a recent era."
            ]
instance Bounded AnyRecentEra where
    minBound = AnyRecentEra RecentEraBabbage
    maxBound = AnyRecentEra RecentEraConway

instance Show AnyRecentEra where
    show (AnyRecentEra era) = "AnyRecentEra " <> show era

instance Eq AnyRecentEra where
    AnyRecentEra e1 == AnyRecentEra e2 =
        isJust $ testEquality e1 e2

toAnyCardanoEra :: AnyRecentEra -> Cardano.AnyCardanoEra
toAnyCardanoEra (AnyRecentEra era) = Cardano.AnyCardanoEra (fromRecentEra era)

fromAnyCardanoEra
    :: Cardano.AnyCardanoEra
    -> Maybe AnyRecentEra
fromAnyCardanoEra = \case
    Cardano.AnyCardanoEra Cardano.ByronEra -> Nothing
    Cardano.AnyCardanoEra Cardano.ShelleyEra -> Nothing
    Cardano.AnyCardanoEra Cardano.AllegraEra -> Nothing
    Cardano.AnyCardanoEra Cardano.MaryEra -> Nothing
    Cardano.AnyCardanoEra Cardano.AlonzoEra -> Nothing
    Cardano.AnyCardanoEra Cardano.BabbageEra
        -> Just $ AnyRecentEra RecentEraBabbage
    Cardano.AnyCardanoEra Cardano.ConwayEra
        -> Just $ AnyRecentEra RecentEraConway

withRecentEra ::
    AnyRecentEra -> (forall era. IsRecentEra era => RecentEra era -> a) -> a
withRecentEra (AnyRecentEra era) f = f era
