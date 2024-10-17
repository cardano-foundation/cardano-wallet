{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Recent eras.
module Internal.Cardano.Write.Eras
    (
    -- * Eras
      BabbageEra
    , ConwayEra

    -- ** RecentEra
    , RecentEra (..)
    , IsRecentEra (..)
    , CardanoApiEra
    , MaybeInRecentEra (..)
    , LatestLedgerEra
    , RecentEraConstraints
    , allRecentEras

    -- ** Existential wrapper
    , AnyRecentEra (..)

    -- ** Helpers for cardano-api compatibility
    , cardanoEra
    , shelleyBasedEra
    , CardanoApi.ShelleyLedgerEra
    , cardanoEraFromRecentEra
    , shelleyBasedEraFromRecentEra
    ) where

import Prelude

import Cardano.Ledger.Allegra.Scripts
    ( AllegraEraScript
    , Timelock
    )
import Cardano.Ledger.Alonzo.Plutus.Context
    ( EraPlutusContext
    )
import Cardano.Ledger.Alonzo.Scripts
    ( AlonzoEraScript
    , AlonzoScript (..)
    )
import Cardano.Ledger.Alonzo.TxWits
    ( AlonzoTxWits
    )
import Cardano.Ledger.Alonzo.UTxO
    ( AlonzoScriptsNeeded
    )
import Cardano.Ledger.Api.UTxO
    ( EraUTxO (ScriptsNeeded)
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Mary
    ( MaryValue
    )
import Data.Function
    ( on
    )
import Data.Generics.Labels
    ()
import Data.Kind
    ( Type
    )
import Data.Maybe
    ( fromMaybe
    , isJust
    )
import Data.Set
    ( Set
    )
import Data.Type.Equality
    ( TestEquality (testEquality)
    , (:~:) (Refl)
    )
import Data.Typeable
    ( Typeable
    )

import qualified Cardano.Api as CardanoApi
import qualified Cardano.Api.Shelley as CardanoApi
import qualified Cardano.Ledger.Alonzo.Core as Alonzo
import qualified Cardano.Ledger.Api as Ledger
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Shelley.UTxO as Shelley
import qualified Data.Set as Set

--------------------------------------------------------------------------------
-- Eras
--------------------------------------------------------------------------------

type BabbageEra = Ledger.BabbageEra StandardCrypto
type ConwayEra = Ledger.ConwayEra StandardCrypto

type LatestLedgerEra = ConwayEra

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
    ( CardanoApi.IsShelleyBasedEra (CardanoApiEra era)
    , CardanoApi.ShelleyLedgerEra (CardanoApiEra era) ~ era
    , Typeable era
    , RecentEraConstraints era
    ) => IsRecentEra era where
    recentEra :: RecentEra era

type family CardanoApiEra era = cardanoApiEra | cardanoApiEra -> era
type instance CardanoApiEra BabbageEra = CardanoApi.BabbageEra
type instance CardanoApiEra ConwayEra = CardanoApi.ConwayEra

-- | Convenient constraints. Constraints may be dropped as we move to new eras.
--
-- Adding too many constraints shouldn't be a concern as the point of
-- 'RecentEra' is to work with a small closed set of eras, anyway.
type RecentEraConstraints era =
    ( Core.Era era
    , Core.EraTx era
    , Core.EraCrypto era ~ StandardCrypto
    , Core.Script era ~ AlonzoScript era
    , Core.Tx era ~ Babbage.AlonzoTx era
    , Core.EraTxCert era
    , Core.Value era ~ MaryValue StandardCrypto
    , Core.TxWits era ~ AlonzoTxWits era
    , Alonzo.AlonzoEraPParams era
    , Ledger.AlonzoEraTx era
    , ScriptsNeeded era ~ AlonzoScriptsNeeded era
    , AlonzoEraScript era
    , Ledger.Crypto (Core.EraCrypto era)
    , Eq (Core.TxOut era)
    , Eq (Core.Tx era)
    , Babbage.BabbageEraTxBody era
    , Alonzo.AlonzoEraTxBody era
    , Shelley.EraUTxO era
    , Show (Core.TxOut era)
    , Show (Core.Tx era)
    , Show (Core.PParams era)
    , Show (AlonzoScript era)
    , EraPlutusContext era
    , AllegraEraScript era
    , Core.NativeScript era ~ Timelock era
    )

instance IsRecentEra BabbageEra where
    recentEra = RecentEraBabbage

instance IsRecentEra ConwayEra where
    recentEra = RecentEraConway

cardanoEraFromRecentEra
    :: RecentEra era
    -> CardanoApi.CardanoEra (CardanoApiEra era)
cardanoEraFromRecentEra = \case
    RecentEraConway -> CardanoApi.ConwayEra
    RecentEraBabbage -> CardanoApi.BabbageEra

shelleyBasedEraFromRecentEra
    :: RecentEra era
    -> CardanoApi.ShelleyBasedEra (CardanoApiEra era)
shelleyBasedEraFromRecentEra = \case
    RecentEraConway -> CardanoApi.ShelleyBasedEraConway
    RecentEraBabbage -> CardanoApi.ShelleyBasedEraBabbage

-- Similar to 'CardanoApi.cardanoEra', but with an 'IsRecentEra era' constraint
-- instead of 'CardanoApi.IsCardanoEra'.
cardanoEra
    :: forall era. IsRecentEra era
    => CardanoApi.CardanoEra (CardanoApiEra era)
cardanoEra = cardanoEraFromRecentEra $ recentEra @era

-- | For convenience working with 'IsRecentEra'.
--
-- Similar to 'CardanoApi.shelleyBasedEra, but with a 'IsRecentEra era'
-- constraint instead of 'CardanoApi.IsShelleyBasedEra'.
shelleyBasedEra
    :: forall era. IsRecentEra era
    => CardanoApi.ShelleyBasedEra (CardanoApiEra era)
shelleyBasedEra = shelleyBasedEraFromRecentEra $ recentEra @era

data MaybeInRecentEra (thing :: Type -> Type)
    = InNonRecentEraByron
    | InNonRecentEraShelley
    | InNonRecentEraAllegra
    | InNonRecentEraMary
    | InNonRecentEraAlonzo
    | InRecentEraBabbage (thing BabbageEra)
    | InRecentEraConway (thing ConwayEra)

deriving instance (Eq (a BabbageEra), (Eq (a ConwayEra)))
    => Eq (MaybeInRecentEra a)
deriving instance (Show (a BabbageEra), (Show (a ConwayEra)))
    => Show (MaybeInRecentEra a)

-- | An existential type like 'AnyCardanoEra', but for 'RecentEra'.
data AnyRecentEra where
    AnyRecentEra
        :: IsRecentEra era -- Provide class constraint
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

instance Ord AnyRecentEra where
    compare = compare `on` fromEnum

instance Show AnyRecentEra where
    show (AnyRecentEra era) = "AnyRecentEra " <> show era

instance Eq AnyRecentEra where
    AnyRecentEra e1 == AnyRecentEra e2 =
        isJust $ testEquality e1 e2

-- | The complete set of recent eras.
--
allRecentEras :: Set AnyRecentEra
allRecentEras = Set.fromList [minBound .. maxBound]

toAnyCardanoEra :: AnyRecentEra -> CardanoApi.AnyCardanoEra
toAnyCardanoEra (AnyRecentEra era) =
    CardanoApi.AnyCardanoEra (cardanoEraFromRecentEra era)

fromAnyCardanoEra
    :: CardanoApi.AnyCardanoEra
    -> Maybe AnyRecentEra
fromAnyCardanoEra = \case
    CardanoApi.AnyCardanoEra CardanoApi.ByronEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.ShelleyEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.AllegraEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.MaryEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.AlonzoEra ->
        Nothing
    CardanoApi.AnyCardanoEra CardanoApi.BabbageEra ->
        Just $ AnyRecentEra RecentEraBabbage
    CardanoApi.AnyCardanoEra CardanoApi.ConwayEra ->
        Just $ AnyRecentEra RecentEraConway
