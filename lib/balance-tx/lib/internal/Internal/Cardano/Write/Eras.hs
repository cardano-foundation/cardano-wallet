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
    ( Babbage
    , Conway
    , LatestLedgerEra

    , RecentEra (..)
    , IsRecentEra (..)
    , CardanoApiEra
    , MaybeInRecentEra (..)
    , RecentEraConstraints

    , AnyRecentEra (..)
    , allRecentEras

    , InAnyRecentEra (..)
    , toInAnyRecentEra

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
import Cardano.Ledger.Api
    ( BabbageEra
    , ConwayEra
    )
import Cardano.Ledger.Api.UTxO
    ( EraUTxO (ScriptsNeeded)
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

-- | Type alias for backward compatibility
type Babbage = BabbageEra

-- | Type alias for backward compatibility
type Conway = ConwayEra

type LatestLedgerEra = ConwayEra

--------------------------------------------------------------------------------
-- RecentEra
--------------------------------------------------------------------------------

-- | We define the term "recent era" to mean one of the /two most recent eras/.
-- This represents eras we care constructing transactions for.
--
-- === Why /two/ eras?
--
-- To have the same software constructing transactions just before and just
-- after a hard-fork, we need to, for a time, support two eras.
--
-- === 'RecentEra' the value vs 'IsRecentEra' the class
--
-- We keep track of the era on the type-level, and our code is mostly
-- polymorphic in the era. In order to simplify such code, we impose the
-- following two guidelines:
--
-- 1. For any type declaration that mentions the era type parameter, you SHOULD
-- add a type class constraint IsRecentEra era. (Think of this step as adding a
-- type signature 'era : IsRecent'.)
--
-- 2. For any declaration that depends on era, but does not mention era in the
-- type, you SHOULD add a value-level argument of type RecentEra era in order
-- to disambiguate the type and AVOID the use of AllowAmbiguousTypes. (In old
-- Haskell code, the Proxy type was often used for this purpose.) In most other
-- cases, you SHOULD NOT add a value-level argument.
--
-- DO:
--
-- @@
-- isBabbageOnwards   :: IsRecentEra era => BabbageEraOnwards (CardanoApiEra era)
-- signTxByAlice      :: IsRecentEra era => Tx era -> Tx era
-- makeAndSerializeTx :: IsRecentEra era => RecentEra era -> Intent -> ByteString
-- @@
--
-- DON'T:
--
-- @@
--    -- The type class constraint is necessary
-- isBabbageOnwards   :: RecentEra era -> BabbageEraOnwards (CardanoApiEra era)
--    -- The value-level argument is probaby superfluous
-- signTxByAlice      :: IsRecentEra era => RecentEra era -> Tx era -> Tx era
--    -- This type is ambiguous
-- makeAndSerializeTx :: IsRecentEra era => Intent -> ByteString
-- @@
data RecentEra era where
    RecentEraBabbage :: RecentEra Babbage
    RecentEraConway :: RecentEra Conway

deriving instance Eq (RecentEra era)
deriving instance Show (RecentEra era)

instance TestEquality RecentEra where
    testEquality RecentEraBabbage RecentEraBabbage = Just Refl
    testEquality RecentEraConway RecentEraConway = Just Refl
    testEquality RecentEraBabbage RecentEraConway = Nothing
    testEquality RecentEraConway RecentEraBabbage = Nothing

-- | C.f. 'RecentEra'
class
    ( CardanoApi.IsShelleyBasedEra (CardanoApiEra era)
    , CardanoApi.ShelleyLedgerEra (CardanoApiEra era) ~ era
    , Typeable era
    , RecentEraConstraints era
    ) => IsRecentEra era where
    recentEra :: RecentEra era

-- | Convenient constraints. Constraints may be dropped as we move to new eras.
--
-- Adding too many constraints shouldn't be a concern as the point of
-- 'RecentEra' is to work with a small closed set of eras, anyway.
type RecentEraConstraints era =
    ( Core.Era era
    , Core.EraTx era
    , Core.Script era ~ AlonzoScript era
    , Core.Tx era ~ Babbage.AlonzoTx era
    , Core.EraTxOut era
    , Core.EraTxCert era
    , Core.Value era ~ MaryValue
    , Core.TxWits era ~ AlonzoTxWits era
    , Alonzo.AlonzoEraPParams era
    , Ledger.AlonzoEraTx era
    , ScriptsNeeded era ~ AlonzoScriptsNeeded era
    , AlonzoEraScript era
    , Eq (Core.TxOut era)
    , Eq (Core.Tx era)
    , Babbage.BabbageEraTxBody era
    , Alonzo.AlonzoEraTxBody era
    , Shelley.EraUTxO era
    , Ledger.ShelleyEraTxCert era
    , Show (Core.TxOut era)
    , Show (Core.Tx era)
    , Show (Core.PParams era)
    , Show (AlonzoScript era)
    , EraPlutusContext era
    , AllegraEraScript era
    , Core.NativeScript era ~ Timelock era
    )

instance IsRecentEra Babbage where
    recentEra = RecentEraBabbage

instance IsRecentEra Conway where
    recentEra = RecentEraConway

data MaybeInRecentEra (thing :: Type -> Type)
    = InNonRecentEraByron
    | InNonRecentEraShelley
    | InNonRecentEraAllegra
    | InNonRecentEraMary
    | InNonRecentEraAlonzo
    | InRecentEraBabbage (thing Babbage)
    | InRecentEraConway (thing Conway)

deriving instance (Eq (a Babbage), (Eq (a Conway)))
    => Eq (MaybeInRecentEra a)
deriving instance (Show (a Babbage), (Show (a Conway)))
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

data InAnyRecentEra thing
    = InConway (thing Conway)
    | InBabbage (thing Babbage)

deriving instance (Show (thing Conway), (Show (thing Babbage))) => Show (InAnyRecentEra thing)
deriving instance (Eq (thing Conway), (Eq (thing Babbage))) => Eq (InAnyRecentEra thing)

toInAnyRecentEra
    :: forall era thing. IsRecentEra era => thing era -> InAnyRecentEra thing
toInAnyRecentEra thing = case recentEra @era of
    RecentEraConway -> InConway thing
    RecentEraBabbage -> InBabbage thing

--------------------------------------------------------------------------------
-- Cardano.Api compatibility
--------------------------------------------------------------------------------
-- | Type family for converting to "Cardano.Api" eras.
type family CardanoApiEra era = cardanoApiEra | cardanoApiEra -> era
type instance CardanoApiEra Babbage = CardanoApi.BabbageEra
type instance CardanoApiEra Conway = CardanoApi.ConwayEra

-- | Convert to a 'CardanoEra'.
cardanoEraFromRecentEra
    :: RecentEra era
    -> CardanoApi.CardanoEra (CardanoApiEra era)
cardanoEraFromRecentEra = \case
    RecentEraConway -> CardanoApi.ConwayEra
    RecentEraBabbage -> CardanoApi.BabbageEra

-- | Convert to a 'ShelleyBasedEra'.
-- At this time, every 'RecentEra' is Shelley-based.
shelleyBasedEraFromRecentEra
    :: RecentEra era
    -> CardanoApi.ShelleyBasedEra (CardanoApiEra era)
shelleyBasedEraFromRecentEra = \case
    RecentEraConway -> CardanoApi.ShelleyBasedEraConway
    RecentEraBabbage -> CardanoApi.ShelleyBasedEraBabbage

-- | Currently needed for 'Enum' instance.
toAnyCardanoEra :: AnyRecentEra -> CardanoApi.AnyCardanoEra
toAnyCardanoEra (AnyRecentEra era) =
    CardanoApi.AnyCardanoEra (cardanoEraFromRecentEra era)

-- | Currently needed for 'Enum' instance.
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
