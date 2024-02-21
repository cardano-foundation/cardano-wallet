{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Common helper functions for LSQs
--
module Cardano.Wallet.Network.LocalStateQuery.Extra
    ( byronOrShelleyBased
    , onAnyEra
    , shelleyBased
    , currentEra
    ) where

import Prelude

import Cardano.Api
    ( AnyCardanoEra (..)
    , CardanoEra (..)
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( LSQ (..)
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( BlockQuery (..)
    , CardanoEras
    )
import Ouroboros.Consensus.HardFork.Combinator
    ( EraIndex (..)
    , QueryHardFork (..)
    , eraIndexToInt
    )
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras
    ( MismatchEraInfo
    )
import Ouroboros.Consensus.Protocol.Praos
    ( Praos
    )
import Ouroboros.Consensus.Protocol.TPraos
    ( TPraos
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardConway
    , StandardCrypto
    , StandardMary
    , StandardShelley
    )

import qualified Ouroboros.Consensus.Byron.Ledger as Byron
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

{-------------------------------------------------------------------------------
    Local State Query Helpers
-------------------------------------------------------------------------------}
byronOrShelleyBased
    :: LSQ Byron.ByronBlock m a
    -> ( forall shelleyEra praos
          . LSQ
                ( Shelley.ShelleyBlock
                    (praos StandardCrypto)
                    (shelleyEra StandardCrypto)
                )
                m
                a
       )
    -> LSQ (CardanoBlock StandardCrypto) m a
byronOrShelleyBased onByron onShelleyBased =
    onAnyEra
        onByron
        onShelleyBased
        onShelleyBased
        onShelleyBased
        onShelleyBased
        onShelleyBased
        onShelleyBased

-- | Create a local state query specific to the each era.
--
-- This combinator treats @MismatchEraInfo@ as impossible, which is true if the
-- @LSQEra@ value the @LSQ@ interpreter uses always matches the era of the
-- acquired point.
--
-- Where possible, the more convenient @shelleyBased@ or @byronOrShelleyBased@
-- should be used. This more raw helper was added to simplify dealing with
-- @PParams@ in alonzo.
onAnyEra
    :: LSQ Byron.ByronBlock m a
    -> LSQ (Shelley.ShelleyBlock (TPraos StandardCrypto) StandardShelley) m a
    -> LSQ (Shelley.ShelleyBlock (TPraos StandardCrypto) StandardAllegra) m a
    -> LSQ (Shelley.ShelleyBlock (TPraos StandardCrypto) StandardMary) m a
    -> LSQ (Shelley.ShelleyBlock (TPraos StandardCrypto) StandardAlonzo) m a
    -> LSQ (Shelley.ShelleyBlock (Praos StandardCrypto) StandardBabbage) m a
    -> LSQ (Shelley.ShelleyBlock (Praos StandardCrypto) StandardConway) m a
    -> LSQ (CardanoBlock StandardCrypto) m a
onAnyEra onByron onShelley onAllegra onMary onAlonzo onBabbage onConway =
    currentEra >>= \case
        AnyCardanoEra ByronEra -> mapQuery QueryIfCurrentByron onByron
        AnyCardanoEra ShelleyEra -> mapQuery QueryIfCurrentShelley onShelley
        AnyCardanoEra AllegraEra -> mapQuery QueryIfCurrentAllegra onAllegra
        AnyCardanoEra MaryEra -> mapQuery QueryIfCurrentMary onMary
        AnyCardanoEra AlonzoEra -> mapQuery QueryIfCurrentAlonzo onAlonzo
        AnyCardanoEra BabbageEra -> mapQuery QueryIfCurrentBabbage onBabbage
        AnyCardanoEra ConwayEra -> mapQuery QueryIfCurrentConway onConway
  where
    mapQuery
        :: ( forall r
              . BlockQuery block1 r
             -> BlockQuery
                    block2
                    ((Either (MismatchEraInfo (CardanoEras StandardCrypto))) r)
           )
        -> LSQ block1 m a
        -> LSQ block2 m a
    mapQuery _ (LSQPure x) = LSQPure x
    mapQuery f (LSQBind ma f') = LSQBind (mapQuery f ma) (mapQuery f . f')
    mapQuery f (LSQry q) = unwrap <$> LSQry (f q)

    unwrap =
        either
            ( error
                "impossible: byronOrShelleyBased query resulted in an \
                \era mismatch"
            )
            id

-- | Return Nothings in Byron, or @Just result@ in Shelley.
shelleyBased
    :: ( forall shelleyEra praos
          . LSQ
                ( Shelley.ShelleyBlock
                    (praos StandardCrypto)
                    (shelleyEra StandardCrypto)
                )
                m
                a
       )
    -> LSQ (CardanoBlock StandardCrypto) m (Maybe a)
shelleyBased onShelleyBased =
    byronOrShelleyBased
        (pure Nothing) -- on byron
        (Just <$> onShelleyBased)

-- NOTE:
-- In theory we should be able to know the current era from the tip sync
-- client. But there are is a problem from the combination of:
-- 1. We can't tell the era from a rollback message
-- 2. The initial tip we get is from a rollback message
--
-- which would make us unable to send Local State Queries until the node has
-- updated its tip once.
currentEra :: LSQ (CardanoBlock StandardCrypto) m AnyCardanoEra
currentEra = eraIndexToAnyCardanoEra <$> LSQry (QueryHardFork GetCurrentEra)

-- | Provides a mapping from 'EraIndex' to 'AnyCardanoEra'.
--
-- This mapping replaces a conversion between enumerations.
--
-- The following is used as a reference for the index mapping:
-- https://github.com/IntersectMBO/cardano-node/blob/3531289c9f79eab7ac5d3272ce6e6821504fec4c/cardano-api/src/Cardano/Api/Eras.hs#L188
eraIndexToAnyCardanoEra :: EraIndex xs -> AnyCardanoEra
eraIndexToAnyCardanoEra index =
    case eraIndexToInt index of
        0 -> AnyCardanoEra ByronEra
        1 -> AnyCardanoEra ShelleyEra
        2 -> AnyCardanoEra AllegraEra
        3 -> AnyCardanoEra MaryEra
        4 -> AnyCardanoEra AlonzoEra
        5 -> AnyCardanoEra BabbageEra
        6 -> AnyCardanoEra ConwayEra
        _ -> error "eraIndexToAnyCardanoEra: unknown era"
