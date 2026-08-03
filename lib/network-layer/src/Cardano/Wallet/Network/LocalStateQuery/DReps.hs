{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2026 Cardano Foundation
-- License: Apache-2.0
--
-- Local state queries for DRep registry data (Conway era and later).
module Cardano.Wallet.Network.LocalStateQuery.DReps
    ( listDReps
    ) where

import Cardano.Ledger.BaseTypes
    ( Anchor (..)
    , EpochNo (..)
    , urlToText
    )
import Cardano.Ledger.Coin
    ( Coin (..)
    )
import Cardano.Ledger.Compactible
    ( fromCompact
    )
import Cardano.Ledger.Conway.Governance
    ( ConwayEraGov
    )
import Cardano.Ledger.Conway.State
    ( ConwayEraCertState
    )
import Cardano.Ledger.DRep
    ( DRepState (..)
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( LSQ (..)
    )
import Cardano.Wallet.Network.LocalStateQuery.Extra
    ( onAnyEra
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRepAnchor (..)
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepRegistration (..)
    , DRepScriptHash (..)
    )
import Data.ByteString.Short
    ( fromShort
    )
import Data.Map.Strict
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Maybe.Strict
    ( strictMaybeToMaybe
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )
import Prelude

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.DRep as Ledger
import qualified Cardano.Ledger.Hashes as Hashes
import qualified Cardano.Ledger.Keys as SL
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

{-----------------------------------------------------------------------------
    Local State Query for DRep Registry (Conway+)
------------------------------------------------------------------------------}

type LSQ' = LSQ (CardanoBlock StandardCrypto) IO

-- | Query all registered DReps with their voting power and status.
--
-- Returns 'Nothing' when the connected node is running a pre-Conway era.
-- Returns an empty list when no DReps are registered.
listDReps :: LSQ' (Maybe [DRepRegistration])
listDReps =
    onAnyEra
        (pure Nothing) -- Byron
        (pure Nothing) -- Shelley
        (pure Nothing) -- Allegra
        (pure Nothing) -- Mary
        (pure Nothing) -- Alonzo
        (pure Nothing) -- Babbage
        (Just <$> drepQuery) -- Conway
        (Just <$> drepQuery) -- Dijkstra

drepQuery
    :: (ConwayEraGov era, ConwayEraCertState era)
    => LSQ (Shelley.ShelleyBlock proto era) IO [DRepRegistration]
drepQuery = do
    epochNo <- LSQry Shelley.GetEpochNo
    states  <- LSQry (Shelley.GetDRepState Set.empty)
    distr   <- LSQry (Shelley.GetDRepStakeDistr Set.empty)
    pure $ Map.foldrWithKey (buildEntry epochNo distr) [] states

buildEntry
    :: EpochNo
    -> Map Ledger.DRep Coin
    -> SL.Credential SL.DRepRole
    -> DRepState
    -> [DRepRegistration]
    -> [DRepRegistration]
buildEntry (EpochNo currentEpoch) distr cred DRepState{..} acc =
    case credToDRepID cred of
        Nothing    -> acc
        Just drepId ->
            let ledgerDRep = Ledger.DRepCredential cred
                votingPower =
                    Ledger.toWalletCoin
                        $ fromMaybe (Coin 0)
                        $ Map.lookup ledgerDRep distr
                reg = DRepRegistration
                    { drepRegId          = drepId
                    , drepRegExpiryEpoch = unEpochNo drepExpiry
                    , drepRegAnchor      = fromAnchor <$> strictMaybeToMaybe drepAnchor
                    , drepRegDeposit     = Ledger.toWalletCoin (fromCompact drepDeposit)
                    , drepRegVotingPower = votingPower
                    , drepRegIsActive    = unEpochNo drepExpiry >= currentEpoch
                    }
            in  reg : acc

credToDRepID
    :: SL.Credential SL.DRepRole
    -> Maybe DRepID
credToDRepID = \case
    SL.KeyHashObj (SL.KeyHash h) ->
        Just $ DRepFromKeyHash $ DRepKeyHash $ fromShort $ Crypto.hashToBytesShort h
    SL.ScriptHashObj (Hashes.ScriptHash h) ->
        Just $ DRepFromScriptHash $ DRepScriptHash $ fromShort $ Crypto.hashToBytesShort h

fromAnchor :: Anchor -> DRepAnchor
fromAnchor anchor =
    DRepAnchor
        { drepAnchorUrl  = urlToText (anchorUrl anchor)
        , drepAnchorHash =
            Crypto.hashToBytes . Hashes.extractHash $ anchorDataHash anchor
        }
