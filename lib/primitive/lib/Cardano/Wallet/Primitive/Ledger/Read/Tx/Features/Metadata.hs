{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Metadata
    ( getMetadata
    , fromShelleyMetadata
    , fromAllegraMetadata
    , fromMaryMetadata
    , fromAlonzoMetadata
    , fromBabbageMetadata
    , fromConwayMetadata
    )
    where

import Prelude

import Cardano.Ledger.Allegra.TxAuxData
    ( AllegraTxAuxData (..)
    )
import Cardano.Ledger.Alonzo.TxAuxData
    ( AlonzoTxAuxData (..)
    )
import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe
    )
import Cardano.Ledger.Shelley.TxAuxData
    ( Metadatum
    , ShelleyTxAuxData (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx.Metadata
    ( Metadata (..)
    )
import Data.Map
    ( Map
    )
import Data.Word
    ( Word64
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardConway
    , StandardMary
    , StandardShelley
    )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Api.Shelley as CardanoAPI
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W

getMetadata :: forall era . IsEra era => Metadata era -> Maybe W.TxMetadata
getMetadata = case theEra @era of
    Byron -> noMetadatas
    Shelley -> yesMetadata fromShelleyMetadata
    Allegra -> yesMetadata fromAllegraMetadata
    Mary -> yesMetadata fromMaryMetadata
    Alonzo -> yesMetadata fromAlonzoMetadata
    Babbage -> yesMetadata fromBabbageMetadata
    Conway -> yesMetadata fromConwayMetadata
  where
    noMetadatas _ = Nothing
    yesMetadata f (Metadata s) = f <$> strictMaybeToMaybe s

fromShelleyMetadata :: ShelleyTxAuxData StandardShelley -> W.TxMetadata
fromShelleyMetadata (ShelleyTxAuxData md) = fromMetadata md

-- fixme: [ADP-525] It is fine for now since we do not look at script
-- pre-images. But this is precisely what we want as part of the
-- multisig/script balance reporting.
fromAllegraMetadata :: AllegraTxAuxData StandardAllegra -> W.TxMetadata
fromAllegraMetadata (AllegraTxAuxData md _scripts) = fromMetadata md

fromMaryMetadata :: AllegraTxAuxData StandardMary -> W.TxMetadata
fromMaryMetadata (AllegraTxAuxData md _scripts) = fromMetadata md

fromAlonzoMetadata :: AlonzoTxAuxData StandardAlonzo -> W.TxMetadata
fromAlonzoMetadata (AlonzoTxAuxData md _timelock _plutus) = fromMetadata md

fromBabbageMetadata :: AlonzoTxAuxData StandardBabbage -> W.TxMetadata
fromBabbageMetadata (AlonzoTxAuxData md _timelock _plutus) = fromMetadata md

fromConwayMetadata :: AlonzoTxAuxData StandardConway -> W.TxMetadata
fromConwayMetadata (AlonzoTxAuxData md _timelock _plutus) = fromMetadata md

fromMetadata :: Map Word64 Metadatum -> W.TxMetadata
fromMetadata =
    Cardano.makeTransactionMetadata . CardanoAPI.fromShelleyMetadata
