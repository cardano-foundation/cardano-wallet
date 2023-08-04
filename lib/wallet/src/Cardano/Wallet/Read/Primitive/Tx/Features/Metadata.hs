{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
  ( getMetadata
  , fromShelleyMetadata
  , fromAllegraMetadata
  , fromMaryMetadata
  , fromAlonzoMetadata
  , fromBabbageMetadata
  , fromConwayMetadata
  )
where

import Cardano.Api.Shelley qualified as Cardano
import Cardano.Api.Shelley qualified as CardanoAPI
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
import Cardano.Wallet.Primitive.Types.Tx.Tx qualified as W
import Cardano.Wallet.Read.Eras
  ( EraFun (..)
  , K (..)
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
import Prelude

getMetadata :: EraFun Metadata (K (Maybe (W.TxMetadata)))
getMetadata =
  EraFun
    { byronFun = noMetadatas
    , shelleyFun = yesMetadata fromShelleyMetadata
    , allegraFun = yesMetadata fromAllegraMetadata
    , maryFun = yesMetadata fromMaryMetadata
    , alonzoFun = yesMetadata fromAlonzoMetadata
    , babbageFun = yesMetadata fromBabbageMetadata
    , conwayFun = yesMetadata fromConwayMetadata
    }
  where
    noMetadatas _ = K Nothing
    yesMetadata f (Metadata s) = K . fmap f $ strictMaybeToMaybe s

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
