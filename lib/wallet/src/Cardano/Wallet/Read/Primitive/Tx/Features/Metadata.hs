{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.Metadata
    ( getMetadata
    , fromShelleyMetadata
    , fromAllegraMetadata
    , fromMaryMetadata
    , fromAlonzoMetadata
    , fromBabbageMetadata
    )
    where

import Prelude

import Cardano.Ledger.BaseTypes
    ( strictMaybeToMaybe )
import Cardano.Ledger.Core
    ( AuxiliaryData )
import Cardano.Ledger.ShelleyMA
    ( MaryOrAllegra (..), ShelleyMAEra )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.Metadata
    ( Metadata (..) )
import Ouroboros.Consensus.Shelley.Eras
    ( AlonzoEra, BabbageEra, StandardCrypto )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Api.Shelley as CardanoAPI
import qualified Cardano.Ledger.Alonzo.Data as AL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Wallet.Primitive.Types.Tx.Tx as W

getMetadata :: EraFun Metadata (K (Maybe (W.TxMetadata) ))
getMetadata = EraFun
    { byronFun = noMetadatas
    , shelleyFun = yesMetadata fromShelleyMetadata
    , allegraFun = yesMetadata fromAllegraMetadata
    , maryFun = yesMetadata  fromMaryMetadata
    , alonzoFun = yesMetadata fromAlonzoMetadata
    , babbageFun = yesMetadata fromBabbageMetadata
    }
    where
        noMetadatas _ = K Nothing
        yesMetadata f (Metadata s) = K . fmap f $ strictMaybeToMaybe s

fromShelleyMetadata :: SL.Metadata c ->  W.TxMetadata
fromShelleyMetadata (SL.Metadata m) =
    Cardano.makeTransactionMetadata . CardanoAPI.fromShelleyMetadata $ m

-- fixme: [ADP-525] It is fine for now since we do not look at script
-- pre-images. But this is precisely what we want as part of the
-- multisig/script balance reporting.
fromAllegraMetadata :: AuxiliaryData (ShelleyMAEra 'Allegra StandardCrypto)
    -> W.TxMetadata
fromAllegraMetadata (MA.MAAuxiliaryData blob _scripts)
    = fromShelleyMetadata $ SL.Metadata blob

fromMaryMetadata :: AuxiliaryData (ShelleyMAEra 'Mary StandardCrypto)
    -> W.TxMetadata
fromMaryMetadata (MA.MAAuxiliaryData blob _scripts)
    = fromShelleyMetadata $ SL.Metadata blob

fromAlonzoMetadata :: AuxiliaryData (AlonzoEra StandardCrypto) -> W.TxMetadata
fromAlonzoMetadata (AL.AlonzoAuxiliaryData blob _scripts)
    = fromShelleyMetadata $ SL.Metadata blob

fromBabbageMetadata :: AuxiliaryData (BabbageEra StandardCrypto) -> W.TxMetadata
fromBabbageMetadata (AL.AlonzoAuxiliaryData blob _scripts)
    = fromShelleyMetadata $ SL.Metadata blob
