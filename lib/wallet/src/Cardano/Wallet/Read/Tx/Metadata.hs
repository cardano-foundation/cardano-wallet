{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw mint data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Metadata
    ( MetadataType
    , Metadata (..)
    , getEraMetadata
    ) where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Control.Lens
    ( (^.) )

import Cardano.Ledger.Core
    ( AuxiliaryData, auxDataTxL )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Data.Maybe.Strict
    ( StrictMaybe )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardMary
    , StandardShelley
    )

type family MetadataType era where
  MetadataType ByronEra = ()
  MetadataType ShelleyEra = StrictMaybe (AuxiliaryData StandardShelley)
  MetadataType AllegraEra = StrictMaybe (AuxiliaryData StandardAllegra)
  MetadataType MaryEra = StrictMaybe (AuxiliaryData StandardMary)
  MetadataType AlonzoEra = StrictMaybe (AuxiliaryData StandardAlonzo)
  MetadataType BabbageEra = StrictMaybe (AuxiliaryData StandardBabbage)

newtype Metadata era = Metadata (MetadataType era)

deriving instance Show (MetadataType era) => Show (Metadata era)
deriving instance Eq (MetadataType era) => Eq (Metadata era)

getEraMetadata :: EraFun Tx Metadata
getEraMetadata = EraFun
    { byronFun = \_ -> Metadata ()
    , shelleyFun =  onTx $ \tx -> Metadata (tx ^. auxDataTxL)
    , allegraFun = onTx $ \tx -> Metadata (tx ^. auxDataTxL)
    , maryFun = onTx $ \tx -> Metadata (tx ^. auxDataTxL)
    , alonzoFun = onTx $ \tx -> Metadata (tx ^. auxDataTxL)
    , babbageFun = onTx $ \tx -> Metadata (tx ^. auxDataTxL)
    }


