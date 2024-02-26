{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw extra signers required data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.ExtraSigs where

import Prelude

import Cardano.Ledger.Alonzo.TxBody
    ( reqSignerHashesTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Keys
    ( KeyHash
    , KeyRole (..)
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..)
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
    )
import Control.Lens
    ( (^.)
    )
import Data.Set
    ( Set
    )

type family ExtraSigsType era where
    ExtraSigsType Byron = ()
    ExtraSigsType Shelley = ()
    ExtraSigsType Allegra = ()
    ExtraSigsType Mary = ()
    ExtraSigsType Alonzo = Set (KeyHash 'Witness StandardCrypto)
    ExtraSigsType Babbage = Set (KeyHash 'Witness StandardCrypto)
    ExtraSigsType Conway = Set (KeyHash 'Witness StandardCrypto)

newtype ExtraSigs era = ExtraSigs (ExtraSigsType era)

deriving instance Show (ExtraSigsType era) => Show (ExtraSigs era)
deriving instance Eq (ExtraSigsType era) => Eq (ExtraSigs era)

-- | Get extra signatures required for a transaction in any era.
getEraExtraSigs :: EraFun Tx ExtraSigs
getEraExtraSigs
    = EraFun
        { byronFun = \_ -> ExtraSigs ()
        , shelleyFun = \_ -> ExtraSigs ()
        , allegraFun = \_ -> ExtraSigs ()
        , maryFun = \_ -> ExtraSigs ()
        , alonzoFun = mkExtraSignatures
        , babbageFun = mkExtraSignatures
        , conwayFun = mkExtraSignatures
        }
        where mkExtraSignatures =  onTx $ \tx -> ExtraSigs
                $ tx ^. bodyTxL . reqSignerHashesTxBodyL
