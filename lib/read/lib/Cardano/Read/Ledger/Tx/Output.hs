{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: © 2024 Cardano Foundation
License: Apache-2.0

Era-indexed transaction output.
-}
module Cardano.Read.Ledger.Tx.Output
    ( OutputType
    , Output (..)
    , getEraCompactAddr
    , getEraValue
    )
    where

import Prelude

import Cardano.Ledger.Alonzo.TxOut
    ( AlonzoTxOut
    )
import Cardano.Ledger.Core
    ( compactAddrTxOutL
    , valueTxOutL
    )
import Cardano.Ledger.Babbage.TxOut
    ( BabbageTxOut
    )
import Cardano.Ledger.Shelley.TxOut
    ( ShelleyTxOut
    )
import Cardano.Read.Ledger.Address
    ( CompactAddr (..)
    , CompactAddrType
    )
import Cardano.Read.Ledger.Value
    ( Value (..)
    , ValueType
    )
import Cardano.Wallet.Read.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import Control.Lens
    ( view
    )

import qualified Cardano.Chain.UTxO as BY

{-----------------------------------------------------------------------------
    Output
------------------------------------------------------------------------------}

type family OutputType era where
    OutputType Byron = BY.TxOut
    OutputType Shelley = ShelleyTxOut Shelley
    OutputType Allegra = ShelleyTxOut Allegra
    OutputType Mary = ShelleyTxOut Mary
    OutputType Alonzo = AlonzoTxOut Alonzo
    OutputType Babbage = BabbageTxOut Babbage
    OutputType Conway = BabbageTxOut Conway

newtype Output era = Output (OutputType era)

deriving instance Show (OutputType era) => Show (Output era)
deriving instance Eq (OutputType era) => Eq (Output era)

{-# INLINEABLE getEraCompactAddr #-}
getEraCompactAddr :: forall era. IsEra era => Output era -> CompactAddr era
getEraCompactAddr = case theEra :: Era era of
    Byron -> address $ (\(BY.CompactTxOut a _) -> a) . BY.toCompactTxOut
    Shelley -> address (view compactAddrTxOutL)
    Allegra -> address (view compactAddrTxOutL)
    Mary -> address (view compactAddrTxOutL)
    Alonzo -> address (view compactAddrTxOutL)
    Babbage -> address (view compactAddrTxOutL)
    Conway -> address (view compactAddrTxOutL)

-- Helper function for type inference
address
    :: (OutputType era -> CompactAddrType era)
    -> Output era -> CompactAddr era
address f (Output x) = CompactAddr (f x)

{-# INLINEABLE getEraValue #-}
getEraValue :: forall era. IsEra era => Output era -> Value era
getEraValue = case theEra :: Era era of
    Byron -> value BY.txOutValue
    Shelley -> value (view valueTxOutL)
    Allegra -> value (view valueTxOutL)
    Mary -> value (view valueTxOutL)
    Alonzo -> value (view valueTxOutL)
    Babbage -> value (view valueTxOutL)
    Conway -> value (view valueTxOutL)

-- Helper function for type inference
value :: (OutputType era -> ValueType era) -> Output era -> Value era
value f (Output x) = Value (f x)
