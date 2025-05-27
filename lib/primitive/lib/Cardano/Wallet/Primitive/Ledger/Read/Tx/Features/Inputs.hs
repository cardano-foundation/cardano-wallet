{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Inputs
    ( getInputs
    , fromByronTxIn
    , fromShelleyTxIns
    , fromShelleyTxIn
    )
    where

import Prelude

import Cardano.Read.Ledger.Tx.Hash
    ( fromShelleyTxId
    )
import Cardano.Read.Ledger.Tx.Inputs
    ( Inputs (..)
    , InputsType
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Data.Foldable
    ( toList
    )

import qualified Cardano.Chain.UTxO as BY
import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.API as SH
import qualified Cardano.Ledger.TxIn as SL
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W

{-# INLINABLE getInputs #-}
getInputs :: forall era. IsEra era => Inputs era -> [W.TxIn]
getInputs = case theEra @era of
    Byron -> \(Inputs ins) -> fromByronTxIn <$> toList ins
    Shelley -> mkShelleyTxInputsIns
    Allegra -> mkShelleyTxInputsIns
    Mary -> mkShelleyTxInputsIns
    Alonzo -> mkShelleyTxInputsIns
    Babbage -> mkShelleyTxInputsIns
    Conway -> mkShelleyTxInputsIns

fromShelleyTxIns :: Foldable t => (t SH.TxIn) -> [W.TxIn]
fromShelleyTxIns ins = fromShelleyTxIn <$> toList ins

mkShelleyTxInputsIns :: (Foldable t, InputsType era ~ t SH.TxIn)
    => Inputs era -- ^
  -> [W.TxIn]
mkShelleyTxInputsIns (Inputs ins) = fromShelleyTxIns ins

fromByronTxIn :: BY.TxIn -> W.TxIn
fromByronTxIn (BY.TxInUtxo id_ ix) = W.TxIn
    { inputId = W.Hash $ CC.hashToBytes id_
    , inputIx = fromIntegral ix
    }

fromShelleyTxIn
    :: SL.TxIn
    -> W.TxIn
fromShelleyTxIn (SL.TxIn txid (SL.TxIx ix)) =
    W.TxIn (W.Hash $ fromShelleyTxId txid) (fromIntegral ix)
