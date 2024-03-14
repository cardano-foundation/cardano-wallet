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

import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx.Hash
    ( fromShelleyTxId
    )
import Cardano.Wallet.Read.Tx.Inputs
    ( Inputs (..)
    , InputsType
    )
import Data.Foldable
    ( toList
    )
import Data.Word
    ( Word16
    , Word32
    , Word64
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

fromShelleyTxIns :: Foldable t => (t (SH.TxIn crypto)) -> [W.TxIn]
fromShelleyTxIns ins = fromShelleyTxIn <$> toList ins

mkShelleyTxInputsIns :: (Foldable t, InputsType era ~ t (SH.TxIn crypto))
    => Inputs era -- ^
  -> [W.TxIn]
mkShelleyTxInputsIns (Inputs ins) = fromShelleyTxIns ins

fromByronTxIn :: BY.TxIn -> W.TxIn
fromByronTxIn (BY.TxInUtxo id_ ix) = W.TxIn
    { inputId = W.Hash $ CC.hashToBytes id_
    , inputIx = fromIntegral ix
    }

fromShelleyTxIn
    :: SL.TxIn crypto
    -> W.TxIn
fromShelleyTxIn (SL.TxIn txid (SL.TxIx ix)) =
    W.TxIn (W.Hash $ fromShelleyTxId txid) (unsafeCast ix)
  where
    -- During the Vasil hard-fork the cardano-ledger team moved from
    -- representing transaction indices with Word16s, to using Word64s (see
    -- commit
    -- https://github.com/IntersectMBO/cardano-ledger/commit/4097a9055e6ea57161755e6a8cbfcf719b65e9ab).
    -- However, the valid range is still 0 <= x <= (maxBound :: Word16), so we
    -- reflect that here.
    unsafeCast :: Word64 -> Word32
    unsafeCast txIx =
        if txIx > fromIntegral (maxBound :: Word16)
        then error $ "Value for wallet TxIx is out of a valid range: " <> show txIx
        else fromIntegral txIx
