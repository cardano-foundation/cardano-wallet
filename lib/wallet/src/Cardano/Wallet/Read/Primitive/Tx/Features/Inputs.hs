{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( getInputs
    , fromByronTxIn
    , fromShelleyTxIns
    , fromShelleyTxIn
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.Hash
    ( fromShelleyTxId )
import Cardano.Wallet.Read.Tx.Inputs
    ( Inputs (..), InputsType )
import Data.Foldable
    ( toList )
import Data.Word
    ( Word16, Word32, Word64 )

import qualified Cardano.Chain.UTxO as BY
import qualified Cardano.Crypto.Hashing as CC
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Shelley.API as SH
import qualified Cardano.Ledger.TxIn as SL
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.Tx.TxIn as W

getInputs :: EraFun Inputs (K [W.TxIn])
getInputs = EraFun
    { byronFun = \(Inputs ins) -> K . fmap fromByronTxIn $ toList ins
    , shelleyFun = mkShelleyTxInputsIns
    , allegraFun = mkShelleyTxInputsIns
    , maryFun = mkShelleyTxInputsIns
    , alonzoFun = mkShelleyTxInputsIns
    , babbageFun = mkShelleyTxInputsIns
    , conwayFun = mkShelleyTxInputsIns
    }

fromShelleyTxIns :: Foldable t => (t (SH.TxIn crypto)) -> K [W.TxIn] b
fromShelleyTxIns ins = K . fmap fromShelleyTxIn $ toList ins

mkShelleyTxInputsIns :: (Foldable t, InputsType era ~ t (SH.TxIn crypto))
    => Inputs era -- ^
  -> K [W.TxIn] b
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
    -- https://github.com/input-output-hk/cardano-ledger/commit/4097a9055e6ea57161755e6a8cbfcf719b65e9ab).
    -- However, the valid range is still 0 <= x <= (maxBound :: Word16), so we
    -- reflect that here.
    unsafeCast :: Word64 -> Word32
    unsafeCast txIx =
        if txIx > fromIntegral (maxBound :: Word16)
        then error $ "Value for wallet TxIx is out of a valid range: " <> show txIx
        else fromIntegral txIx
