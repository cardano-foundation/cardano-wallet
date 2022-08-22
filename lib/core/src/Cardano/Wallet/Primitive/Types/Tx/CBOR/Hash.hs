module Cardano.Wallet.Primitive.Types.Tx.CBOR.Hash 
    ( byronTxHash )
    where

import Prelude

import Cardano.Chain.UTxO (ATxAux, taTx)
import Cardano.Crypto ( hashToBytes, serializeCborHash )

import qualified Cardano.Wallet.Primitive.Types.Hash as W

byronTxHash :: ATxAux a -> W.Hash tag
byronTxHash = W.Hash . hashToBytes . serializeCborHash . taTx