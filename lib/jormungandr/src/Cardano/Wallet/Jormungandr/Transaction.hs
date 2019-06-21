{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (AddressK), Key, Passphrase (..), XPrv, getKey )
import Cardano.Wallet.Primitive.Types
    ( Hash (..)
    , txId
    , TxOut (..)
    , TxWitness (..)
    )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), TransactionLayer (..) )
import Control.Arrow
    ( second )
import Control.Monad
    ( forM )
import Data.ByteString
    ( ByteString )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Quantity
    ( Quantity (..) )

import qualified Cardano.Crypto.Wallet as CC

-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer
    :: forall n . Hash "Block0Hash"
    -> TransactionLayer (Jormungandr n)
newTransactionLayer block0Hash = TransactionLayer
    { mkStdTx = \keyFrom inps outs -> do
        let tx = Tx (fmap (second coin) inps) outs
        let witData = witnessUtxoData block0Hash (txId @(Jormungandr n) tx)
        txWitnesses <- forM inps $ \(_in, TxOut addr _c) -> sign witData
            <$> maybeToRight (ErrKeyNotFoundForAddress addr) (keyFrom addr)
        return (tx, txWitnesses)

    -- NOTE: at this point 'Jörmungandr' node does not support fee calculation
    , estimateSize = \_ -> Quantity 0
    }
  where
    witnessUtxoData :: Hash "Block0Hash" -> Hash "Tx" -> WitnessData
    witnessUtxoData (Hash block0) (Hash tx) = WitnessData (block0 <> tx)

    sign
        :: WitnessData
        -> (Key 'AddressK XPrv, Passphrase "encryption")
        -> TxWitness
    sign (WitnessData contents) (key, (Passphrase pwd)) =
        TxWitness . CC.unXSignature $ CC.sign pwd (getKey key) contents

newtype WitnessData = WitnessData ByteString
