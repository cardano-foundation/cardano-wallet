{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    ( Hash (..), TxIn (..), TxOut (..), TxWitness (..) )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), TransactionLayer (..) )
import Control.Arrow
    ( second )
import Control.Monad
    ( forM )
import Data.Either.Combinators
    ( maybeToRight )
import Data.Quantity
    ( Quantity (..) )

import qualified Cardano.Crypto.Wallet as CC

-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer
    :: Hash "Genesis"
    -> TransactionLayer (Jormungandr n)
newTransactionLayer (Hash block0) = TransactionLayer
    { mkStdTx = \keyFrom inps outs -> do
        let tx = Tx (fmap (second coin) inps) outs
        txWitnesses <- forM inps $ \(txin, TxOut addr _) -> sign txin
            <$> maybeToRight (ErrKeyNotFoundForAddress addr) (keyFrom addr)
        return (tx, txWitnesses)

    -- NOTE: at this point 'Jörmungandr' node does not support fee calculation
    , estimateSize = \_ -> Quantity 0
    }
  where
    sign
        :: TxIn
        -> (Key 'AddressK XPrv, Passphrase "encryption")
        -> TxWitness
    sign (TxIn (Hash tx) _) (key, (Passphrase pwd)) =
        TxWitness . CC.unXSignature $ CC.sign pwd (getKey key) (block0 <> tx)
