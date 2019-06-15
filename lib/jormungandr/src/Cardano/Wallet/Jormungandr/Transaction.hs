{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Compatibility
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (AddressK), Key, Passphrase (..), XPrv, getKey )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), Tx (..), TxId (..), TxOut (..), TxWitness (..) )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), TransactionLayer (..) )
import Control.Monad
    ( forM )
import Data.ByteString
    ( ByteString )

import qualified Cardano.Crypto.Wallet as CC

-- | Construct a 'TransactionLayer' compatible with Shelley and 'Jörmungandr'
newTransactionLayer
    :: forall n . Hash "Block0Hash"
    -> TransactionLayer (Jormungandr n)
newTransactionLayer block0Hash = TransactionLayer
    { mkStdTx = \keyFrom inps outs -> do
        let ins = (fmap fst inps)
        let tx = Tx ins outs
        let witData = witnessUtxoData block0Hash (txId @(Jormungandr n) tx)
        txWitnesses <- forM inps $ \(_in, TxOut addr _c) -> mkWitness witData
            <$> withEither (ErrKeyNotFoundForAddress addr) (keyFrom addr)
        return (tx, txWitnesses)
    , estimateSize = error "TODO: See http-bridge as starting point"
    }
  where
    withEither :: e -> Maybe a -> Either e a
    withEither e = maybe (Left e) Right


newtype WitnessData = WitnessData ByteString

witnessUtxoData :: Hash "Block0Hash" -> Hash "Tx" -> WitnessData
witnessUtxoData (Hash block0) (Hash tx) = WitnessData (block0 <> tx)

mkWitness
    :: WitnessData
    -> (Key 'AddressK XPrv, Passphrase "encryption")
    -> TxWitness
mkWitness (WitnessData dat) (xPrv, pwd) =
    PublicKeyWitness dummyXPub $ sign dat (xPrv, pwd)
  where
    -- We can't easily modify the TxWitness type. This is a temporary solution
    -- before we can decide on better abstractions. We might want to have
    -- different witness types for Jormungandr and http-bridge.
    dummyXPub = error "The witness xPub should not be used for the new scheme."

sign
    :: ByteString
    -> (Key 'AddressK XPrv, Passphrase "encryption")
    -> Hash "signature"
sign contents (key, (Passphrase pwd)) =
    Hash . CC.unXSignature $ CC.sign pwd (getKey key) contents
