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
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Hash (..), Tx (..), TxId (..), TxOut (..), TxWitness (..) )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), TransactionLayer (..) )
import Control.Monad
    ( forM )
import Data.ByteString
    ( ByteString )
import Data.Quantity
    ( Quantity (..) )

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
    , estimateSize = \(CoinSelection inps outs chngs) ->
        Quantity $
        -- I have no idea now how this should be done properly.
        2
        + (length inps) * sizeOfInput
        + (length outs + length chngs) * sizeOfOutput
        + (length inps) * sizeOfTxWitness
    }
  where
    withEither :: e -> Maybe a -> Either e a
    withEither e = maybe (Left e) Right

    sizeOfInput = 1 + sizeOfTxId
    sizeOfOutput = sizeOfAddress + 8

    -- TODO: There are different types of witnesses! Not just this one!
    -- 1. Old witness scheme     128
    -- 2. New witness scheme     64
    -- 3. Account witness scheme 68
    sizeOfTxWitness = 128

    sizeOfTxId = 32
    sizeOfAddress = 32


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
