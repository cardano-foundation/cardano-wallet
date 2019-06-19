{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Environment
    ( Network (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (AddressK), Key, Passphrase (..), XPrv, XPub, getKey, publicKey )
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
    :: TransactionLayer (Jormungandr 'Testnet)
newTransactionLayer = TransactionLayer
    { mkStdTx = \keyFrom inps outs -> do
        let ins = (fmap fst inps)
        let tx = Tx ins outs
        let txSigData = txId @(Jormungandr 'Testnet) tx
        txWitnesses <- forM inps $ \(_in, TxOut addr _c) -> mkWitness txSigData
            <$> withEither (ErrKeyNotFoundForAddress addr) (keyFrom addr)
        return (tx, txWitnesses)

    -- NOTE: at this point 'Jörmungandr' node does not support fee calculation
    , estimateSize = \_ -> Quantity 0
    }
  where
    withEither :: e -> Maybe a -> Either e a
    withEither e = maybe (Left e) Right

    mkWitness
        :: Hash "Tx"
        -> (Key 'AddressK XPrv, Passphrase "encryption")
        -> TxWitness
    mkWitness tx (xPrv, pwd) = PublicKeyWitness
        (encodeXPub $ publicKey xPrv)
        (sign (SignTx tx) (xPrv, pwd))

    encodeXPub :: (Key level XPub) -> ByteString
    encodeXPub = CC.xpubPublicKey . getKey

    sign
        :: SignTag
        -> (Key 'AddressK XPrv, Passphrase "encryption")
        -> Hash "signature"
    sign _tag (_key, (Passphrase _pwd)) = undefined

-- | To protect agains replay attacks (i.e. when an attacker intercepts a
-- signed piece of data and later sends it again), we add a tag to all data
-- that we sign. This ensures that even if some bytestring can be
-- deserialized into two different types of messages (A and B), the attacker
-- can't take message A and send it as message B.
--
-- We also automatically add the network tag ('protocolMagic') whenever it
-- makes sense, to ensure that things intended for testnet won't work for
-- mainnet.
--
-- The wallet only cares about the 'SignTx' tag. In 'cardano-sl' there was
-- a lot more cases.
newtype SignTag
    = SignTx (Hash "Tx")
