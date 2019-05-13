{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Faucet
    ( mkRedeemTx
    ) where

import Prelude

import Cardano.Environment.HttpBridge
    ( ProtocolMagic (..), network, protocolMagic )
import Cardano.Wallet.Binary.HttpBridge
    ( toByteString )
import Cardano.Wallet.Compatibility.HttpBridge
    ( HttpBridge )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..), XPrv )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Hash (..)
    , Tx (..)
    , TxId (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..) )
import Data.Word
    ( Word32 )

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Encoding as CBOR

-- | Redeem funds from an address, to the given outputs. The corresponding
-- private keys are known by the redeem function and will be looked up
-- for the given address.
--
-- NOTE (1): We produce a list of witness as a result for better interoperability
-- with the 'TransactionLayer' although the list contains a single element.
--
-- NOTE (2): This is a one-time call only. There's no change output generated.
-- However, we can put here as many outputs as we want / can (8kb transaction
-- means that we can add up to ~145 outputs before needing two transactions...).
--
-- NOTE (3): There is a total of `4 500 000 000 ADA` available in the faucet.
mkRedeemTx
    :: [TxOut]
    -> (Tx, [TxWitness])
mkRedeemTx outs =
    let
        (txin, _, xprv, pwd) = faucet
        tx = Tx [txin] outs
        witness = mkWitness (txId @HttpBridge tx) (xprv, pwd)
    in
        (tx, [witness])
  where
    mkWitness
        :: Hash "Tx"
        -> (XPrv, Passphrase "encryption")
        -> TxWitness
    mkWitness (Hash tx) (xprv, Passphrase pwd) =
        PublicKeyWitness xpub sig
      where
        xpub = CC.unXPub $ CC.toXPub xprv
        (ProtocolMagic pm) = protocolMagic network
        sig = Hash $ CC.unXSignature $ CC.sign pwd xprv $ mempty
            <> "\x01" -- Public Key Witness Tag
            <> toByteString (CBOR.encodeInt32 pm)
            <> toByteString (CBOR.encodeBytes tx)

-- | A faucet / genesis UTxO generate from the configuration.yaml.
-- The secret key can be generated using `cardano-keygen`, and then, using
-- `deriveForstHDAddress` from cardano-sl to get the corresponding address key.
--
-- A peek in the node's database / UTxO will also yield the initial "fake"
-- transaction id and input index to use as a an entry. Funds available to the
-- address depends on:
--
-- - `genesis.spec.initializer.testBalance.totalBalance`
-- - `genesis.spec.initializer.testBalance.poors`
-- - `genesis.spec.initializer.testBalance.richmenShare`
--
-- If funds were to miss for the integration tests, increase the `totalBalance`.
faucet :: (TxIn, Address, XPrv, Passphrase "encryption")
faucet =
    ( unsafeTxIn 0
        "ad348750ba0673f5829ac2c73e1ddf59ae4219222ee5703b05a5af5457981c17"
    , unsafeAddress
        "37btjrVyb4KBW8ydTZxU2aX4nCeBTw8n1BW9ZgVFvqs8jguGK1udPXL2r\
        \4KmhSKPHxyjgnB7uRguYt6QmAog4vLPGzCuYPke9mXMzbeaxXdUy6TrJ1"
    , unsafeXPrv
        "a20b14741984551c2da8d317256b0f81a523ef1b69f27212a5e5f486ab23370d\
        \d83fc1d1e80832493cf6240e697ec6e6ed12bf455674dab6366eda502e76022a\
        \c04874623194abe76883f0db9c568f7972eeb8cd08fdb280f2cc50cd86ea7dff\
        \5a79a468b440f002972c147600234a1bb04c46261eacb629be0cff3beccd705b"
    , mempty
    )

-- | Build a 'TxIn' from a known index and hex-encoded bytestring
unsafeTxIn :: Word32 -> ByteString -> TxIn
unsafeTxIn ix hex =
    case convertFromBase Base16 hex of
        Left e -> error $ "unsafeHash: " <> e
        Right a -> TxIn (Hash a) ix

-- | Build an 'Address' from a base58-encoded string
unsafeAddress :: Text -> Address
unsafeAddress txt =
    case fromText txt of
        Left e -> error $ "unsafeAddress: " <> show e
        Right a -> a

-- | Build a 'XPrv' from an hex-encoded bytestring
unsafeXPrv :: ByteString -> XPrv
unsafeXPrv hex =
    case convertFromBase @_ @ByteString Base16 hex >>= CC.xprv of
        Left e -> error $ "unsafeXPrv: " <> e
        Right a -> a
