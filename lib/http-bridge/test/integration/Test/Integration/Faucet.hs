{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Faucet
    ( Faucet
    , initFaucet
    , nextWallet
    ) where

import Prelude

import Cardano.Wallet
    ( unsafeRunExceptT )
import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge )
import Cardano.Wallet.HttpBridge.Environment
    ( KnownNetwork (..), Network (..), ProtocolMagic (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (postTx) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , KeyToAddress (..)
    , Passphrase (..)
    , XPrv
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.Primitive.Mnemonic
    ( Mnemonic
    , entropyToBytes
    , entropyToMnemonic
    , genEntropy
    , mnemonicToEntropy
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , Tx (..)
    , TxId (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Control.Concurrent.MVar
    ( MVar, newMVar, putMVar, takeMVar )
import Control.Monad
    ( replicateM )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Functor
    ( (<$) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..) )
import Data.Word
    ( Word32 )

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR

-- | An opaque 'Faucet' type from which one can get a wallet with funds
newtype Faucet = Faucet (MVar [Mnemonic 15])

-- | Get the next faucet wallet. Requires the 'initFaucet' to be called in order
-- to get a hand on a 'Faucet'.
nextWallet :: Faucet -> IO (Mnemonic 15)
nextWallet (Faucet mvar) = do
    takeMVar mvar >>= \case
        [] -> fail "nextWallet: Awe crap! No more faucet wallet available!"
        (h:q) -> h <$ putMVar mvar q

-- | Initialize a bunch of faucet wallets and make them available for the
-- integration tests scenarios.
initFaucet :: NetworkLayer t IO -> IO Faucet
initFaucet nl = do
    wallets <- replicateM 100 genMnemonic
    let outs = uncurry TxOut . (,Coin 100000000000) . firstAddress <$> wallets
    unsafeRunExceptT $ postTx nl (mkRedeemTx $ mconcat $ replicate 10 outs)
    Faucet <$> newMVar wallets
  where
    genMnemonic :: IO (Mnemonic 15)
    genMnemonic = entropyToMnemonic <$> genEntropy
    firstAddress :: Mnemonic 15 -> Address
    firstAddress mw =
        let
            (seed, pwd) =
                (Passphrase $ entropyToBytes $ mnemonicToEntropy mw, mempty)
            rootXPrv =
                generateKeyFromSeed (seed, mempty) pwd
            accXPrv =
                deriveAccountPrivateKey pwd rootXPrv minBound
            addrXPrv =
                deriveAddressPrivateKey pwd accXPrv ExternalChain minBound
        in
            keyToAddress @(HttpBridge 'Testnet) (publicKey addrXPrv)

{-------------------------------------------------------------------------------
                                    Internal
-------------------------------------------------------------------------------}

-- | Redeem funds from an address, to the given outputs. The corresponding
-- private keys are known by the redeem function and will be looked up
-- for the given address.
--
-- NOTE (1): We produce a list of witness as a result for better interoperability
-- with the 'TransactionLayer' although the list contains a single element.
--
-- NOTE (2): This is a one-time call only. There's no change output generated.
-- However, we can put here as many outputs as we want / can (64kb transaction
-- means that we can add up to ~1K outputs before needing two transactions...).
--
-- NOTE (3): There is a total of `4 500 000 000 ADA` available in the genesis.
mkRedeemTx
    :: [TxOut]
    -> (Tx, [TxWitness])
mkRedeemTx outs =
    let
        (txin, _, xprv, pwd) = genesis
        tx = Tx [txin] outs
        witness = mkWitness (txId @(HttpBridge 'Testnet) tx) (xprv, pwd)
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
        (ProtocolMagic pm) = protocolMagic @'Testnet
        sig = Hash $ CC.unXSignature $ CC.sign pwd xprv $ mempty
            <> "\x01" -- Public Key Witness Tag
            <> CBOR.toStrictByteString (CBOR.encodeInt32 pm)
            <> CBOR.toStrictByteString (CBOR.encodeBytes tx)

-- | A genesis UTxO generated from the configuration.yaml.
-- The secret key can be generated using `cardano-keygen`, and then, using
-- `deriveFirstHDAddress` from cardano-sl to get the corresponding address key.
--
-- A peek in the node's database / UTxO will also yield the initial "fake"
-- transaction id and input index to use as an entry. Funds available to the
-- address depends on:
--
-- - `genesis.spec.initializer.testBalance.totalBalance`
-- - `genesis.spec.initializer.testBalance.poors`
-- - `genesis.spec.initializer.testBalance.richmenShare`
--
-- If funds were to miss for the integration tests, increase the `totalBalance`.
genesis :: (TxIn, Address, XPrv, Passphrase "encryption")
genesis =
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
