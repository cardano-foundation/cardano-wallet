{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Cardano.Wallet.Primitive.Signing where

import Prelude

import Cardano.Wallet.Binary
    ( TxWitness (..), toByteString )
import Cardano.Wallet.Primitive.Types
    ( Address, Hash (..), Tx (..), TxIn, TxOut, protocolMagic )
import Control.Monad
    ( forM )
import Data.ByteString
    ( ByteString )
import GHC.Generics
    ( Generic )

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Encoding as CBOR

import Cardano.Wallet.Primitive.AddressDerivation
import Cardano.Wallet.Primitive.AddressDiscovery

{-
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (AddressK, RootK)
    , DerivationType (Soft)
    , Index
    , Key (..)
    , Passphrase
    , XPrv
    , XPub
    , deriveAccountPrivateKey
    , deriveAddressPrivateKey
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPool, lookupAddress )
-}

type TxOwnedInputs owner = [(owner, TxIn)]

-- | Build a transaction

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
--
-- TODO: re-add shuffle
-- TODO: I removed FakeSigner/SafeSigner. Might be wrong.
mkStdTx :: (SeqState)
        -> Key 'RootK XPrv
        -> Passphrase "encryption"
        -> [(TxIn, Address)]
        -- ^ Selected inputs
        -> [TxOut]
        -- ^ Selected outputs (including change)
        -> Maybe (Tx, [TxWitness])
mkStdTx _s xprv passphrase ownedIns outs = do

    let ins = (fmap fst ownedIns)
        tx = Tx ins outs

    txWitness <- forM ownedIns (\(_, ownerAddr) -> mkWit <$> key ownerAddr)

    return (tx, txWitness)

  where
    pool = undefined
    txSigData = Hash "tx"

    -- TODO: Do we need to verify that addresses actually exist? No, I think.
    -- because we get them from lookupAddress

    key :: Address -> (Maybe (Key 'AddressK XPrv))
    key addr = do
        -- We are ignoring the new state/pool. We won't discover any new
        -- addresses when submitting transactions.
        index <- fst $ lookupAddress addr pool
        return $ addressPrvKeyFor index

    account :: Index 'Hardened 'AccountK
    account = Index 0

    accountPrv :: Key 'AccountK XPrv
    accountPrv = deriveAccountPrivateKey passphrase xprv account

    -- naming? "Prv"?
    addressPrvKeyFor :: (Index 'Soft 'AddressK) -> Key 'AddressK XPrv
    addressPrvKeyFor = deriveAddressPrivateKey passphrase accountPrv ExternalChain

    mkWit :: Key 'AddressK XPrv -> TxWitness
    mkWit ss =
        PublicKeyWitness
            (encode $ publicKey ss)
            (Hash $ signRaw signTag ss txSigData)

    encode :: (Key level XPub) -> ByteString
    encode (Key k) = CC.unXPub k




{-------------------------------------------------------------------------------
                              SignTag
-------------------------------------------------------------------------------}

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
-- a whole @SignTag@ data-type
signTag :: ByteString
signTag = signTxTag <> network
  where
    signTxTag = "\x01"
    network = toByteString $ CBOR.encodeInt32 protocolMagic

-- Signatures


-- | Wrapper around 'CC.XSignature'.
newtype Signature a = Signature CC.XSignature
    deriving (Eq, Ord, Show, Generic)

-- | Sign a bytestring.
signRaw
    :: ByteString
    -> Key level XPrv
    -> Hash "tx"
    -> ByteString -- Previously Raw
signRaw tag (Key k) (Hash x) = CC.unXSignature $ CC.sign emptyPassphrase k (tag <> x)
  where
    emptyPassphrase :: ByteString
    emptyPassphrase = mempty


--
--
-- ProtocolMAgic
--
--

