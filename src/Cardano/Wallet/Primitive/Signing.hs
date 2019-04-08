{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Wallet.Primitive.Signing where

import Prelude

import Cardano.Wallet.Binary
    ( TxWitness (..), encodeTx, toByteString )
import Cardano.Wallet.Primitive.Types
    ( Address, Hash (..), Tx (..), TxIn, TxOut, protocolMagic )
import Control.Monad
    ( forM )
import Crypto.Hash
    ( Blake2b_256, hash )
import Data.ByteArray
    ( convert )
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
mkStdTx :: SeqState
        -> Key 'RootK XPrv
        -> Passphrase "encryption"
        -> [(TxIn, Address)]
        -- ^ Selected inputs
        -> [TxOut]
        -- ^ Selected outputs (including change)
        -> Maybe (Tx, [TxWitness])
mkStdTx seqState rootPrv pass ownedIns outs = do

    let ins = (fmap fst ownedIns)
        tx = Tx ins outs
        txSigData = hashTx tx

    txWitness <- forM ownedIns (\(_in, addr) -> mkWitness txSigData <$> keyFrom addr)

    return (tx, txWitness)

  where
    hashTx :: Tx -> Hash "tx"
    hashTx tx = Hash
        $ convert
        $ (hash @ByteString @Blake2b_256)
        $ toByteString
        $ encodeTx tx

    keyFrom :: Address -> (Maybe (Key 'AddressK XPrv))
    keyFrom addr = do
        -- We are assuming there is only one account
        let account = Index 0
        let accountPrv = deriveAccountPrivateKey pass rootPrv account

        -- We are ignoring the new state/pool. We won't discover any new
        -- addresses when submitting transactions.
        index <- fst $ lookupAddress addr (externalPool seqState)

        return $ deriveAddressPrivateKey pass accountPrv ExternalChain index

    mkWitness :: Hash "tx" -> Key 'AddressK XPrv -> TxWitness
    mkWitness txSigData xPrv =
        PublicKeyWitness
            (encode $ publicKey xPrv)
            (Hash $ signRaw (Just SignTx) xPrv txSigData)

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
data SignTag
    = SignForTestingOnly  -- ^ Anything (to be used for testing only)
    | SignTx              -- ^ Tx:               @TxSigData@
    | SignRedeemTx        -- ^ Redeem tx:        @TxSigData@
    | SignVssCert         -- ^ Vss certificate:  @(VssPublicKey, EpochIndex)@
    | SignUSProposal      -- ^ Update proposal:  @UpdateProposalToSign@
    | SignCommitment      -- ^ Commitment:       @(EpochIndex, Commitment)@
    | SignUSVote          -- ^ US proposal vote: @(UpId, Bool)@
    | SignMainBlock       -- ^ Main block:       @MainToSign@
    | SignMainBlockLight
    | SignMainBlockHeavy
    | SignProxySK         -- ^ Proxy key:        @ProxySecretKey@
    deriving (Eq, Ord, Show, Generic)

-- TODO: it would be nice if we couldn't use 'SignTag' with wrong
-- types. Maybe something with GADTs and data families?



-- | Get magic bytes corresponding to a 'SignTag'. Guaranteed to be different
-- (and begin with a different byte) for different tags.
signTag :: SignTag -> ByteString
signTag = \case
    SignForTestingOnly -> "\x00"
    SignTx             -> "\x01" <> network
    SignRedeemTx       -> "\x02" <> network
    SignVssCert        -> "\x03" <> network
    SignUSProposal     -> "\x04" <> network
    SignCommitment     -> "\x05" <> network
    SignUSVote         -> "\x06" <> network
    SignMainBlock      -> "\x07" <> network
    SignMainBlockLight -> "\x08" <> network
    SignMainBlockHeavy -> "\x09" <> network
    SignProxySK        -> "\x0a" <> network
  where
    network = toByteString . CBOR.encodeInt32 $ pm
    pm = protocolMagic

-- Signatures


-- | Wrapper around 'CC.XSignature'.
newtype Signature a = Signature CC.XSignature
    deriving (Eq, Ord, Show, Generic)


--
--
--
--
--


-- | Sign a bytestring.
signRaw
    :: Maybe SignTag   -- ^ See docs for 'SignTag'. Unlike in 'sign', we
                       -- allow no tag to be provided just in case you need
                       -- to sign /exactly/ the bytestring you provided
    -> Key level XPrv
    -> Hash "tx"
    -> ByteString -- Previously Raw
signRaw mbTag (Key k) (Hash x) = CC.unXSignature $ CC.sign emptyPassphrase k (tag <> x)
  where
    tag = maybe mempty signTag mbTag

    emptyPassphrase :: ByteString
    emptyPassphrase = mempty
