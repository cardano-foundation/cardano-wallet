{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.Primitive.Signing where

import Prelude

import Cardano.Wallet.Binary
    ( TxWitness (..), toByteString )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (RootK), Key (..), XPrv, XPub, publicKey )
import Cardano.Wallet.Primitive.Types
    ( Address, Hash (..), ProtocolMagic (..), Tx (..), TxIn, TxOut (TxOut) )
import Control.Monad
    ( forM )
import Data.ByteString
    ( ByteString )
import GHC.Generics
    ( Generic )

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Encoding as CBOR



newtype PassPhrase = PassPhrase ByteString -- TODO: Was ScrubbedBytes previously
data TxAux = TxAux Tx [TxWitness]
type TxOwnedInputs owner = [(owner, TxIn)]

-- | Build a transaction

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
--
-- TODO: re-add shuffle
-- TODO: I removed FakeSigner/SafeSigner. Might be wrong.
mkStdTx :: ProtocolMagic
        -> (Address -> Either e (Key 'RootK XPrv))
        -- ^ Signer for each input of the transaction
        -> [(TxIn, TxOut)]
        -- ^ Selected inputs
        -> [TxOut]
        -- ^ Selected outputs (including change)
        -> Either e TxAux
mkStdTx pm signer ownedIns outs = do

    let ins = (fmap fst ownedIns)
        tx = Tx ins outs

    txWitness <- forM ownedIns (\(_, TxOut ownerAddr _) ->
        mkWit <$> signer ownerAddr)

    return $ TxAux tx txWitness

  where
    txSigData = Hash "tx"

    mkWit ss =
        PublicKeyWitness
            (encode $ publicKey ss)
            (Hash $ signRaw pm (Just SignTx) ss txSigData)

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
signTag :: ProtocolMagic -> SignTag -> ByteString
signTag (ProtocolMagic pm) = \case
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
    :: ProtocolMagic
    -> Maybe SignTag   -- ^ See docs for 'SignTag'. Unlike in 'sign', we
                       -- allow no tag to be provided just in case you need
                       -- to sign /exactly/ the bytestring you provided
    -> Key 'RootK XPrv
    -> Hash "tx"
    -> ByteString -- Previously Raw
signRaw pm mbTag (Key k) (Hash x) = CC.unXSignature $ CC.sign emptyPassphrase k (tag <> x)
  where
    tag = maybe mempty (signTag pm) mbTag

    emptyPassphrase :: ByteString
    emptyPassphrase = mempty


--
--
-- ProtocolMAgic
--
--

