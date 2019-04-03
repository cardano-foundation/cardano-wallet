{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the wallet layer functions that are used by API layer and uses both
-- "Cardano.Wallet.DB" and "Cardano.Wallet.Network" to realize its role as being
-- intermediary between the three.


module Cardano.Wallet where

import Prelude

import Cardano.Wallet.Binary
    ( TxWitness (..), toByteString )
import Cardano.Wallet.DB
    ( DBLayer (..), PrimaryKey (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..), listen )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Depth (RootK)
    , Key (..)
    , Passphrase
    , XPrv
    , XPub
    , deriveAccountPrivateKey
    , generateKeyFromSeed
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPoolGap, SeqState (..), mkAddressPool )
import Cardano.Wallet.Primitive.Model
    ( Wallet, applyBlock, initWallet )
import Cardano.Wallet.Primitive.Types
    ( Address
    , Block (..)
    , Hash (..)
    , ProtocolMagic (..)
    , Tx (..)
    , TxIn
    , TxOut (TxOut)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    )
import Control.Monad
    ( forM )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( ExceptT, throwE )
import Data.ByteString
    ( ByteString )
import Data.List
    ( foldl' )
import GHC.Generics
    ( Generic )

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Encoding as CBOR

-- | Types
data WalletLayer s = WalletLayer
    { createWallet
        :: NewWallet
        -> ExceptT CreateWalletError IO WalletId
    , readWallet
        :: WalletId
        -> ExceptT ReadWalletError IO (Wallet s, WalletMetadata)
    , watchWallet
        :: WalletId
        -> IO ()
    }

data NewWallet = NewWallet
    { seed
        :: !(Passphrase "seed")
    , secondFactor
        :: !(Passphrase "generation")
    , name
        :: !WalletName
    , passphrase
        :: !(Passphrase "encryption")
    , gap
        :: !AddressPoolGap
    } deriving (Show, Generic)

-- | Errors occuring when fetching a wallet
newtype ReadWalletError
    = ErrReadWalletNotFound WalletId
    deriving (Eq, Show)

-- | Errors occuring when creating a wallet
newtype CreateWalletError
    = ErrCreateWalletIdAlreadyExists WalletId
    deriving (Eq, Show)


-- | Create a new instance of the wallet layer.
mkWalletLayer
    :: (Show e0)
    => DBLayer IO SeqState
    -> NetworkLayer IO e0 e1
    -> WalletLayer SeqState
mkWalletLayer db network = WalletLayer
    { createWallet = \w -> do
        let rootXPrv =
                generateKeyFromSeed (seed w, secondFactor w) (passphrase w)
        let accXPrv =
                deriveAccountPrivateKey mempty rootXPrv minBound
        let extPool =
                mkAddressPool (publicKey accXPrv) (gap w) ExternalChain []
        let intPool =
                mkAddressPool (publicKey accXPrv) minBound InternalChain []
        let wallet = initWallet $ SeqState
                { externalPool = extPool
                , internalPool = intPool
                }
        -- FIXME Compute the wallet id deterministically from the seed
        let wid = WalletId (read "00000000-0000-0000-0000-000000000000")
        liftIO (readCheckpoint db (PrimaryKey wid)) >>= \case
            Nothing -> do
                liftIO $ putCheckpoint db (PrimaryKey wid) wallet
                return wid
            Just _ ->
                throwE $ ErrCreateWalletIdAlreadyExists wid
    , readWallet = \wid -> liftIO (readCheckpoint db (PrimaryKey wid)) >>= \case
        Nothing ->
            throwE $ ErrReadWalletNotFound wid
        Just w ->
            return (w, error "FIXME: store and retrieve wallet metadata")

    , watchWallet = liftIO . listen network . applyBlocks
    }
  where
    applyBlocks :: WalletId -> [Block] -> IO ()
    applyBlocks wid blocks = do
        cp' <- readCheckpoint db (PrimaryKey wid) >>= \case
            Nothing ->
                fail $ "couldn't find worker wallet: " <> show wid
            Just cp -> do
                let nonEmpty = not . null . transactions
                return $ foldl' (flip applyBlock) cp (filter nonEmpty blocks)
        putCheckpoint db (PrimaryKey wid) cp'


newtype PassPhrase = PassPhrase ByteString -- ScrubbedBytes
data TxAux = TxAux Tx [TxWitness]
type TxOwnedInputs owner = [(owner, TxIn)]

-- | Build a transaction

-- | Construct a standard transaction
--
-- " Standard " here refers to the fact that we do not deal with redemption,
-- multisignature transactions, etc.
mkStdTx :: Monad m
        => ProtocolMagic
        -> (forall a. [a] -> m [a])
        -- ^ Shuffle function
        -> (Address -> Either e (Key 'RootK XPrv))
        -- ^ Signer for each input of the transaction
        -> [(TxIn, TxOut)]
        -- ^ Selected inputs
        -> [TxOut]
        -- ^ Selected outputs
        -> [TxOut]
        -- ^ Change outputs
        -> m (Either e TxAux)
mkStdTx pm shuffle signer inps outs change = do
    allOuts <- shuffle (outs ++ change)
    return $ makeMPubKeyTx pm signer' (fmap repack inps) allOuts
    where
         -- | Repacks a utxo-derived tuple into a format suitable for
         -- 'TxOwnedInputs'.
        repack :: (TxIn, TxOut) -> (TxOut, TxIn)
        repack (txIn, txOut) = (txOut, txIn)

        signer' (TxOut addr _) = signer addr


-- | Like 'makePubKeyTx', but allows usage of different signers
makeMPubKeyTx
    :: ProtocolMagic
    -> (owner -> Either e (Key 'RootK XPrv))
    -> TxOwnedInputs owner
    -> [TxOut]
    -> Either e TxAux
makeMPubKeyTx pm getSs ownedIns outs = do
    let ins = (fmap snd ownedIns)

    let tx = Tx ins outs
        txSigData = Hash "tx"
    txWitness <- forM ownedIns (\(addr, _) -> mkWit addr txSigData)
    pure $ TxAux tx txWitness

  where
    mkWit addr hash =
        getSs addr <&> \ss ->
            PublicKeyWitness
                (encode (publicKey ss))
                (Hash (signRaw pm (Just SignTx) ss hash))

    (<&>) = flip (<$>)

    encode :: (Key level XPub) -> ByteString
    encode (Key k) = CC.unXPub k


-- TODO: I removed FakeSigner/SafeSigner. Might be wrong.



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

