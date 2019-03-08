{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- These are (partial) CBOR decoders for blocks and block headers. Note that we
-- ignore most of the block's and header's content and only retrieve the pieces
-- of information relevant to us, wallet (we do assume a trusted node and
-- therefore, we needn't to care about verifying signatures and blocks
-- themselves).
--
-- The format described in the decoders below are the one used in the Byron era
-- of Cardano and will endure in the first stages of Shelley. They are also used
-- by components like the Rust <https://github.com/input-output-hk/cardano-http-bridge cardano-http-bridge>.

module Cardano.Wallet.Binary
    (
    -- * Decoding
      decodeBlock
    , decodeBlockHeader
    , decodeTx

    -- * Encoding
    , encodeTx

    -- * Hashing
    , txId

    -- * Helpers
    , inspectNextToken
    , decodeList
    , decodeListIndef
    ) where

import Prelude

import Cardano.Wallet.Primitive
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Hash (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    )
import Control.Monad
    ( void )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.ByteString
    ( ByteString )
import Data.Digest.CRC32
    ( crc32 )
import Data.Set
    ( Set )
import Data.Word
    ( Word16, Word64 )
import Debug.Trace
    ( traceShow )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set as Set


-- Decoding

decodeAddress :: CBOR.Decoder s Address
decodeAddress = do
    _ <- CBOR.decodeListLenCanonicalOf 2 -- CRC Protection Wrapper
    tag <- CBOR.decodeTag -- Mysterious hard-coded tag cardano-sl seems to so much like
    bytes <- CBOR.decodeBytes -- Addr Root + Attributes + Type
    crc <- CBOR.decodeWord32 -- CRC
    -- NOTE 1:
    -- Treating addresses as a blob here, so we just re-encode them as such
    -- Ultimately for us, addresses are nothing more than a bunch of bytes that
    -- we display in a Base58 format when we have to.
    --
    -- NOTE 2:
    -- We may want to check the CRC at this level as-well... maybe not.
    return $ Address $ CBOR.toStrictByteString $ mempty
        <> CBOR.encodeListLen 2
        <> CBOR.encodeTag tag
        <> CBOR.encodeBytes bytes
        <> CBOR.encodeWord32 crc

decodeAddressPayload :: CBOR.Decoder s ByteString
decodeAddressPayload = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    _ <- CBOR.decodeTag
    bytes <- CBOR.decodeBytes
    _ <- CBOR.decodeWord32 -- CRC
    return bytes

decodeAttributes :: CBOR.Decoder s ((), CBOR.Encoding)
decodeAttributes = do
    _ <- CBOR.decodeMapLenCanonical -- Empty map of attributes
    return ((), CBOR.encodeMapLen 0)

{-# ANN decodeBlock ("HLint: ignore Use <$>" :: String) #-}
decodeBlock :: CBOR.Decoder s Block
decodeBlock = do
    CBOR.decodeListLenCanonicalOf 2
    t <- CBOR.decodeWordCanonical
    case t of
        0 -> do -- Genesis Block
            _ <- CBOR.decodeListLenCanonicalOf 3
            h <- decodeGenesisBlockHeader
            -- NOTE
            -- We don't decode the body of genesis block because we don't need
            -- it. Genesis blocks occur at boundaries and contain various pieces
            -- of information about protocol updates, slot leader elections and
            -- delegation.
            -- Yet, they don't contain any transactions and so we can get away
            -- with a 'mempty' here.
            -- In theory, we should also:
            --
            -- _ <- decodeGenesisBlockBody
            return $ Block h mempty

        1 -> do -- Main Block
            _ <- CBOR.decodeListLenCanonicalOf 3
            h <- decodeMainBlockHeader
            txs <- decodeMainBlockBody
            -- _ <- decodeMainExtraData
            return $ Block h txs

        _ -> do
            fail $ "decodeBlock: unknown block constructor: " <> show t

decodeBlockHeader :: CBOR.Decoder s BlockHeader
decodeBlockHeader = do
    CBOR.decodeListLenCanonicalOf 2
    t <- CBOR.decodeWordCanonical
    case t of
      0 -> decodeGenesisBlockHeader
      1 -> decodeMainBlockHeader
      _ -> fail $ "decodeBlockHeader: unknown block header constructor: " <> show t

decodeBlockVersion :: CBOR.Decoder s ()
decodeBlockVersion = do
    _ <- CBOR.decodeListLenCanonicalOf 3
    _ <- CBOR.decodeWord16 -- Major
    _ <- CBOR.decodeWord16 -- Minor
    _ <- CBOR.decodeWord8  -- Patch
    return ()

decodeDataProof :: CBOR.Decoder s ()
decodeDataProof = do
    _ <- CBOR.decodeBytes -- Proof Hash
    return ()

decodeCertificatesProof :: CBOR.Decoder s ()
decodeCertificatesProof = do
    _ <- CBOR.decodeBytes -- Vss Certificates Hash
    return ()

decodeCommitmentsProof :: CBOR.Decoder s ()
decodeCommitmentsProof = do
    _ <- CBOR.decodeBytes -- Commitments Hash
    _ <- CBOR.decodeBytes -- Vss Certificates Hash
    return ()

decodeDifficulty :: CBOR.Decoder s ()
decodeDifficulty = do
    _ <- CBOR.decodeListLenCanonicalOf 1
    _ <- CBOR.decodeWord64
    return ()

decodeGenesisBlockHeader :: CBOR.Decoder s BlockHeader
decodeGenesisBlockHeader = do
    _ <- CBOR.decodeListLenCanonicalOf 5
    _ <- decodeProtocolMagic
    previous <- decodePreviousBlockHeader
    _ <- decodeGenesisProof
    epoch <- decodeGenesisConsensusData
    _ <- decodeGenesisExtraData
    -- NOTE
    -- Careful here, we do return a slot number of 0, which means that if we
    -- naively parse all blocks from an epoch, two of them will have a slot
    -- number of `0`. In practices, when parsing a full epoch, we can discard
    -- the genesis block entirely and we won't bother about modelling this
    -- extra complexity at the type-level. That's a bit dodgy though.
    return $ BlockHeader epoch 0 previous

decodeGenesisConsensusData :: CBOR.Decoder s Word64
decodeGenesisConsensusData = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    epoch <- CBOR.decodeWord64
    _ <- decodeDifficulty
    return epoch

decodeGenesisExtraData :: CBOR.Decoder s ()
decodeGenesisExtraData = do
    _ <- CBOR.decodeListLenCanonicalOf 1
    _ <- decodeAttributes
    return ()

decodeGenesisProof :: CBOR.Decoder s ()
decodeGenesisProof = do
    _ <- CBOR.decodeBytes -- Slot Leaders Hash
    return ()

decodeHeavyIndex :: CBOR.Decoder s ()
decodeHeavyIndex = do
    _ <- CBOR.decodeWord64 -- Epoch Index
    return ()

decodeLeaderKey :: CBOR.Decoder s ()
decodeLeaderKey = do
    _ <- CBOR.decodeBytes
    return ()

decodeLightIndex :: CBOR.Decoder s ()
decodeLightIndex = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    _ <- CBOR.decodeWord64 -- Epoch Index #1
    _ <- CBOR.decodeWord64 -- Epoch Index #2
    return ()

decodeMainBlockBody :: CBOR.Decoder s (Set Tx)
decodeMainBlockBody = do
    _ <- CBOR.decodeListLenCanonicalOf 4
    decodeTxPayload
    -- NOTE:
    -- Would remain after that:
    --  - SscPayload
    --  - DlsPayload
    --  - UpdatePayload

decodeMainBlockHeader :: CBOR.Decoder s BlockHeader
decodeMainBlockHeader = do
    _ <- CBOR.decodeListLenCanonicalOf 5
    _ <- decodeProtocolMagic
    previous <- decodePreviousBlockHeader
    _ <- decodeMainProof
    (epoch, slot) <- decodeMainConsensusData
    _ <- decodeMainExtraData
    return $ BlockHeader epoch slot previous

decodeMainConsensusData :: CBOR.Decoder s (Word64, Word16)
decodeMainConsensusData = do
    _ <- CBOR.decodeListLenCanonicalOf 4
    slot <- decodeSlotId
    _ <- decodeLeaderKey
    _ <- decodeDifficulty
    _ <- decodeSignature
    return slot

decodeMainExtraData :: CBOR.Decoder s ()
decodeMainExtraData = do
    _ <- CBOR.decodeListLenCanonicalOf 4
    _ <- decodeBlockVersion
    _ <- decodeSoftwareVersion
    _ <- decodeAttributes
    _ <- decodeDataProof
    return ()

decodeMainProof :: CBOR.Decoder s ()
decodeMainProof = do
    CBOR.decodeListLenCanonicalOf 4
    decodeTxProof
    decodeMpcProof
    decodeProxySKsProof
    decodeUpdateProof

-- Multi-party computation proof
decodeMpcProof :: CBOR.Decoder s ()
decodeMpcProof = do
    _ <- CBOR.decodeListLenCanonical
    t <- CBOR.decodeWord8
    case t of
      0 -> decodeCommitmentsProof
      1 -> decodeOpeningsProof
      2 -> decodeSharesProof
      3 -> decodeCertificatesProof
      _ -> fail $ "decodeMpcProof: unknown proof constructor: " <> show t

decodeOpeningsProof :: CBOR.Decoder s ()
decodeOpeningsProof = do
    _ <- CBOR.decodeBytes -- Openings Hash
    _ <- CBOR.decodeBytes -- Vss Certificates Hash
    return ()

decodePreviousBlockHeader :: CBOR.Decoder s (Hash "BlockHeader")
decodePreviousBlockHeader = Hash <$> CBOR.decodeBytes

decodeProtocolMagic :: CBOR.Decoder s ()
decodeProtocolMagic = do
    _ <- CBOR.decodeInt32
    return ()

decodeProxySignature
    :: (forall x. CBOR.Decoder x ())
    -> CBOR.Decoder s ()
decodeProxySignature decodeIndex = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    _ <- decodeProxySecretKey
    _ <- CBOR.decodeBytes -- Proxy Signature
    return ()
  where
    decodeProxySecretKey :: CBOR.Decoder s ()
    decodeProxySecretKey = do
        _ <- CBOR.decodeListLenCanonicalOf 4
        _ <- decodeIndex
        _ <- CBOR.decodeBytes -- Issuer Public Key
        _ <- CBOR.decodeBytes -- Delegate Public Key
        _ <- CBOR.decodeBytes -- Proxy Certificate Key
        return ()

decodeProxySKsProof :: CBOR.Decoder s ()
decodeProxySKsProof = do
    _ <- CBOR.decodeBytes -- Dlg Payload Hash
    return ()

decodeSignature :: CBOR.Decoder s ()
decodeSignature = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    t <- CBOR.decodeWord8
    case t of
        0 -> void CBOR.decodeBytes
        1 -> decodeProxySignature decodeLightIndex
        2 -> decodeProxySignature decodeHeavyIndex
        _ -> fail $ "decodeSignature: unknown signature constructor: " <> show t

decodeSignedTx :: CBOR.Decoder s Tx
decodeSignedTx = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    _ <- CBOR.decodeListLenCanonicalOf 3
    ins <- decodeListIndef decodeTxIn
    outs <- decodeListIndef decodeTxOut
    _ <- decodeAttributes
    _ <- decodeList decodeTxWitness
    return $ Tx ins outs

decodeSharesProof :: CBOR.Decoder s ()
decodeSharesProof = do
    _ <- CBOR.decodeBytes -- Shares Hash
    _ <- CBOR.decodeBytes -- Vss Certificates Hash
    return ()

decodeSlotId :: CBOR.Decoder s (Word64, Word16)
decodeSlotId = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    epoch <- CBOR.decodeWord64
    slot <- CBOR.decodeWord16
    return (epoch, slot)

decodeSoftwareVersion :: CBOR.Decoder s ()
decodeSoftwareVersion = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    _ <- CBOR.decodeString -- Application Name
    _ <- CBOR.decodeWord32 -- Software Version
    return ()

decodeTx :: CBOR.Decoder s Tx
decodeTx = do
    _ <- CBOR.decodeListLenCanonicalOf 3
    ins <- decodeListIndef decodeTxIn
    outs <- decodeListIndef decodeTxOut
    _ <- decodeAttributes
    return $ Tx ins outs

decodeTxPayload :: CBOR.Decoder s (Set Tx)
decodeTxPayload = Set.fromList <$> decodeListIndef decodeSignedTx

{-# ANN decodeTxIn ("HLint: ignore Use <$>" :: String) #-}
decodeTxIn :: CBOR.Decoder s TxIn
decodeTxIn = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    t <- CBOR.decodeWord8
    case t of
        0 -> do
            _tag <- CBOR.decodeTag
            bytes <- CBOR.decodeBytes
            case CBOR.deserialiseFromBytes decodeTxIn' (BL.fromStrict bytes) of
                Left err -> fail $ show err
                Right (_, input) -> return input
        _ -> fail $ "decodeTxIn: unknown tx input constructor: " <> show t
  where
    decodeTxIn' :: CBOR.Decoder s TxIn
    decodeTxIn' = do
        _ <- CBOR.decodeListLenCanonicalOf 2
        tx <- Hash <$> CBOR.decodeBytes
        index <- CBOR.decodeWord32
        return $ TxIn tx index

{-# ANN decodeTxOut ("HLint: ignore Use <$>" :: String) #-}
decodeTxOut :: CBOR.Decoder s TxOut
decodeTxOut = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    addr <- decodeAddress
    c <- CBOR.decodeWord64
    return $ TxOut addr (Coin c)

decodeTxProof :: CBOR.Decoder s ()
decodeTxProof = do
    CBOR.decodeListLenCanonicalOf 3
    _ <- CBOR.decodeWord32 -- Number
    _ <- CBOR.decodeBytes  -- Merkle Root Hash
    _ <- CBOR.decodeBytes  -- Witnesses Hash
    return ()

decodeTxWitness :: CBOR.Decoder s ()
decodeTxWitness = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    t <- CBOR.decodeWord8
    void $ case t of
        0 -> CBOR.decodeTag *> CBOR.decodeBytes -- PKWitness
        1 -> CBOR.decodeTag *> CBOR.decodeBytes -- Script Witness
        2 -> CBOR.decodeTag *> CBOR.decodeBytes -- Redeem Witness
        _ -> fail $ "decodeTxWitness: unknown tx witness constructor: " <> show t

decodeUpdateProof :: CBOR.Decoder s ()
decodeUpdateProof = do
    _ <- CBOR.decodeBytes -- Update Hash
    return ()


-- * Encoding

encodeAddressPayload :: ByteString -> CBOR.Encoding
encodeAddressPayload payload = mempty
    <> CBOR.encodeListLen 2
    <> CBOR.encodeTag 24 -- Hard-Coded Tag value in cardano-sl
    <> CBOR.encodeBytes payload
    <> CBOR.encodeWord32 (crc32 payload)

encodeTx :: Tx -> CBOR.Encoding
encodeTx tx = mempty
    <> CBOR.encodeListLen 3
    <> CBOR.encodeListLenIndef
    <> mconcat (encodeTxIn <$> inputs tx)
    <> CBOR.encodeBreak
    <> CBOR.encodeListLenIndef
    <> mconcat (encodeTxOut <$> outputs tx)
    <> CBOR.encodeBreak
    <> encodeTxAttributes

encodeTxAttributes :: CBOR.Encoding
encodeTxAttributes = mempty
    <> CBOR.encodeMapLen 0

encodeTxIn :: TxIn -> CBOR.Encoding
encodeTxIn (TxIn (Hash txid) ix) = mempty
    <> CBOR.encodeListLen 2
    <> CBOR.encodeWord8 0
    <> CBOR.encodeTag 24 -- Hard-coded Tag value in cardano-sl
    <> CBOR.encodeBytes bytes
  where
    bytes = CBOR.toStrictByteString $ mempty
        <> CBOR.encodeListLen 2
        <> CBOR.encodeBytes txid
        <> CBOR.encodeWord32 ix

encodeTxOut :: TxOut -> CBOR.Encoding
encodeTxOut (TxOut (Address addr) (Coin c)) = mempty
    <> CBOR.encodeListLen 2
    <> encodeAddressPayload payload
    <> CBOR.encodeWord64 c
  where
    invariant =
        error $ "encodeTxOut: unable to decode address payload: " <> show addr
    payload =
        either (const invariant) snd $ CBOR.deserialiseFromBytes
            decodeAddressPayload
            (BL.fromStrict addr)

-- * Hashing

-- | Compute a transaction id; assumed to be effectively injective.
-- It returns an hex-encoded 64-byte hash.
--
-- NOTE: This is a rather expensive operation
txId :: Tx -> Hash "Tx"
txId = blake2b256 . encodeTx


-- * Helpers

-- | Inspect the next token that has to be decoded and print it to the console
-- as a trace. Useful for debugging Decoders.
-- Example:
--
-- @
--     myDecoder :: CBOR.Decoder s MyType
--     myDecoder = do
--         a <- CBOR.decodeWord64
--         inspectNextToken
--         [...]
-- @
inspectNextToken :: CBOR.Decoder s ()
inspectNextToken =
  CBOR.peekTokenType >>= flip traceShow (return ())

-- | Decode an list of known length. Very similar to @decodeListIndef@.
--
-- @
--     myDecoder :: CBOR.Decoder s [MyType]
--     myDecoder = decodeList decodeOne
--       where
--         decodeOne :: CBOR.Decoder s MyType
-- @
decodeList :: forall s a . CBOR.Decoder s a -> CBOR.Decoder s [a]
decodeList decodeOne = do
    l <- CBOR.decodeListLenCanonical
    CBOR.decodeSequenceLenN (flip (:)) [] reverse l decodeOne

-- | Decode an arbitrary long list. CBOR introduce a "break" character to
-- mark the end of the list, so we simply decode each item until we encounter
-- a break character.
--
-- @
--     myDecoder :: CBOR.Decoder s [MyType]
--     myDecoder = decodeListIndef decodeOne
--       where
--         decodeOne :: CBOR.Decoder s MyType
-- @
decodeListIndef :: forall s a. CBOR.Decoder s a -> CBOR.Decoder s [a]
decodeListIndef decodeOne = do
    _ <- CBOR.decodeListLenIndef
    CBOR.decodeSequenceLenIndef (flip (:)) [] reverse decodeOne

-- | Encode a value to a corresponding Hash.
--
-- @
--     txId :: Tx -> Hash "Tx"
--     txId = blake2b256 . encodeTx
-- @
blake2b256 :: forall tag. CBOR.Encoding -> Hash tag
blake2b256 =
    Hash . BA.convert . hash @_ @Blake2b_256 . CBOR.toStrictByteString
