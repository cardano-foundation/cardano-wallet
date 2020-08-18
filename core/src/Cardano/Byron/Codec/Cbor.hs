-- need this for {-# HLINT ... #-}; see https://github.com/ndmitchell/hlint#ignoring-hints
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- These are (partial) CBOR decoders for Byron binary types. Note that we
-- ignore most of the block's and header's content and only retrieve the pieces
-- of information relevant to us, wallet (we do assume a trusted node and
-- therefore, we needn't to care about verifying signatures and blocks
-- themselves).
--
-- The format described in the decoders below are the one used in the Byron era
-- of Cardano and will endure in the first stages of Shelley. They are also used
-- by components like the Rust <https://github.com/input-output-hk/cardano-http-bridge cardano-http-bridge>.

module Cardano.Byron.Codec.Cbor
    (
    -- * Decoding
      decodeAddressDerivationPath
    , decodeAddressPayload
    , decodeAllAttributes
    , decodeDerivationPathAttr
    , decodeSignedTx
    , decodeTx
    , decodeTxWitness
    , decodeProtocolMagicAttr

    -- * Encoding
    , encodeAddress
    , encodeAttributes
    , encodeDerivationPathAttr
    , encodeProtocolMagicAttr
    , encodePublicKeyWitness
    , encodeTx
    , encodeSignedTx
    , encodeTxWitness

    -- * Helpers
    , deserialiseCbor
    , inspectNextToken
    , decodeList
    , decodeListIndef
    , decodeNestedBytes
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub, xpubToBytes )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..), Passphrase (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , ProtocolMagic (..)
    , TxIn (..)
    , TxOut (..)
    )
import Control.Monad
    ( replicateM, when )
import Control.Monad.Fail
    ( MonadFail )
import Crypto.Error
    ( CryptoError (..), CryptoFailable (..) )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_224, SHA3_256 )
import Data.ByteString
    ( ByteString )
import Data.Digest.CRC32
    ( crc32 )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.List
    ( find )
import Data.Word
    ( Word8 )
import Debug.Trace
    ( traceShow )

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Crypto.Cipher.ChaChaPoly1305 as Poly
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

{-------------------------------------------------------------------------------
                       Byron Address Binary Format

In the composition of a Cardano address, the following functions concern the
"Derivation Path" box.

+-------------------------------------------------------------------------------+
|                                                                               |
|                        CBOR-Serialized Object with CRC¹                       |
|                                                                               |
+-------------------------------------------------------------------------------+
                                        |
                                        |
                                        v
+-------------------------------------------------------------------------------+
|     Address Root    |     Address Attributes    |           AddrType          |
|                     |                           |                             |
|   Hash (224 bits)   |  Der. Path² + Stake + NM  |  PubKey | (Script) | Redeem |
|                     |    (open for extension)   |     (open for extension)    |
+-------------------------------------------------------------------------------+
             |                 |
             |                 |     +----------------------------------+
             v                 |     |        Derivation Path           |
+---------------------------+  |---->|                                  |
| SHA3-256                  |  |     | ChaChaPoly⁴ AccountIx/AddressIx  |
|   |> Blake2b 224          |  |     +----------------------------------+
|   |> CBOR                 |  |
|                           |  |
|  -AddrType                |  |     +----------------------------------+
|  -ASD³ (~AddrType+PubKey) |  |     |       Stake Distribution         |
|  -Address Attributes      |  |     |                                  |
+---------------------------+  |---->|  BootstrapEra | (Single | Multi) |
                               |     +----------------------------------+
                               |
                               |
                               |     +----------------------------------+
                               |     |          Network Magic           |
                               |---->|                                  |
                                     | Addr Discr: MainNet vs TestNet   |
                                     +----------------------------------+

-------------------------------------------------------------------------------}

decodeAddress :: CBOR.Decoder s Address
decodeAddress = do
    _ <- CBOR.decodeListLenCanonicalOf 2
        -- CRC Protection Wrapper
    tag <- CBOR.decodeTag
        -- Mysterious hard-coded tag cardano-sl seems to so much like
    bytes <- CBOR.decodeBytes
        -- Addr Root + Attributes + Type
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

decodeAddressDerivationPath
    :: Passphrase "addr-derivation-payload"
    -> CBOR.Decoder s (Maybe
        ( Index 'WholeDomain 'AccountK
        , Index 'WholeDomain 'AddressK
        ))
decodeAddressDerivationPath pwd = do
    _ <- CBOR.decodeListLenCanonicalOf 3
    _ <- CBOR.decodeBytes
    path <- decodeAllAttributes >>= decodeDerivationPathAttr pwd
    addrType <- CBOR.decodeWord8 -- Type
    when (addrType /= 0) $
        fail $ mconcat
            [ "decodeAddressDerivationPath: type is not 0 (public key), it is "
            , show addrType
            ]
    pure path

decodeProtocolMagicAttr
    :: CBOR.Decoder s (Maybe ProtocolMagic)
decodeProtocolMagicAttr = do
    _ <- CBOR.decodeListLenCanonicalOf 3
    _ <- CBOR.decodeBytes
    attrs <- decodeAllAttributes
    case find ((== 2) . fst) attrs of
        Nothing -> pure Nothing
        Just (_, bytes) -> case deserialiseCbor decodeProtocolMagic bytes of
            Nothing -> fail "unable to decode attribute into protocol magic"
            Just pm -> pure (Just pm)

decodeEmptyAttributes :: CBOR.Decoder s ((), CBOR.Encoding)
decodeEmptyAttributes = do
    _ <- CBOR.decodeMapLenCanonical -- Empty map of attributes
    return ((), CBOR.encodeMapLen 0)

-- | The attributes are pairs of numeric tags and bytes, where the bytes will be
-- CBOR-encoded stuff. This decoder does not enforce "canonicity" of entries.
decodeAllAttributes
    :: CBOR.Decoder s [(Word8, ByteString)]
decodeAllAttributes = do
    n <- CBOR.decodeMapLenCanonical -- Address Attributes length
    replicateM n decodeAttr
  where
    decodeAttr = (,) <$> CBOR.decodeWord8 <*> CBOR.decodeBytes

decodeDerivationPathAttr
    :: Passphrase "addr-derivation-payload"
    -> [(Word8, ByteString)]
    -> CBOR.Decoder s (Maybe
        ( Index 'WholeDomain 'AccountK
        , Index 'WholeDomain 'AddressK
        ))
decodeDerivationPathAttr pwd attrs = do
    case lookup derPathTag attrs of
        Just payload -> do
            decodeNestedBytes decoder payload
        Nothing -> fail $ mconcat
            [ "decodeDerivationPathAttr: Missing attribute "
            , show derPathTag
            ]
  where
    derPathTag = 1
    decoder :: CBOR.Decoder s (Maybe
        ( Index 'WholeDomain 'AccountK
        , Index 'WholeDomain 'AddressK
        ))
    decoder = do
        bytes <- CBOR.decodeBytes
        case decryptDerivationPath pwd bytes of
            CryptoPassed plaintext ->
                Just <$> decodeNestedBytes decodeDerivationPath plaintext
            CryptoFailed _ ->
                pure Nothing

-- Opposite of 'encodeDerivationPath'.
decodeDerivationPath
    :: CBOR.Decoder s
        ( Index 'WholeDomain 'AccountK
        , Index 'WholeDomain 'AddressK
        )
decodeDerivationPath = do
    ixs <- decodeListIndef CBOR.decodeWord32
    case ixs of
        [acctIx, addrIx] ->
            pure (toEnum $ fromIntegral acctIx, toEnum $ fromIntegral addrIx)
        _ ->
            fail $ mconcat
                [ "decodeDerivationPath: invalid derivation path payload: "
                , "expected two indexes but got: "
                , show ixs
                ]

decodeProtocolMagic :: CBOR.Decoder s ProtocolMagic
decodeProtocolMagic = ProtocolMagic <$> CBOR.decodeInt32

decodeSignedTx :: CBOR.Decoder s (([TxIn], [TxOut]), [ByteString])
decodeSignedTx = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    tx <- decodeTx
    witnesses <- decodeList decodeTxWitness
    return (tx, witnesses)

decodeTx :: CBOR.Decoder s ([TxIn], [TxOut])
decodeTx = do
    _ <- CBOR.decodeListLenCanonicalOf 3
    ins <- decodeListIndef decodeTxIn
    outs <- decodeListIndef decodeTxOut
    _ <- decodeEmptyAttributes
    return (ins, outs)

{-# HLINT ignore decodeTxIn "Use <$>" #-}
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
        TxIn tx <$> CBOR.decodeWord32

{-# HLINT ignore decodeTxOut "Use <$>" #-}
decodeTxOut :: CBOR.Decoder s TxOut
decodeTxOut = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    addr <- decodeAddress
    TxOut addr . Coin <$> CBOR.decodeWord64

decodeTxWitness :: CBOR.Decoder s ByteString
decodeTxWitness = do
    _ <- CBOR.decodeListLenCanonicalOf 2
    t <- CBOR.decodeWord8
    _ <- CBOR.decodeTag
    case t of
        0 -> CBOR.decodeBytes
        1 -> CBOR.decodeBytes
        2 -> CBOR.decodeBytes
        _ -> fail
            $ "decodeTxWitness: unknown tx witness constructor: " <> show t

-- * Encoding

-- | Encode a public key to a corresponding Cardano Address. The encoding of the
-- attributes part of an address is left out to the caller; This allows for
-- distinguishing between Sequential and Random addresses (the former doesn't
-- have any attributes to encode).
--
-- @
-- -- Old / Random Addresses
-- let encodeAddrAttributes = mempty
--      <> CBOR.encodeMapLen 1
--      <> CBOR.encodeWord8 1
--      <> encodeDerivationPath (hdPassphrase rootXPub) accIx addrIx
-- let addr = encodeAddress xpub encodeAddrAttributes
--
-- -- New / Sequential Addresses
-- let encodeAddrAttributes = mempty <> CBOR.encodeMapLen 0
-- let addr = encodeAddress xpub encodeAddrAttributes
-- @
--
-- Note that we are passing the behavior to encode attributes as a parameter
-- here and do not handle multiple cases in 'encodeAddress' itself for multiple
-- reasons:
--
-- - Inversion of control gives us a nicer implementation overall
--
-- - Encoding attributes for Random addresses requires more context than just
--   the public key (like the wallet root id and some extra logic for encoding
--   passphrases). This is just scheme-specific and is better left out of this
--   particular function
encodeAddress :: XPub -> [CBOR.Encoding] -> CBOR.Encoding
encodeAddress xpub attrs =
    encodeAddressPayload payload
  where
    blake2b224 = hash @_ @Blake2b_224
    sha3256 = hash @_ @SHA3_256
    payload = CBOR.toStrictByteString $ mempty
        <> CBOR.encodeListLen 3
        <> CBOR.encodeBytes root
        <> encodeAttributes attrs
        <> CBOR.encodeWord8 0 -- Address Type, 0 = Public Key
    root = BA.convert $ blake2b224 $ sha3256 $ CBOR.toStrictByteString $ mempty
        <> CBOR.encodeListLen 3
        <> CBOR.encodeWord8 0 -- Address Type, 0 = Public Key
        <> encodeSpendingData
        <> encodeAttributes attrs
    encodeXPub =
        CBOR.encodeBytes (xpubToBytes xpub)
    encodeSpendingData = CBOR.encodeListLen 2
        <> CBOR.encodeWord8 0
        <> encodeXPub

encodeAddressPayload :: ByteString -> CBOR.Encoding
encodeAddressPayload payload = mempty
    <> CBOR.encodeListLen 2
    <> CBOR.encodeTag 24 -- Hard-Coded Tag value in cardano-sl
    <> CBOR.encodeBytes payload
    <> CBOR.encodeWord32 (crc32 payload)

encodeAttributes :: [CBOR.Encoding] -> CBOR.Encoding
encodeAttributes attrs = CBOR.encodeMapLen l <> mconcat attrs
  where
    l = fromIntegral (length attrs)

encodeProtocolMagicAttr :: ProtocolMagic -> CBOR.Encoding
encodeProtocolMagicAttr pm = mempty
    <> CBOR.encodeWord 2 -- Tag for 'ProtocolMagic' attribute
    <> CBOR.encodeBytes (CBOR.toStrictByteString $ encodeProtocolMagic pm)

-- This is the opposite of 'decodeDerivationPathAttr'.
--
-- NOTE: The caller must ensure that the passphrase length is 32 bytes.
encodeDerivationPathAttr
    :: Passphrase "addr-derivation-payload"
    -> Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> CBOR.Encoding
encodeDerivationPathAttr pwd acctIx addrIx = mempty
    <> CBOR.encodeWord8 1 -- Tag for 'DerivationPath' attribute
    <> CBOR.encodeBytes (encryptDerivationPath pwd path)
  where
    path = encodeDerivationPath acctIx addrIx

encodeDerivationPath
    :: Index 'WholeDomain 'AccountK
    -> Index 'WholeDomain 'AddressK
    -> CBOR.Encoding
encodeDerivationPath (Index acctIx) (Index addrIx) = mempty
    <> CBOR.encodeListLenIndef
    <> CBOR.encodeWord32 acctIx
    <> CBOR.encodeWord32 addrIx
    <> CBOR.encodeBreak

encodePublicKeyWitness :: XPub -> ByteString -> CBOR.Encoding
encodePublicKeyWitness xpub signatur = mempty
    <> CBOR.encodeListLen 2
    <> CBOR.encodeBytes (xpubToBytes xpub)
    <> CBOR.encodeBytes signatur

encodeTx :: ([TxIn], [TxOut]) -> CBOR.Encoding
encodeTx (inps, outs) = mempty
    <> CBOR.encodeListLen 3
    <> CBOR.encodeListLenIndef
    <> mconcat (encodeTxIn <$> inps)
    <> CBOR.encodeBreak
    <> CBOR.encodeListLenIndef
    <> mconcat (encodeTxOut <$> outs)
    <> CBOR.encodeBreak
    <> encodeTxAttributes

encodeSignedTx :: ([TxIn], [TxOut]) -> [ByteString] -> CBOR.Encoding
encodeSignedTx tx witnesses = mempty
    <> CBOR.encodeListLen 2
    <> encodeTx tx
    <> CBOR.encodeListLen (fromIntegral $ length witnesses)
    <> mconcat (map encodeTxWitness witnesses)

encodeTxWitness :: ByteString -> CBOR.Encoding
encodeTxWitness bytes = mempty
    <> CBOR.encodeListLen 2
    <> CBOR.encodeWord8 tag
    <> CBOR.encodeTag 24
    <> CBOR.encodeBytes bytes
  where
    -- NOTE
    -- We only support 'PublicKey' witness types at the moment. However,
    -- Byron nodes support more:
    --
    --   * 0 for Public Key
    --   * 1 for Script
    --   * 2 for Redeem
    tag = 0

encodeTxAttributes :: CBOR.Encoding
encodeTxAttributes = mempty
    <> CBOR.encodeMapLen 0

encodeProtocolMagic :: ProtocolMagic -> CBOR.Encoding
encodeProtocolMagic (ProtocolMagic i) = CBOR.encodeInt32 i

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

{-------------------------------------------------------------------------------
                    HD payload encryption and authentication
-------------------------------------------------------------------------------}

-- | Hard-coded nonce from the legacy code-base.
cardanoNonce :: ByteString
cardanoNonce = "serokellfore"

-- | ChaCha20/Poly1305 encrypting and signing the HD payload of addresses.
--
-- NOTE: The caller must ensure that the passphrase length is 32 bytes.
encryptDerivationPath
    :: Passphrase "addr-derivation-payload"
       -- ^ Symmetric key / passphrase, 32-byte long
    -> CBOR.Encoding
        -- ^ Payload to be encrypted
    -> ByteString
        -- ^ Ciphertext with a 128-bit crypto-tag appended.
encryptDerivationPath (Passphrase passphrase) payload = unsafeSerialize $ do
    nonce <- Poly.nonce12 cardanoNonce
    st1 <- Poly.finalizeAAD <$> Poly.initialize passphrase nonce
    let (out, st2) = Poly.encrypt (CBOR.toStrictByteString payload) st1
    return $ out <> BA.convert (Poly.finalize st2)
  where
    unsafeSerialize :: CryptoFailable ByteString -> ByteString
    unsafeSerialize =
        CBOR.toStrictByteString . CBOR.encodeBytes . useInvariant

    -- Encryption will fail if the key is the wrong size, but that won't happen
    -- if the key was created with 'generateKeyFromSeed'.
    useInvariant = \case
        CryptoPassed res -> res
        CryptoFailed err -> error $ "encodeAddressKey: " ++ show err

-- | ChaCha20/Poly1305 decrypting and authenticating the HD payload of
-- addresses.
decryptDerivationPath
    :: Passphrase "addr-derivation-payload"
       -- ^ Symmetric key / passphrase, 32-byte long
    -> ByteString
        -- ^ Payload to be decrypted
    -> CryptoFailable ByteString
decryptDerivationPath (Passphrase passphrase) bytes = do
    let (payload, tag) = BS.splitAt (BS.length bytes - 16) bytes
    nonce <- Poly.nonce12 cardanoNonce
    st1 <- Poly.finalizeAAD <$> Poly.initialize passphrase nonce
    let (out, st2) = Poly.decrypt payload st1
    when (BA.convert (Poly.finalize st2) /= tag) $
        CryptoFailed CryptoError_MacKeyInvalid
    return out

{-------------------------------------------------------------------------------
                                Helpers
-------------------------------------------------------------------------------}

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

-- | Byron CBOR encodings often have CBOR nested in CBOR. This helps decoding
-- a particular 'ByteString' that represents a CBOR object.
decodeNestedBytes
    :: MonadFail m
    => (forall s. CBOR.Decoder s r)
    -> ByteString
    -> m r
decodeNestedBytes dec bytes =
    case CBOR.deserialiseFromBytes dec (BL.fromStrict bytes) of
        Right ("", res) ->
            pure res
        Right _ ->
            fail "Leftovers when decoding nested bytes"
        _ ->
            fail "Could not decode nested bytes"

-- | Shortcut for deserialising a strict 'Bytestring' with the given decoder.
deserialiseCbor
    :: (forall s. CBOR.Decoder s a)
    -> ByteString
    -> Maybe a
deserialiseCbor dec =
    fmap snd . eitherToMaybe . CBOR.deserialiseFromBytes dec . BL.fromStrict
