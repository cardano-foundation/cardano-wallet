{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

-- | Operations for saving a private key into a database, and restoring it from
-- a database. The keys should be encoded as hexadecimal strings.
module Cardano.Wallet.Address.Keys.PersistPrivateKey
    ( serializeXPrv
    , unsafeDeserializeXPrv
    ) where

import Cardano.Address.Derivation
    ( XPrv
    )
import Cardano.Crypto.Wallet
    ( unXPrv
    , xprv
    )
import Cardano.Crypto.WalletV2.Encrypted
    ( EncryptedKey
    , encryptedKey
    , unEncryptedKey
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (RootK)
    , fromHex
    , hex
    )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..)
    )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey (..)
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..)
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..)
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseHash (..)
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( HashedCredentials (..)
    )
import Data.ByteString
    ( ByteString
    )
import Prelude

import qualified Cardano.Wallet.Address.Derivation.Icarus as Icarus
import qualified Cardano.Wallet.Address.Derivation.SharedKey as Shared
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

-- | Convert a 'HashedCredentials' value into hexadecimal strings suitable for
-- storing in a text file or database column.
--
-- The first element of the pair is the key column; the second is the hash
-- column (empty for v2 keys, which are self-authenticating).
--
-- Serialization format
-- ====================
--
-- V1 non-Byron : @hex(unXPrv k)@  (exactly 256 hex chars = 128 bytes)
-- V1 Byron     : @hex(unXPrv k) ":" hex(payloadPass)@
--
-- V2 non-Byron : @hex(unEncryptedKey ekey)@  (> 256 hex chars, CBOR envelope)
-- V2 Byron     : @hex(unEncryptedKey ekey) ":" hex(payloadPass)@
--
-- V1 and V2 are distinguished by the length of the first colon-delimited
-- segment: exactly 256 hex chars ↔ V1 (128-byte XPrv), longer ↔ V2.
serializeXPrv
    :: KeyFlavorS k
    -> HashedCredentials k
    -> (ByteString, ByteString)
serializeXPrv kF = \case
    HashedCredentialsV1 k h -> serializeV1 kF k h
    HashedCredentialsV2 ekey mPayload -> serializeV2 kF ekey mPayload

serializeV1
    :: KeyFlavorS k
    -> k 'RootK XPrv
    -> PassphraseHash
    -> (ByteString, ByteString)
serializeV1 kF k h = (keyBytes, hex . getPassphraseHash $ h)
  where
    keyBytes = case kF of
        ByronKeyS ->
            let ByronKey xk _ (Passphrase p) = k
            in  hex (unXPrv xk) <> ":" <> hex p
        IcarusKeyS  -> hex . unXPrv . Icarus.getKey  $ k
        ShelleyKeyS -> hex . unXPrv . Shelley.getKey $ k
        SharedKeyS  -> hex . unXPrv . Shared.getKey  $ k

serializeV2
    :: KeyFlavorS k
    -> EncryptedKey
    -> Maybe (Passphrase "addr-derivation-payload")
    -> (ByteString, ByteString)
serializeV2 kF ekey mPayload = (keyBytes, "")
  where
    keyBytes = case (kF, mPayload) of
        (ByronKeyS, Just (Passphrase p)) ->
            hex (unEncryptedKey ekey) <> ":" <> hex p
        _ -> hex (unEncryptedKey ekey)

-- | The reverse of 'serializeXPrv'. Dies with 'error' if the inputs are not
-- valid hexadecimal strings or if the key is of the wrong length\/format.
unsafeDeserializeXPrv
    :: KeyFlavorS k
    -> (ByteString, ByteString)
    -> HashedCredentials k
unsafeDeserializeXPrv kF (keyCol, hashCol) = case kF of
    ByronKeyS   -> deserializeByron   keyCol hashCol
    IcarusKeyS  -> deserializeSimple  IcarusKey  keyCol hashCol
    ShelleyKeyS -> deserializeSimple  ShelleyKey keyCol hashCol
    SharedKeyS  -> deserializeSimple  SharedKey  keyCol hashCol

-- | Detect whether the key column encodes a V1 (128-byte) or V2 (CBOR) key.
-- We split on ':' and check the length of the first segment: exactly 256 hex
-- chars means 128 bytes = V1.
isV1KeySegment :: ByteString -> Bool
isV1KeySegment seg = BS.length seg == 256

-- Deserialize a non-Byron key (Icarus / Shelley / Shared).
-- V1: @hex(xprv)@        → HashedCredentialsV1 (wrap xprv) hash
-- V2: @hex(ekey cbor)@   → HashedCredentialsV2 ekey Nothing
deserializeSimple
    :: (XPrv -> k 'RootK XPrv)
    -> ByteString
    -> ByteString
    -> HashedCredentials k
deserializeSimple wrap keyCol hashCol
    | isV1KeySegment keyCol =
        case fromHex @ByteString keyCol of
            Left _    -> bad
            Right rawK -> case xprv rawK of
                Left _  -> bad
                Right k -> case fromHex @ByteString hashCol of
                    Left _     -> bad
                    Right rawH -> HashedCredentialsV1 (wrap k) (PassphraseHash (BA.convert rawH))
    | otherwise =
        case fromHex @ByteString keyCol of
            Left _    -> bad
            Right rawE -> case encryptedKey rawE of
                Left _     -> bad
                Right ekey -> HashedCredentialsV2 ekey Nothing
  where
    bad = error "unsafeDeserializeXPrv: unable to deserialize key"

-- Deserialize a Byron key.
-- V1: @hex(xprv):hex(payload)@ → HashedCredentialsV1 (ByronKey xprv () payload) hash
-- V2: @hex(ekey):hex(payload)@ → HashedCredentialsV2 ekey (Just payload)
deserializeByron :: ByteString -> ByteString -> HashedCredentials ByronKey
deserializeByron keyCol hashCol =
    case B8.split ':' keyCol of
        [keyHex, payloadHex]
            | isV1KeySegment keyHex ->
                case (fromHex @ByteString keyHex, fromHex @ByteString payloadHex) of
                    (Right rawK, Right rawP) -> case xprv rawK of
                        Left _  -> bad
                        Right k -> case fromHex @ByteString hashCol of
                            Left _     -> bad
                            Right rawH ->
                                let p = Passphrase (BA.convert rawP)
                                    h = PassphraseHash (BA.convert rawH)
                                in  HashedCredentialsV1 (ByronKey k () p) h
                    _ -> bad
            | otherwise ->
                case (fromHex @ByteString keyHex, fromHex @ByteString payloadHex) of
                    (Right rawE, Right rawP) -> case encryptedKey rawE of
                        Left _     -> bad
                        Right ekey ->
                            let p = Passphrase (BA.convert rawP)
                            in  HashedCredentialsV2 ekey (Just p)
                    _ -> bad
        _ -> bad
  where
    bad = error "unsafeDeserializeXPrv: unable to deserialize ByronKey"
