{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Cardano.Crypto.WalletHD.Encrypted
    ( EncryptedKey
    , mkEncryptedKey
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
-- V2 non-Byron : @"V2:" hex(unXPrv k) ":" hex(unEncryptedKey ekey)@
-- V2 Byron     : @"V2:" hex(unXPrv k) ":" hex(unEncryptedKey ekey) ":" hex(payloadPass)@
--
-- V1 and V2 are distinguished by the "V2:" prefix in the key column.
-- Within V1, Byron keys are distinguished by the presence of a single ":"
-- separator (the key segment is exactly 256 hex chars).
serializeXPrv
    :: KeyFlavorS k
    -> HashedCredentials k
    -> (ByteString, ByteString)
serializeXPrv kF = \case
    HashedCredentialsV1 k h -> serializeV1 kF k h
    HashedCredentialsV2 k ekey -> serializeV2 kF k ekey

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
        IcarusKeyS -> hex . unXPrv . Icarus.getKey $ k
        ShelleyKeyS -> hex . unXPrv . Shelley.getKey $ k
        SharedKeyS -> hex . unXPrv . Shared.getKey $ k

serializeV2
    :: KeyFlavorS k
    -> k 'RootK XPrv
    -> EncryptedKey
    -> (ByteString, ByteString)
serializeV2 kF k ekey = (keyBytes, "")
  where
    ekeyHex = hex (unEncryptedKey ekey)
    keyBytes = case kF of
        ByronKeyS ->
            let ByronKey xk _ (Passphrase p) = k
            in  "V2:" <> hex (unXPrv xk) <> ":" <> ekeyHex <> ":" <> hex p
        IcarusKeyS -> "V2:" <> hex (unXPrv (Icarus.getKey k)) <> ":" <> ekeyHex
        ShelleyKeyS -> "V2:" <> hex (unXPrv (Shelley.getKey k)) <> ":" <> ekeyHex
        SharedKeyS -> "V2:" <> hex (unXPrv (Shared.getKey k)) <> ":" <> ekeyHex

-- | The reverse of 'serializeXPrv'. Dies with 'error' if the inputs are not
-- valid hexadecimal strings or if the key is of the wrong length\/format.
unsafeDeserializeXPrv
    :: KeyFlavorS k
    -> (ByteString, ByteString)
    -> HashedCredentials k
unsafeDeserializeXPrv kF (keyCol, hashCol) = case kF of
    ByronKeyS -> deserializeByron keyCol hashCol
    IcarusKeyS -> deserializeSimple IcarusKey keyCol hashCol
    ShelleyKeyS -> deserializeSimple ShelleyKey keyCol hashCol
    SharedKeyS -> deserializeSimple SharedKey keyCol hashCol

-- | Detect whether a key segment (first colon-delimited field) is V1.
-- V1 XPrv hex is exactly 256 chars (128 bytes).
isV1KeySegment :: ByteString -> Bool
isV1KeySegment seg = BS.length seg == 256

-- Deserialize a non-Byron key (Icarus / Shelley / Shared).
-- V1: @hex(xprv)@                      → HashedCredentialsV1 (wrap xprv) hash
-- V2: @"V2:" hex(xprv) ":" hex(ekey)@  → HashedCredentialsV2 (wrap xprv) ekey
deserializeSimple
    :: (XPrv -> k 'RootK XPrv)
    -> ByteString
    -> ByteString
    -> HashedCredentials k
deserializeSimple wrap keyCol hashCol
    | "V2:" `BS.isPrefixOf` keyCol =
        case B8.split ':' (BS.drop 3 keyCol) of
            [keyHex, ekeyHex]
                | isV1KeySegment keyHex ->
                    case (fromHex @ByteString keyHex, fromHex @ByteString ekeyHex) of
                        (Right rawK, Right rawE) ->
                            case (xprv rawK, mkEncryptedKey rawE) of
                                (Right k, Right ekey) -> HashedCredentialsV2 (wrap k) ekey
                                _ -> bad
                        _ -> bad
            _ -> bad
    | isV1KeySegment keyCol =
        case fromHex @ByteString keyCol of
            Left _ -> bad
            Right rawK -> case xprv rawK of
                Left _ -> bad
                Right k -> case fromHex @ByteString hashCol of
                    Left _ -> bad
                    Right rawH -> HashedCredentialsV1 (wrap k) (PassphraseHash (BA.convert rawH))
    | otherwise = bad
  where
    bad = error "unsafeDeserializeXPrv: unable to deserialize key"

-- Deserialize a Byron key.
-- V1: @hex(xprv) ":" hex(payload)@
--     → HashedCredentialsV1 (ByronKey xprv () payload) hash
-- V2: @"V2:" hex(xprv) ":" hex(ekey) ":" hex(payload)@
--     → HashedCredentialsV2 (ByronKey xprv () payload) ekey
deserializeByron
    :: ByteString -> ByteString -> HashedCredentials ByronKey
deserializeByron keyCol hashCol
    | "V2:" `BS.isPrefixOf` keyCol =
        case B8.split ':' (BS.drop 3 keyCol) of
            [keyHex, ekeyHex, payloadHex]
                | isV1KeySegment keyHex ->
                    case ( fromHex @ByteString keyHex
                         , fromHex @ByteString ekeyHex
                         , fromHex @ByteString payloadHex
                         ) of
                        (Right rawK, Right rawE, Right rawP) ->
                            case (xprv rawK, mkEncryptedKey rawE) of
                                (Right k, Right ekey) ->
                                    let p = Passphrase (BA.convert rawP)
                                    in  HashedCredentialsV2 (ByronKey k () p) ekey
                                _ -> bad
                        _ -> bad
            _ -> bad
    | otherwise =
        case B8.split ':' keyCol of
            [keyHex, payloadHex]
                | isV1KeySegment keyHex ->
                    case (fromHex @ByteString keyHex, fromHex @ByteString payloadHex) of
                        (Right rawK, Right rawP) -> case xprv rawK of
                            Left _ -> bad
                            Right k -> case fromHex @ByteString hashCol of
                                Left _ -> bad
                                Right rawH ->
                                    let p = Passphrase (BA.convert rawP)
                                        h = PassphraseHash (BA.convert rawH)
                                    in  HashedCredentialsV1 (ByronKey k () p) h
                        _ -> bad
                | otherwise -> bad
            _ -> bad
  where
    bad = error "unsafeDeserializeXPrv: unable to deserialize ByronKey"
