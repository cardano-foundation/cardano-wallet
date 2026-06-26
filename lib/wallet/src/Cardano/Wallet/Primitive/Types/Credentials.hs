{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Primitive.Types.Credentials
    ( RootCredentials (..)
    , HashedCredentials (..)
    , ClearCredentials
    ) where

import Cardano.Address.Derivation
    ( XPrv
    )
import Cardano.Crypto.WalletHD.Encrypted
    ( EncryptedKey
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (RootK)
    )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase
    , PassphraseHash
    )
import Prelude

-- | A 'PrivateKey' for a given 'KeyFlavor'.
data RootCredentials k pw = RootCredentials
    { credentialsKey :: k 'RootK XPrv
    , credentialsPassword :: pw
    }

deriving instance
    (Eq (k 'RootK XPrv), Eq pw) => Eq (RootCredentials k pw)
deriving instance
    (Show (k 'RootK XPrv), Show pw) => Show (RootCredentials k pw)

-- | Stored credentials for a wallet root key.
--
-- 'HashedCredentialsV1' uses the legacy PBKDF2\/Scrypt passphrase-hash
-- scheme; present only for wallets that have not yet been migrated.
--
-- 'HashedCredentialsV2' stores a PBKDF2-encrypted root key for key
-- operations alongside an Argon2id+XChaCha20-Poly1305 AEAD envelope
-- for passphrase authentication.  The AEAD tag replaces the
-- 'PassphraseHash'; the stored key remains PBKDF2-encrypted so that
-- all key-operation callers use the same 'preparePassphrase' path.
data HashedCredentials k
    = HashedCredentialsV1 !(k 'RootK XPrv) !PassphraseHash
    | HashedCredentialsV2 !(k 'RootK XPrv) !EncryptedKey

deriving instance (Eq (k 'RootK XPrv)) => Eq (HashedCredentials k)
deriving instance (Show (k 'RootK XPrv)) => Show (HashedCredentials k)

type ClearCredentials k = RootCredentials k (Passphrase "encryption")
