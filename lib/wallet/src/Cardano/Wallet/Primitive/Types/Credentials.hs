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
import Cardano.Crypto.WalletV2.Encrypted
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
-- 'HashedCredentialsV2' uses a v2 Argon2id+XChaCha20-Poly1305 AEAD
-- envelope.  The 'PassphraseHash' is gone; the AEAD tag authenticates
-- the passphrase directly.  The @Maybe@ field carries the Byron
-- address-derivation-payload passphrase (only present for Byron keys).
data HashedCredentials k
    = HashedCredentialsV1 !(k 'RootK XPrv) !PassphraseHash
    | HashedCredentialsV2 !EncryptedKey !(Maybe (Passphrase "addr-derivation-payload"))

deriving instance (Eq (k 'RootK XPrv)) => Eq (HashedCredentials k)
deriving instance (Show (k 'RootK XPrv)) => Show (HashedCredentials k)

type ClearCredentials k = RootCredentials k (Passphrase "encryption")
