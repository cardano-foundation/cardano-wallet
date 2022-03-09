{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Passphrase.Types
    ( -- * Passphrases from the user
      Passphrase (..)
    , PassphraseMinLength (..)
    , PassphraseMaxLength (..)
    , validatePassphrase

      -- * Wallet passphrases stored as hashes
    , PassphraseHash (..)
    , PassphraseScheme (..)
    , WalletPassphraseInfo (..)

      -- * Error types
    , ErrWrongPassphrase(..)
    ) where

import Prelude

import Control.DeepSeq
    ( NFData )
import Crypto.Random.Types
    ( MonadRandom (..) )
import Data.Bifunctor
    ( first )
import Data.ByteArray
    ( ByteArrayAccess, ScrubbedBytes )
import Data.ByteArray.Encoding
    ( Base (..), convertToBase )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..), TextDecodingError (..), ToText (..) )
import Data.Time.Clock
    ( UTCTime )
import GHC.Generics
    ( Generic )
import GHC.TypeLits
    ( Symbol )

import qualified Data.ByteArray as BA
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

{-------------------------------------------------------------------------------
                           Passphrases from the user
-------------------------------------------------------------------------------}

-- | An encapsulated passphrase. The inner format is free, but the wrapper helps
-- readability in function signatures.
--
-- Some type parameters in use are:
--
--  * @"user"@ - a passphrase entered by the user through the API. The 'FromText'
--    instance enforces password length rules.
--
--  * @"lenient"@ - like @"user"@, except without a minimum length restriction
--    in 'FromText'.`
--
--  * @"encryption"@ - the user's passphrase, transformed so that it can be used
--    as the key for encrypting wallet keys.
--
--  * @"salt"@ - the random salt part of a hashed passphrase.
--
newtype Passphrase (purpose :: Symbol) = Passphrase
    { unPassphrase :: ScrubbedBytes }
    deriving stock (Eq, Show)
    deriving newtype (Semigroup, Monoid, NFData, ByteArrayAccess)

type role Passphrase phantom

-- | Little trick to be able to provide our own "random" salt in order to
-- deterministically re-compute a passphrase hash from a known salt. Note that,
-- this boils down to giving an extra argument to the `encryptPassphrase`
-- function which is the salt, in order to make it behave deterministically.
--
-- @
-- encryptPassphrase
--   :: MonadRandom m
--   => Passphrase purpose
--   -> m (Hash purpose)
--
--  ~
--
-- encryptPassphrase
--   :: Passphrase purpose
--   -> Passphrase "salt"
--   -> m (Hash purpose)
-- @
--
-- >>> encryptPassphrase pwd (Passphrase @"salt" salt)
-- Hash "..."
--
instance MonadRandom ((->) (Passphrase "salt")) where
    getRandomBytes _ (Passphrase salt) = BA.convert salt

class PassphraseMinLength (purpose :: Symbol) where
    -- | Minimal Length for a passphrase, for lack of better validations
    passphraseMinLength :: Proxy purpose -> Int

class PassphraseMaxLength (purpose :: Symbol) where
    -- | Maximum length for a passphrase
    passphraseMaxLength :: Proxy purpose -> Int

instance PassphraseMinLength "user" where passphraseMinLength _ = 10
instance PassphraseMaxLength "user" where passphraseMaxLength _ = 255

instance PassphraseMinLength "lenient" where passphraseMinLength _ = 0
instance PassphraseMaxLength "lenient" where passphraseMaxLength _ = 255

validatePassphrase
    :: forall purpose.
       (PassphraseMaxLength purpose, PassphraseMinLength purpose)
    => Text
    -> Either String (Passphrase purpose)
validatePassphrase pwd
    | T.length pwd < minLength = Left $
        "passphrase is too short: expected at least "
        <> show minLength <> " characters"
    | T.length pwd > maxLength = Left $
        "passphrase is too long: expected at most "
        <> show maxLength <> " characters"
    | otherwise = Right $ Passphrase $ BA.convert $ T.encodeUtf8 pwd
  where
    minLength = passphraseMinLength (Proxy :: Proxy purpose)
    maxLength = passphraseMaxLength (Proxy :: Proxy purpose)

instance
    ( PassphraseMaxLength purpose
    , PassphraseMinLength purpose
    ) => FromText (Passphrase purpose) where
    fromText = first TextDecodingError . validatePassphrase

instance ToText (Passphrase purpose) where
    toText (Passphrase bytes) = T.decodeUtf8 $ BA.convert bytes

{-------------------------------------------------------------------------------
                      Wallet passphrases stored as hashes
-------------------------------------------------------------------------------}

-- | A type to capture which encryption scheme should be used
data PassphraseScheme
    = EncryptWithScrypt
        -- ^ Legacy encryption scheme for passphrases
    | EncryptWithPBKDF2
        -- ^ Encryption scheme used since cardano-wallet
    deriving (Generic, Eq, Ord, Show, Read)

instance NFData PassphraseScheme

instance ToText PassphraseScheme where
    toText EncryptWithScrypt = "scrypt"
    toText EncryptWithPBKDF2 = "pbkdf2-hmac-sha512"

newtype PassphraseHash = PassphraseHash { getPassphraseHash :: ScrubbedBytes }
    deriving stock (Show)
    deriving newtype (Eq, NFData, ByteArrayAccess)

instance ToText PassphraseHash where
    toText = T.decodeUtf8 . convertToBase Base16 . getPassphraseHash

data WalletPassphraseInfo = WalletPassphraseInfo
    { lastUpdatedAt :: UTCTime
    , passphraseScheme :: PassphraseScheme
    } deriving (Generic, Eq, Ord, Show)

instance NFData WalletPassphraseInfo

{-------------------------------------------------------------------------------
                                  Error types
-------------------------------------------------------------------------------}

-- | Indicate a failure when checking for a given 'Passphrase' match
data ErrWrongPassphrase
    = ErrWrongPassphrase
    | ErrPassphraseSchemeUnsupported PassphraseScheme
    deriving stock (Generic, Show, Eq)

instance NFData ErrWrongPassphrase
