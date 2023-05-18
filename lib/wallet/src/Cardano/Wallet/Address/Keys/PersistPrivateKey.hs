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

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Crypto.Wallet
    ( unXPrv, xprv )
import Cardano.Wallet.Address.Derivation
    ( Depth (RootK), fromHex, hex )
import Cardano.Wallet.Address.Derivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..) )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..) )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..), PassphraseHash (..) )
import Control.Monad
    ( (<=<) )
import Data.ByteString
    ( ByteString )

import qualified Cardano.Crypto.Wallet as CC
import qualified Cardano.Wallet.Address.Derivation.Icarus as Icarus
import qualified Cardano.Wallet.Address.Derivation.SharedKey as Shared
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8

-- | Convert a private key and its password hash into hexadecimal strings
-- suitable for storing in a text file or database column.
serializeXPrv
    :: KeyFlavorS k
    -- ^ The type of key to serialize.
    -> (k 'RootK CC.XPrv, PassphraseHash)
    -- ^ Private key and its password hash.
    -> (ByteString, ByteString)
    -- ^ Hexadecimal strings.
serializeXPrv = \case
    ByronKeyS -> f serializeXPrvByron
    IcarusKeyS -> f serializeXPrvIcarus
    ShelleyKeyS -> f serializeXPrvShelley
    SharedKeyS -> f serializeXPrvShared
  where
    f g (k, h) = (g k, hex . getPassphraseHash $ h)

    serializeXPrvByron ((ByronKey k _ (Passphrase p))) =
        hex (unXPrv k) <> ":" <> hex p
    serializeXPrvIcarus = hex . unXPrv . Icarus.getKey
    serializeXPrvShelley = hex . unXPrv . Shelley.getKey
    serializeXPrvShared = hex . unXPrv . Shared.getKey

-- | The reverse of 'serializeXPrv'. This may fail if the inputs are not
-- valid hexadecimal strings, or if the key is of the wrong length.
unsafeDeserializeXPrv
    :: KeyFlavorS k
    -- ^ The type of key to deserialize.
    -> (ByteString, ByteString)
    -- ^ Hexadecimal strings.
    -> (k 'RootK CC.XPrv, PassphraseHash)
    -- ^ Private key and its password hash.
unsafeDeserializeXPrv = \case
    ByronKeyS -> deserialize unsafeDeserializeXPrvByron "ByronKey"
    IcarusKeyS -> deserialize (fmap IcarusKey . xprvFromText) "IcarusKey"
    ShelleyKeyS -> deserialize (fmap ShelleyKey . xprvFromText) "ShelleyKey"
    SharedKeyS -> deserialize (fmap SharedKey . xprvFromText) "SharedKey"
  where
    deserialize deserializeKey errText (k, h) =
        either err id
            $ (,)
                <$> deserializeKey k
                <*> fmap PassphraseHash (fromHex h)
      where
        err _ =
            error
                $ "unsafeDeserializeXPrv: unable to deserialize " <> errText

    xprvFromText :: ByteString -> Either String XPrv
    xprvFromText = xprv <=< fromHex @ByteString

    unsafeDeserializeXPrvByron
        :: ByteString
        -> Either String (ByronKey 'RootK XPrv)
    unsafeDeserializeXPrvByron = fmap mkKey . deserializeKey
      where
        mkKey (key, pwd) = ByronKey key () pwd
        deserializeKey
            :: ByteString
            -> Either
                String
                ( XPrv
                , Passphrase "addr-derivation-payload"
                )
        deserializeKey b = case map (fromHex @ByteString) (B8.split ':' b) of
            [Right rawK, Right p] ->
                case xprv rawK of
                    Right k' -> Right (k', Passphrase (BA.convert p))
                    Left e -> Left e
            _ ->
                Left "Key input must be two hex strings separated by :"
