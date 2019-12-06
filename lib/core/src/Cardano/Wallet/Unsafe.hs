{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wallet.Unsafe
    ( unsafeFromHex
    , unsafeFromHexFile
    , unsafeDecodeAddress
    , unsafeDecodeHex
    , unsafeFromText
    , unsafeRunExceptT
    , unsafeXPrv
    , unsafeMkMnemonic
    , unsafeDeserialiseCbor
    , unsafeBech32DecodeFile
    , unsafeBech32Decode
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.Api.Types
    ( DecodeAddress (..) )
import Cardano.Wallet.Primitive.Mnemonic
    ( ConsistentEntropy, EntropySize, Mnemonic, mkMnemonic )
import Cardano.Wallet.Primitive.Types
    ( Address )
import Control.Monad
    ( (>=>) )
import Control.Monad.Fail
    ( MonadFail )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT )
import Data.Binary.Get
    ( Get, runGet )
import Data.ByteArray.Encoding
    ( Base (..), convertFromBase )
import Data.ByteString
    ( ByteString )
import Data.Char
    ( isHexDigit )
import Data.Text
    ( Text )
import Data.Text.Class
    ( FromText (..) )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: HasCallStack => ByteString -> ByteString
unsafeFromHex =
    either (error . show) id . convertFromBase @ByteString @ByteString Base16

-- | Load a hex string from file. Any non-hexadecimal characters are ignored.
unsafeFromHexFile :: HasCallStack => FilePath -> IO ByteString
unsafeFromHexFile = fmap (unsafeFromHex . B8.filter isHexDigit) . B8.readFile

-- | Decode a bech32-encoded 'Text' into an 'Address', or fail.
unsafeDecodeAddress
    :: forall n. (HasCallStack, DecodeAddress n)
    => Text
    -> Address
unsafeDecodeAddress =
    either (error . show ) id . decodeAddress @n

-- | Run a decoder on a hex-encoded 'ByteString', or fail.
unsafeDecodeHex :: HasCallStack => Get a -> ByteString -> a
unsafeDecodeHex get = runGet get . BL.fromStrict . unsafeFromHex

-- | Decode the given data-type from a textual representation, or fail.
unsafeFromText :: (FromText a, HasCallStack) => Text -> a
unsafeFromText = either (error . show) id . fromText

-- | Build a 'XPrv' from a bytestring
unsafeXPrv :: HasCallStack => ByteString -> XPrv
unsafeXPrv bytes =
    case CC.xprv bytes of
        Left e -> error $ "unsafeXPrv: " <> e
        Right a -> a

-- | Build 'Mnemonic' from literals
unsafeMkMnemonic
    :: forall mw n csz
    .  (ConsistentEntropy n mw csz, EntropySize mw ~ n, HasCallStack)
    => [Text]
    -> Mnemonic mw
unsafeMkMnemonic m =
    case mkMnemonic m of
        Left e -> error $ "unsafeMnemonic: " <> show e
        Right a -> a

-- | Run an 'ExceptT' and throws the error if any. This makes sense only if
-- called after checking for an invariant or, after ensuring that preconditions
-- for meeting the underlying error have been discarded.
unsafeRunExceptT :: (MonadFail m, Show e) => ExceptT e m a -> m a
unsafeRunExceptT = runExceptT >=> \case
    Left e ->
        fail $ "unexpected error: " <> show e
    Right a ->
        return a

-- | CBOR deserialise without error handling - handy for prototypes or testing.
unsafeDeserialiseCbor
    :: HasCallStack
    => (forall s. CBOR.Decoder s a)
    -> BL.ByteString
    -> a
unsafeDeserialiseCbor decoder bytes = either
    (\e -> error $ "unsafeSerializeCbor: " <> show e)
    snd
    (CBOR.deserialiseFromBytes decoder bytes)

-- | Load the data part of a bech32-encoded string from file. These files often
-- come from @jcli@. Only the first line of the file is read.
unsafeBech32DecodeFile :: HasCallStack => FilePath -> IO BL.ByteString
unsafeBech32DecodeFile = fmap (unsafeBech32Decode . firstLine) . TIO.readFile
  where
    firstLine = T.takeWhile (/= '\n')

-- | Get the data part of a bech32-encoded string, ignoring the human-readable part.
unsafeBech32Decode :: HasCallStack => Text -> BL.ByteString
unsafeBech32Decode txt = case Bech32.decodeLenient txt of
    Right (_hrp, dp) -> maybe (bomb "missing data part")
        BL.fromStrict (Bech32.dataPartToBytes dp)
    Left e -> bomb (show e)
  where
    bomb msg = error $ "Could not decode bech32 string " ++ show txt
        ++ " because " ++ msg
