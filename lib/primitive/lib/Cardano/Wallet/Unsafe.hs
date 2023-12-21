{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2021 IOHK
-- License: Apache-2.0
--
-- Conversion functions which don't have error handling. These are quite
-- convenient to use for jobs like testing, debugging, etc.
--
-- But these "unsafe" functions should not be used in application code, unless
-- it's certain that the error case will never happen.

module Cardano.Wallet.Unsafe
    ( unsafeRight
    , unsafeFromHex
    , unsafeFromHexText
    , unsafeFromBase64
    , unsafeFromHexFile
    , unsafeDecodeHex
    , unsafeFromText
    , unsafeRunExceptT
    , unsafeXPrv
    , unsafeXPub
    , unsafeDeserialiseCbor
    , unsafeBech32DecodeFile
    , unsafeBech32Decode
    , unsafeMkPercentage
    , unsafeIntToWord

    , someDummyMnemonic
    , unsafeMkMnemonic
    , unsafeMkEntropy
    , unsafeMkSomeMnemonicFromEntropy
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv
    , XPub
    )
import Cardano.Mnemonic
    ( ConsistentEntropy
    , Entropy
    , EntropySize
    , Mnemonic
    , MnemonicWords
    , SomeMnemonic (..)
    , ValidChecksumSize
    , ValidEntropySize
    , ValidMnemonicSentence
    , entropyToMnemonic
    , mkEntropy
    , mkMnemonic
    )
import Cardano.Wallet.Util
    ( internalError
    )
import Control.Monad
    ( (>=>)
    )
import Control.Monad.Trans.Except
    ( ExceptT (..)
    , runExceptT
    )
import Data.Binary.Get
    ( Get
    , runGet
    )
import Data.ByteArray
    ( ByteArray
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Char
    ( isHexDigit
    )
import Data.Either
    ( fromRight
    )
import Data.Percentage
    ( Percentage
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    )
import Data.Typeable
    ( Typeable
    , typeRep
    )
import Fmt
    ( Buildable
    , Builder
    , build
    , (+||)
    , (|+)
    , (||+)
    )
import GHC.Stack
    ( HasCallStack
    )
import GHC.TypeLits
    ( natVal
    )

import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Percentage as Percentage
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

-- | Take the right side of an 'Either' value. Crash badly if it was a left.
unsafeRight :: (Buildable e, HasCallStack) => Either e a -> a
unsafeRight = either (internalError . build) id

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: forall b. (HasCallStack, ByteArray b) => ByteString -> b
unsafeFromHex = unsafeRight . convertFromBase @ByteString @b Base16

-- | Decode hex-encoded 'Text' into a 'ByteString', or fail. This variant of
-- 'unsafeFromHex' may be easier to use because it's not polymorphic.
unsafeFromHexText :: HasCallStack => Text -> ByteString
unsafeFromHexText = unsafeFromHex . T.encodeUtf8

-- | Decode a base64-encoded 'ByteString' into raw bytes, or fail.
unsafeFromBase64 :: HasCallStack => ByteString -> ByteString
unsafeFromBase64 = unsafeRight . convertFromBase @ByteString @ByteString Base64

-- | Load a hex string from file. Any non-hexadecimal characters are ignored.
unsafeFromHexFile :: HasCallStack => FilePath -> IO ByteString
unsafeFromHexFile = fmap (unsafeFromHex . B8.filter isHexDigit) . B8.readFile

-- | Run a decoder on a hex-encoded 'ByteString', or fail.
unsafeDecodeHex :: HasCallStack => Get a -> ByteString -> a
unsafeDecodeHex get = runGet get . BL.fromStrict . unsafeFromHex

-- | Decode the given data-type from a textual representation, or fail.
unsafeFromText :: (FromText a, HasCallStack) => Text -> a
unsafeFromText = unsafeRight . fromText

-- | Build a 'XPrv' from a bytestring
unsafeXPrv :: HasCallStack => ByteString -> XPrv
unsafeXPrv bytes =
    case CC.xprv bytes of
        Left e -> error $ "unsafeXPrv: " <> e
        Right a -> a

-- | Build a 'XPub' from a bytestring
unsafeXPub :: HasCallStack => ByteString -> XPub
unsafeXPub bytes =
    case CC.xpub bytes of
        Left e -> error $ "unsafeXPub: " <> e
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

unsafeMkEntropy
    :: forall ent csz.
        ( HasCallStack
        , ValidEntropySize ent
        , ValidChecksumSize ent csz
        )
    => ByteString
    -> Entropy ent
unsafeMkEntropy = either (error . show) id . mkEntropy . BA.convert

unsafeMkSomeMnemonicFromEntropy
    :: forall mw ent csz.
        ( HasCallStack
        , ValidEntropySize ent
        , ValidChecksumSize ent csz
        , ValidMnemonicSentence mw
        , ent ~ EntropySize mw
        , mw ~ MnemonicWords ent
        )
    => Proxy mw
    -> ByteString
    -> SomeMnemonic
unsafeMkSomeMnemonicFromEntropy _ = SomeMnemonic
    . entropyToMnemonic
    . unsafeMkEntropy @ent

-- | A dummy @SomeMnemonic@ for testing.
--
-- Could have been named @dummySomeMnemonic@, but this way it sounds more like
-- valid english.
someDummyMnemonic
    :: forall mw ent csz.
        ( HasCallStack
        , ValidEntropySize ent
        , ValidChecksumSize ent csz
        , ValidMnemonicSentence mw
        , ent ~ EntropySize mw
        , mw ~ MnemonicWords ent
        )
    => Proxy mw
    -> SomeMnemonic
someDummyMnemonic proxy =
    let
        n = fromIntegral $ natVal (Proxy @ent) `div` 8
        entropy = BS.replicate n 0
    in
        unsafeMkSomeMnemonicFromEntropy proxy entropy

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

unsafeMkPercentage :: HasCallStack => Rational -> Percentage
unsafeMkPercentage r = fromRight bomb $ Percentage.fromRational r
  where
    bomb = error $ "unsafeMkPercentage: " ++ show r ++ " is out of bounds."

-- | Convert an integer type of any range to a machine word.
--
-- Only use it for values which have come from the ledger, and should fit in the
-- given type, according to the spec.
--
-- If this conversion would under/overflow, there is not much we can do except
-- to hastily exit.
unsafeIntToWord
    :: forall from to
     . ( HasCallStack
       , Integral from
       , Bounded to
       , Integral to
       , Typeable from
       , Typeable to
       , Show from)
    => from -> to
unsafeIntToWord n
    | n < fromIntegral (minBound :: to) = crash "underflow"
    | n > fromIntegral (maxBound :: to) = crash "overflow"
    | otherwise = fromIntegral n
  where
    crash :: Builder -> to
    crash err = internalError $ err |+" converting value "+|| n ||+
        " from " +|| typeRep (Proxy @from) ||+
        " to "+|| typeRep (Proxy @to) ||+"!"
