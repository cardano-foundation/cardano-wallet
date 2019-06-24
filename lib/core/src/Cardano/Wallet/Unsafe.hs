{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Unsafe
    ( unsafeFromHex
    , unsafeDecodeAddress
    , unsafeDecodeHex
    , unsafeRunExceptT
    , unsafeXPrv
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPrv )
import Cardano.Wallet.Primitive.Types
    ( Address, DecodeAddress (..) )
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
import Data.Proxy
    ( Proxy )
import Data.Text
    ( Text )

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.ByteString.Lazy as BL

-- | Decode an hex-encoded 'ByteString' into raw bytes, or fail.
unsafeFromHex :: ByteString -> ByteString
unsafeFromHex =
    either (error . show) id . convertFromBase @ByteString @ByteString Base16

-- | Decode a bech32-encoded 'Text' into an 'Address', or fail.
unsafeDecodeAddress :: DecodeAddress t => Proxy t -> Text -> Address
unsafeDecodeAddress proxy =
    either (error . show ) id . decodeAddress proxy

-- | Run a decoder on a hex-encoded 'ByteString', or fail.
unsafeDecodeHex :: Get a -> ByteString -> a
unsafeDecodeHex get = runGet get . BL.fromStrict . unsafeFromHex

-- | Build a 'XPrv' from an hex-encoded bytestring
unsafeXPrv :: ByteString -> XPrv
unsafeXPrv hex =
    case convertFromBase @_ @ByteString Base16 hex >>= CC.xprv of
        Left e -> error $ "unsafeXPrv: " <> e
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
