{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Primitive.Types.TokenFingerprint
    ( TokenFingerprint (..)
    , mkTokenFingerprint
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenName
    ( TokenName (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId (UnsafeTokenPolicyId)
    )
import Codec.Binary.Bech32.TH
    ( humanReadablePart
    )
import Control.DeepSeq
    ( NFData
    )
import Crypto.Hash
    ( hash
    )
import Crypto.Hash.Algorithms
    ( Blake2b_160
    )
import Data.ByteArray
    ( convert
    )
import Data.Function
    ( (&)
    )
import Data.Hashable
    ( Hashable
    )
import Data.Text
    ( Text
    )
import Data.Text.Class
    ( FromText (..)
    , TextDecodingError (..)
    , ToText (..)
    )
import GHC.Generics
    ( Generic
    )
import Quiet
    ( Quiet (..)
    )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.ByteString as BS

newtype TokenFingerprint =
    UnsafeTokenFingerprint { unTokenFingerprint :: Text }
    deriving stock (Eq, Ord, Generic)
    deriving (Read, Show) via (Quiet TokenFingerprint)
    deriving anyclass Hashable

instance NFData TokenFingerprint

-- | Construct a fingerprint from a 'TokenPolicyId' and 'TokenName'. The
-- fingerprint is not necessarily unique, but can be used in user-facing
-- interfaces as a comparison mechanism.
mkTokenFingerprint :: TokenPolicyId -> TokenName -> TokenFingerprint
mkTokenFingerprint (UnsafeTokenPolicyId (Hash p)) (UnsafeTokenName n)
    = (p <> n)
    & convert . hash @_ @Blake2b_160
    & Bech32.encodeLenient tokenFingerprintHrp . Bech32.dataPartFromBytes
    & UnsafeTokenFingerprint

tokenFingerprintHrp :: Bech32.HumanReadablePart
tokenFingerprintHrp = [humanReadablePart|asset|]

instance ToText TokenFingerprint where
    toText = unTokenFingerprint

instance FromText TokenFingerprint where
    fromText = tokenFingerprintFromText

tokenFingerprintFromText :: Text -> Either TextDecodingError TokenFingerprint
tokenFingerprintFromText txt = case Bech32.decodeLenient txt of
    Left{} -> Left invalidBech32String
    Right (hrp, dp)
        | hrp /= tokenFingerprintHrp -> Left unrecognizedHrp
        | otherwise -> case BS.length <$> Bech32.dataPartToBytes dp of
            Just 20 -> Right (UnsafeTokenFingerprint txt)
            _ -> Left invalidDatapart
  where
    invalidBech32String = TextDecodingError
        "A 'TokenFingerprint' must be a valid bech32-encoded string."
    unrecognizedHrp = TextDecodingError
        "Expected 'asset' as a human-readable part, but got something else."
    invalidDatapart = TextDecodingError
        "Expected a Blake2b-160 digest as data payload, but got something else."
