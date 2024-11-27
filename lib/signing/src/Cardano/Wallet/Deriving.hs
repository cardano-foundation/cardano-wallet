{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Deriving
    ( DerivedKeys (..)
    , ErrDeriveKey (..)
    , KeyIndexType
    , deriveKeys
    , prettyErrDeriveKey
    , createWitness

    , toHex
    , fromHex
    , indexToWord32
    )
    where

import Prelude

import Cardano.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , indexFromWord32
    , indexToWord32
    , pubToBytes
    , toXPub
    , xprvFromBytes
    , xprvPrivateKey
    , xprvToBytes
    , xpubToBytes
    , xpubToPub
    )
import Cardano.Address.Style.Shelley
    ( Role (..)
    , deriveAddressPrivateKey
    , getKey
    , liftXPrv
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Either.Combinators
    ( rightToMaybe
    )
import Data.Either.Extra
    ( maybeToEither
    )
import Data.Text
    ( Text
    )
import Data.Word
    ( Word32
    )

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data ErrDeriveKey =
      ErrDeriveKeyOutsideAddressIxBound
    | ErrDeriveKeyWrongAccountKeyLength
    deriving (Show, Eq)

data DerivedKeys = DerivedKeys
    { extendedPrivate :: ByteString
    , private :: ByteString
    , extendedPublic :: ByteString
    , public :: ByteString
    }  deriving (Show, Eq)

type KeyIndexType = Index 'Soft 'PaymentK

prettyErrDeriveKey :: ErrDeriveKey -> Text
prettyErrDeriveKey = \case
    ErrDeriveKeyOutsideAddressIxBound ->
        "Payment index is expected to be inside <" <>
        T.pack (show (indexToWord32 (minBound @(Index 'Soft 'PaymentK)))) <>
        ", " <>
        T.pack (show (indexToWord32 (maxBound @(Index 'Soft 'PaymentK)))) <>
        "."
    ErrDeriveKeyWrongAccountKeyLength ->
        "Account private key expect 96-byte payload"

deriveKeys
    :: ByteString
    -> Word32
    -> Either ErrDeriveKey DerivedKeys
deriveKeys addrXPrvBytes addrIx = do
    acctXPrv <-
        maybeToEither ErrDeriveKeyWrongAccountKeyLength . id
        $ xprvFromBytes addrXPrvBytes
    addrIxCorrect <-
        maybeToEither ErrDeriveKeyOutsideAddressIxBound . id
        $ indexFromWord32 @(Index 'Soft 'PaymentK) addrIx
    let xprv =
            deriveAddressPrivateKey (liftXPrv acctXPrv) UTxOExternal addrIxCorrect
    let xpub =
            toXPub $ getKey xprv
    pure DerivedKeys
        { extendedPrivate = xprvToBytes $ getKey xprv
        , private = xprvPrivateKey $ getKey xprv
        , extendedPublic = xpubToBytes xpub
        , public = pubToBytes $ xpubToPub xpub
        }

fromHex
    :: Text
    -> Maybe ByteString
fromHex =
    rightToMaybe . convertFromBase Base16 . T.encodeUtf8

toHex
    :: ByteString
    -> Text
toHex =
    T.decodeUtf8 . convertToBase Base16


createWitness
    :: ByteString
    -> Word32
    -> ByteString
    -> ByteString
createWitness _addrXPrvBytes _addrIx _cbor = undefined
