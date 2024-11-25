{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Signing
    ( DerivedKeys (..)
    , ErrDeriveKey (..)
    , deriveKeys
    , prettyErrDeriveKey
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
import Data.ByteString
    ( ByteString
    )
import Data.Text
    ( Text
    )
import Data.Word
    ( Word32
    )

import qualified Data.Text as T

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
        case xprvFromBytes addrXPrvBytes of
            Just xprv -> Right xprv
            Nothing -> Left ErrDeriveKeyWrongAccountKeyLength
    addrIxCorrect <-
        case indexFromWord32 @(Index 'Soft 'PaymentK) addrIx of
            Just val -> Right val
            Nothing -> Left ErrDeriveKeyOutsideAddressIxBound
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
