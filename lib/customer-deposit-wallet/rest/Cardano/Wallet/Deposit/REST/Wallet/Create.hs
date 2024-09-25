{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Cardano.Wallet.Deposit.REST.Wallet.Create
    ( PostWalletViaMenmonic (..)
    , PostWalletViaXPub (..)
    , decodeXPub
    , xpubFromMnemonics
    , encodeXPub
    )
where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , generate
    , toXPub
    , xpubFromBytes
    , xpubToBytes
    )
import Data.ByteArray.Encoding
    ( Base (Base64)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString.Char8
    ( ByteString
    )
import Data.Text
    ( Text
    )
import GHC.Generics
    ( Generic
    )

import qualified Data.Text.Encoding as T

-- | Data for a request to create a wallet via a mnemonic.
data PostWalletViaMenmonic = PostWalletViaMenmonic
    { mnemonics :: Text
    , trackedCustomers :: Int
    }
    deriving (Generic)

-- | Data for a request to create a wallet via an extended public key.
data PostWalletViaXPub = PostWalletViaXPub
    { xpub :: Text
    , trackedCustomers :: Int
    }
    deriving (Generic)

unBase64 :: ByteString -> Either String ByteString
unBase64 = convertFromBase Base64

-- | Decode an extended public key from a base64-encoded text.
decodeXPub :: Text -> Either String (Maybe XPub)
decodeXPub = fmap xpubFromBytes . unBase64 . T.encodeUtf8

-- | Encode an extended public key to a base64-encoded text.
encodeXPub :: XPub -> Text
encodeXPub = T.decodeUtf8 . convertToBase Base64 . xpubToBytes

-- | Generate an extended public key from a mnemonic.
-- this is not what one wants to use in production
xpubFromMnemonics :: Text -> XPub
xpubFromMnemonics = toXPub . generate . T.encodeUtf8
