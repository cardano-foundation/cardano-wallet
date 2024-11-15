{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Cardano.Wallet.Deposit.Pure.State.Creation
    ( WalletPublicIdentity (..)
    , fromCredentialsAndGenesis
    , Credentials (..)
    , credentialsFromMnemonics
    , credentialsFromEncodedXPub
    , xpubFromCredentials
    , xprvFromCredentials
    , ErrDecodingXPub (..)
    , encodedXPubFromCredentials
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Address.Derivation
    ( xpubFromBytes
    , xpubToBytes
    )
import Cardano.Crypto.Wallet
    ( XPrv
    , XPub
    , generate
    , toXPub
    , unXPrv
    )
import Cardano.Wallet.Deposit.Pure.State.Type
    ( WalletState (..)
    )
import Data.Text
    ( Text
    )
import Data.Word.Odd
    ( Word31
    )
import GHC.Generics
    ( Generic
    )

import qualified Cardano.Wallet.Deposit.Pure.Address as Address
import qualified Cardano.Wallet.Deposit.Pure.Submissions as Sbm
import qualified Cardano.Wallet.Deposit.Pure.UTxO.UTxOHistory as UTxOHistory
import qualified Cardano.Wallet.Deposit.Read as Read
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text.Encoding as T

data WalletPublicIdentity = WalletPublicIdentity
    { pubXpub :: XPub
    , pubNextUser :: Word31
    }
    deriving (Show)

data Credentials
    = XPubCredentials !XPub
    | XPrvCredentials !XPrv !XPub
    deriving (Generic, Show, Eq)

instance Show XPrv where
    show = B8.unpack . B16.encode . unXPrv

instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

xpubFromCredentials :: Credentials -> XPub
xpubFromCredentials (XPubCredentials xpub) = xpub
xpubFromCredentials (XPrvCredentials _ xpub) = xpub

xprvFromCredentials :: Credentials -> Maybe XPrv
xprvFromCredentials (XPubCredentials _) = Nothing
xprvFromCredentials (XPrvCredentials xprv _) = Just xprv

fromCredentialsAndGenesis
    :: Credentials -> Word31 -> Read.GenesisData -> WalletState
fromCredentialsAndGenesis credentials knownCustomerCount genesisData =
    WalletState
        { walletTip = Read.GenesisPoint
        , addresses =
            Address.fromXPubAndCount
                network
                (xpubFromCredentials credentials)
                knownCustomerCount
        , utxoHistory = UTxOHistory.fromOrigin initialUTxO
        , txHistory = mempty
        , submissions = Sbm.empty
        , rootXSignKey = xprvFromCredentials credentials
        }
  where
    network = Read.getNetworkId genesisData
    initialUTxO = mempty

-- | Create 'Credentials' from a mnemonic sentence and a passphrase.
credentialsFromMnemonics
    :: Text
    -- ^ Mnemonics
    -> Text
    -- ^ Passphrase
    -> Credentials
credentialsFromMnemonics mnemonics passphrase =
    let
        unencryptedXPrv =
            generate
                (T.encodeUtf8 mnemonics)
                (T.encodeUtf8 mempty)
        encryptedXPrv =
            generate
                (T.encodeUtf8 mnemonics)
                (T.encodeUtf8 passphrase)
    in
        XPrvCredentials encryptedXPrv (toXPub unencryptedXPrv)

-- | Create 'Credentials' from an extended public key failures to decode
data ErrDecodingXPub = ErrFromXPubBase16 | ErrFromXPubDecodeKey
    deriving (Show, Eq)

-- | Create 'Credentials' from an extended public key encoded in base16.
credentialsFromEncodedXPub
    :: Text
    -> Either ErrDecodingXPub Credentials
credentialsFromEncodedXPub xpub = case B16.decode (T.encodeUtf8 xpub) of
    Left _ -> Left ErrFromXPubBase16
    Right bytes -> case xpubFromBytes bytes of
        Nothing -> Left ErrFromXPubDecodeKey
        Just key -> Right $ XPubCredentials key

-- | Encode an extended public key to base16.
encodedXPubFromCredentials
    :: Credentials
    -> Text
encodedXPubFromCredentials (XPubCredentials xpub) =
    T.decodeUtf8
        $ B16.encode
        $ xpubToBytes xpub
encodedXPubFromCredentials (XPrvCredentials _ xpub) =
    encodedXPubFromCredentials (XPubCredentials xpub)
