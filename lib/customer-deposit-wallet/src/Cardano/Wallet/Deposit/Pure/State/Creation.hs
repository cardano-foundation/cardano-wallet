{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Deposit.Pure.State.Creation
    ( WalletPublicIdentity (..)
    , fromCredentialsAndGenesis
    , deriveAccountXPrv
    , Credentials (..)
    , credentialsFromMnemonics
    , credentialsFromEncodedXPub
    , accountXPubFromCredentials
    , rootXPrvFromCredentials
    , ErrDecodingXPub (..)
    , encodedXPubFromCredentials
    , canSign
    , CanSign (..)
    ) where

import Prelude hiding
    ( lookup
    )

import Cardano.Address.Derivation
    ( xpubFromBytes
    , xpubToBytes
    )
import Cardano.Crypto.Wallet
    ( generate
    )
import Cardano.Wallet.Address.BIP32_Ed25519
    ( XPrv
    , XPub
    , deriveXPrvHard
    , rawSerialiseXPrv
    , toXPub
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
    show = B8.unpack . B16.encode . rawSerialiseXPrv

instance Eq XPrv where
    a == b = rawSerialiseXPrv a == rawSerialiseXPrv b

-- | Get /account/ 'XPub' from credentials if available.
--
-- The account public key corresponds to the account
-- private key obtained from 'deriveAccountXPrv',
-- /not/ the root private key.
accountXPubFromCredentials :: Credentials -> XPub
accountXPubFromCredentials (XPubCredentials xpub) = xpub
accountXPubFromCredentials (XPrvCredentials _ xpub) = xpub

-- | Derive account 'XPrv' from the root 'XPrv'.
deriveAccountXPrv :: XPrv -> XPrv
deriveAccountXPrv xprv =
    ( deriveXPrvHard
        ( deriveXPrvHard
            ( deriveXPrvHard
                xprv
                1857 -- Address derivation standard
            )
            1815 -- ADA
        )
        0 -- Account number
    )

-- | Get root 'XPrv' from credentials if available.
rootXPrvFromCredentials :: Credentials -> Maybe XPrv
rootXPrvFromCredentials (XPubCredentials _) = Nothing
rootXPrvFromCredentials (XPrvCredentials xprv _) = Just xprv

fromCredentialsAndGenesis
    :: Credentials -> Word31 -> Read.GenesisData -> WalletState
fromCredentialsAndGenesis credentials knownCustomerCount genesisData =
    WalletState
        { walletTip = Read.GenesisPoint
        , addresses =
            Address.fromXPubAndCount
                network
                (accountXPubFromCredentials credentials)
                knownCustomerCount
        , utxoHistory = UTxOHistory.fromOrigin initialUTxO
        , txHistory = mempty
        , submissions = Sbm.empty
        , rootXSignKey = rootXPrvFromCredentials credentials
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
        XPrvCredentials
            encryptedXPrv
            (toXPub $ deriveAccountXPrv unencryptedXPrv)

data CanSign = CanSign | CannotSign
    deriving (Eq, Show)

canSign :: WalletState -> CanSign
canSign WalletState{rootXSignKey} = case rootXSignKey of
    Nothing -> CannotSign
    Just _ -> CanSign

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
