{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Deposit.Pure.API.Address
    ( encodeAddress
    , decodeAddress
    , DecodingError (..)
    , NetworkTag (..)
    , getNetworkTag
    )
where

import Prelude

import Cardano.Wallet.Deposit.Read
    ( Address
    , NetworkTag (..)
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( StandardCrypto
    )
import Cardano.Wallet.Read.Address
    ( toShortByteString
    )
import Codec.Binary.Bech32
    ( DataPart
    , HumanReadablePart
    , dataPartFromBytes
    , dataPartToBytes
    , decodeLenient
    )
import Control.Arrow
    ( ArrowChoice (..)
    )
import Control.Monad
    ( (>=>)
    )
import Control.Monad.State.Strict
    ( evalStateT
    )
import Data.ByteString
    ( ByteString
    )
import Data.ByteString.Base58
    ( bitcoinAlphabet
    , decodeBase58
    , encodeBase58
    )
import Data.Text
    ( Text
    )

import qualified Cardano.Ledger.Address as SH
import qualified Cardano.Ledger.Address as SL
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString.Short as B8
import qualified Data.Text.Encoding as T

data AddressFlavor a b
    = Bootstrap
        {bootstrapFlavor :: a}
    | Shelley
        {shelleyFlavor :: b}
    deriving (Eq, Show)

withAddressFlavor
    :: (a -> c)
    -> (b -> c)
    -> AddressFlavor a b
    -> c
withAddressFlavor f _ (Bootstrap x) = f x
withAddressFlavor _ g (Shelley x) = g x

-- | Errors that can occur when decoding an 'Address'.
data DecodingError
    = InvalidBech32Encoding Bech32.DecodingError
    | InvalidBase58Encoding
    | InvalidHumanReadablePart HumanReadablePart
    | InvalidDataPart DataPart
    | AddressFlavorMismatch
    | AddressDecodingError String
    | AddressNetworkMismatch
    deriving (Eq, Show)

humanPart :: NetworkTag -> HumanReadablePart
humanPart = \case
    MainnetTag -> [Bech32.humanReadablePart|addr|]
    TestnetTag -> [Bech32.humanReadablePart|addr_test|]

decodeBase58Address
    :: ByteString
    -> Either
        DecodingError
        ( AddressFlavor
            ByteString
            (ByteString, HumanReadablePart)
        )
decodeBase58Address =
    fmap Bootstrap
        . maybe (Left InvalidBase58Encoding) Right
        . decodeBase58 bitcoinAlphabet

decodeBech32Address
    :: Text
    -> Either
        DecodingError
        (AddressFlavor ByteString (ByteString, HumanReadablePart))
decodeBech32Address bech32 = do
    (hrp, dataPart) <- left InvalidBech32Encoding $ decodeLenient bech32
    case dataPartToBytes dataPart of
        Nothing -> Left $ InvalidDataPart dataPart
        Just bytes -> pure $ Shelley (bytes, hrp)

decodeHumanAddress
    :: Text
    -> Either
        DecodingError
        (AddressFlavor ByteString (ByteString, HumanReadablePart))
decodeHumanAddress t =
    decodeBech32Address t
        <> decodeBase58Address (T.encodeUtf8 t)

newtype CatchFail a = CatchFail {runCatchFail :: Either String a}
    deriving (Functor, Applicative, Monad)

instance MonadFail CatchFail where
    fail = CatchFail . Left

ledgerAddressFlavor :: SL.Addr c -> AddressFlavor () ()
ledgerAddressFlavor (SL.AddrBootstrap _) = Bootstrap ()
ledgerAddressFlavor _ = Shelley ()

ledgerAddressNetworkTag :: SL.Addr c -> NetworkTag
ledgerAddressNetworkTag addr = case SL.getNetwork addr of
    SL.Testnet -> TestnetTag
    SL.Mainnet -> MainnetTag

-- | Get the network tag of an 'Address'.
getNetworkTag :: Address -> NetworkTag
getNetworkTag = ledgerAddressNetworkTag . SL.decompactAddr

ledgerDecode
    :: ByteString
    -> Either DecodingError (SL.Addr StandardCrypto)
ledgerDecode bs =
    left AddressDecodingError
        $ runCatchFail
        $ evalStateT
            (SH.decodeAddrStateLenientT @StandardCrypto True True bs)
            0

inspectAddress
    :: AddressFlavor ByteString (ByteString, HumanReadablePart)
    -> Either DecodingError (AddressFlavor Address Address)
inspectAddress (Bootstrap a) = do
    r <- ledgerDecode a
    case ledgerAddressFlavor r of
        Bootstrap () ->
            pure (Bootstrap $ SH.compactAddr r)
        _ -> Left AddressFlavorMismatch
inspectAddress (Shelley (bytes, hrp)) = do
    r <- ledgerDecode bytes
    case (ledgerAddressNetworkTag r, ledgerAddressFlavor r) of
        (network, Shelley ()) ->
            if humanPart network == hrp
                then pure (Shelley $ SH.compactAddr r)
                else Left AddressNetworkMismatch
        _ -> Left AddressFlavorMismatch

decodeFlavoredAddress
    :: Text
    -> Either DecodingError (AddressFlavor Address Address)
decodeFlavoredAddress = decodeHumanAddress >=> inspectAddress

-- | Decode an 'Address' from a 'Text' representation.
decodeAddress
    :: Text
    -- ^ Text to decode
    -> Either DecodingError Address
decodeAddress text = withAddressFlavor id id <$> decodeFlavoredAddress text

addFlavorToAddress :: Address -> AddressFlavor Address Address
addFlavorToAddress x
    | SL.isBootstrapCompactAddr x = Bootstrap x
    | otherwise = Shelley x

encodeFlavoredAddress
    :: AddressFlavor Address Address
    -> Text
encodeFlavoredAddress (Shelley addr) = bech32
  where
    bytes = B8.fromShort $ toShortByteString addr
    bech32 = Bech32.encodeLenient hrp (dataPartFromBytes bytes)
    hrp = humanPart $ getNetworkTag addr
encodeFlavoredAddress (Bootstrap addr) =
    T.decodeUtf8 . encodeBase58 bitcoinAlphabet
        $ B8.fromShort
        $ toShortByteString addr

-- | Encode an 'Address' to a 'Text' representation.
encodeAddress
    :: Address
    -- ^ Address to encode
    -> Text
encodeAddress = encodeFlavoredAddress . addFlavorToAddress
