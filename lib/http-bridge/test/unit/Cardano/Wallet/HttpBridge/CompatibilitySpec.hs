{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.HttpBridge.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.HttpBridge.Compatibility
    ( HttpBridge, Network (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), decodeAddress, encodeAddress )
import Data.Digest.CRC32
    ( crc32 )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose, property, vectorOf, (===) )

import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString as BS

spec :: Spec
spec = describe "Compatibility - HttpBridge" $ do
    it "decodeAddress . encodeAddress = pure" $
        property $ \a -> decodeAddress' (encodeAddress' a) === Right a

    it "decodeAddress failure \"0000\"" $ do
        let err = "Unable to decode Address: expected Base58 encoding."
        decodeAddress' "0000" === Left (TextDecodingError err)

    it "decodeAddress failure \"EkxDbkPo\"" $ do
        let err = "Unable to decode Address: not a valid Byron address."
        decodeAddress' "EkxDbkPo" === Left (TextDecodingError err)
  where
    decodeAddress' = decodeAddress (Proxy @(HttpBridge 'Testnet))
    encodeAddress' = encodeAddress (Proxy @(HttpBridge 'Testnet))

instance Arbitrary Address where
    arbitrary = genAddress (30, 100)

-- The address format on 'Staging' is the same as 'Mainnet'.
genAddress :: (Int, Int) -> Gen Address
genAddress range = do
    n <- choose range
    let prefix = BS.pack
            [ 130       -- Array(2)
            , 216, 24   -- Tag 24
            , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
            ]
    payload <- BS.pack <$> vectorOf n arbitrary
    let crc = CBOR.toStrictByteString (CBOR.encodeWord32 $ crc32 payload)
    return $ Address (prefix <> payload <> crc)
