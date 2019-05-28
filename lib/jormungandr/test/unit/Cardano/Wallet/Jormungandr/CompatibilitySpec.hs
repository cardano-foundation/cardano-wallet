{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.CompatibilitySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Environment
    ( discriminant, network )
import Cardano.Wallet.Primitive.Types
    ( Address (..), ShowFmt (..), decodeAddress, encodeAddress )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..) )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , choose
    , elements
    , frequency
    , property
    , vectorOf
    , withMaxSuccess
    , (===)
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

spec :: Spec
spec = describe "Compatibility - Jormungandr" $ do
    it "decodeAddress . encodeAddress = pure" $
        withMaxSuccess 1000 $ property $ \(ShowFmt a) ->
            (ShowFmt <$> decodeAddress' (encodeAddress' a)) === Right (ShowFmt a)

    let net = show network
    negativeTest "bc1qvqsyqcyq5rqwzqfpg9scrgyg0p0q"
        ("This address belongs to another network. Network is: " <> net <> ".")
    negativeTest "ca1qvqsyqcyq5rqwzqfpg9scrgk66qs0"
        "Invalid address length (14): expected either 33 or 65 bytes."
    let firstByte = B8.unpack (BS.pack [discriminant])
    negativeTest "ca1dvqsyqcyq5rqwzqfpg9scrgwpugpzysnzs23v9ccrydpk8qarc0jqscdket"
        ("Invalid address first byte: k =/= " <> firstByte <> ".")
    negativeTest "EkxDbkPo"
        "Unable to decode address: neither Bech32-encoded nor a valid Byron \
        \address."
    negativeTest ".%14'"
        ("Unable to decode address: encoding is neither Bech32 nor Base58.")
  where
    decodeAddress' = decodeAddress (Proxy @Jormungandr)
    encodeAddress' = encodeAddress (Proxy @Jormungandr)
    negativeTest input msg = it ("decodeAddress failure: " <> msg) $
        decodeAddress' input === Left (TextDecodingError msg)


{-------------------------------------------------------------------------------
                             Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary (ShowFmt Address) where
    arbitrary = ShowFmt <$> frequency
        [ (10, genAddress [32,64])
        , (1, genLegacyAddress (30, 100))
        ]

genAddress :: [Int] -> Gen Address
genAddress sz = do
    bytes <- elements sz >>= \n -> vectorOf n arbitrary
    return . Address $ BS.pack (discriminant:bytes)

genLegacyAddress :: (Int, Int) -> Gen Address
genLegacyAddress range = do
    n <- choose range
    let prefix = BS.pack
            [ 130       -- Array(2)
            , 216, 24   -- Tag 24
            , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
            ]
    payload <- BS.pack <$> vectorOf n arbitrary
    let crc = BS.pack [26,1,2,3,4]
    return $ Address (prefix <> payload <> crc)
