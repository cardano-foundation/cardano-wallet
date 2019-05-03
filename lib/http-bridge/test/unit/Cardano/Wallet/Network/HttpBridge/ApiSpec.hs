{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Network.HttpBridge.ApiSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Network.HttpBridge.Api
    ( ApiT (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Data.Aeson
    ( FromJSON, ToJSON )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy )
import Data.Typeable
    ( Typeable )
import Test.Aeson.GenericSpecs
    ( GoldenDirectoryOption (CustomDirectoryName)
    , Proxy (Proxy)
    , Settings
    , defaultSettings
    , goldenDirectoryOption
    , roundtripAndGoldenSpecsWithSettings
    , sampleSize
    , useModuleNameAsSubDirectory
    )
import Test.Hspec
    ( Spec, describe )
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose, vectorOf )

import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Roundtrip & goldens" $ do
        roundtripAndGolden $ Proxy @(ApiT (Tx, [TxWitness]))

-- | See the similar function in @Cardano.Wallet.Api.TypesSpec@ for usage
roundtripAndGolden
    :: (Arbitrary a, ToJSON a, FromJSON a, Typeable a)
    => Proxy a
    -> Spec
roundtripAndGolden = roundtripAndGoldenSpecsWithSettings settings
  where
    settings :: Settings
    settings = defaultSettings
        { goldenDirectoryOption =
            CustomDirectoryName "test/data/Cardano/Wallet/Network/HttpBridge"
        , useModuleNameAsSubDirectory =
            False
        , sampleSize = 4
        }

genBytes :: Int -> Gen ByteString
genBytes n = BS.pack <$> vectorOf n arbitrary

instance Arbitrary Tx where
    arbitrary = Tx <$> genIns <*> genOuts
      where
        genIns :: Gen [TxIn]
        genIns = do
            n <- choose (1, 30)
            ids <- fmap Hash <$> vectorOf n (genBytes 32)
            ixs <- vectorOf n (choose (1, 30))
            return $ zipWith TxIn ids ixs

        genOuts :: Gen [TxOut]
        genOuts = do
            n <- choose (1, 30)
            coins <- fmap Coin <$> vectorOf n arbitrary
            addrs <- vectorOf n (genAddress 64)
            return $ zipWith TxOut addrs coins

        genAddress :: Int -> Gen Address
        genAddress n = do
            let prefix = BS.pack
                    [ 130       -- Array(2)
                    , 216, 24   -- Tag 24
                    , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
                    ]
            payload <- genBytes n
            crc <- (BS.singleton 26 <>) <$> genBytes 4
            return $ Address (prefix <> payload <> crc)

instance Arbitrary TxWitness where
    arbitrary = do
        xpub <- genBytes 32
        sig <- Hash <$> genBytes 32
        return $ PublicKeyWitness xpub sig

instance Arbitrary a => Arbitrary (ApiT a) where
    arbitrary = ApiT <$> arbitrary
    shrink = fmap ApiT . shrink . getApiT
