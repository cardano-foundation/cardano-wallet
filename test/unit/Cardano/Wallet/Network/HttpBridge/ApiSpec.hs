{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Cardano.Wallet.Network.HttpBridge.ApiSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Network.HttpBridge.Api
    ( ApiT (..) )
import Cardano.Wallet.Primitive.Types
    ( SignedTx (..) )
import Data.Aeson
    ( FromJSON, ToJSON )
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
    ( Arbitrary (..) )

import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "Roundtrip & goldens" $ do
        roundtripAndGolden $ Proxy @(ApiT SignedTx)

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

instance Arbitrary SignedTx where
    -- For testing the JSON encoding we don't care about the actual CBOR data
    -- or generating it correctly. Hence just random data.
    arbitrary = do
        bs <- BS.pack <$> arbitrary
        return . SignedTx $ bs

instance Arbitrary a => Arbitrary (ApiT a) where
    arbitrary = ApiT <$> arbitrary
    shrink = fmap ApiT . shrink . getApiT
