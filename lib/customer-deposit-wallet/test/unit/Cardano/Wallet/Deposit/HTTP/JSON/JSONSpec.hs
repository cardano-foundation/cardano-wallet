{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Deposit.HTTP.JSON.JSONSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Deposit.HTTP.Types.JSON
    ( Address
    , ApiT (..)
    , ChainPoint (..)
    , Customer
    , CustomerList
    )
import Cardano.Wallet.Deposit.HTTP.Types.OpenAPI
    ( addressSchema
    , chainPointSchema
    , customerListSchema
    , customerSchema
    , depositDefinitions
    )
import Cardano.Wallet.Deposit.Pure
    ( Word31
    , fromRawCustomer
    )
import Cardano.Wallet.Deposit.Read
    ( mkEnterpriseAddress
    , toSlot
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value
    , decode
    , encode
    )
import Data.Aeson.Encode.Pretty
    ( encodePretty
    )
import Data.OpenApi
    ( Definitions
    , Schema
    , validateJSON
    )
import Data.Word
    ( Word64
    )
import Numeric.Natural
    ( Natural
    )
import Test.Hspec
    ( Expectation
    , Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , Testable
    , arbitrarySizedBoundedIntegral
    , chooseInt
    , counterexample
    , forAll
    , oneof
    , property
    , shrinkIntegral
    , vectorOf
    , (===)
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L

spec :: Spec
spec = do
    describe "JSON serialization & deserialization" $ do
        it "ApiT Address" $ property $
            prop_jsonRoundtrip @(ApiT Address)
        it "ApiT Customer" $ property $
            prop_jsonRoundtrip @(ApiT Customer)
        it "ApiT CustomerList" $ property $
            prop_jsonRoundtrip @(ApiT CustomerList)
        it "ApiT ChainPoint" $ property $
            prop_jsonRoundtrip @(ApiT ChainPoint)
    describe "schema checks" $ do
        it "ApiT Address"
            $ jsonMatchesSchema genApiTAddress depositDefinitions addressSchema
        it "ApiT Customer"
            $ jsonMatchesSchema genApiTCustomer depositDefinitions customerSchema
        it "ApiT CustomerList"
            $ jsonMatchesSchema genApiTCustomerList depositDefinitions customerListSchema
        it "ApiT ChainPoint"
            $ jsonMatchesSchema genApiTChainPoint depositDefinitions chainPointSchema

jsonMatchesSchema
    :: (ToJSON a, Show a)
    => Gen a
    -> Definitions Schema
    -> Schema
    -> Property
jsonMatchesSchema gen defs schema =
    forAll gen
        $ counterExampleJSON "validate"
        $ validateInstance defs schema
  where
    validate :: Definitions Schema -> Schema -> Value -> Expectation
    validate defs' sch' x = validateJSON defs' sch' x `shouldBe` []

    validateInstance :: ToJSON a => Definitions Schema -> Schema -> a -> Expectation
    validateInstance defs' sch' = validate defs' sch' . toJSON

    counterExampleJSON
        :: (Testable prop, ToJSON a)
        => String
        -> (a -> prop)
        -> a
        -> Property
    counterExampleJSON t f x =
        counterexample
            ("Failed to " <> t <> ":\n" <> BL.unpack (encodePretty $ toJSON x))
            $ f x

prop_jsonRoundtrip :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Property
prop_jsonRoundtrip val =
    decode (encode val) === Just val

genAddress :: Gen Address
genAddress = do
    keyhashCred <- BS.pack <$> vectorOf 28 arbitrary
    pure $ mkEnterpriseAddress keyhashCred

genApiTAddress :: Gen (ApiT Address)
genApiTAddress = ApiT <$> genAddress

genApiTCustomer :: Gen (ApiT Customer)
genApiTCustomer =
    ApiT . fromRawCustomer <$> arbitrary

genApiTCustomerList :: Gen (ApiT CustomerList)
genApiTCustomerList = do
    listLen <- chooseInt (0, 100)
    let genPair = (,) <$> (unApiT <$> arbitrary) <*> (unApiT <$> arbitrary)
    vectors <- vectorOf listLen genPair
    let uniqueCustomer = L.nubBy (\a b -> fst a == fst b)
    let uniqueAddr = L.nubBy (\a b -> snd a == snd b)
    pure $ ApiT $ uniqueAddr $ uniqueCustomer vectors

genApiTChainPoint :: Gen (ApiT ChainPoint)
genApiTChainPoint = oneof
        [ pure . ApiT $ Origin
        , ApiT . At . toSlot . fromIntegral @Word64 @Natural <$> arbitrary
        ]

instance Arbitrary (ApiT Address) where
    arbitrary = genApiTAddress

instance Arbitrary (ApiT Customer) where
    arbitrary = genApiTCustomer

instance Arbitrary (ApiT CustomerList) where
    arbitrary = genApiTCustomerList

instance Arbitrary (ApiT ChainPoint) where
    arbitrary = genApiTChainPoint

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral
