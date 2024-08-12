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
    , Customer
    , CustomerList
    )
import Cardano.Wallet.Deposit.HTTP.Types.OpenAPI
    ( addressSchema
    , customerListSchema
    , customerSchema
    , depositDefinitions
    )
import Cardano.Wallet.Deposit.Pure
    ( Word31
    , fromRawCustomer
    )
import Cardano.Wallet.Deposit.Read
    ( fromRawAddress
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
    describe "schema checks" $ do
        it "ApiT Address"
            $ forAll genApiAddress
            $ counterExampleJSON "validate"
            $ validateInstance depositDefinitions addressSchema
        it "ApiT Customer"
            $ forAll genApiCustomer
            $ counterExampleJSON "validate"
            $ validateInstance depositDefinitions customerSchema
        it "ApiT CustomerList"
            $ forAll genApiCustomerList
            $ counterExampleJSON "validate"
            $ validateInstance depositDefinitions customerListSchema

validate :: Definitions Schema -> Schema -> Value -> Expectation
validate defs sch x = validateJSON defs sch x `shouldBe` []

validateInstance :: ToJSON a => Definitions Schema -> Schema -> a -> Expectation
validateInstance defs sch = validate defs sch . toJSON

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

genApiAddress :: Gen (ApiT Address)
genApiAddress = do
    --enterprise address type with key hash credential is 01100000, network (mainnet) is 1
    --meaning first byte is 01100001 ie. 96+1=97
    let firstByte = 97
    keyhashCred <- BS.pack <$> vectorOf 28 arbitrary
    pure $ ApiT $ fromRawAddress $ BS.append (BS.singleton firstByte) keyhashCred

genApiCustomer :: Gen (ApiT Customer)
genApiCustomer =
    ApiT . fromRawCustomer <$> arbitrary

genApiCustomerList :: Gen (ApiT CustomerList)
genApiCustomerList = do
    listLen <- chooseInt (0, 100)
    let genPair = (,) <$> (unApiT <$> arbitrary) <*> (unApiT <$> arbitrary)
    vectors <- vectorOf listLen genPair
    let uniqueCustomer = L.nubBy (\a b -> fst a == fst b)
    let uniqueAddr = L.nubBy (\a b -> snd a == snd b)
    pure $ ApiT $ uniqueAddr $ uniqueCustomer vectors

instance Arbitrary (ApiT Address) where
    arbitrary = genApiAddress

instance Arbitrary (ApiT Customer) where
    arbitrary = genApiCustomer

instance Arbitrary (ApiT CustomerList) where
    arbitrary = genApiCustomerList

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral
