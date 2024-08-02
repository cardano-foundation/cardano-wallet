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
    , decode
    , encode
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitrarySizedBoundedIntegral
    , chooseInt
    , property
    , shrinkIntegral
    , vectorOf
    , (===)
    )

import qualified Data.ByteString as BS

spec :: Spec
spec =
    describe "JSON serialization & deserialization" $ do
        it "ApiT Address" $ property $
            prop_jsonRoundtrip @(ApiT Address)
        it "ApiT Customer" $ property $
            prop_jsonRoundtrip @(ApiT Customer)
        it "ApiT CustomerList" $ property $
            prop_jsonRoundtrip @(ApiT CustomerList)

prop_jsonRoundtrip :: (Eq a, Show a, FromJSON a, ToJSON a) => a -> Property
prop_jsonRoundtrip val =
    decode (encode val) === Just val

instance Arbitrary (ApiT Address) where
    arbitrary = do
        --enterprise address type with key hash credential is 01100000, network (mainnet) is 1
        --meaning first byte is 01100001 ie. 96+1=97
        let firstByte = 97
        keyhashCred <- BS.pack <$> vectorOf 28 arbitrary
        pure $ ApiT $ fromRawAddress $ BS.append (BS.singleton firstByte) keyhashCred

instance Arbitrary (ApiT Customer) where
    arbitrary =
        ApiT . fromRawCustomer <$> arbitrary

instance Arbitrary (ApiT CustomerList) where
    arbitrary = do
        listLen <- chooseInt (0, 100)
        let genPair = (,) <$> (unApiT <$> arbitrary) <*> (unApiT <$> arbitrary)
        ApiT <$> vectorOf listLen genPair

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral
