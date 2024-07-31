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
import Cardano.Wallet.Deposit.Read
    ( fromRawAddress
    , fromRawCustomer
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
    , chooseInt
    , property
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
        --enterprise address type is 0b11110000, network (mainnet) is 1
        --meaning first byte is 0b11110001 ie. 241
        let firstByte = 241
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
