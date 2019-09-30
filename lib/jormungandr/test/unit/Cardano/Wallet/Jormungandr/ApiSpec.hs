{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.ApiSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Api
    ( ApiStakeDistribution (..), ApiT (..), StakeApiResponse (..) )
import Cardano.Wallet.Primitive.Types
    ( PoolId (..) )
import Data.Aeson
    ( eitherDecode )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )

import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec = do
    describe "Jormungandr Api" $ do

        it "example stake endpoint response is properly decoded" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[[\"7d749ef424507fb80fed0d2289d535a94f6870add0cf8b3\
                    \74cfe6cae078320ec\",1]],\"unassigned\":100100000000000}}"
            decodeJSON exampleStake `shouldBe`
                Right StakeApiResponse {
                   epoch = 252054,
                   stake = ApiStakeDistribution {
                       dangling = ApiT (Quantity 0),
                       pools = [((ApiT (PoolId "}t\158\244$P\DEL\184\SI\237\r\"\137\213\&5\169Ohp\173\208\207\139\&7L\254l\174\a\131 \236"))
                                 , (ApiT (Quantity 1)))],
                       unassigned = ApiT (Quantity 100100000000000)
                       }
                    }
            return ()

        it "example empty stake endpoint response is properly decoded" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[],\"unassigned\":100100000000000}}"
            decodeJSON exampleStake `shouldBe`
                Right StakeApiResponse {
                   epoch = 252054,
                   stake = ApiStakeDistribution {
                       dangling = ApiT (Quantity 0),
                       pools = [],
                       unassigned = ApiT (Quantity 100100000000000)
                       }
                    }
            return ()

        it "invalid stake pool id in endpoint response gives expected error" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[[\"b80fed0d2289d535a94f6870add0cf8b3\
                    \74cfe6cae078320ec\",1]],\"unassigned\":100100000000000}}"
            decodeJSON exampleStake `shouldBe`
                Left "Error in $.stake.pools[0][0]: stake pool id invalid: \
                     \expected 32 bytes but got 25"
            return ()

        it "invalid stake pool id in endpoint response gives expected error" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[[12345,1]],\"unassigned\":100100000000000}}"
            decodeJSON exampleStake `shouldBe`
                Left "Error in $.stake.pools[0][0]: expected Text, encountered Number"
            return ()

        it "invalid stake pair in endpoint response gives expected error" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[[\"7d749ef424507fb80fed0d2289d535a94f6870add0cf8b3\
                    \74cfe6cae078320ec\",1, \"not needed field\"]],\"unassigned\":\
                    \100100000000000}}"
            decodeJSON exampleStake `shouldBe`
                Left "Error in $.stake.pools[0]: cannot unpack array of length 3\
                     \ into a tuple of length 2"
            return ()

        it "invalid numerical field value in endpoint response gives expected error" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[[\"7d749ef424507fb80fed0d2289d535a94f6870add0cf8b3\
                    \74cfe6cae078320ec\",1]],\"unassigned\":[]}}"
            decodeJSON exampleStake `shouldBe`
                Left "Error in $.stake.unassigned: expected Word64, \
                     \encountered Array"
            return ()

        it "invalid non-numerical field value in endpoint response gives expected error" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[[\"7d749ef424507fb80fed0d2289d535a94f6870add0cf8b3\
                    \74cfe6cae078320ec\",1]],\"unassigned\":-10010000}}"
            decodeJSON exampleStake `shouldBe`
                Left "Error in $.stake.unassigned: Word64 is either floating or \
                     \will cause over or underflow: -1.001e7"
            return ()

        it "invalid non-numerical field value in endpoint response gives expected error" $ do
            let exampleStake = "{\"epoch\": 252054,\"stake\": {\"dangling\":0,\"\
                    \pools\":[[\"7d749ef424507fb80fed0d2289d535a94f6870add0cf8b3\
                    \74cfe6cae078320ec\",1]],\"unassigned\":10010000.23}}"
            decodeJSON exampleStake `shouldBe`
                Left "Error in $.stake.unassigned: Word64 is either floating or \
                     \will cause over or underflow: 1.001000023e7"
            return ()
  where
    decodeJSON = eitherDecode :: BL.ByteString -> Either String StakeApiResponse
