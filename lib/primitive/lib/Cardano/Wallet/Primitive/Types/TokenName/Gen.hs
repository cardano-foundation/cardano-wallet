module Cardano.Wallet.Primitive.Types.TokenName.Gen
    (
    -- * Generators and shrinkers
      genTokenName
    , genTokenNameLargeRange
    , shrinkTokenName

    -- * Test values
    , testTokenNames

    -- * Creation of test values
    , mkTokenName

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenName
    ( TokenName (..)
    )
import Test.QuickCheck
    ( Gen
    , elements
    , sized
    , vector
    )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8

--------------------------------------------------------------------------------
-- Token names chosen from a range that depends on the size parameter
--------------------------------------------------------------------------------

genTokenName :: Gen TokenName
genTokenName = sized $ \n -> elements $ take (max 1 n) testTokenNames

shrinkTokenName :: TokenName -> [TokenName]
shrinkTokenName i
    | i == simplest = []
    | otherwise = [simplest]
  where
    simplest = head testTokenNames

--------------------------------------------------------------------------------
-- Token names chosen from a large range (to minimize the risk of collisions)
--------------------------------------------------------------------------------

genTokenNameLargeRange :: Gen TokenName
genTokenNameLargeRange = UnsafeTokenName . BS.pack <$> vector 32

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

testTokenNames :: [TokenName]
testTokenNames = mkTokenName <$> ['A' .. 'Z']

mkTokenName :: Char -> TokenName
mkTokenName = UnsafeTokenName . B8.snoc "Token"
