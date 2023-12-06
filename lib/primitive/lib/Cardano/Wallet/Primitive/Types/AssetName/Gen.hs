module Cardano.Wallet.Primitive.Types.AssetName.Gen
    (
    -- * Generators and shrinkers
      genAssetName
    , genAssetNameLargeRange
    , shrinkAssetName

    -- * Test values
    , testAssetNames

    -- * Creation of test values
    , mkAssetName

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.AssetName
    ( AssetName (..)
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
-- Asset names chosen from a range that depends on the size parameter
--------------------------------------------------------------------------------

genAssetName :: Gen AssetName
genAssetName = sized $ \n -> elements $ take (max 1 n) testAssetNames

shrinkAssetName :: AssetName -> [AssetName]
shrinkAssetName i
    | i == simplest = []
    | otherwise = [simplest]
  where
    simplest = head testAssetNames

--------------------------------------------------------------------------------
-- Asset names chosen from a large range (to minimize the risk of collisions)
--------------------------------------------------------------------------------

genAssetNameLargeRange :: Gen AssetName
genAssetNameLargeRange = UnsafeAssetName . BS.pack <$> vector 32

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

testAssetNames :: [AssetName]
testAssetNames = mkAssetName <$> ['A' .. 'Z']

mkAssetName :: Char -> AssetName
mkAssetName = UnsafeAssetName . B8.snoc "Asset"
