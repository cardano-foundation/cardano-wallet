module Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    (
    -- * Generators and shrinkers
      genTokenPolicyId
    , genTokenPolicyIdLargeRange
    , shrinkTokenPolicyId

    -- * Test values
    , testTokenPolicyIds

    -- * Creation of test values
    , mkTokenPolicyId

    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenPolicyId
    ( TokenPolicyId (..)
    )
import Data.Either
    ( fromRight
    )
import Data.Text.Class
    ( FromText (..)
    )
import Test.QuickCheck
    ( Gen
    , elements
    , sized
    , vector
    )

import qualified Data.ByteString as BS
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Token policy identifiers chosen from a range that depends on the size
-- parameter
--------------------------------------------------------------------------------

genTokenPolicyId :: Gen TokenPolicyId
genTokenPolicyId = sized $ \n -> elements $ take (max 1 n) testTokenPolicyIds

shrinkTokenPolicyId :: TokenPolicyId -> [TokenPolicyId]
shrinkTokenPolicyId i
    | i == simplest = []
    | otherwise = [simplest]
  where
    simplest = head testTokenPolicyIds

--------------------------------------------------------------------------------
-- Token policy identifiers chosen from a large range (to minimize the risk of
-- collisions)
--------------------------------------------------------------------------------

genTokenPolicyIdLargeRange :: Gen TokenPolicyId
genTokenPolicyIdLargeRange = UnsafeTokenPolicyId . Hash . BS.pack <$> vector 28

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

testTokenPolicyIds :: [TokenPolicyId]
testTokenPolicyIds = mkTokenPolicyId <$> mkTokenPolicyIdValidChars

-- The set of characters that can be passed to the 'mkTokenPolicyId' function.
--
mkTokenPolicyIdValidChars :: [Char]
mkTokenPolicyIdValidChars = ['0' .. '9'] <> ['A' .. 'F']

-- The input must be a character in the range [0-9] or [A-F].
--
mkTokenPolicyId :: Char -> TokenPolicyId
mkTokenPolicyId c
    = fromRight reportError
    $ fromText
    $ T.pack
    $ replicate tokenPolicyIdHexStringLength c
  where
    reportError = error $
        "Unable to generate token policy id from character: " <> show c

tokenPolicyIdHexStringLength :: Int
tokenPolicyIdHexStringLength = 56
