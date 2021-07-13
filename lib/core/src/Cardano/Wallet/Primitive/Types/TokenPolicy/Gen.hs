module Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameSized
    , genTokenNameLargeRange
    , genTokenNameMediumRange
    , genTokenNameSmallRange
    , genTokenPolicyIdSized
    , genTokenPolicyIdLargeRange
    , genTokenPolicyIdSmallRange
    , mkTokenPolicyId
    , shrinkTokenNameSized
    , shrinkTokenNameMediumRange
    , shrinkTokenNameSmallRange
    , shrinkTokenPolicyIdSized
    , shrinkTokenPolicyIdSmallRange
    , tokenNamesMediumRange
    , tokenNamesSmallRange
    , tokenPolicies
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Data.Either
    ( fromRight )
import Data.Text.Class
    ( FromText (..) )
import Test.QuickCheck
    ( Gen, elements, sized, vector )

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Token names chosen from a range that depends on the size parameter
--------------------------------------------------------------------------------

genTokenNameSized :: Gen TokenName
genTokenNameSized = sized $ \size ->
    elements $ UnsafeTokenName . B8.snoc "Token" <$> take size ['A' ..]

shrinkTokenNameSized :: TokenName -> [TokenName]
shrinkTokenNameSized x
    | x == simplest = []
    | otherwise = [simplest]
  where
    simplest = UnsafeTokenName "TokenA"

--------------------------------------------------------------------------------
-- Token names chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTokenNameSmallRange :: Gen TokenName
genTokenNameSmallRange = elements tokenNamesSmallRange

shrinkTokenNameSmallRange :: TokenName -> [TokenName]
shrinkTokenNameSmallRange x
    | x == simplest = []
    | otherwise = [simplest]
  where
    simplest = head tokenNamesSmallRange

tokenNamesSmallRange :: [TokenName]
tokenNamesSmallRange = UnsafeTokenName . B8.snoc "Token" <$> ['A' .. 'D']

--------------------------------------------------------------------------------
-- Token names chosen from a medium-sized range (to minimize the risk of
-- collisions)
--------------------------------------------------------------------------------

genTokenNameMediumRange :: Gen TokenName
genTokenNameMediumRange = elements tokenNamesMediumRange

shrinkTokenNameMediumRange :: TokenName -> [TokenName]
shrinkTokenNameMediumRange x
    | x == simplest = []
    | otherwise = [simplest]
  where
    simplest = head tokenNamesMediumRange

tokenNamesMediumRange :: [TokenName]
tokenNamesMediumRange = UnsafeTokenName . B8.snoc "Token" <$> ['A' .. 'Z']

--------------------------------------------------------------------------------
-- Token names chosen from a large range (to minimize the risk of collisions)
--------------------------------------------------------------------------------

genTokenNameLargeRange :: Gen TokenName
genTokenNameLargeRange = UnsafeTokenName . BS.pack <$> vector 32

--------------------------------------------------------------------------------
-- Token policy identifiers chosen from a range that depends on the size
-- parameter
--------------------------------------------------------------------------------

genTokenPolicyIdSized :: Gen TokenPolicyId
genTokenPolicyIdSized = sized $ \size ->
    elements $ mkTokenPolicyId <$> take size mkTokenPolicyIdValidChars

shrinkTokenPolicyIdSized :: TokenPolicyId -> [TokenPolicyId]
shrinkTokenPolicyIdSized x
    | x == simplest = []
    | otherwise = [simplest]
  where
    simplest = mkTokenPolicyId 'A'

--------------------------------------------------------------------------------
-- Token policy identifiers chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTokenPolicyIdSmallRange :: Gen TokenPolicyId
genTokenPolicyIdSmallRange = elements tokenPolicies

shrinkTokenPolicyIdSmallRange :: TokenPolicyId -> [TokenPolicyId]
shrinkTokenPolicyIdSmallRange x
    | x == simplest = []
    | otherwise = [simplest]
  where
    simplest = head tokenPolicies

tokenPolicies :: [TokenPolicyId]
tokenPolicies = mkTokenPolicyId <$> ['A' .. 'D']

--------------------------------------------------------------------------------
-- Token policy identifiers chosen from a large range (to minimize the risk of
-- collisions)
--------------------------------------------------------------------------------

genTokenPolicyIdLargeRange :: Gen TokenPolicyId
genTokenPolicyIdLargeRange = UnsafeTokenPolicyId . Hash . BS.pack <$> vector 28

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

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
