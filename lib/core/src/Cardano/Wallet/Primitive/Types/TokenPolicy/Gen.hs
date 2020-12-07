module Cardano.Wallet.Primitive.Types.TokenPolicy.Gen
    ( genTokenNameSmallRange
    , genTokenPolicyIdSmallRange
    , shrinkTokenNameSmallRange
    , shrinkTokenPolicyIdSmallRange
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName, TokenPolicyId )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Test.QuickCheck
    ( Gen, elements )

import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as TP
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Token names chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTokenNameSmallRange :: Gen TokenName
genTokenNameSmallRange = elements tokenNames

shrinkTokenNameSmallRange :: TokenName -> [TokenName]
shrinkTokenNameSmallRange name = filter (< name) tokenNames

tokenNames :: [TokenName]
tokenNames = TP.mkTokenName . ("Token" `T.snoc`) <$> ['A' .. 'D']

--------------------------------------------------------------------------------
-- Token policy identifiers chosen from a small range (to allow collisions)
--------------------------------------------------------------------------------

genTokenPolicyIdSmallRange :: Gen TokenPolicyId
genTokenPolicyIdSmallRange = elements tokenPolicies

shrinkTokenPolicyIdSmallRange :: TokenPolicyId -> [TokenPolicyId]
shrinkTokenPolicyIdSmallRange policy = filter (< policy) tokenPolicies

tokenPolicies :: [TokenPolicyId]
tokenPolicies = mkTokenPolicyId <$> ['A' .. 'D']

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

mkTokenPolicyId :: Char -> TokenPolicyId
mkTokenPolicyId
    = TP.mkTokenPolicyId
    . unsafeFromHex
    . B8.replicate tokenPolicyIdHexStringLength

tokenPolicyIdHexStringLength :: Int
tokenPolicyIdHexStringLength = 56
