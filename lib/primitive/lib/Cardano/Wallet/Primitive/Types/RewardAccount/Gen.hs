module Cardano.Wallet.Primitive.Types.RewardAccount.Gen
    ( genRewardAccount
    , shrinkRewardAccount
    )
    where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..), mockHashRewardAccount )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Test.QuickCheck
    ( Gen, elements, sized )

import qualified Data.ByteString.Char8 as B8

--------------------------------------------------------------------------------
-- Reward accounts generated according to the size parameter
--------------------------------------------------------------------------------

genRewardAccount :: Gen RewardAccount
genRewardAccount = sized $ \size -> elements $ take (max 1 size) addresses

shrinkRewardAccount :: RewardAccount -> [RewardAccount]
shrinkRewardAccount a
    | a == simplest = []
    | otherwise = [simplest]
  where
    simplest = head addresses

addresses :: [RewardAccount]
addresses = mkRewardAccount <$> ['0' ..]

--------------------------------------------------------------------------------
-- Internal utilities
--------------------------------------------------------------------------------

mkRewardAccount :: Char -> RewardAccount
mkRewardAccount c
    = FromKeyHash . getHash
    . mockHashRewardAccount $ "Reward" `B8.snoc` c
