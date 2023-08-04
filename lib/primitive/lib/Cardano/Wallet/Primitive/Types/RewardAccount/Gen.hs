module Cardano.Wallet.Primitive.Types.RewardAccount.Gen
  ( genRewardAccount
  , shrinkRewardAccount
  )
where

import Cardano.Wallet.Primitive.Types.Hash
  ( Hash (..)
  , mockHashRewardAccount
  )
import Cardano.Wallet.Primitive.Types.RewardAccount
  ( RewardAccount (..)
  )
import Data.ByteString.Char8 qualified as B8
import Test.QuickCheck
  ( Gen
  , elements
  , sized
  )
import Prelude

--------------------------------------------------------------------------------
-- Reward accounts generated according to the size parameter
--------------------------------------------------------------------------------

genRewardAccount :: Gen (RewardAccount)
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
mkRewardAccount c =
  FromKeyHash
    . getHash
    . mockHashRewardAccount
    $ "Reward" `B8.snoc` c
