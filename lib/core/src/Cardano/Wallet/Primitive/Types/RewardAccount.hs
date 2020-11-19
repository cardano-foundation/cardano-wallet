{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the 'RewardAccount' data type.
--
module Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.ByteString
    ( ByteString )
import Data.Text.Class
    ( FromText (..), ToText (..) )
import Fmt
    ( Buildable (..) )
import GHC.Generics
    ( Generic )

-- | A reward account is used in group-type addresses for delegation.
--
-- It is the public key of the account address.
--
newtype RewardAccount = RewardAccount { unRewardAccount :: ByteString }
    deriving (Generic, Show, Eq, Ord)

instance NFData RewardAccount

instance Buildable RewardAccount where
    build = build . Hash @"RewardAccount" . unRewardAccount

instance ToText RewardAccount where
    toText = toText . Hash @"RewardAccount" . unRewardAccount

instance FromText RewardAccount where
    fromText = fmap (RewardAccount . getHash @"RewardAccount") . fromText
