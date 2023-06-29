{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the 'RewardAccount' data type.
module Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (String)
    )
import Data.Aeson.Extra
    ( aesonFromText
    )
import Data.ByteString
    ( ByteString
    )
import Data.Text.Class
    ( FromText (..)
    , ToText (..)
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )
import Quiet
    ( Quiet (..)
    )

import Data.Text as T

-- | A reward account is used in group-type addresses for delegation.
--
-- It is either the public key or script hash.
data RewardAccount
    = FromKeyHash ByteString
    | FromScriptHash ByteString
    deriving (Generic, Eq, Ord)
    deriving (Show, Read) via (Quiet RewardAccount)

instance NFData RewardAccount

instance Buildable RewardAccount where
    build (FromKeyHash bs) = build . Hash @"RewardAccount" $ bs
    build (FromScriptHash bs) = build . Hash @"RewardAccount" $ bs

instance ToText RewardAccount where
    toText (FromKeyHash bs) = toText . Hash @"RewardAccount" $ bs
    toText (FromScriptHash bs) = T.cons 's' . toText . Hash @"RewardAccount" $ bs

instance FromText RewardAccount where
    fromText txt = case T.splitAt 1 txt of
        ("s", txt') ->
            fmap (FromScriptHash . getHash @"RewardAccount") . fromText $ txt'
        _ ->
            -- Hash is hex encoded so 0-9af
            fmap (FromKeyHash . getHash @"RewardAccount") . fromText $ txt

instance ToJSON RewardAccount where
    toJSON = String . toText

instance FromJSON RewardAccount where
    parseJSON = aesonFromText "RewardAccount"
