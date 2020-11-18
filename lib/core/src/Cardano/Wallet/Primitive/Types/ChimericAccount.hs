{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module provides the 'ChimericAccount' data type, used for staking
-- purposes.
--
module Cardano.Wallet.Primitive.Types.ChimericAccount
    ( ChimericAccount (..)
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

-- | Also known as a staking key, a chimeric account is used in group-type
--   addresses for staking purposes.
--
-- It is the public key of the account address.
--
newtype ChimericAccount = ChimericAccount { unChimericAccount :: ByteString }
    deriving (Generic, Show, Eq, Ord)

instance NFData ChimericAccount

instance Buildable ChimericAccount where
    build = build . Hash @"ChimericAccount" . unChimericAccount

instance ToText ChimericAccount where
    toText = toText . Hash @"ChimericAccount" . unChimericAccount

instance FromText ChimericAccount where
    fromText = fmap (ChimericAccount . getHash @"ChimericAccount") . fromText
