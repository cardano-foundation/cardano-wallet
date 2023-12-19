{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- |
-- Copyright: © 2023 Cardano Foundation
-- License: Apache-2.0
--
-- Haskell implementation of the API `amount` type, which represents a natural
-- quantity of lovelace.
--
module Cardano.Wallet.Api.Types.Amount
    ( ApiAmount (..)
    , fromCoin
    , fromNatural
    , fromWord64
    , toCoin
    , toInteger
    )
    where

import Prelude hiding
    ( toInteger
    )

import Control.DeepSeq
    ( NFData
    )
import Data.Aeson.Types
    ( FromJSON
    , ToJSON
    )
import Data.Data
    ( Data
    )
import Data.Hashable
    ( Hashable
    )
import Data.IntCast
    ( intCast
    )
import Data.Monoid
    ( Sum (Sum)
    )
import Data.Quantity
    ( Quantity (..)
    )
import Data.Text.Class
    ( FromText
    , ToText
    )
import Data.Word
    ( Word64
    )
import GHC.Generics
    ( Generic
    )
import Numeric.Natural
    ( Natural
    )

import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Data.Quantity as W

newtype ApiAmount = ApiAmount {toNatural :: Natural}
    deriving stock (Data, Eq, Generic, Ord, Show)
    deriving (FromJSON, ToJSON) via W.Quantity "lovelace" Natural
    deriving (FromText, ToText) via W.Quantity "lovelace" Natural
    deriving (Semigroup, Monoid) via Sum Natural
    deriving anyclass (Hashable, NFData)

fromCoin :: W.Coin -> ApiAmount
fromCoin (W.Coin n) = ApiAmount n

fromNatural :: Natural -> ApiAmount
fromNatural = ApiAmount

fromWord64 :: Word64 -> ApiAmount
fromWord64 = ApiAmount . intCast

toCoin :: ApiAmount -> W.Coin
toCoin (ApiAmount n) = W.Coin n

toInteger :: ApiAmount -> Integer
toInteger (ApiAmount n) = intCast n
