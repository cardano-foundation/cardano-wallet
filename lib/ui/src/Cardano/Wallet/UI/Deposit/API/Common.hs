{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.Deposit.API.Common where

import Prelude

import Cardano.Wallet.Deposit.Pure
    ( Customer
    )
import Cardano.Wallet.Deposit.Read
    ( TxId
    )
import Cardano.Wallet.Read
    ( SlotNo (..)
    , WithOrigin (..)
    , hashFromTxId
    , txIdFromHash
    )
import Cardano.Wallet.Read.Hash
    ( hashFromStringAsHex
    , hashToStringAsHex
    )
import Data.Hashable
    ( Hashable
    )
import Data.Text.Class
    ( ToText (..)
    )
import Data.Time
    ( UTCTime
    )
import Servant
    ( FromHttpApiData (..)
    , ToHttpApiData (..)
    )
import Web.FormUrlEncoded
    ( FromForm (..)
    , parseUnique
    )

import qualified Data.Text as T

instance FromForm Customer where
    fromForm form = fromIntegral @Int <$> parseUnique "customer" form

instance FromHttpApiData SlotNo where
    parseUrlPiece = fmap SlotNo . parseUrlPiece

instance FromHttpApiData t => FromHttpApiData (WithOrigin t) where
    parseUrlPiece "Origin" = pure Origin
    parseUrlPiece t = At <$> parseUrlPiece t

instance ToHttpApiData SlotNo where
    toUrlPiece (SlotNo t) = toUrlPiece t

instance ToHttpApiData t => ToHttpApiData (WithOrigin t) where
    toUrlPiece Origin = "Origin"
    toUrlPiece (At t) = toUrlPiece t

data Expand = Expand | Collapse
    deriving (Eq, Show, Enum, Bounded)

instance ToHttpApiData Expand where
    toUrlPiece Expand = "expand"
    toUrlPiece Collapse = "collapse"

instance FromHttpApiData Expand where
    parseUrlPiece "expand" = Right Expand
    parseUrlPiece "collapse" = Right Collapse
    parseUrlPiece _ = Left "Invalid expand/collapse"

deriving instance Hashable (WithOrigin UTCTime)

instance ToHttpApiData Customer where
    toUrlPiece = toUrlPiece . toText

instance FromHttpApiData Customer where
    parseUrlPiece = fmap (fromIntegral @Int) . parseUrlPiece

instance FromHttpApiData TxId where
    parseUrlPiece x =
        case fmap txIdFromHash . hashFromStringAsHex . T.unpack $ x of
            Just txId -> Right txId
            _ -> Left "Invalid TxId"

instance ToHttpApiData TxId where
    toUrlPiece = T.pack . hashToStringAsHex . hashFromTxId
