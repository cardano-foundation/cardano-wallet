{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.UI.Deposit.Types.Payments
    ( Receiver (..)
    )
where

import Prelude

import Cardano.Wallet.Deposit.Pure.API.Address
    ( decodeAddress
    , encodeAddress
    )
import Cardano.Wallet.Deposit.Read
    ( Address
    )
import Numeric.Natural
    ( Natural
    )
import Web.HttpApiData
    ( FromHttpApiData (parseUrlPiece)
    , ToHttpApiData (toUrlPiece)
    )

import qualified Data.Text as T

-- | A receiver of a payment.
data Receiver = Receiver
    { address :: Address
    -- ^ The address of the receiver.
    , amount :: Natural
    -- ^ The amount of lovelace to send to the receiver.
    }
    deriving (Eq, Show)

instance FromHttpApiData Receiver where
    parseUrlPiece t = case T.splitOn "," t of
        [addressText, amountText] -> do
            amount :: Natural <- case reads (T.unpack amountText) of
                [(n, "")] -> pure n
                _ -> Left "Amount must be a number"
            address <- parseUrlPiece addressText
            pure $ Receiver{address, amount}
        _ -> Left "Receiver must be in the format 'address,amount'"

instance ToHttpApiData Receiver where
    toUrlPiece Receiver{address, amount} =
        T.intercalate ","
            [ encodeAddress address
            , T.pack $ show amount
            ]

instance FromHttpApiData Address where
    parseUrlPiece t = case decodeAddress t of
        Left err -> Left $ T.pack $ show err
        Right address -> pure address
