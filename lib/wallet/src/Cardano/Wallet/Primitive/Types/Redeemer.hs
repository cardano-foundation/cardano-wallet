{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Types and functions relating to Plutus script redeemers
module Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer (..)
    , redeemerData
    ) where

import Prelude

import Cardano.Api
    ( StakeAddress
    , serialiseToBech32
    )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn
    )
import Data.ByteString
    ( ByteString
    )
import Fmt
    ( Buildable (..)
    )
import GHC.Generics
    ( Generic
    )

data Redeemer
    = RedeemerSpending ByteString TxIn
    | RedeemerMinting ByteString TokenPolicyId
    | RedeemerRewarding ByteString StakeAddress
    deriving (Eq, Generic, Show)

instance Buildable Redeemer where
    build = \case
        RedeemerSpending _ input ->
            "spending(" <> build input <> ")"
        RedeemerMinting _ pid ->
            "minting(" <> build pid <> ")"
        RedeemerRewarding _ addr ->
            "rewarding(" <> build (serialiseToBech32 addr) <> ")"

redeemerData :: Redeemer -> ByteString
redeemerData = \case
    RedeemerSpending bytes _ -> bytes
    RedeemerMinting bytes _ -> bytes
    RedeemerRewarding bytes _ -> bytes
