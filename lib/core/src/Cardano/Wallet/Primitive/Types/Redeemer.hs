{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Types and functions relating to Plutus script redeemers
--

module Cardano.Wallet.Primitive.Types.Redeemer
    ( Redeemer (..)
    , redeemerData
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxIn )
import Data.ByteString
    ( ByteString )
import GHC.Generics
    ( Generic )

data Redeemer
    = RedeemerSpending ByteString TxIn
    | RedeemerMinting ByteString TokenPolicyId
    | RedeemerRewarding ByteString RewardAccount
    deriving (Eq, Generic, Show)

redeemerData :: Redeemer -> ByteString
redeemerData = \case
    RedeemerSpending  bytes _ -> bytes
    RedeemerMinting   bytes _ -> bytes
    RedeemerRewarding bytes _ -> bytes
