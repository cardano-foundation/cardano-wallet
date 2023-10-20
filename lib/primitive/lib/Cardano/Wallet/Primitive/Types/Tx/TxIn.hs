{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
--
-- This module defines the 'TxIn' type.
--
module Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    ) where

import Prelude

import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Data.Word
    ( Word32
    )
import Fmt
    ( Buildable (..)
    , ordinalF
    )
import GHC.Generics
    ( Generic
    )

data TxIn = TxIn
    { inputId
        :: !(Hash "Tx")
    , inputIx
        :: !Word32
    }
    deriving (Read, Show, Generic, Eq, Ord)

instance NFData TxIn

instance Buildable TxIn where
    build txin = mempty
        <> ordinalF (inputIx txin + 1)
        <> " "
        <> build (inputId txin)
