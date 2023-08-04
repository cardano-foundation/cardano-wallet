{-# LANGUAGE OverloadedStrings #-}

{- Serialise the contract

    Plutus.Contracts.Currency

from the

    plutus-use-cases

repository. Running this code requires a working Plutus environment.
This includes, but is not limited to, the following packages:

    plutus-core, plutus-tx, plutus-contract, plutus-ledger, plutus-ledger-api

-}
module CurrencyContract where

{- HLINT ignore "Avoid restricted qualification" -}

import Codec.Serialise
  ( serialise
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Ledger
  ( MintingPolicy (..)
  , Script
  , TxId (..)
  , TxOutRef (..)
  )
import Plutus.Contracts.Currency as Example
  ( OneShotCurrency (..)
  , curPolicy
  )
import qualified PlutusTx.AssocMap as AssocMap
import PlutusTx.Builtins.Class
import Prelude

{-----------------------------------------------------------------------------
    Serialize the example script
------------------------------------------------------------------------------}
myscript :: Script
myscript = getMintingPolicy $ Example.curPolicy mycurrency

mycurrency :: OneShotCurrency
mycurrency = OneShotCurrency (h, i) amounts
  where
    TxOutRef h i = dummyTxOutRef
    amounts = AssocMap.fromList [("apfel", 1000), ("banana", 1)]

{-----------------------------------------------------------------------------
    Utility functions for serialization
------------------------------------------------------------------------------}

-- | Hex encoded bytes
type Base16 = String

-- | 'serialise' produces a CBOR representation of the binary script
rawScript :: Script -> Base16
rawScript = B8.unpack . B16.encode . BL.toStrict . serialise

-- | A dummy TxOutRef that is easy to copy & replace.
dummyTxOutRef :: TxOutRef
dummyTxOutRef = TxOutRef (mkTxId s32) 31
  where
    s32 = mconcat $ replicate 8 "DEADBEEF" -- 32 = 4*8

-- | TxId corresponds to 32 bytes
mkTxId :: Base16 -> TxId
mkTxId = TxId . toBuiltin . B16.decodeLenient . B8.pack
