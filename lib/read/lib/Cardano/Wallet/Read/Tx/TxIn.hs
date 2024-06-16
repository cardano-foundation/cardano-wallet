{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- 'TxIn' — transaction input.
--

module Cardano.Wallet.Read.Tx.TxIn
    ( TxIn
    , pattern TxIn
    , inputId
    , inputIx
    , TxIx
    , pattern TxIx
    , word16FromTxIx
    )
    where

import Prelude

import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Wallet.Read.Tx.TxId
    ( TxId
    )
import Data.Word
    ( Word16
    )

import qualified Cardano.Ledger.BaseTypes as SH
import qualified Cardano.Ledger.TxIn as SH

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}

-- | A 'TxIn' is a unique reference to a transaction output.
-- It references the 'TxId' and an output index.
--
-- Note: We use a type synonym here because we want zero-cost
-- coercion between @Set TxIn@ and @Set SH.TxIn StandardCrypto@.
-- Unfortunately, 'Set' expects a nominal role.
-- (See the design literature on 'Data.Coercible'.)
type TxIn = SH.TxIn StandardCrypto

{-# COMPLETE TxIn #-}
pattern TxIn :: TxId -> TxIx -> TxIn
pattern TxIn{inputId,inputIx} = SH.TxIn inputId inputIx

-- | Index of a transaction output.
-- Equivalent to 'Word16'.
type TxIx = SH.TxIx

{-# COMPLETE TxIx #-}
pattern TxIx :: Word16 -> TxIx
pattern TxIx{word16FromTxIx} <- (fromTxIx -> word16FromTxIx) where
    TxIx w16 = SH.mkTxIx w16

fromTxIx :: TxIx -> Word16
fromTxIx (SH.TxIx w16) = fromIntegral w16
