{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Copyright: © 2020-2022 IOHK
License: Apache-2.0

Raw withdrawals data extraction from 'Tx'
-}
module Cardano.Read.Ledger.Tx.Withdrawals
    ( -- * Withdrawal type
      WithdrawalsType
    , Withdrawals (..)
    , RewardWithdrawals

      -- * Extraction
    , getEraWithdrawals
    , shelleyWithdrawals
    ) where

import Prelude

import Cardano.Ledger.Address
    ( RewardAccount
    , unWithdrawals
    )
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Ledger.Core
    ( bodyTxL
    , withdrawalsTxBodyL
    )
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import Cardano.Read.Ledger.Tx.Eras
    ( onTx
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx (..)
    )
import Control.Lens
    ( view
    )
import Data.Map
    ( Map
    )

-- |
-- Era-specific withdrawal type.
--
-- Byron does not support staking, so withdrawals return unit @()@.
-- Shelley and later return a map from reward accounts to coin amounts.
type family WithdrawalsType era where
    WithdrawalsType Byron = ()
    WithdrawalsType Shelley = RewardWithdrawals
    WithdrawalsType Allegra = RewardWithdrawals
    WithdrawalsType Mary = RewardWithdrawals
    WithdrawalsType Alonzo = RewardWithdrawals
    WithdrawalsType Babbage = RewardWithdrawals
    WithdrawalsType Conway = RewardWithdrawals

-- | Map from reward accounts to coin amounts being withdrawn.
type RewardWithdrawals = Map RewardAccount Coin

-- | Era-indexed stake reward withdrawals wrapper.
newtype Withdrawals era
    = Withdrawals {withdrawalsAsMap :: WithdrawalsType era}

deriving instance Show (WithdrawalsType era) => Show (Withdrawals era)
deriving instance Eq (WithdrawalsType era) => Eq (Withdrawals era)

{-# INLINEABLE getEraWithdrawals #-}

-- | Extract withdrawals from tx for any available era.
getEraWithdrawals
    :: forall era. IsEra era => Tx era -> Withdrawals era
getEraWithdrawals = case theEra @era of
    Byron -> \_ -> Withdrawals ()
    Shelley -> withdrawals
    Allegra -> withdrawals
    Mary -> withdrawals
    Alonzo -> withdrawals
    Babbage -> withdrawals
    Conway -> withdrawals
  where
    withdrawals = onTx $ Withdrawals . shelleyWithdrawals

-- | Extract withdrawals from a Shelley-era (or later) transaction.
shelleyWithdrawals
    :: Ledger.EraTx era
    => Ledger.Tx era
    -> Map RewardAccount Coin
shelleyWithdrawals = unWithdrawals . view (bodyTxL . withdrawalsTxBodyL)
