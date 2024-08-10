{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw withdrawals data extraction from 'Tx'
--

module Cardano.Read.Ledger.Tx.Withdrawals
    ( WithdrawalsType
    , Withdrawals (..)
    , getEraWithdrawals
    , RewardWithdrawals
    , shelleyWithdrawals
    ) where

import Prelude

import Cardano.Ledger.Address
    ( RewardAccount
    , unWithdrawals
    )
import Cardano.Ledger.Api
    ( StandardCrypto
    )
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

import qualified Cardano.Ledger.Api as Ledger

type family WithdrawalsType era where
  WithdrawalsType Byron = ()
  WithdrawalsType Shelley = RewardWithdrawals
  WithdrawalsType Allegra = RewardWithdrawals
  WithdrawalsType Mary = RewardWithdrawals
  WithdrawalsType Alonzo = RewardWithdrawals
  WithdrawalsType Babbage = RewardWithdrawals
  WithdrawalsType Conway = RewardWithdrawals

type RewardWithdrawals = Map (RewardAccount StandardCrypto) Coin

newtype Withdrawals era
    = Withdrawals { withdrawalsAsMap :: WithdrawalsType era }

deriving instance Show (WithdrawalsType era) => Show (Withdrawals era)
deriving instance Eq (WithdrawalsType era) => Eq (Withdrawals era)

{-# INLINABLE getEraWithdrawals #-}
-- | Extract withdrawals from tx for any available era.
getEraWithdrawals :: forall era . IsEra era => Tx era -> Withdrawals era
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

shelleyWithdrawals
    :: Ledger.EraTx era
    => Ledger.Tx era
    -> Map (RewardAccount (Ledger.EraCrypto era)) Coin
shelleyWithdrawals = unWithdrawals . view (bodyTxL . withdrawalsTxBodyL)
