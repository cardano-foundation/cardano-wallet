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
-- Raw fee data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Fee
    ( FeeType
    , Fee (..)
    , getEraFee
    )
    where

import Prelude

import Cardano.Ledger.Coin
    ( Coin
    )
import Cardano.Ledger.Core
    ( bodyTxL
    , feeTxBodyL
    )
import Cardano.Wallet.Read.Eras
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
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
    )
import Control.Lens
    ( (^.)
    )

type family FeeType era where
    FeeType Byron = ()
    FeeType Shelley = Coin
    FeeType Allegra = Coin
    FeeType Mary = Coin
    FeeType Alonzo = Coin
    FeeType Babbage = Coin
    FeeType Conway = Coin

newtype Fee era = Fee (FeeType era)

deriving instance Show (FeeType era) => Show (Fee era)
deriving instance Eq (FeeType era) => Eq (Fee era)

{-# INLINABLE getEraFee #-}
-- | Extract fee from 'Tx' in all available eras.
getEraFee :: forall era. IsEra era => Tx era -> Fee era
getEraFee = case theEra @era of
    Byron -> onTx $ \_ -> Fee ()
    Shelley -> mkFee
    Allegra -> mkFee
    Mary -> mkFee
    Alonzo -> mkFee
    Babbage -> mkFee
    Conway -> mkFee
  where
    mkFee = onTx $ \tx -> Fee $ tx ^. bodyTxL . feeTxBodyL
