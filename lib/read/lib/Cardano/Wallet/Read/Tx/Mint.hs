{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw mint data extraction from 'Tx'
--

module Cardano.Wallet.Read.Tx.Mint
    ( MintType
    , Mint (..)
    , getEraMint
    ) where

import Prelude

import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Mary.Core
    ( mintTxBodyL
    )
import Cardano.Ledger.Mary.Value
    ( MultiAsset
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
    ( view
    )

type family MintType era where
  MintType Byron = ()
  MintType Shelley = ()
  MintType Allegra = ()
  MintType Mary = MultiAsset StandardCrypto
  MintType Alonzo = MultiAsset StandardCrypto
  MintType Babbage = MultiAsset StandardCrypto
  MintType Conway = MultiAsset StandardCrypto

newtype Mint era = Mint (MintType era)

deriving instance Show (MintType era) => Show (Mint era)
deriving instance Eq (MintType era) => Eq (Mint era)

getEraMint :: forall era. IsEra era => Tx era -> Mint era
getEraMint = case theEra @era of
    Byron -> \_ -> Mint ()
    Shelley -> \_ -> Mint ()
    Allegra -> \_ -> Mint ()
    Mary -> mint
    Alonzo -> mint
    Babbage -> mint
    Conway -> mint
  where
    mint = onTx $ Mint . view (bodyTxL . mintTxBodyL)
