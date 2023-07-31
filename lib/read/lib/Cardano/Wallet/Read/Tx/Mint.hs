{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
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

import Cardano.Api
    ( AllegraEra
    , AlonzoEra
    , BabbageEra
    , ByronEra
    , ConwayEra
    , MaryEra
    , ShelleyEra
    )
import Cardano.Ledger.Core
    ( bodyTxL )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.Mary.Core
    ( mintTxBodyL )
import Cardano.Ledger.Mary.Value
    ( MultiAsset )
import Cardano.Wallet.Read.Eras.EraFun
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import Control.Lens
    ( view )

type family MintType era where
  MintType ByronEra = ()
  MintType ShelleyEra = ()
  MintType AllegraEra = ()
  MintType MaryEra = MultiAsset StandardCrypto
  MintType AlonzoEra = MultiAsset StandardCrypto
  MintType BabbageEra = MultiAsset StandardCrypto
  MintType ConwayEra = MultiAsset StandardCrypto

newtype Mint era = Mint (MintType era)

deriving instance Show (MintType era) => Show (Mint era)
deriving instance Eq (MintType era) => Eq (Mint era)

getEraMint :: EraFun Tx Mint
getEraMint =
    EraFun
        { byronFun = \_ -> Mint ()
        , shelleyFun = \_ -> Mint ()
        , allegraFun =  \_ -> Mint ()
        , maryFun = mint
        , alonzoFun = mint
        , babbageFun = mint
        , conwayFun = mint
        }
  where
    mint = onTx $ Mint . view (bodyTxL . mintTxBodyL)
