{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, ByronEra, MaryEra, ShelleyEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Read.Eras
    ( EraFun (..) )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.Shelley.API as SH

type family MintType era where
  MintType ByronEra = ()
  MintType ShelleyEra = ()
  MintType AllegraEra = SH.Coin
  MintType MaryEra = Mary.Value StandardCrypto
  MintType AlonzoEra = Mary.Value StandardCrypto
  MintType BabbageEra = Mary.Value StandardCrypto

newtype Mint era = Mint (MintType era)

deriving instance Show (MintType era) => Show (Mint era)
deriving instance Eq (MintType era) => Eq (Mint era)

getEraMint :: EraFun Tx Mint
getEraMint = EraFun
    { byronFun = \_ -> Mint ()
    , shelleyFun = \_ -> Mint ()
    , allegraFun = onTx $ \(SH.Tx b _ _ ) -> getMint b
    , maryFun = onTx $ \(SH.Tx b _ _ ) -> getMint b
    , alonzoFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getMint b
    , babbageFun = onTx $ \(AL.ValidatedTx b _ _ _) -> getMint b
    }

getMint :: HasField "mint" a (MintType b) => a -> Mint b
getMint =  Mint . getField @"mint"
