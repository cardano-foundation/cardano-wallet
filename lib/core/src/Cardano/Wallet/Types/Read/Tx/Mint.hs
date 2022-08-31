{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Types.Read.Tx.Mint where

import Prelude

import Cardano.Api
    ( AllegraEra, AlonzoEra, BabbageEra, MaryEra )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Wallet.Types.Read.Tx.Eras
    ( UseTxAllEra, UseTxEra (..) )
import Generics.SOP
    ( NP ((:*), Nil) )
import GHC.Records
    ( HasField (..) )

import qualified Cardano.Ledger.Alonzo.Tx as AL
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.Shelley.API as SH

type family MintType era where
  MintType AllegraEra = SH.Coin
  MintType MaryEra = Mary.Value StandardCrypto
  MintType AlonzoEra = Mary.Value StandardCrypto
  MintType BabbageEra = Mary.Value StandardCrypto
  MintType _ = ()

newtype Mint era = Mint (MintType era)

deriving instance Show (MintType era) => Show (Mint era)
deriving instance Eq (MintType era) => Eq (Mint era)

getEraMint :: UseTxAllEra Mint
getEraMint =
    UseTxEra (\_ -> Mint ())
    :* UseTxEra (\_ -> Mint ())
    :* UseTxEra (\(SH.Tx b _ _ )  -> Mint $ getField @"mint" b)
    :* UseTxEra (\(SH.Tx b _ _ ) -> Mint $ getField @"mint" b)
    :* UseTxEra (\(AL.ValidatedTx b _ _ _) -> Mint $ getField @"mint" b)
    :* UseTxEra (\(AL.ValidatedTx b _ _ _) -> Mint $ getField @"mint" b)
    :* Nil
