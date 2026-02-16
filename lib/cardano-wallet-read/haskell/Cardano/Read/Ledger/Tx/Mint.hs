{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Raw mint data extraction from 'Tx'
module Cardano.Read.Ledger.Tx.Mint
    ( -- * Mint type
      MintType
    , Mint (..)

      -- * Extraction
    , getEraMint
    ) where

import Cardano.Ledger.Core
    ( bodyTxL
    )
import Cardano.Ledger.Mary.Core
    ( mintTxBodyL
    )
import Cardano.Ledger.Mary.Value
    ( MultiAsset
    )
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Dijkstra
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
import Prelude

-- |
-- Era-specific minting type.
--
-- Pre-Mary eras return unit @()@ as native tokens are not supported.
-- Mary and later return 'MultiAsset' representing minted\/burned tokens.
type family MintType era where
    MintType Byron = ()
    MintType Shelley = ()
    MintType Allegra = ()
    MintType Mary = MultiAsset
    MintType Alonzo = MultiAsset
    MintType Babbage = MultiAsset
    MintType Conway = MultiAsset
    MintType Dijkstra = MultiAsset

-- | Era-indexed minting\/burning wrapper.
newtype Mint era = Mint (MintType era)

deriving instance Show (MintType era) => Show (Mint era)
deriving instance Eq (MintType era) => Eq (Mint era)

{-# INLINEABLE getEraMint #-}

-- | Extract the minted\/burned native tokens from a transaction in any era.
getEraMint :: forall era. IsEra era => Tx era -> Mint era
getEraMint = case theEra @era of
    Byron -> \_ -> Mint ()
    Shelley -> \_ -> Mint ()
    Allegra -> \_ -> Mint ()
    Mary -> mint
    Alonzo -> mint
    Babbage -> mint
    Conway -> mint
    Dijkstra -> mint
  where
    mint = onTx $ Mint . view (bodyTxL . mintTxBodyL)
