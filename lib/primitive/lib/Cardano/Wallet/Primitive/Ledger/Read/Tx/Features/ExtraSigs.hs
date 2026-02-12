{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.ExtraSigs (extraSigs)
where

import Cardano.Crypto.Hash
    ( Hash (..)
    )
import Cardano.Ledger.Keys
    ( KeyHash (..)
    , KeyRole (..)
    )
import Cardano.Read.Ledger.Tx.ExtraSigs
    ( ExtraSigs (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Data.ByteString.Short
    ( fromShort
    )
import Data.Foldable
    ( toList
    )
import Data.Functor
    ( (<&>)
    )
import Data.Set
    ( Set
    )
import Prelude

import qualified Cardano.Wallet.Primitive.Types.Hash as W

{-# INLINEABLE extraSigs #-}
extraSigs
    :: forall era. IsEra era => ExtraSigs era -> [W.Hash "ExtraSignature"]
extraSigs = case theEra @era of
    Byron -> noExtraSigs
    Shelley -> noExtraSigs
    Allegra -> noExtraSigs
    Mary -> noExtraSigs
    Alonzo -> yesExtraSigs
    Babbage -> yesExtraSigs
    Conway -> yesExtraSigs
  where
    noExtraSigs = const []
    yesExtraSigs (ExtraSigs es) = getExtraSigs es

getExtraSigs
    :: Set (KeyHash 'Witness)
    -> [W.Hash "ExtraSignature"]
getExtraSigs es =
    toList es <&> \(KeyHash (UnsafeHash h)) -> W.Hash $ fromShort h
