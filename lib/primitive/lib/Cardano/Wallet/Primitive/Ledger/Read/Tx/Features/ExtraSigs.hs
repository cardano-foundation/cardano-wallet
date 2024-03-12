{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.ExtraSigs
    ( extraSigs )

 where

import Prelude

import Cardano.Crypto.Hash
    ( Hash (..)
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.Keys
    ( KeyHash (..)
    , KeyRole (..)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Wallet.Read.Tx.ExtraSigs
    ( ExtraSigs (..)
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

import qualified Cardano.Wallet.Primitive.Types.Hash as W

extraSigs :: forall era. IsEra era => ExtraSigs era -> [W.Hash "ExtraSignature"]
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
    :: Set (KeyHash 'Witness StandardCrypto)
    -> [W.Hash "ExtraSignature"]
getExtraSigs es =
    toList es <&> \(KeyHash (UnsafeHash h)) -> W.Hash $ fromShort h
