{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Integrity
    ( integrity
    , txIntegrity
    )

 where

import Prelude hiding
    ( (.)
    )

import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash
    )
import Cardano.Ledger.Crypto
    ( StandardCrypto
    )
import Cardano.Ledger.SafeHash
    ( SafeToHash (originalBytes)
    )
import Cardano.Wallet.Read.Eras
    ( Era (..)
    , EraValue
    , IsEra (..)
    , applyEraFun
    )
import Cardano.Wallet.Read.Tx
    ( Tx
    )
import Cardano.Wallet.Read.Tx.Integrity
    ( Integrity (..)
    , getEraIntegrity
    )
import Control.Category
    ( (.)
    )
import Data.Maybe.Strict
    ( StrictMaybe
    , strictMaybeToMaybe
    )

import qualified Cardano.Wallet.Primitive.Types.Hash as W

integrity :: forall era . IsEra era
    => Integrity era -> Maybe (W.Hash "ScriptIntegrity")
integrity = case theEra @era of
    Byron -> noIntegrity
    Shelley -> noIntegrity
    Allegra -> noIntegrity
    Mary -> noIntegrity
    Alonzo -> yesIntegrity
    Babbage -> yesIntegrity
    Conway -> yesIntegrity
  where
    noIntegrity = const Nothing
    yesIntegrity (Integrity es) = getIntegrity es

getIntegrity
    :: StrictMaybe (ScriptIntegrityHash StandardCrypto)
    -> Maybe (W.Hash "ScriptIntegrity")
getIntegrity = strictMaybeToMaybe . fmap (W.Hash . originalBytes)

-- Era functions extract from Tx to primitive W.Hash.
-- Useful to cache this composition here, to be exported in case of reuse.
txIntegrityEraFun :: IsEra era => Tx era -> Maybe (W.Hash "ScriptIntegrity")
txIntegrityEraFun = integrity . getEraIntegrity

-- | Extract from Tx in any era to primitive W.Hash.
txIntegrity :: EraValue Tx -> Maybe (W.Hash "ScriptIntegrity")
txIntegrity = applyEraFun txIntegrityEraFun
