{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.Integrity
    ( integrity
    , txIntegrity
    )

 where

import Prelude hiding
    ( (.) )

import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.SafeHash
    ( SafeToHash (originalBytes) )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), EraValue, K (..), applyEraFun, extractEraValue )
import Cardano.Wallet.Read.Tx
    ( Tx )
import Cardano.Wallet.Read.Tx.Integrity
    ( Integrity (..), getEraIntegrity )
import Control.Category
    ( (.) )
import Data.Maybe.Strict
    ( StrictMaybe, strictMaybeToMaybe )

import qualified Cardano.Wallet.Primitive.Types.Hash as W

integrity :: EraFun Integrity (K (Maybe (W.Hash "ScriptIntegrity")))
integrity = EraFun
    { byronFun = noIntegrity
    , shelleyFun = noIntegrity
    , allegraFun = noIntegrity
    , maryFun = noIntegrity
    , alonzoFun = yesIntegrity
    , babbageFun = yesIntegrity
    , conwayFun = yesIntegrity
    }
  where
    noIntegrity = const $ K Nothing
    yesIntegrity (Integrity es) = K $ getIntegrity es

getIntegrity
    :: StrictMaybe (ScriptIntegrityHash StandardCrypto)
    -> Maybe (W.Hash "ScriptIntegrity")
getIntegrity = strictMaybeToMaybe . fmap (W.Hash . originalBytes)

-- Era functions extract from Tx to primitive W.Hash.
-- Useful to cache this composition here, to be exported in case of reuse.
txIntegrityEraFun :: EraFun Tx (K (Maybe (W.Hash "ScriptIntegrity")))
txIntegrityEraFun = integrity . getEraIntegrity

-- | Extract from Tx in any era to primitive W.Hash.
txIntegrity :: EraValue Tx -> Maybe (W.Hash "ScriptIntegrity")
txIntegrity = extractEraValue . applyEraFun txIntegrityEraFun
