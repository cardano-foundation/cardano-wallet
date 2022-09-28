{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.Integrity
    ( integrity )

 where

import Prelude

import Cardano.Ledger.Alonzo.Tx
    ( ScriptIntegrityHash )
import Cardano.Ledger.Crypto
    ( StandardCrypto )
import Cardano.Ledger.SafeHash
    ( SafeToHash (originalBytes) )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.Integrity
    ( Integrity (..) )
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
    }
    where
        noIntegrity = const $ K Nothing
        yesIntegrity (Integrity es) = K $ getIntegrity es

getIntegrity
    :: StrictMaybe (ScriptIntegrityHash StandardCrypto)
    -> Maybe (W.Hash "ScriptIntegrity")
getIntegrity = strictMaybeToMaybe . fmap (W.Hash . originalBytes)


