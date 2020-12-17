{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- An implementation of wallet's verification key discovery
-- in a script.

module Cardano.Wallet.Primitive.Scripts
    ( isShared
    , retrieveAllVerKeyHashes
    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash (..), Script (..), foldScript, toScriptHash )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), SoftDerivation, deriveVerificationKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..)
    , extendVerificationKeyPool
    , lookupKeyHash
    , updateKnownScripts
    , verPoolAccountPubKey
    )
import Control.Arrow
    ( first )
import Control.Monad.Trans.State.Strict
    ( runState, state )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( catMaybes )

import qualified Data.Map.Strict as Map

isShared
    :: (k ~ ShelleyKey, SoftDerivation k)
    => Script
    -> SeqState n k
    -> ([k 'ScriptK XPub], SeqState n k)
isShared script (SeqState !s1 !s2 !pending !rpk !prefix !s3) =
    let
        hashes = retrieveAllVerKeyHashes script
        accXPub = verPoolAccountPubKey s3

        (ixs, s3') = s3
            & runState (mapM (state . lookupKeyHash) hashes)
            & first catMaybes
            & \(ixs', s3'') ->
                ( ixs'
                , updateKnownScripts
                  (insertIf (not . null) (toScriptHash script) ixs' )
                  s3''
                )
    in
        if null ixs then
            ( []
            , SeqState s1 s2 pending rpk prefix s3
            )
        else
            ( deriveVerificationKey accXPub <$> ixs
            , SeqState s1 s2 pending rpk prefix
              (extendVerificationKeyPool (maximum ixs) s3')
            )


insertIf :: Ord k => (v -> Bool) -> k -> v -> Map k v -> Map k v
insertIf predicate k v = if predicate v then Map.insert k v else id

retrieveAllVerKeyHashes :: Script -> [KeyHash]
retrieveAllVerKeyHashes = foldScript (:) []
