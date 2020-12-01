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
    ( KeyHash (..), Script (..), ScriptHash (..), toScriptHash )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), Index (..), Role (..), SoftDerivation (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..)
    , lookupKeyHash
    , mkVerificationKeyPool
    , verPoolAccountPubKey
    , verPoolGap
    , verPoolIndexedKeys
    , verPoolKnownScripts
    )
import Control.Arrow
    ( first )
import Control.Monad
    ( foldM )
import Control.Monad.Trans.State.Strict
    ( runState, state )
import Data.Coerce
    ( coerce )
import Data.Function
    ( (&) )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Maybe
    ( catMaybes )

import qualified Data.Map.Strict as Map

deriving instance Ord ScriptHash

isShared
    :: (k ~ ShelleyKey, SoftDerivation k)
    => Script
    -> SeqState n k
    -> ([k 'ScriptK XPub], SeqState n k)
isShared script (SeqState !s1 !s2 !pending !rpk !prefix !s3) =
    let
        hashes = retrieveAllVerKeyHashes script
        insertIf predicate k v = if predicate v then Map.insert k v else id
        accXPub = verPoolAccountPubKey s3
        toVerKey = coerce . deriveAddressPublicKey accXPub MultisigScript . coerce

        (ixs, s3') = s3
            & runState (mapM (state . lookupKeyHash) hashes)
            & first catMaybes
            & \(ixs', s3'') ->
                ( ixs'
                , mkVerificationKeyPool accXPub (verPoolGap s3'') (verPoolIndexedKeys s3'')
                  (insertIf (not . null) (toScriptHash script) ixs' (verPoolKnownScripts s3''))
                )
    in
        ( toVerKey <$> ixs
        , SeqState s1 s2 pending rpk prefix s3'
        )

retrieveAllVerKeyHashes :: Script -> [KeyHash]
retrieveAllVerKeyHashes = foldScript (:) []

foldScript :: (KeyHash -> b -> b) -> b -> Script -> b
foldScript fn zero = \case
    RequireSignatureOf k -> fn k zero
    RequireAllOf xs      -> foldMScripts xs
    RequireAnyOf xs      -> foldMScripts xs
    RequireSomeOf _ xs   -> foldMScripts xs
  where
    foldMScripts =
        runIdentity . foldM (\acc -> Identity . foldScript fn acc) zero
