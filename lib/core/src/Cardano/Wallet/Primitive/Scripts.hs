{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    , mkVerificationKeyPool
    , verPoolAccountPubKey
    , verPoolGap
    , verPoolIndexedKeys
    , verPoolKnownScripts
    )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )
import Control.Monad
    ( foldM )
import Data.Coerce
    ( coerce )
import Data.Functor.Identity
    ( Identity (..) )

import qualified Data.List as L
import qualified Data.Map.Strict as Map

instance Ord ScriptHash where
    compare (ScriptHash sh1) (ScriptHash sh2) = compare sh1 sh2

isShared
    :: (k ~ ShelleyKey, SoftDerivation k)
    => Script
    -> SeqState n k
    -> ([k 'ScriptK XPub], SeqState n k)
isShared script (SeqState !s1 !s2 !ixs !rpk !prefix !s3) =
    let verKeysInScript = retrieveAllVerKeyHashes script
        toVerKey = deriveAddressPublicKey (verPoolAccountPubKey s3) MultisigScript
        ourVerKeysInScript =
            map (\(_,(ix,_)) -> toVerKey (coerce ix) ) $
            filter (\(keyH,_) -> keyH `elem` verKeysInScript) $
            Map.toList (verPoolIndexedKeys s3)
        scriptXPubs = L.nub $ map coerce ourVerKeysInScript
        updateAddressState k current =
            if k `elem` verKeysInScript then
                Used
            else
                current
        indexedKeys =
            Map.mapWithKey (\keyH (ix,isUsed) -> (ix, updateAddressState keyH isUsed) )
            (verPoolIndexedKeys s3)
        insertIf predicate k v = if predicate v then Map.insert k v else id
        knownScripts =
            insertIf (not . null) (toScriptHash script) scriptXPubs
            (verPoolKnownScripts s3)

        -- if there are no gap number of consecutive NotUsed verification keys
        -- then we extend the verification key pool
        s3' = mkVerificationKeyPool (verPoolAccountPubKey s3) (verPoolGap s3)
              indexedKeys knownScripts
    in ( scriptXPubs
       , SeqState s1 s2 ixs rpk prefix s3'
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
