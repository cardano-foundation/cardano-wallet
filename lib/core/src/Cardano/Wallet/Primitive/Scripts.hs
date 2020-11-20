{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , Role (..)
    , SoftDerivation (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState (..)
    , VerificationKeyPool (..)
    , defaultAddressPoolGap
    , getAddressPoolGap
    , mkUnboundedAddressPoolGap
    , mkVerificationKeyPool
    )
import Cardano.Wallet.Primitive.Types.Address
    ( AddressState (..) )

import qualified Data.List as L
import qualified Data.Map.Strict as Map

instance Ord ScriptHash where
    compare (ScriptHash sh1) (ScriptHash sh2) = compare sh1 sh2

isShared
    :: (k ~ ShelleyKey, SoftDerivation k)
    => Script
    -> SeqState n k
    -> ([k 'ScriptK XPub], SeqState n k)
isShared script s@(SeqState !s1 !s2 !ixs !rpk !prefix !scripts !s3) =
    let verKeysInScript = retrieveAllVerKeyHashes script
        (VerificationKeyPool accXPub currentGap verKeyMap) = s3
        projectKey (ShelleyKey k) = ShelleyKey k
        toVerKey = deriveAddressPublicKey accXPub MultisigScript
        projectIndex = toEnum . fromInteger . toInteger . getIndex @'Soft
        ourVerKeysInScript =
            map (\(_,(ix,_)) -> toVerKey (projectIndex ix) ) $
            filter (\(keyH,_) -> keyH `elem` verKeysInScript)
            (Map.toList verKeyMap)
        scriptXPubs = L.nub $ map projectKey ourVerKeysInScript
        updateAddressState k current =
            if k `elem` verKeysInScript then
                Used
            else
                current
        markedVerKeyMap =
            Map.fromList $
            map (\(keyH,(ix,isUsed)) -> (keyH, (ix, updateAddressState keyH isUsed)) )
            (Map.toList verKeyMap)
        -- if all verification keys are used (after discovering) we are extending multisigPool
        extendingPool =
            all (==Used) $
            map (\(_,(_,isUsed)) -> isUsed) $
            Map.toList markedVerKeyMap
        s3' = if extendingPool then
                  mkVerificationKeyPool
                  accXPub
                  (mkUnboundedAddressPoolGap (getAddressPoolGap currentGap + getAddressPoolGap defaultAddressPoolGap))
                  markedVerKeyMap
              else
                  VerificationKeyPool accXPub currentGap markedVerKeyMap
    in if null ourVerKeysInScript then
        ([], s)
       else
        ( scriptXPubs
        , SeqState s1 s2 ixs rpk prefix (Map.insert (toScriptHash script) scriptXPubs scripts) s3')

retrieveAllVerKeyHashes :: Script -> [KeyHash]
retrieveAllVerKeyHashes = extractVerKey []
  where
      extractVerKey acc (RequireSignatureOf verKey) = verKey : acc
      extractVerKey acc (RequireAllOf xs) = foldr (flip extractVerKey) acc xs
      extractVerKey acc (RequireAnyOf xs) = foldr (flip extractVerKey) acc xs
      extractVerKey acc (RequireSomeOf _ xs) = foldr (flip extractVerKey) acc xs
