{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import Control.Monad
    ( foldM )
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
        (VerificationKeyPool accXPub currentGap verKeyMap scripts) = s3
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
            Map.mapWithKey (\keyH (ix,isUsed) -> (ix, updateAddressState keyH isUsed) )
            verKeyMap
        -- if all verification keys are used (after discovering) we are extending scriptPool
        extendingPool =
            all ((== Used) . (\ (_, (_, isUsed)) -> isUsed))
            (Map.toList markedVerKeyMap)
        insertIf predicate k v = if predicate v then Map.insert k v else id
        script' = insertIf (not . null) (toScriptHash script) scriptXPubs scripts
        s3' = if extendingPool then
                  mkVerificationKeyPool
                  accXPub
                  (mkUnboundedAddressPoolGap (getAddressPoolGap currentGap + getAddressPoolGap defaultAddressPoolGap))
                  markedVerKeyMap
                  script'
              else
                  VerificationKeyPool accXPub currentGap markedVerKeyMap script'
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
