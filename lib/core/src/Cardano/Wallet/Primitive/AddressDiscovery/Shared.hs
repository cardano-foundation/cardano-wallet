{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- An implementation of shared script state using
-- scheme specified in BIP-0044.

module Cardano.Wallet.Primitive.AddressDiscovery.Shared
    (
    -- ** State
      SharedState (..)
    , KeyNumberScope (..)
    , mkSharedState
    ) where

import Prelude

import Cardano.Address.Script
    ( ScriptTemplate )
import Cardano.Crypto.Wallet
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( DerivationPrefix (..), coinTypeAda, purposeBIP44 )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Control.DeepSeq
    ( NFData )
import Data.Maybe
    ( isNothing )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )

{-------------------------------------------------------------------------------
                                 State
-------------------------------------------------------------------------------}

-- | A state to keep track of script templates, account public keys for cosigners,
-- | and a combination of possible script addresses to look for in a ledger
data SharedState (n :: NetworkDiscriminant) k = SeqState
    { accountKey :: k 'AccountK XPub
        -- ^ Reward account public key associated with this wallet
    , derivationPrefix :: DerivationPrefix
        -- ^ Derivation path prefix from a root key up to the account key
    , derivationKeyNumber :: KeyNumberScope
        -- ^ Number of first keys that are used to produce candidate addresses
    , paymentScriptTemplate :: !(Maybe ScriptTemplate)
        -- ^ Script template together with a map of account keys and cosigners
        -- for payment credential
    , stakingScriptTemplate :: !(Maybe ScriptTemplate)
        -- ^ Script template together with a map of account keys and cosigners
        -- for staking credential
    , addressCandidates :: [Address]
    }
    deriving stock (Generic)

deriving instance
    ( Show (k 'AccountK XPub)
    ) => Show (SharedState n k)

deriving instance
    ( Eq (k 'AccountK XPub)
    ) => Eq (SharedState n k)

instance
    ( NFData (k 'AccountK XPub)
    )
    => NFData (SharedState n k)

newtype KeyNumberScope =
    KeyNumberScope { unKeyNumberScope :: Word8 }
    deriving (Eq, Generic, Show)
    deriving anyclass NFData

-- | Construct a SharedState for a wallet from public account key, script templates
-- for both staking and spending, and the number of keys to generate key candidates.
mkSharedState
    :: forall (n :: NetworkDiscriminant) k. k 'AccountK XPub
    -> Index 'Hardened 'AccountK
    -> Maybe ScriptTemplate
    -> Maybe ScriptTemplate
    -> KeyNumberScope
    -> Maybe (SharedState n k)
mkSharedState accXPub accIx spendingTemplate stakingTemplate keyIx =
    let
        prefix =
            DerivationPrefix ( purposeBIP44, coinTypeAda, accIx )
        addressDerived = []
    in
        if all isNothing [spendingTemplate, stakingTemplate] then
            Nothing
        else
            Just $ SeqState accXPub prefix keyIx spendingTemplate stakingTemplate addressDerived
