{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2018-2021 IOHK
-- License: Apache-2.0
--
-- Definition of 'Shared' Keys.
module Cardano.Wallet.Address.Derivation.SharedKey
  ( -- * Types
    SharedKey (..)
  , sharedKey
  , purposeCIP1854
  , constructAddressFromIx
  , toNetworkTag
  , replaceCosignersWithVerKeys
  )
where

import Cardano.Address qualified as CA
import Cardano.Address.Derivation qualified as CA
import Cardano.Address.Script
  ( Cosigner
  , KeyHash
  , Script (..)
  , ScriptTemplate (..)
  , toScriptHash
  )
import Cardano.Address.Script qualified as CA
import Cardano.Address.Style.Shared
  ( deriveAddressPublicKey
  , deriveDelegationPublicKey
  , hashKey
  , liftXPub
  )
import Cardano.Address.Style.Shelley
  ( Credential (..)
  , delegationAddress
  , paymentAddress
  )
import Cardano.Address.Style.Shelley qualified as CA
import Cardano.Wallet.Address.Derivation
  ( Depth (..)
  , DerivationType (..)
  , Index (..)
  , Role (..)
  )
import Cardano.Wallet.Primitive.NetworkId
  ( HasSNetworkId (sNetworkId)
  , SNetworkId (..)
  )
import Cardano.Wallet.Primitive.Types.Address
  ( Address (..)
  )
import Control.DeepSeq
  ( NFData (..)
  )
import Control.Lens
  ( Iso
  , iso
  )
import Data.Map.Strict qualified as Map
import Data.Maybe
  ( fromJust
  )
import GHC.Generics
  ( Generic
  )
import GHC.Stack
  ( HasCallStack
  )
import Prelude

-- | Purpose for shared wallets is a constant set to 1854' (or 0x8000073E) following the original
-- CIP-1854 Multi-signature Wallets.
--
-- It indicates that the subtree of this node is used according to this
-- specification.
--
-- Hardened derivation is used at this level.
purposeCIP1854 :: Index 'Hardened 'PurposeK
purposeCIP1854 = toEnum 0x8000073E

-- | A cryptographic key for Shared address derivation, with phantom-types to
-- disambiguate derivation paths
--
-- @
-- let rootPrivateKey = SharedKey 'RootK XPrv
-- let accountPubKey = SharedKey 'AccountK XPub
-- let addressPubKey = SharedKey 'CredFromScriptK XPub
-- @
newtype SharedKey (depth :: Depth) key = SharedKey {getKey :: key}
  deriving stock (Generic, Show, Eq)

sharedKey :: Iso (SharedKey depth key) (SharedKey depth1 key') key key'
sharedKey = iso getKey SharedKey

instance NFData key => NFData (SharedKey depth key)

constructAddressFromIx
  :: forall n
   . HasSNetworkId n
  => Role
  -> ScriptTemplate
  -> Maybe ScriptTemplate
  -> Index 'Soft 'CredFromScriptK
  -> Address
constructAddressFromIx role pTemplate dTemplate ix =
  let
    delegationCredential = DelegationFromScriptHash . toScriptHash
    paymentCredential = PaymentFromScriptHash . toScriptHash
    tag = toNetworkTag @n
    createBaseAddress pScript' dScript' =
      CA.unAddress
        $ delegationAddress
          tag
          (paymentCredential pScript')
          (delegationCredential dScript')
    createEnterpriseAddress pScript' =
      CA.unAddress
        $ paymentAddress
          tag
          (paymentCredential pScript')
    role' = case role of
      UtxoExternal -> CA.UTxOExternal
      UtxoInternal -> CA.UTxOInternal
      MutableAccount ->
        error "role is specified only for payment credential"
    pScript =
      replaceCosignersWithVerKeys role' pTemplate ix
    dScript s =
      replaceCosignersWithVerKeys CA.Stake s minBound
  in
    Address $ case dTemplate of
      Just dTemplate' ->
        createBaseAddress pScript (dScript dTemplate')
      Nothing ->
        createEnterpriseAddress pScript

replaceCosignersWithVerKeys
  :: CA.Role
  -> ScriptTemplate
  -> Index 'Soft 'CredFromScriptK
  -> Script KeyHash
replaceCosignersWithVerKeys role' (ScriptTemplate xpubs scriptTemplate) ix =
  replaceCosigner scriptTemplate
  where
    replaceCosigner :: Script Cosigner -> Script KeyHash
    replaceCosigner = \case
      RequireSignatureOf c -> RequireSignatureOf $ toKeyHash c
      RequireAllOf xs -> RequireAllOf (map replaceCosigner xs)
      RequireAnyOf xs -> RequireAnyOf (map replaceCosigner xs)
      RequireSomeOf m xs -> RequireSomeOf m (map replaceCosigner xs)
      ActiveFromSlot s -> ActiveFromSlot s
      ActiveUntilSlot s -> ActiveUntilSlot s

    convertIndex
      :: Index 'Soft 'CredFromScriptK -> CA.Index 'CA.Soft 'CA.PaymentK
    convertIndex = fromJust . CA.indexFromWord32 . fromIntegral . fromEnum

    toKeyHash :: HasCallStack => Cosigner -> KeyHash
    toKeyHash c =
      case Map.lookup c xpubs of
        Nothing -> error "Impossible: cosigner without accXPpub."
        Just accXPub ->
          hashKey walletRole
            $ deriveMultisigPublicKey (liftXPub accXPub) (convertIndex ix)

    walletRole = case role' of
      CA.UTxOExternal -> CA.Payment
      CA.UTxOInternal -> CA.Payment
      CA.Stake -> CA.Delegation
    deriveMultisigPublicKey accXPub = case role' of
      CA.UTxOExternal -> deriveAddressPublicKey accXPub role'
      CA.UTxOInternal -> deriveAddressPublicKey accXPub role'
      CA.Stake -> deriveDelegationPublicKey accXPub

-- | Convert 'NetworkDiscriminant type parameter to
-- 'Cardano.Address.NetworkTag'.
toNetworkTag :: forall n. HasSNetworkId n => CA.NetworkTag
toNetworkTag = case sNetworkId @n of
  SMainnet -> CA.NetworkTag 1
  STestnet _ -> CA.NetworkTag 0 -- fixme: Not all testnets have NetworkTag=0
