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

module Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( -- * Types
      SharedKey(..)

    , purposeCIP1854
    , constructAddressFromIx
    , toNetworkTag
    , replaceCosignersWithVerKeys
    ) where

import Prelude

import Cardano.Address.Script
    ( Cosigner, KeyHash, Script (..), ScriptTemplate (..), toScriptHash )
import Cardano.Address.Style.Shared
    ( deriveAddressPublicKey, deriveDelegationPublicKey, hashKey, liftXPub )
import Cardano.Address.Style.Shelley
    ( Credential (..)
    , delegationAddress
    , mkNetworkDiscriminant
    , paymentAddress
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), DerivationType (..), Index (..), NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Types
    ( invariant )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Control.DeepSeq
    ( NFData (..) )
import Data.Either.Combinators
    ( fromRight', rightToMaybe )
import Data.Maybe
    ( fromJust, fromMaybe, isJust )
import Data.Type.Equality
    ( (:~:) (..), testEquality )
import GHC.Generics
    ( Generic )
import Type.Reflection
    ( Typeable, typeRep )

import qualified Cardano.Address as CA
import qualified Cardano.Address.Derivation as CA
import qualified Cardano.Address.Script as CA
import qualified Cardano.Address.Style.Shelley as CA
import qualified Data.Map.Strict as Map


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
-- let addressPubKey = SharedKey 'AddressK XPub
-- @
newtype SharedKey (depth :: Depth) key =
    SharedKey { getKey :: key }
    deriving stock (Generic, Show, Eq)

instance (NFData key) => NFData (SharedKey depth key)

constructAddressFromIx
    :: forall (n :: NetworkDiscriminant).  Typeable n
    => ScriptTemplate
    -> Maybe ScriptTemplate
    -> Index 'Soft 'ScriptK
    -> Address
constructAddressFromIx pTemplate dTemplate ix =
    let delegationCredential = DelegationFromScript . toScriptHash
        paymentCredential = PaymentFromScript . toScriptHash
        tag = toNetworkTag @n
        createBaseAddress pScript' dScript' =
            CA.unAddress $
            delegationAddress tag
            (paymentCredential pScript') (delegationCredential dScript')
        createEnterpriseAddress pScript' =
            CA.unAddress $
            paymentAddress tag
            (paymentCredential pScript')
        pScript =
            replaceCosignersWithVerKeys CA.UTxOExternal pTemplate ix
        dScript s =
            replaceCosignersWithVerKeys CA.Stake s ix
    in Address $ case dTemplate of
        Just dTemplate' ->
            createBaseAddress pScript (dScript dTemplate')
        Nothing ->
            createEnterpriseAddress pScript

replaceCosignersWithVerKeys
    :: CA.Role
    -> ScriptTemplate
    -> Index 'Soft 'ScriptK
    -> Script KeyHash
replaceCosignersWithVerKeys role' (ScriptTemplate xpubs scriptTemplate) ix =
    replaceCosigner scriptTemplate
  where
    replaceCosigner :: Script Cosigner -> Script KeyHash
    replaceCosigner = \case
        RequireSignatureOf c -> RequireSignatureOf $ toKeyHash c
        RequireAllOf xs      -> RequireAllOf (map replaceCosigner xs)
        RequireAnyOf xs      -> RequireAnyOf (map replaceCosigner xs)
        RequireSomeOf m xs   -> RequireSomeOf m (map replaceCosigner xs)
        ActiveFromSlot s     -> ActiveFromSlot s
        ActiveUntilSlot s    -> ActiveUntilSlot s
    convertIndex :: Index 'Soft 'ScriptK -> CA.Index 'CA.Soft 'CA.PaymentK
    convertIndex = fromJust . CA.indexFromWord32 . fromIntegral . fromEnum
    toKeyHash :: Cosigner -> KeyHash
    toKeyHash c =
        let (Just accXPub) =
                invariant "we should have accXPubs of all cosigners at this point"
                (liftXPub <$> Map.lookup c xpubs)
                isJust
            verKey = deriveMultisigPublicKey accXPub (convertIndex ix)
        in hashKey walletRole verKey
    walletRole = case role' of
        CA.UTxOExternal -> CA.Payment
        CA.Stake -> CA.Delegation
        _ ->  error "replaceCosignersWithVerKeys is supported only for role=0 and role=2"
    deriveMultisigPublicKey = case role' of
        CA.UTxOExternal -> deriveAddressPublicKey
        CA.Stake -> deriveDelegationPublicKey
        _ ->  error "replaceCosignersWithVerKeys is supported only for role=0 and role=2"

toNetworkTag
    :: forall (n :: NetworkDiscriminant). Typeable n => CA.NetworkTag
toNetworkTag =
    fromMaybe (fromRight' $ mkNetworkDiscriminant 0) tryMainnet
  where
    tryMainnet =
        case testEquality (typeRep @n) (typeRep @'Mainnet) of
            Just Refl  -> rightToMaybe $ mkNetworkDiscriminant 1
            Nothing -> Nothing
