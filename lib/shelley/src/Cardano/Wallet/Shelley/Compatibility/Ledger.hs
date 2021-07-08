{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Exposes a wallet-friendly interface to types and functions exported by the
-- ledger specification.
--
module Cardano.Wallet.Shelley.Compatibility.Ledger
    (
      -- * Exported ledger functions
      computeMinimumAdaQuantity

      -- * Conversions from wallet types to ledger specification types
    , toLedgerCoin
    , toLedgerTokenBundle
    , toLedgerTokenPolicyId
    , toLedgerTokenName
    , toLedgerTokenQuantity

      -- * Conversions from ledger specification types to wallet types
    , toWalletCoin
    , toWalletTokenBundle
    , toWalletTokenPolicyId
    , toWalletTokenName
    , toWalletTokenQuantity

      -- * Roundtrip conversion between wallet types and ledger specification
      --   types
    , Convert (..)

      -- * Internal functions
    , computeMinimumAdaQuantityInternal

    ) where

import Prelude

import Cardano.Crypto.Hash
    ( hashFromBytes, hashToBytes )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.Word
    ( Word64 )
import Fmt
    ( pretty )
import GHC.Stack
    ( HasCallStack )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto )

import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as Ledger
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap
import qualified Shelley.Spec.Ledger.API as Ledger

--------------------------------------------------------------------------------
-- Public functions
--------------------------------------------------------------------------------

-- | Uses the ledger specification to compute the minimum required ada quantity
--   for a token map.
--
computeMinimumAdaQuantity
    :: Coin
    -- ^ The absolute minimum ada quantity specified by the protocol.
    -> TokenMap
    -- ^ The token map to evaluate.
    -> Coin
    -- ^ The minimum ada quantity for the given token map.
computeMinimumAdaQuantity protocolMinimum m =
    -- Note:
    --
    -- We assume here that 'computeMinimumAdaQuantityInternal' has the property
    -- of being constant w.r.t. to the ada value. Assuming this property holds,
    -- it is safe to call it with an ada value of 0.
    --
    -- See 'prop_computeMinimumAdaQuantity_agnosticToAdaQuantity'.
    --
    computeMinimumAdaQuantityInternal protocolMinimum (TokenBundle (Coin 0) m)

--------------------------------------------------------------------------------
-- Roundtrip conversion between wallet types and ledger specification types
--------------------------------------------------------------------------------

-- | Connects a wallet type with its equivalent ledger specification type.
--
-- Instances of this class should satisfy the following laws:
--
-- >>> toLedger . toWallet == id
-- >>> toWallet . toLedger == id
--
class Convert wallet ledger | wallet -> ledger where
    -- | Converts a value from a wallet type to the equivalent ledger
    --   specification type.
    toLedger
        :: HasCallStack => wallet -> ledger
    -- | Converts a value from a ledger specification type to the equivalent
    --   wallet type.
    toWallet
        :: HasCallStack => ledger -> wallet

--------------------------------------------------------------------------------
-- Conversions for 'Coin'
--------------------------------------------------------------------------------

instance Convert Coin Ledger.Coin where
    toLedger = toLedgerCoin
    toWallet = toWalletCoin

toLedgerCoin :: Coin -> Ledger.Coin
toLedgerCoin (Coin c) =
      Ledger.Coin $ fromIntegral @Word64 @Integer c

toWalletCoin :: Ledger.Coin -> Coin
toWalletCoin (Ledger.Coin c)
    | isValidCoin =
        Coin $ fromIntegral @Integer @Word64 c
    | otherwise =
        error $ unwords
            [ "Ledger.toWalletCoin:"
            , "Unexpected invalid coin value:"
            , pretty c
            ]
  where
    isValidCoin = (&&)
        (c >= fromIntegral @Word64 @Integer (unCoin minBound))
        (c <= fromIntegral @Word64 @Integer (unCoin maxBound))

--------------------------------------------------------------------------------
-- Conversions for 'TokenBundle'
--------------------------------------------------------------------------------

-- Values of the ledger specification's 'Value' type are constructed in a way
-- that is similar to the wallet's 'TokenBundle' type. The ada quantity is
-- stored as a separate value, and asset quantities are stored in a nested map.

instance Convert TokenBundle (Ledger.Value StandardCrypto) where
    toLedger = toLedgerTokenBundle
    toWallet = toWalletTokenBundle

toLedgerTokenBundle :: TokenBundle -> Ledger.Value StandardCrypto
toLedgerTokenBundle bundle =
    Ledger.Value ledgerAda ledgerTokens
  where
    (Ledger.Coin ledgerAda) = toLedgerCoin $ TokenBundle.getCoin bundle
    ledgerTokens = bundle
        & view #tokens
        & TokenMap.toNestedMap
        & Map.mapKeys toLedgerTokenPolicyId
        & Map.map mapInner
    mapInner inner = inner
        & NonEmptyMap.toMap
        & Map.mapKeys toLedgerTokenName
        & Map.map toLedgerTokenQuantity

toWalletTokenBundle :: Ledger.Value StandardCrypto -> TokenBundle
toWalletTokenBundle (Ledger.Value ledgerAda ledgerTokens) =
    TokenBundle.fromNestedMap (walletAda, walletTokens)
  where
    walletAda = toWalletCoin $ Ledger.Coin ledgerAda
    walletTokens = ledgerTokens
        & Map.mapKeys toWalletTokenPolicyId
        & Map.map mapInner
        & Map.mapMaybe NonEmptyMap.fromMap
    mapInner inner = inner
        & Map.mapKeys toWalletTokenName
        & Map.map toWalletTokenQuantity

--------------------------------------------------------------------------------
-- Conversions for 'TokenName'
--------------------------------------------------------------------------------

instance Convert TokenName Ledger.AssetName where
    toLedger = toLedgerTokenName
    toWallet = toWalletTokenName

toLedgerTokenName :: TokenName -> Ledger.AssetName
toLedgerTokenName (UnsafeTokenName bytes) =
    Ledger.AssetName bytes

toWalletTokenName :: Ledger.AssetName -> TokenName
toWalletTokenName (Ledger.AssetName bytes) =
    UnsafeTokenName bytes

--------------------------------------------------------------------------------
-- Conversions for 'TokenPolicyId'
--------------------------------------------------------------------------------

instance Convert TokenPolicyId (Ledger.PolicyID StandardCrypto) where
    toLedger = toLedgerTokenPolicyId
    toWallet = toWalletTokenPolicyId

toLedgerTokenPolicyId :: TokenPolicyId -> Ledger.PolicyID StandardCrypto
toLedgerTokenPolicyId p@(UnsafeTokenPolicyId (Hash bytes)) =
    case hashFromBytes bytes of
        Just hash ->
            Ledger.PolicyID (Ledger.ScriptHash hash)
        Nothing ->
            error $ unwords
                [ "Ledger.toLedgerTokenPolicyId"
                , "Unable to construct hash for token policy:"
                , pretty p
                ]

toWalletTokenPolicyId :: Ledger.PolicyID StandardCrypto -> TokenPolicyId
toWalletTokenPolicyId (Ledger.PolicyID (Ledger.ScriptHash hash)) =
    UnsafeTokenPolicyId (Hash (hashToBytes hash))

--------------------------------------------------------------------------------
-- Conversions for 'TokenQuantity'
--------------------------------------------------------------------------------

instance Convert TokenQuantity Integer where
    toLedger = toLedgerTokenQuantity
    toWallet = toWalletTokenQuantity

toLedgerTokenQuantity :: TokenQuantity -> Integer
toLedgerTokenQuantity (TokenQuantity q) = fromIntegral q

toWalletTokenQuantity :: Integer -> TokenQuantity
toWalletTokenQuantity q
    | q >= 0 =
        TokenQuantity $ fromIntegral q
    | otherwise =
        error $ unwords
            [ "Ledger.toWalletTokenQuantity:"
            , "Unexpected negative value:"
            , pretty q
            ]

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

-- | Uses the ledger specification to compute the minimum required ada quantity
--   for a token bundle.
--
-- This function is intended to be constant with respect to:
--
--    - the ada quantity;
--    - the quantities of individual assets.
--
-- See the following properties:
--
--    - 'prop_computeMinimumAdaQuantity_agnosticToAdaQuantity';
--    - 'prop_computeMinimumAdaQuantity_agnosticToAssetQuantities'.
--
computeMinimumAdaQuantityInternal
    :: Coin
    -- ^ The absolute minimum ada quantity specified by the protocol.
    -> TokenBundle
    -- ^ The token bundle to evaluate.
    -> Coin
    -- ^ The minimum ada quantity for the given token bundle.
computeMinimumAdaQuantityInternal protocolMinimum bundle =
    toWalletCoin $
        Ledger.scaledMinDeposit
            (toLedgerTokenBundle bundle)
            (toLedgerCoin protocolMinimum)
