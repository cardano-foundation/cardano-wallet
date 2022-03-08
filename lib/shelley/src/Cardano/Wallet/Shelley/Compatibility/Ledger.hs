{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
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
    , toAlonzoTxOut

      -- * Conversions from ledger specification types to wallet types
    , toWalletCoin
    , toWalletTokenBundle
    , toWalletTokenPolicyId
    , toWalletTokenName
    , toWalletTokenQuantity
    , toWalletScript

      -- * Roundtrip conversion between wallet types and ledger specification
      --   types
    , Convert (..)

      -- * Internal functions
    , computeMinimumAdaQuantityInternal

    ) where

import Prelude

import Cardano.Address.Script
    ( KeyHash (..), KeyRole (..), Script (..) )
import Cardano.Crypto.Hash
    ( hashFromBytes, hashToBytes )
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash )
import Cardano.Wallet.Primitive.Types
    ( MinimumUTxOValue (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
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
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..) )
import Data.ByteString.Short
    ( toShort )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.Generics.Internal.VL.Lens
    ( view )
import Data.Generics.Labels
    ()
import Data.IntCast
    ( intCast )
import Fmt
    ( pretty )
import GHC.Stack
    ( HasCallStack )
import Numeric.Natural
    ( Natural )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto )

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as Ledger
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Crypto as Ledger
import qualified Cardano.Ledger.Keys as Ledger
import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.Shelley.API as Ledger
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as Ledger
import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap
import qualified Ouroboros.Network.Block as O

--------------------------------------------------------------------------------
-- Public functions
--------------------------------------------------------------------------------

-- | Uses the ledger specification to compute the minimum required ada quantity
--   for a token map.
--
computeMinimumAdaQuantity
    :: MinimumUTxOValue
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
toLedgerCoin (Coin c) = Ledger.Coin $ intCast @Natural @Integer c

toWalletCoin :: Ledger.Coin -> Coin
toWalletCoin (Ledger.Coin c) = Coin.unsafeFromIntegral c

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
-- Conversions for 'Address'
--------------------------------------------------------------------------------

instance Convert Address (Ledger.Addr StandardCrypto) where
    toLedger (Address bytes ) = case Ledger.deserialiseAddr bytes of
        Just addr -> addr
        Nothing -> error $ unwords
            [ "toLedger @Address: Invalid address:"
            , pretty (Address bytes)
            ]
    toWallet = Address . Ledger.serialiseAddr

toAlonzoTxOut
    :: TxOut
    -> Maybe (Hash "Datum")
    -> Alonzo.TxOut (Alonzo.AlonzoEra StandardCrypto)
toAlonzoTxOut (TxOut addr bundle) = \case
    Nothing ->
        Alonzo.TxOut
            (toLedger addr)
            (toLedger bundle)
            Ledger.SNothing
    Just (Hash bytes) ->
        Alonzo.TxOut
            (toLedger addr)
            (toLedger bundle)
            (Ledger.SJust $ unsafeMakeSafeHash $ Crypto.UnsafeHash $ toShort bytes)

toWalletScript
    :: Ledger.Crypto crypto
    => KeyRole
    -> MA.Timelock crypto
    -> Script KeyHash
toWalletScript keyrole = fromLedgerScript
  where
    fromLedgerScript (MA.RequireSignature (Ledger.KeyHash h)) =
        RequireSignatureOf (KeyHash keyrole (hashToBytes h))
    fromLedgerScript (MA.RequireAllOf contents) =
        RequireAllOf $ map fromLedgerScript $ toList contents
    fromLedgerScript (MA.RequireAnyOf contents) =
        RequireAnyOf $ map fromLedgerScript $ toList contents
    fromLedgerScript (MA.RequireMOf num contents) =
        RequireSomeOf (fromIntegral num) $ map fromLedgerScript $ toList contents
    fromLedgerScript (MA.RequireTimeExpire (O.SlotNo slot)) =
        ActiveUntilSlot $ fromIntegral slot
    fromLedgerScript (MA.RequireTimeStart (O.SlotNo slot)) =
        ActiveFromSlot $ fromIntegral slot

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
-- TODO: [ADP-954] Datum hashes are currently not taken into account.
computeMinimumAdaQuantityInternal
    :: MinimumUTxOValue
    -- ^ The absolute minimum ada quantity specified by the protocol.
    -> TokenBundle
    -- ^ The token bundle to evaluate.
    -> Coin
    -- ^ The minimum ada quantity for the given token bundle.
computeMinimumAdaQuantityInternal (MinimumUTxOValue protocolMinimum) bundle =
    toWalletCoin $
        Ledger.scaledMinDeposit
            (toLedgerTokenBundle bundle)
            (toLedgerCoin protocolMinimum)
computeMinimumAdaQuantityInternal (MinimumUTxOValueCostPerWord (Coin perWord)) bundle =
    let
        outputSize = Alonzo.utxoEntrySize $
            toAlonzoTxOut (TxOut dummyAddr bundle) Nothing
    in
        Coin $ fromIntegral outputSize * perWord
  where
    -- We just need an address the ledger can deserialize. It doesn't actually
    -- use the length of it.
    --
    -- This should not change (if Alonzo is already in-use, it would have to be
    -- changed in a new era).
    --
    -- Regardless, the dummy address is a payment / enterprise address -- can't
    -- get any shorter than that. The integration tests use longer addresses.
    -- They should break if this were to be wrong.
    --
    -- Because the ledger function is pure and not taking a network, passing in
    -- a mainnet network should be fine regardless of network.
    dummyAddr = Address $ BS.pack $ 97 : replicate 28 0
