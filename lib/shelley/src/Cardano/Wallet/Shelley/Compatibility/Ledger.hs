{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Exposes a wallet-friendly interface to ledger types and functions.
--
module Cardano.Wallet.Shelley.Compatibility.Ledger
    (
      -- * Exported functions
      computeMinimumAdaQuantity

      -- * Internal conversions between wallet types and ledger types
    , HasLedgerType (..)

    ) where

import Prelude

import Cardano.Crypto.Hash
    ( hashFromBytes, hashToBytes )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle )
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
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )

import qualified Cardano.Ledger.Mary.Value as Ledger
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap
import qualified Shelley.Spec.Ledger.API as Ledger

--------------------------------------------------------------------------------
-- Exported functions
--------------------------------------------------------------------------------

-- | Uses the ledger specification to compute the minimum required ada quantity
--   for a token bundle.
--
computeMinimumAdaQuantity
    :: Coin
    -- ^ The absolute minimum ada quantity specified by the protocol.
    -> TokenBundle
    -- ^ The token bundle to evaluate.
    -> Coin
    -- ^ The minimum ada quantity for the given token bundle.
computeMinimumAdaQuantity protocolMinimum bundle =
    fromLedger @Coin $
        Ledger.scaledMinDeposit
            (toLedger @TokenBundle bundle)
            (toLedger @Coin protocolMinimum)

--------------------------------------------------------------------------------
-- Internal conversions between wallet types and ledger types
--------------------------------------------------------------------------------

class HasLedgerType wallet ledger | wallet -> ledger where
    -- | Converts a value from a ledger type to the equivalent wallet type.
    fromLedger
        :: HasCallStack => ledger -> wallet
    -- | Converts a value from a wallet type to the equivalent ledger type.
    toLedger
        :: HasCallStack => wallet -> ledger

instance HasLedgerType Coin Ledger.Coin where
    fromLedger (Ledger.Coin c)
        | isValidCoin =
            Coin $ fromIntegral @Integer @Word64 c
        | otherwise =
            error $ unwords
                [ "HasLedgerType.Coin.fromLedger:"
                , "Unexpected invalid coin value:"
                , pretty c
                ]
      where
        isValidCoin = (&&)
            (c >= fromIntegral @Word64 @Integer (unCoin minBound))
            (c <= fromIntegral @Word64 @Integer (unCoin maxBound))
    toLedger (Coin c) =
        Ledger.Coin $ fromIntegral @Word64 @Integer c

instance HasLedgerType TokenBundle (Ledger.Value StandardCrypto) where

    -- A ledger 'Value' is constructed in a similar way to the wallet's
    -- 'TokenBundle' type. The ada quantity is stored as a completely
    -- separate value, and asset quantities are stored in a nested map.

    fromLedger (Ledger.Value ledgerAda ledgerTokens) =
        TokenBundle.fromNestedMap (ada, tokens)
      where
        ada = Coin $ fromIntegral ledgerAda
        tokens = ledgerTokens
            & Map.mapKeys (fromLedger @TokenPolicyId)
            & Map.map mapInner
            & Map.mapMaybe (NonEmptyMap.fromMap)
        mapInner inner = inner
            & Map.mapKeys (fromLedger @TokenName)
            & Map.map (fromLedger @TokenQuantity)

    toLedger bundle =
        Ledger.Value ledgerAda ledgerTokens
      where
        ledgerAda = fromIntegral $ Coin.unCoin $ TokenBundle.getCoin bundle
        ledgerTokens = bundle
            & view #tokens
            & TokenMap.toNestedMap
            & Map.mapKeys (toLedger @TokenPolicyId)
            & Map.map mapInner
        mapInner inner = inner
            & NonEmptyMap.toMap
            & Map.mapKeys (toLedger @TokenName)
            & Map.map (toLedger @TokenQuantity)

instance HasLedgerType TokenName Ledger.AssetName where
    fromLedger (Ledger.AssetName bytes) =
        UnsafeTokenName bytes
    toLedger (UnsafeTokenName bytes) =
        Ledger.AssetName bytes

instance HasLedgerType TokenPolicyId (Ledger.PolicyID StandardCrypto) where
    fromLedger (Ledger.PolicyID (Ledger.ScriptHash hash)) =
        UnsafeTokenPolicyId (Hash (hashToBytes hash))
    toLedger p@(UnsafeTokenPolicyId (Hash bytes)) =
        case hashFromBytes bytes of
            Just hash ->
                Ledger.PolicyID (Ledger.ScriptHash hash)
            Nothing ->
                error $ unwords
                    [ "HasLedgerType.TokenPolicyId.fromLedger:"
                    , "Unable to construct hash for token policy:"
                    , pretty p
                    ]

instance HasLedgerType TokenQuantity Integer where
    fromLedger q
        | q >= 0 =
            TokenQuantity $ fromIntegral q
        | otherwise =
            error $ unwords
                [ "HasLedgerType.TokenQuantity.fromLedger:"
                , "Unexpected negative value:"
                , pretty q
                ]
    toLedger (TokenQuantity q) = fromIntegral q
