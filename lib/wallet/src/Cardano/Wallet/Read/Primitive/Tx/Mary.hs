{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Mary
    ( fromMaryTx
    , getScriptMap
    , fromLedgerMintValue
    , fromCardanoValue
    )
    where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..) )
import Cardano.Api
    ( MaryEra )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap, toNestedList )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Cardano.Wallet.Read.Eras
    ( inject, mary )
import Cardano.Wallet.Read.Primitive.Tx.Allegra
    ( fromLedgerTxValidity )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyAddress
    , fromShelleyCoin
    , fromShelleyMD
    , fromShelleyTxIn
    , fromShelleyWdrl
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..) )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript
    , toWalletTokenName
    , toWalletTokenPolicyId
    , toWalletTokenQuantity
    )
import Cardano.Wallet.Transaction
    ( AnyScript (..), TokenMapWithScripts (..), ValidityIntervalExplicit (..) )
import Cardano.Wallet.Util
    ( internalError )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust )
import GHC.Stack
    ( HasCallStack )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.BaseTypes as SL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.API as SLAPI
import qualified Cardano.Ledger.Shelley.Tx as Shelley
import qualified Cardano.Ledger.ShelleyMA as MA
import qualified Cardano.Ledger.ShelleyMA.AuxiliaryData as MA
import qualified Cardano.Ledger.ShelleyMA.TxBody as MA
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.Hash as W
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Cardano.Wallet.Primitive.Types.TokenPolicy as W
import qualified Cardano.Wallet.Primitive.Types.TokenQuantity as W
import qualified Cardano.Wallet.Primitive.Types.Tx as W
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap

fromMaryTx
    :: SLAPI.Tx (Cardano.ShelleyLedgerEra MaryEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       )
fromMaryTx tx =
    ( W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject mary $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList ins)
        , resolvedCollateralInputs =
            []
        , outputs =
            map fromMaryTxOut (toList outs)
        , collateralOutput =
            -- Collateral outputs are not supported in Mary.
            Nothing
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMD . toSLMetadata <$> SL.strictMaybeToMaybe mad
        , scriptValidity =
            Nothing
        }
    , anyEraCerts certs
    , TokenMapWithScripts assetsToMint mintScriptMap
    , TokenMapWithScripts assetsToBurn burnScriptMap
    , Just (fromLedgerTxValidity ttl)
    )
  where
    SL.Tx bod wits mad = tx
    MA.TxBody ins outs certs wdrls fee ttl _upd _adh mint = bod
    (assetsToMint, assetsToBurn) = fromLedgerMintValue mint
    scriptMap = fromMaryScriptMap $ Shelley.scriptWits wits

    mintScriptMap = getScriptMap scriptMap assetsToMint
    burnScriptMap = getScriptMap scriptMap assetsToBurn

    -- fixme: [ADP-525] It is fine for now since we do not look at script
    -- pre-images. But this is precisely what we want as part of the
    -- multisig/script balance reporting.
    toSLMetadata (MA.AuxiliaryData blob _scripts) = SL.Metadata blob

    fromMaryTxOut
        :: SLAPI.TxOut (Cardano.ShelleyLedgerEra MaryEra)
        -> W.TxOut
    fromMaryTxOut (SL.TxOut addr value) =
        W.TxOut (fromShelleyAddress addr) $
        fromCardanoValue $ Cardano.fromMaryValue value

    fromMaryScriptMap
        :: Map
            (SL.ScriptHash (Crypto (MA.ShelleyMAEra 'MA.Mary StandardCrypto)))
            (SL.Core.Script (MA.ShelleyMAEra 'MA.Mary StandardCrypto))
        -> Map TokenPolicyId AnyScript
    fromMaryScriptMap =
        Map.map (NativeScript . toWalletScript Policy) .
        Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)

getScriptMap
    :: Map TokenPolicyId AnyScript
    -> TokenMap
    -> Map TokenPolicyId AnyScript
getScriptMap scriptMap =
    Map.fromList .
    map (\(policyid, Just script) -> (policyid, script)) .
    filter (isJust . snd) .
    map (\(policyid, _) -> (policyid, Map.lookup policyid scriptMap) ) .
    toNestedList

-- Lovelace to coin. Quantities from ledger should always fit in Word64.
fromCardanoLovelace :: HasCallStack => Cardano.Lovelace -> W.Coin
fromCardanoLovelace =
    Coin.unsafeFromIntegral . unQuantity . Cardano.lovelaceToQuantity
  where
    unQuantity (Cardano.Quantity q) = q

fromCardanoValue :: HasCallStack => Cardano.Value -> TokenBundle.TokenBundle
fromCardanoValue = uncurry TokenBundle.fromFlatList . extract
  where
    extract value =
        ( fromCardanoLovelace $ Cardano.selectLovelace value
        , mkBundle $ Cardano.valueToList value
        )

    -- Do Integer to Natural conversion. Quantities from ledger TxOuts can
    -- never be negative (but unminted values could be negative).
    mkQuantity :: Integer -> W.TokenQuantity
    mkQuantity = W.TokenQuantity . checkBounds
      where
        checkBounds n
          | n >= 0 = fromIntegral n
          | otherwise = internalError "negative token quantity"

    mkBundle assets =
        [ (TokenBundle.AssetId (mkPolicyId p) (mkTokenName n) , mkQuantity q)
        | (Cardano.AssetId p n, Cardano.Quantity q) <- assets
        ]

    mkPolicyId = W.UnsafeTokenPolicyId . W.Hash . Cardano.serialiseToRawBytes
    mkTokenName = W.UnsafeTokenName . Cardano.serialiseToRawBytes

fromLedgerMintValue
    :: SL.Value StandardCrypto
    -> (TokenMap, TokenMap)
fromLedgerMintValue (SL.Value _ ledgerTokens) =
    (assetsToMint, assetsToBurn)
  where
    assetsToMint = ledgerTokens
        & Map.map (Map.filter (> 0))
        & Map.mapKeys toWalletTokenPolicyId
        & Map.map mapInner
        & Map.mapMaybe NonEmptyMap.fromMap
        & TokenMap.fromNestedMap

    assetsToBurn = ledgerTokens
        & Map.map (Map.mapMaybe (\n -> if n > 0 then Nothing else Just (-n)))
        & Map.mapKeys toWalletTokenPolicyId
        & Map.map mapInner
        & Map.mapMaybe NonEmptyMap.fromMap
        & TokenMap.fromNestedMap

    mapInner inner = inner
        & Map.mapKeys toWalletTokenName
        & Map.map toWalletTokenQuantity
