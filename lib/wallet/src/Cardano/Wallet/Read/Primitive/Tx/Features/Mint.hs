{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Read.Primitive.Tx.Features.Mint
    ( mint
    , maryMint
    , alonzoMint
    , babbageMint
    , conwayMint
    , fromLedgerScriptHash
    )
    where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..), ScriptHash (..) )
import Cardano.Crypto.Hash
    ( hashToBytes )
import Cardano.Ledger.Alonzo
    ( AlonzoEra )
import Cardano.Ledger.Babbage
    ( BabbageEra )
import Cardano.Ledger.Conway
    ( ConwayEra )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Shelley.Tx
    ( WitnessSetHKD (scriptWits) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap, toNestedList, toNestedMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId (..) )
import Cardano.Wallet.Read.Eras
    ( EraFun (..), K (..), (:*:) (..) )
import Cardano.Wallet.Read.Primitive.Tx.Features.Inputs
    ( fromShelleyTxIn )
import Cardano.Wallet.Read.Tx.Mint
    ( Mint (..) )
import Cardano.Wallet.Read.Tx.ReferenceInputs
    ( ReferenceInputs (..) )
import Cardano.Wallet.Read.Tx.Witnesses
    ( Witnesses (..) )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript, toWalletTokenName, toWalletTokenPolicyId,
    toWalletTokenQuantity )
import Cardano.Wallet.Transaction
    ( AnyScript (..), PlutusScriptInfo (..), PlutusVersion (..),
    ReferenceInput (..), ScriptReference (..), TokenMapWithScripts (..),
    emptyTokenMapWithScripts )
import Data.Foldable
    ( toList )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Set
    ( Set )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAlonzo, StandardBabbage, StandardConway, StandardCrypto )

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Ledger.Allegra.Scripts as Scripts
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap


mint :: EraFun
    (Mint :*: Witnesses :*: ReferenceInputs)
    (K (TokenMapWithScripts, TokenMapWithScripts))
mint = EraFun
    { byronFun = noMints
    , shelleyFun = noMints
    , allegraFun = noMints
    , maryFun = \(Mint mint' :*: Witnesses wits :*: ReferenceInputs _refInps)
        -> K $ maryMint mint' wits
    , alonzoFun = \(Mint mint' :*: Witnesses wits :*: ReferenceInputs _refInps)
        -> K $ alonzoMint mint' wits
    , babbageFun = \(Mint mint' :*: Witnesses wits :*: ReferenceInputs refInps)
        -> K $ babbageMint refInps mint' wits
    , conwayFun = \(Mint mint' :*: Witnesses wits :*: ReferenceInputs refInps)
        -> K $ conwayMint refInps mint' wits
    }
    where
        noMints = const $ K (emptyTokenMapWithScripts, emptyTokenMapWithScripts)

maryMint ::
    ( SL.Core.Script era ~ Scripts.Timelock StandardCrypto
    , Crypto era ~ StandardCrypto
    , SL.Core.EraScript era
    )
    => SL.MaryValue StandardCrypto
    -> SL.WitnessSet era
    -> (TokenMapWithScripts, TokenMapWithScripts)
maryMint = yesMints $ fromMaryScriptMap . scriptWits

alonzoMint ::
    ( SL.Core.Script era ~ Alonzo.AlonzoScript (AlonzoEra StandardCrypto)
    , Crypto era ~ StandardCrypto
    )
    => SL.MaryValue StandardCrypto
    -> AL.TxWitness era
    -> (TokenMapWithScripts, TokenMapWithScripts)
alonzoMint = yesMints $ fromAlonzoScriptMap . AL.txscripts'

babbageMint ::
    (   SL.Core.Script era
        ~ Alonzo.AlonzoScript (BabbageEra StandardCrypto)
    , Crypto era ~ StandardCrypto
    )
    => Set (SL.TxIn StandardCrypto)
    -> SL.MaryValue StandardCrypto
    -> AL.TxWitness era
    -> (TokenMapWithScripts, TokenMapWithScripts)
babbageMint refInps val wits =
    let (map1, map2) = yesMints (fromBabbageScriptMap . AL.txscripts') val wits
    in ( useReferenceScriptIfNeeded refInps map1
       , useReferenceScriptIfNeeded refInps map2 )

conwayMint ::
    (   SL.Core.Script era
        ~ Alonzo.AlonzoScript (ConwayEra StandardCrypto)
    , Crypto era ~ StandardCrypto
    )
    => Set (SL.TxIn StandardCrypto)
    -> SL.MaryValue StandardCrypto
    -> AL.TxWitness era
    -> (TokenMapWithScripts, TokenMapWithScripts)
conwayMint refInps val wits =
    let (map1, map2) = yesMints (fromConwayScriptMap . AL.txscripts') val wits
    in ( useReferenceScriptIfNeeded refInps map1
       , useReferenceScriptIfNeeded refInps map2 )

yesMints :: (t -> Map TokenPolicyId AnyScript)
    -> SL.MaryValue StandardCrypto
    -> t
    -> (TokenMapWithScripts, TokenMapWithScripts)
yesMints scriptMapOf mint' wits =
    let
        (assetsToMint, assetsToBurn) = fromLedgerMintValue mint'
        scriptMap = scriptMapOf wits
        mintScriptMap = getScriptMap scriptMap assetsToMint
        burnScriptMap = getScriptMap scriptMap assetsToBurn
    in
    ( TokenMapWithScripts assetsToMint mintScriptMap
    , TokenMapWithScripts assetsToBurn burnScriptMap
    )

useReferenceScriptIfNeeded
    :: Set (SL.TxIn StandardCrypto)
    -> TokenMapWithScripts
    -> TokenMapWithScripts
useReferenceScriptIfNeeded refInps (TokenMapWithScripts tokenMap tokenScripts) =
    let allTokenPolicyIds = Map.keys $ toNestedMap tokenMap
        refInps' = ReferenceInput . fromShelleyTxIn <$> toList refInps
        toScriptHash = ScriptHash . getHash . unTokenPolicyId
        findTokenPolicy s = case Map.lookup s tokenScripts of
            Just script -> script
            Nothing -> AnyScriptReference (toScriptHash s) refInps'
        replaceScript policy = (policy, findTokenPolicy policy)
        tokenScripts' = Map.fromList $ map replaceScript allTokenPolicyIds
    in TokenMapWithScripts tokenMap tokenScripts'

fromLedgerMintValue
    :: SL.MaryValue StandardCrypto
    -> (TokenMap, TokenMap)
fromLedgerMintValue (SL.MaryValue _ ledgerTokens) =
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

fromMaryScriptMap
    :: Map
        (SL.ScriptHash (Crypto (MA.ShelleyMAEra 'MA.Mary StandardCrypto)))
        (SL.Core.Script (MA.ShelleyMAEra 'MA.Mary StandardCrypto))
    -> Map TokenPolicyId AnyScript
fromMaryScriptMap =
        Map.map (flip NativeScript ViaSpending . toWalletScript (const Policy)) .
        Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)

getScriptMap
    :: Map TokenPolicyId AnyScript
    -> TokenMap
    -> Map TokenPolicyId AnyScript
getScriptMap scriptMap =
    Map.fromList
    . mapMaybe (\(policy, _) -> (policy,) <$> Map.lookup policy scriptMap)
    . toNestedList

fromAlonzoScriptMap
    :: Map
        (SL.ScriptHash (Crypto StandardAlonzo))
        (SL.Core.Script StandardAlonzo)
    -> Map TokenPolicyId AnyScript
fromAlonzoScriptMap =
    Map.map toAnyScript .
    Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)
    where
    toAnyScript (Alonzo.TimelockScript script) =
        NativeScript (toWalletScript (const Policy) script) ViaSpending
    toAnyScript s@(Alonzo.PlutusScript ver _) =
        PlutusScript (PlutusScriptInfo (toPlutusVer ver)
                      (hashAlonzoScript s)) ViaSpending
    hashAlonzoScript = fromLedgerScriptHash .
        SL.Core.hashScript @(Cardano.ShelleyLedgerEra Cardano.AlonzoEra)

fromLedgerScriptToAnyScript
    :: SL.Core.Script StandardBabbage
    -> AnyScript
fromLedgerScriptToAnyScript = toAnyScript
    where
    toAnyScript (Alonzo.TimelockScript script) =
        NativeScript (toWalletScript (const Policy) script) ViaSpending
    toAnyScript s@(Alonzo.PlutusScript ver _) =
        PlutusScript (PlutusScriptInfo (toPlutusVer ver)
                      (hashBabbageScript s)) ViaSpending
    hashBabbageScript = fromLedgerScriptHash .
        SL.Core.hashScript @(Cardano.ShelleyLedgerEra Cardano.BabbageEra)

fromLedgerScriptToAnyScriptConway
    :: SL.Core.Script StandardConway
    -> AnyScript
fromLedgerScriptToAnyScriptConway = toAnyScript
    where
    toAnyScript (Alonzo.TimelockScript script) =
        NativeScript (toWalletScript (const Policy) script) ViaSpending
    toAnyScript s@(Alonzo.PlutusScript ver _) =
        PlutusScript (PlutusScriptInfo (toPlutusVer ver)
                      (hashConwayScript s)) ViaSpending
    hashConwayScript = fromLedgerScriptHash .
        SL.Core.hashScript @(Cardano.ShelleyLedgerEra Cardano.ConwayEra)

toPlutusVer :: Alonzo.Language -> PlutusVersion
toPlutusVer Alonzo.PlutusV1 = PlutusVersionV1
toPlutusVer Alonzo.PlutusV2 = PlutusVersionV2

fromBabbageScriptMap
    :: Map
        (SL.ScriptHash (Crypto StandardBabbage))
        (SL.Core.Script StandardBabbage)
    -> Map TokenPolicyId AnyScript
fromBabbageScriptMap =
    Map.map fromLedgerScriptToAnyScript .
    Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)

fromConwayScriptMap
    :: Map
        (SL.ScriptHash (Crypto StandardConway))
        (SL.Core.Script StandardConway)
    -> Map TokenPolicyId AnyScript
fromConwayScriptMap =
    Map.map fromLedgerScriptToAnyScriptConway .
    Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)

fromLedgerScriptHash
    :: (SL.ScriptHash era)
    -> ScriptHash
fromLedgerScriptHash (SL.ScriptHash h) =
    ScriptHash (hashToBytes h)
