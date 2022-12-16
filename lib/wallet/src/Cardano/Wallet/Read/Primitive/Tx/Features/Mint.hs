{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
    )
    where

import Prelude

import Cardano.Address.Script
    ( KeyRole (..) )
import Cardano.Ledger.Alonzo
    ( AlonzoEra )
import Cardano.Ledger.Babbage
    ( BabbageEra )
import Cardano.Ledger.Era
    ( Era (..) )
import Cardano.Ledger.Shelley.Tx
    ( WitnessSetHKD (scriptWits) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap, toNestedList )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenPolicyId )
import Cardano.Wallet.Read.Eras
    ( (:*:) (..), EraFun (..), K (..) )
import Cardano.Wallet.Read.Tx.Mint
    ( Mint (..) )
import Cardano.Wallet.Read.Tx.Witnesses
    ( Witnesses (..) )
import Cardano.Wallet.Shelley.Compatibility.Ledger
    ( toWalletScript
    , toWalletTokenName
    , toWalletTokenPolicyId
    , toWalletTokenQuantity
    )
import Cardano.Wallet.Transaction
    ( AnyScript (..)
    , PlutusScriptInfo (..)
    , PlutusVersion (..)
    , TokenMapWithScripts (..)
    , TokenMapWithScripts (..)
    , emptyTokenMapWithScripts
    )
import Data.Function
    ( (&) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( isJust )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAlonzo, StandardBabbage, StandardCrypto )

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as AL
import qualified Cardano.Ledger.Core as SL.Core
import qualified Cardano.Ledger.Mary.Value as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.ShelleyMA as MA
import qualified Cardano.Ledger.ShelleyMA.Timelocks as ShelleyMA
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.Map.Strict as Map
import qualified Data.Map.Strict.NonEmptyMap as NonEmptyMap

mint :: EraFun
    (Mint :*: Witnesses)
    (K (TokenMapWithScripts, TokenMapWithScripts))
mint = EraFun
    { byronFun = noMints
    , shelleyFun = noMints
    , allegraFun = noMints
    , maryFun = \(Mint mint' :*: Witnesses wits) -> K $ maryMint mint' wits
    , alonzoFun = \(Mint mint' :*: Witnesses wits) -> K $ alonzoMint mint' wits
    , babbageFun = \(Mint mint' :*: Witnesses wits) -> K $ babbageMint mint' wits
    }
    where
        noMints = const $ K (emptyTokenMapWithScripts, emptyTokenMapWithScripts)

maryMint ::
    ( Era era
    , SL.Core.Script era ~ ShelleyMA.Timelock StandardCrypto
    , Crypto era ~ StandardCrypto
    )
    => SL.Value StandardCrypto
    -> SL.WitnessSet era
    -> (TokenMapWithScripts, TokenMapWithScripts)
maryMint = yesMints $ fromMaryScriptMap . scriptWits

alonzoMint ::
    (   SL.Core.Script era ~ Alonzo.Script (AlonzoEra StandardCrypto)
    ,   Crypto era ~ StandardCrypto
    )
    => SL.Value StandardCrypto
    -> AL.TxWitness era
    -> (TokenMapWithScripts, TokenMapWithScripts)
alonzoMint  =  yesMints $ fromAlonzoScriptMap . AL.txscripts'

babbageMint ::
    (   SL.Core.Script era
        ~ Alonzo.Script (BabbageEra StandardCrypto)
    , Crypto era ~ StandardCrypto
    )
    => SL.Value StandardCrypto
    -> AL.TxWitness era
    -> (TokenMapWithScripts, TokenMapWithScripts)
babbageMint = yesMints $ fromBabbageScriptMap . AL.txscripts'

yesMints :: (t -> Map TokenPolicyId AnyScript)
    -> SL.Value StandardCrypto
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

fromMaryScriptMap
    :: Map
        (SL.ScriptHash (Crypto (MA.ShelleyMAEra 'MA.Mary StandardCrypto)))
        (SL.Core.Script (MA.ShelleyMAEra 'MA.Mary StandardCrypto))
    -> Map TokenPolicyId AnyScript
fromMaryScriptMap =
        Map.map (NativeScript . toWalletScript (const Policy)) .
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
        NativeScript $ toWalletScript (const Policy) script
    toAnyScript (Alonzo.PlutusScript ver _) =
        PlutusScript (PlutusScriptInfo (toPlutusVer ver))

    toPlutusVer Alonzo.PlutusV1 = PlutusVersionV1
    toPlutusVer Alonzo.PlutusV2 = PlutusVersionV2

fromBabbageScriptMap
    :: Map
        (SL.ScriptHash (Crypto StandardBabbage))
        (SL.Core.Script StandardBabbage)
    -> Map TokenPolicyId AnyScript
fromBabbageScriptMap =
    Map.map toAnyScript .
    Map.mapKeys (toWalletTokenPolicyId . SL.PolicyID)
    where
    toAnyScript (Alonzo.TimelockScript script) =
        NativeScript $ toWalletScript (const Policy) script
    toAnyScript (Alonzo.PlutusScript ver _) =
        PlutusScript (PlutusScriptInfo (toPlutusVer ver))

    toPlutusVer Alonzo.PlutusV1 = PlutusVersionV1
    toPlutusVer Alonzo.PlutusV2 = PlutusVersionV2
