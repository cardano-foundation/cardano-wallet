{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLabels #-}

module Cardano.Wallet.MintBurn
    ( MintBurnData

    -- * Construction
    , enrich
    , fromApiMintBurnData

    -- * Getters
    , getTxOuts
    , getMintBurnScript
    , getSigningKey
    , tmpGetAddrMap
    , getPolicyId
    , getAssetName
    , getMonetaryPolicyIndex

    ) where

import Data.Function
    ( (&) )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy )
import Numeric.Natural
    ( Natural )
import Prelude
import Data.Generics.Internal.VL.Lens
    ( view )

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Address.Script
    ( KeyHash, Script (RequireSignatureOf) )
import Cardano.Wallet.Api.Types
    ( ApiMintBurnData, ApiMintBurnOperation, getApiT )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationIndex (..)
    , NetworkDiscriminant
    , Passphrase
    , Role (UtxoExternal)
    , WalletKey
    , hashVerificationKey
    , publicKey
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (AssetId), TokenMap )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), tokenPolicyIdFromScript, TokenPolicyId(..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (TokenQuantity) )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Quantity
    ( Quantity, getQuantity )


-- import qualified Data.Bifunctor as Bifunctor

import qualified Cardano.Wallet.Api.Types as Api
import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TokenBundle
import qualified Cardano.Wallet.Primitive.Types.TokenMap as TokenMap
import qualified Data.List.NonEmpty as NE

data MintBurnOperation
    = Mint        (NonEmpty (Address, TokenQuantity))
    | Burn                                            TokenQuantity
    | MintAndBurn (NonEmpty (Address, TokenQuantity)) TokenQuantity
    deriving (Eq, Show)

data MintBurnData dat = MintBurnData
    { mintBurnOperation :: MintBurnOperation
    , mintBurnData :: dat
    }
    deriving (Eq, Show)

data RequestData = RequestData
    { reqMonetaryPolicyIndex :: DerivationIndex
    , reqAssetName           :: TokenName
    }
    deriving (Eq, Show)

data EnrichedData key = EnrichedData
    { enrichedOriginalRequest :: RequestData
    , enrichedAssetName  :: TokenName
    , enrichedKey        :: key 'ScriptK XPrv
    , enrichedPassphrase :: Passphrase "encryption"
    }

fromApiMintBurnData
    :: ApiMintBurnData (n :: NetworkDiscriminant)
    -> MintBurnData RequestData
fromApiMintBurnData apiReq =
    let
        monetaryPolicyIdx :: DerivationIndex
        monetaryPolicyIdx
          = apiReq
            & view #monetaryPolicyIndex
            & fmap getApiT
            & fromMaybe (DerivationIndex 0)

        assetName :: TokenName
        assetName
          = apiReq
            & view #assetName 
            & getApiT

        op :: MintBurnOperation
        op
          = apiReq
            & Api.operation
            & fromApiMintBurnOperation
    in
        MintBurnData op (RequestData monetaryPolicyIdx assetName)

    where
        fromApiAddress :: (Api.ApiT Address, Proxy (n :: NetworkDiscriminant)) -> Address
        fromApiAddress = getApiT . fst

        fromApiQty :: forall unit. Quantity unit Natural -> TokenQuantity
        fromApiQty = TokenQuantity . getQuantity

        fromApiMintBurnOperation :: ApiMintBurnOperation (n :: NetworkDiscriminant) -> MintBurnOperation
        fromApiMintBurnOperation = \case
            Api.ApiMint mints             ->
                Mint $ (\(Api.ApiMintData addr amt) -> (fromApiAddress addr, fromApiQty amt)) <$> mints
            Api.ApiBurn (Api.ApiBurnData burn)              ->
                Burn $ fromApiQty burn
            Api.ApiMintAndBurn mints (Api.ApiBurnData burn) ->
                MintAndBurn ((\(Api.ApiMintData addr amt) -> (fromApiAddress addr, fromApiQty amt)) <$> mints) (fromApiQty burn)

enrich
    :: Functor f
    => (DerivationIndex -> f (key 'ScriptK XPrv, Passphrase "encryption"))
    -> MintBurnData RequestData
    -> f (MintBurnData (EnrichedData key))
enrich f reqData =
    let
        assetName = reqData & mintBurnData & reqAssetName
        drvIdx    = reqData & mintBurnData & reqMonetaryPolicyIndex
        op        = reqData & mintBurnOperation
    in
        MintBurnData op
          <$> ((\(k, pwd) -> EnrichedData (mintBurnData reqData) assetName k pwd) <$> f drvIdx)

enrichedScript
    :: WalletKey key
    => EnrichedData key
    -> Script KeyHash
enrichedScript =
    RequireSignatureOf
    . hashVerificationKey UtxoExternal
    . publicKey
    . enrichedKey

-- enrichedPolicy :: WalletKey key => EnrichedData key -> TokenPolicyId
-- enrichedPolicy = tokenPolicyIdFromScript . enrichedScript

enrichedAssetId
    :: WalletKey key
    => EnrichedData key
    -> AssetId
enrichedAssetId enrichedData =
    let
        assetName = enrichedData & enrichedAssetName
        policyId  = enrichedData & enrichedScript & tokenPolicyIdFromScript
    in
        AssetId policyId assetName

getMints :: MintBurnData any -> [(Address, TokenQuantity)]
getMints dat =
    case mintBurnOperation dat of
        Mint mints          -> NE.toList mints
        Burn _              -> []
        MintAndBurn mints _ -> NE.toList mints

getMintBurnScript
    :: WalletKey key
    => MintBurnData (EnrichedData key)
    -> Script KeyHash
getMintBurnScript = enrichedScript . mintBurnData

getSigningKey
    :: MintBurnData (EnrichedData key)
    -> (key 'ScriptK XPrv, Passphrase "encryption")
getSigningKey =
    (\enriched -> (enrichedKey enriched, enrichedPassphrase enriched))
    . mintBurnData

getTxOuts
    :: WalletKey key
    => MintBurnData (EnrichedData key)
    -> [TxOut]
getTxOuts enrichedData =
    let
        minting = enrichedData & getMints
        assetId = enrichedData & mintBurnData & enrichedAssetId
    in
        foldMap
            (\(addr, qty) ->
                 [ TxOut addr
                   $ TokenBundle.fromTokenMap
                   $ TokenMap.singleton assetId qty
                 ]
            )
            minting

getPolicyId
    :: WalletKey key
    => MintBurnData (EnrichedData key)
    -> TokenPolicyId
getPolicyId = tokenPolicyIdFromScript . enrichedScript . mintBurnData

getAssetName
    :: MintBurnData (EnrichedData key)
    -> TokenName 
getAssetName = reqAssetName . enrichedOriginalRequest . mintBurnData

getMonetaryPolicyIndex
    :: MintBurnData (EnrichedData key)
    -> DerivationIndex
getMonetaryPolicyIndex = reqMonetaryPolicyIndex . enrichedOriginalRequest . mintBurnData

tmpGetAddrMap
    :: WalletKey key
    => MintBurnData (EnrichedData key)
    -> (Maybe (NonEmpty (Address, TokenMap)), Maybe TokenMap)
tmpGetAddrMap enriched =
  let
      assetId = enriched & mintBurnData & enrichedAssetId

      fm xs = case fmap (fmap (TokenMap.singleton assetId)) xs of
          t NE.:| ts -> Just $ t NE.:| ts

      fb = Just . TokenMap.singleton assetId
  in
      case enriched & mintBurnOperation of
          Mint mints             -> (fm mints, Nothing)
          Burn burn              -> (Nothing, fb burn)
          MintAndBurn mints burn -> (fm mints, fb burn)
