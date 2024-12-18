{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--

module Cardano.Wallet.Api.Http.Server.Handlers.MintBurn
  ( convertApiAssetMintBurn
  , getTxApiAssetMintBurn
  )
  where

import Prelude hiding
    ( (.)
    )

import Cardano.Wallet
    ( WalletLayer
    , readPolicyPublicKey
    )
import Cardano.Wallet.Api.Http.Server.Handlers.TxCBOR
    ( ParsedTxCBOR (..)
    )
import Cardano.Wallet.Api.Types.Key
    ( ApiPolicyKey (ApiPolicyKey)
    , computeKeyPayload
    )
import Cardano.Wallet.Api.Types.MintBurn
    ( ApiAssetMintBurn (..)
    , includePolicyKeyInfo
    , policyIx
    , toApiTokens
    )
import Cardano.Wallet.Flavor
    ( WalletFlavor
    )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts (..)
    )
import Control.Category
    ( (.)
    )
import Control.Monad.IO.Class
    ( MonadIO (liftIO)
    )
import Data.Either.Extra
    ( eitherToMaybe
    )
import Servant
    ( Handler
    )

-- | Promote mint and burn to their API type.
convertApiAssetMintBurn
    :: WalletFlavor s
    => WalletLayer IO s
    -> (TokenMapWithScripts, TokenMapWithScripts)
    -> Handler (ApiAssetMintBurn, ApiAssetMintBurn)
convertApiAssetMintBurn ctx (mint, burn) = do
    xpubM <- fmap (fmap fst . eitherToMaybe)
        <$> liftIO $ readPolicyPublicKey ctx
    let  convert tokenWithScripts =  ApiAssetMintBurn
            { tokens = toApiTokens tokenWithScripts
            , walletPolicyKeyHash =
                uncurry ApiPolicyKey . computeKeyPayload (Just True) <$>
                includePolicyKeyInfo tokenWithScripts xpubM
            , walletPolicyKeyIndex =
                policyIx <$ includePolicyKeyInfo tokenWithScripts xpubM
            }
    pure (convert mint, convert burn)

getTxApiAssetMintBurn
    :: WalletFlavor s
    => WalletLayer IO s
    -> ParsedTxCBOR
    -> (Handler (ApiAssetMintBurn, ApiAssetMintBurn))
getTxApiAssetMintBurn ctx ParsedTxCBOR{..} =
    convertApiAssetMintBurn ctx mintBurn
