{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

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
    ( (.) )

import Cardano.Wallet
    ( HasDBLayer, readPolicyPublicKey )
import Cardano.Wallet.Api.Http.Server.Handlers.TxCBOR
    ( ParsedTxCBOR (..) )
import Cardano.Wallet.Api.Types.Key
    ( ApiPolicyKey (ApiPolicyKey), computeKeyPayload )
import Cardano.Wallet.Api.Types.MintBurn
    ( ApiAssetMintBurn (..), includePolicyKeyInfo, policyIx, toApiTokens )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Read.NetworkId
    ( NetworkDiscriminant )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts (..) )
import Control.Category
    ( (.) )
import Control.Monad.IO.Class
    ( MonadIO (liftIO) )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Either.Extra
    ( eitherToMaybe )
import Data.Typeable
    ( Typeable )
import Servant
    ( Handler )

-- | Promote mint and burn to their API type.
convertApiAssetMintBurn
    ::
      forall ctx s k (n :: NetworkDiscriminant).
        ( HasDBLayer IO s k ctx
        , Typeable n
        , Typeable s
        )
    => ctx
    -> WalletId
    -> (TokenMapWithScripts , TokenMapWithScripts)
    -> Handler (ApiAssetMintBurn, ApiAssetMintBurn)
convertApiAssetMintBurn ctx wid (mint, burn) = do
    xpubM <- fmap (fmap fst . eitherToMaybe)
        <$> liftIO . runExceptT $ readPolicyPublicKey @ctx @s @k @n ctx wid
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
    ::
      forall ctx s k (n :: NetworkDiscriminant).
        ( HasDBLayer IO s k ctx
        , Typeable n
        , Typeable s
        )
    => ctx
    -> WalletId
    -> ParsedTxCBOR
    -> (Handler (ApiAssetMintBurn, ApiAssetMintBurn))
getTxApiAssetMintBurn ctx wid ParsedTxCBOR{..} =
    convertApiAssetMintBurn @ctx @s @k @n ctx wid mintBurn
