{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Api.Server.Handlers.MintBurn
  ( extractMintBurn
  , mkToApiAssetMintBurn
  )
  where

import Prelude hiding
    ( (.) )

import Cardano.Wallet
    ( HasDBLayer, readPolicyPublicKey )
import Cardano.Wallet.Api.Types.Key
    ( ApiPolicyKey (ApiPolicyKey), computeKeyPayload )
import Cardano.Wallet.Api.Types.MintBurn
    ( ApiAssetMintBurn (..), includePolicyKeyInfo, policyIx, toApiTokens )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant )
import Cardano.Wallet.Primitive.Types
    ( WalletId )
import Cardano.Wallet.Read
    ( Tx )
import Cardano.Wallet.Read.Eras
    ( EraFun, K (..), (*&&&*) )
import Cardano.Wallet.Read.Primitive.Tx.Features.Mint
    ( mint )
import Cardano.Wallet.Read.Tx.Mint
    ( getEraMint )
import Cardano.Wallet.Read.Tx.Witnesses
    ( getEraWitnesses )
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

extractMintBurn :: EraFun Tx (K (TokenMapWithScripts, TokenMapWithScripts))
extractMintBurn = mint . (getEraMint *&&&* getEraWitnesses)

-- | Compute a function to promote a certificate to its API type
mkToApiAssetMintBurn
    ::
      forall ctx s k (n :: NetworkDiscriminant).
        ( HasDBLayer IO s k ctx
        , Typeable n
        , Typeable s
        )
    => ctx
    -> WalletId
    -> Handler (TokenMapWithScripts -> ApiAssetMintBurn)
mkToApiAssetMintBurn ctx wid = do
    xpubM <- fmap (fmap fst . eitherToMaybe)
        <$> liftIO . runExceptT $ readPolicyPublicKey @ctx @s @k @n ctx wid
    pure $ \tokenWithScripts -> ApiAssetMintBurn
        { tokens = toApiTokens tokenWithScripts
        , walletPolicyKeyHash =
            uncurry ApiPolicyKey . computeKeyPayload (Just True) <$>
            includePolicyKeyInfo tokenWithScripts xpubM
        , walletPolicyKeyIndex =
            policyIx <$ includePolicyKeyInfo tokenWithScripts xpubM
        }

