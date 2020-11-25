{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This module provides glue code between shelley-ma ledger spec
-- types and functions and the wallet. Types from shelley-ma should
-- generally not leak into the rest of the codebase.
--(HashAlgorithm (ADDRHASH (Crypto era)))
module Cardano.Wallet.Shelley.MultiAsset.Compatibility where

import Prelude

import Cardano.Crypto.Hash
    ( hashFromBytes )
import Cardano.Ledger.Mary.Value
    ( AssetName (..), PolicyID (..), Value (..) )
import Cardano.Ledger.Shelley
    ( ShelleyEra )
import Cardano.Ledger.Val
    ( scaledMinDeposit )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( getHash )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..) )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..) )
import Control.Monad.Trans.Except
    ( except, runExceptT, throwE )
import Data.Bits
    ( toIntegralSized )
import Data.Functor.Identity
    ( runIdentity )
import Data.Map.NonEmpty.Strict
    ( toMap )
import Data.Text.Encoding
    ( encodeUtf8 )
import Ouroboros.Consensus.Shelley.Protocol.Crypto
    ( StandardCrypto )
import Shelley.Spec.Ledger.Scripts
    ( ScriptHash (..) )

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Shelley.Spec.Ledger.Coin as SL

data ErrMinCoinCalc
    = ErrMinCoinCalc_CoinConversionFailed SL.Coin
    | ErrMinCoinCalc_BStoSBSFailed BS.ByteString
    deriving (Show, Eq)

-- | Convert our own @TokenBundle@ type to the corresponding
-- @Value era@ ledger type. This should be very low in cost,
-- because it just converts strict Map to strict Map.
toLedgerSpecValue
    :: TokenBundle
    -> Either ErrMinCoinCalc (Value (ShelleyEra StandardCrypto))
toLedgerSpecValue = \(TokenBundle ada (TokenMap tb)) ->
    runIdentity $ runExceptT $ do
        m <- Map.foldrWithKey' outer (pure Map.empty) tb
        pure $ Value (fromIntegral $ unCoin ada) m
  where
    outer (TokenPolicyId h) !a m =
        let hash = getHash h
        in case hashFromBytes hash of
            Just sbs -> m >>= pure . Map.insert
                (PolicyID (ScriptHash sbs))
                (Map.foldrWithKey inner mempty (toMap a))
            Nothing -> throwE $ ErrMinCoinCalc_BStoSBSFailed hash
    inner (TokenName k) (TokenQuantity a) m = Map.insert (AssetName $ encodeUtf8 k) a m

-- | Calculate the minimal UTxO value for a token bundle.
--
-- Returns @Nothing@ if Coin conversion fails (Integer to Word64).
calculateMinCoin
    :: Coin        -- ^ the protocol minUTxOValue
    -> TokenBundle
    -> Either ErrMinCoinCalc Coin
calculateMinCoin = \ !c !t -> runIdentity $ runExceptT $ do
    val <- except $ toLedgerSpecValue t
    except $ fromLedgerCoin $ scaledMinDeposit val (fromWalletCoin c)
  where
    fromLedgerCoin :: SL.Coin -> Either ErrMinCoinCalc Coin
    fromLedgerCoin slc@(SL.Coin !i) =
        maybe (Left $ ErrMinCoinCalc_CoinConversionFailed slc)
            Right (Coin <$> (toIntegralSized i))

    fromWalletCoin :: Coin -> SL.Coin
    fromWalletCoin (Coin !i) = SL.Coin (fromIntegral i)
