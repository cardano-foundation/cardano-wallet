{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- A local state query that retrieves information about the
-- reward account.
--
module Cardano.Wallet.Network.LocalStateQuery.RewardAccount
    ( fetchRewardAccounts
    ) where

import Prelude

import Cardano.Wallet.Network.Implementation.Ouroboros
    ( LSQ (..)
    )
import Cardano.Wallet.Network.LocalStateQuery.Extra
    ( onAnyEra
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromStakeCredential
    , toLedgerStakeCredential
    )
import Data.Map
    ( Map
    )
import Data.Set
    ( Set
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Cardano.Block
    ( EraCrypto
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )

import qualified Cardano.Crypto.Hash as Crypto
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.Crypto as SL
import qualified Cardano.Ledger.Shelley.API as SL
import qualified Cardano.Ledger.Shelley.LedgerState as SL
import qualified Cardano.Wallet.Primitive.Ledger.Convert as Ledger
import qualified Cardano.Wallet.Primitive.Types.Coin as W
import qualified Cardano.Wallet.Primitive.Types.RewardAccount as W
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley

{-----------------------------------------------------------------------------
    Local State Query for the Reward Account
------------------------------------------------------------------------------}
type LSQ' = LSQ (CardanoBlock StandardCrypto) IO

fetchRewardAccounts
    :: Set W.RewardAccount -> LSQ' (Map W.RewardAccount W.Coin)
fetchRewardAccounts accounts =
    onAnyEra
        (pure byronValue)
        shelleyQry
        shelleyQry
        shelleyQry
        shelleyQry
        shelleyQry
        shelleyQry
  where
    byronValue :: Map W.RewardAccount W.Coin
    byronValue = Map.fromList . map (,W.Coin 0) $ Set.toList accounts

    shelleyQry
        :: (Crypto.HashAlgorithm (SL.ADDRHASH (EraCrypto shelleyEra)))
        => LSQ
            (Shelley.ShelleyBlock protocol shelleyEra)
            IO
            (Map W.RewardAccount W.Coin)
    shelleyQry =
        fmap fromBalanceResult
            . LSQry
            . Shelley.GetFilteredDelegationsAndRewardAccounts
            $ Set.map toLedgerStakeCredential accounts

    fromBalanceResult
        :: ( Map
                (SL.Credential 'SL.Staking crypto)
                (SL.KeyHash 'SL.StakePool crypto)
           , SL.RewardAccounts crypto
           )
        -> Map W.RewardAccount W.Coin
    fromBalanceResult (_, rewardAccounts) =
        ( Map.mapKeys fromStakeCredential
            $ Map.map Ledger.toWalletCoin rewardAccounts
        )
