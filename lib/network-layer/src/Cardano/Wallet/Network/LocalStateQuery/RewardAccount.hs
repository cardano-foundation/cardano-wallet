{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- A local state query that retrieves information about the
-- reward account.
module Cardano.Wallet.Network.LocalStateQuery.RewardAccount
    ( fetchRewardAccounts
    , RewardAccountState (..)
    , fetchRewardAccountStates
    , toRewardAccountStates
    , getStakeDelegDeposits
    ) where

import Cardano.Ledger.Conway.Governance
    ( ConwayEraGov
    )
import Cardano.Ledger.Credential
    ( Credential
    )
import Cardano.Ledger.Keys
    ( Staking
    )
import Cardano.Wallet.Network.Implementation.Ouroboros
    ( LSQ (..)
    )
import Cardano.Wallet.Network.LocalStateQuery.Extra
    ( onAnyEra
    )
import Cardano.Wallet.Primitive.Ledger.Read.Tx.Features.Certificates
    ( fromLedgerDRep
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( fromStakeCredential
    , toLedgerStakeCredential
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( VoteDelegation (..)
    )
import Data.Map
    ( Map
    )
import Data.Set
    ( Set
    )
import Fmt
    ( Buildable (..)
    )
import Ouroboros.Consensus.Cardano
    ( CardanoBlock
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardCrypto
    )
import Prelude

import qualified Cardano.Ledger.Coin as Ledger
import qualified Cardano.Ledger.Credential as SL
import qualified Cardano.Ledger.DRep as Ledger
import qualified Cardano.Ledger.Keys as SL
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
        shelleyQry
  where
    byronValue :: Map W.RewardAccount W.Coin
    byronValue = Map.fromList . map (,W.Coin 0) $ Set.toList accounts

    shelleyQry
        :: LSQ
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
                (SL.Credential SL.Staking)
                (SL.KeyHash SL.StakePool)
           , Map (SL.Credential SL.Staking) Ledger.Coin
           )
        -> Map W.RewardAccount W.Coin
    fromBalanceResult (_, rewardAccounts) =
        ( Map.mapKeys fromStakeCredential
            $ Map.map Ledger.toWalletCoin rewardAccounts
        )

-- | The state of a reward account in the ledger, as of one read.
data RewardAccountState = RewardAccountState
    { rewardBalance :: !W.Coin
    -- ^ The reward balance. Zero for an account that the ledger does not
    -- hold.
    , voteDelegation :: !(Maybe VoteDelegation)
    -- ^ The vote delegation. 'Nothing' in eras before Conway.
    }
    deriving (Eq, Show)

instance Buildable RewardAccountState where
    build RewardAccountState{rewardBalance, voteDelegation} =
        build rewardBalance
            <> maybe mempty (\v -> " (vote: " <> build v <> ")") voteDelegation

-- | Read the reward balance and, from Conway on, the vote delegation of each
-- given reward account, all from the same ledger state.
--
-- Returns one entry for each given account. Sends no query for an empty set
-- of accounts, because 'Shelley.GetFilteredVoteDelegatees' answers an empty
-- set with the vote delegation of every account in the ledger.
fetchRewardAccountStates
    :: Set W.RewardAccount -> LSQ' (Map W.RewardAccount RewardAccountState)
fetchRewardAccountStates accounts
    | Set.null accounts = pure Map.empty
    | otherwise =
        onAnyEra
            (pure $ toRewardAccountStates accounts Map.empty Nothing)
            shelleyQry
            shelleyQry
            shelleyQry
            shelleyQry
            shelleyQry
            conwayQry
            conwayQry
  where
    credentials = Set.map toLedgerStakeCredential accounts

    balanceQry
        :: LSQ
            (Shelley.ShelleyBlock protocol era)
            IO
            (Map (SL.Credential SL.Staking) Ledger.Coin)
    balanceQry =
        snd
            <$> LSQry (Shelley.GetFilteredDelegationsAndRewardAccounts credentials)

    shelleyQry
        :: LSQ
            (Shelley.ShelleyBlock protocol era)
            IO
            (Map W.RewardAccount RewardAccountState)
    shelleyQry =
        (\balances -> toRewardAccountStates accounts balances Nothing)
            <$> balanceQry

    conwayQry
        :: ConwayEraGov era
        => LSQ
            (Shelley.ShelleyBlock protocol era)
            IO
            (Map W.RewardAccount RewardAccountState)
    conwayQry =
        toRewardAccountStates accounts
            <$> balanceQry
            <*> (Just <$> LSQry (Shelley.GetFilteredVoteDelegatees credentials))

-- | Combine the answers of one read into one 'RewardAccountState' for each
-- requested reward account.
--
-- A requested account that is absent from the balances has a zero balance.
-- A requested account that is absent from the vote delegations has
-- 'VoteNotDelegated'.
toRewardAccountStates
    :: Set W.RewardAccount
    -- ^ The requested reward accounts.
    -> Map (SL.Credential SL.Staking) Ledger.Coin
    -- ^ The reward balances that the ledger holds for them.
    -> Maybe (Map (SL.Credential SL.Staking) Ledger.DRep)
    -- ^ The vote delegations that the ledger holds for them, or 'Nothing'
    -- in eras before Conway.
    -> Map W.RewardAccount RewardAccountState
toRewardAccountStates accounts balances delegations =
    Map.fromSet stateOf accounts
  where
    walletBalances =
        Map.mapKeys fromStakeCredential $ Map.map Ledger.toWalletCoin balances
    walletDelegations =
        Map.mapKeys fromStakeCredential . Map.map fromLedgerDRep
            <$> delegations
    stateOf account =
        RewardAccountState
            { rewardBalance =
                Map.findWithDefault (W.Coin 0) account walletBalances
            , voteDelegation =
                maybe VoteNotDelegated VoteDelegatedTo . Map.lookup account
                    <$> walletDelegations
            }

getStakeDelegDeposits
    :: Set (Credential Staking)
    -> LSQ' (Map (Credential Staking) Ledger.Coin)
getStakeDelegDeposits credentials =
    onAnyEra
        (pure byronValue)
        (LSQry $ Shelley.GetStakeDelegDeposits credentials)
        (LSQry $ Shelley.GetStakeDelegDeposits credentials)
        (LSQry $ Shelley.GetStakeDelegDeposits credentials)
        (LSQry $ Shelley.GetStakeDelegDeposits credentials)
        (LSQry $ Shelley.GetStakeDelegDeposits credentials)
        (LSQry $ Shelley.GetStakeDelegDeposits credentials)
        (LSQry $ Shelley.GetStakeDelegDeposits credentials)
  where
    byronValue :: Map (Credential Staking) Ledger.Coin
    byronValue = Map.fromList . map (,Ledger.Coin 0) $ Set.toList credentials
