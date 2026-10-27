{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DelegationSpec
    ( spec
    ) where

import Cardano.Address.Derivation
    ( XPrv
    , xprvFromBytes
    , xprvToBytes
    )
import Cardano.Pool.Types
    ( PoolId (..)
    )
import Cardano.Wallet
    ( PoolRetirementEpochInfo (..)
    )
import Cardano.Wallet.Address.Derivation
    ( DerivationIndex (..)
    )
import Cardano.Wallet.Delegation
    ( VoteRequest (..)
    )
import Cardano.Wallet.Primitive.Types
    ( EpochNo (..)
    , WalletDelegation (..)
    , WalletDelegationNext (WalletDelegationNext)
    , WalletDelegationStatus (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoinPositive
    )
import Cardano.Wallet.Primitive.Types.DRep
    ( DRep (..)
    , DRepID (..)
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    , VoteDelegation (..)
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Cardano.Wallet.Transaction
    ( ErrCannotJoin (..)
    , VotingAction (..)
    , Withdrawal (..)
    )
import Data.Function
    ( on
    )
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import Data.Maybe
    ( fromJust
    , isJust
    , isNothing
    )
import Data.Set
    ( Set
    )
import Data.Word
    ( Word64
    )
import Data.Word.Odd
    ( Word31
    )
import Hedgehog.Corpus
    ( metasyntactic
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldNotBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , NonEmptyList (..)
    , Property
    , applyArbitrary4
    , arbitrarySizedBoundedIntegral
    , checkCoverage
    , cover
    , elements
    , expectFailure
    , forAll
    , label
    , oneof
    , property
    , shrinkIntegral
    , suchThat
    , vector
    , vectorOf
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary
    , genericShrink
    )
import Prelude

import qualified Cardano.Balance.Tx.Eras as Write
import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Delegation as WD
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.ByteString as BS
import qualified Data.Set as Set

spec :: Spec
spec = describe "Cardano.Wallet.DelegationSpec" $ do
    describe "Join/Quit Stake pool properties" $ do
        it "You can quit if you cannot join Conway" $ do
            property (prop_guardJoinQuit guardJoinConway)
        it "You can join if you cannot quit Conway" $ do
            property (prop_guardQuitJoin guardJoinConway)

    describe "Join/Quit Stake pool unit mockEventSource" $ do
        it "Can rejoin A, when active = A in Conway" $ do
            let dlg = WalletDelegation{active = Delegating pidA, next = []}
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidA
                noRetirementPlanned
                VotedDifferently
                `shouldBe` Right ()
        it "Cannot rejoin A, when active = A in Conway" $ do
            let dlg = WalletDelegation{active = Delegating pidA, next = []}
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidA
                noRetirementPlanned
                VotedSameAsBefore
                `shouldBe` Left (W.ErrAlreadyDelegatingVoting pidA)
        it "Can join A, when next = [A] in Conway" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation{active = NotDelegating, next = [next1]}
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidA
                noRetirementPlanned
                VotedDifferently
                `shouldBe` Right ()
        it "Can join A, when next = [A] in Conway" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation{active = NotDelegating, next = [next1]}
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidA
                noRetirementPlanned
                VotedSameAsBefore
                `shouldBe` Left (W.ErrAlreadyDelegatingVoting pidA)
        it "Can join A, when active = A, next = [B] in any era" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let dlg =
                    WalletDelegation
                        { active = Delegating pidA
                        , next = [next1]
                        }
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidA
                noRetirementPlanned
                VotedDifferently
                `shouldBe` Right ()
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidA
                noRetirementPlanned
                VotedSameAsBefore
                `shouldBe` Right ()
        it "Can join A, when active = A, next = [B, A] in Conway" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let next2 = WalletDelegationNext (EpochNo 2) (Delegating pidA)
            let dlg =
                    WalletDelegation
                        { active = Delegating pidA
                        , next = [next1, next2]
                        }
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidA
                noRetirementPlanned
                VotedDifferently
                `shouldBe` Right ()
        it "Cannot join A, when active = A, next = [B, A] in Conway" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let next2 = WalletDelegationNext (EpochNo 2) (Delegating pidA)
            let dlg =
                    WalletDelegation
                        { active = Delegating pidA
                        , next = [next1, next2]
                        }
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidA
                noRetirementPlanned
                VotedSameAsBefore
                `shouldBe` Left (W.ErrAlreadyDelegatingVoting pidA)
        it "Cannot join when pool is unknown in any era" $ do
            let dlg = WalletDelegation{active = NotDelegating, next = []}
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidUnknown
                noRetirementPlanned
                VotedDifferently
                `shouldBe` Left (W.ErrNoSuchPool pidUnknown)
            WD.guardJoin
                Write.RecentEraConway
                knownPools
                dlg
                pidUnknown
                noRetirementPlanned
                VotedSameAsBefore
                `shouldBe` Left (W.ErrNoSuchPool pidUnknown)
        it "Cannot quit when active: not_delegating, next = []" $ do
            let dlg = WalletDelegation{active = NotDelegating, next = []}
            WD.guardQuit dlg NoWithdrawal (Coin 0) False
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [not_delegating]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) NotDelegating
            let dlg =
                    WalletDelegation
                        { active = Delegating pidA
                        , next = [next1]
                        }
            WD.guardQuit dlg NoWithdrawal (Coin 0) False
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [B, not_delegating]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let next2 = WalletDelegationNext (EpochNo 2) NotDelegating
            let dlg =
                    WalletDelegation
                        { active = Delegating pidA
                        , next = [next1, next2]
                        }
            WD.guardQuit dlg NoWithdrawal (Coin 0) False
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Can quit when active: not_delegating, next = [A]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidA)
            let dlg =
                    WalletDelegation
                        { active = NotDelegating
                        , next = [next1]
                        }
            WD.guardQuit dlg NoWithdrawal (Coin 0) False `shouldBe` Right ()

    describe "joinDRepVotingAction" $ do
        it "allows re-delegating to A after scheduled A -> B" $ do
            vote drepA (scheduled votingAB) True
                `shouldBe` Right (Vote drepA)

        it "rejects the same vote when effective delegation is B" $ do
            vote drepB (scheduled votingAB) True
                `shouldBe` Left (W.ErrAlreadyVoted drepB)

        it "rejects the same vote for active-only A" $ do
            vote drepA (activeOnly (Voting drepA)) True
                `shouldBe` Left (W.ErrAlreadyVoted drepA)

        it "allows a different DRep for active-only A" $ do
            vote drepB (activeOnly (Voting drepA)) True
                `shouldBe` Right (Vote drepB)

        it "ignores superseded active A when next is B" $ do
            vote drepA supersededAToB True
                `shouldBe` Right (Vote drepA)

        it "rejects B when scheduled next supersedes active A" $ do
            vote drepB supersededAToB True
                `shouldBe` Left (W.ErrAlreadyVoted drepB)

        it "allows re-delegating to A after a DRep quit" $ do
            vote drepA supersededAToPool True
                `shouldBe` Right (Vote drepA)

        it "rejects predefined Abstain when it is effective" $ do
            vote Abstain supersededAToAbstain True
                `shouldBe` Left (W.ErrAlreadyVoted Abstain)

        it "allows predefined Abstain when effective DRep is A" $ do
            vote Abstain (activeOnly (Voting drepA)) True
                `shouldBe` Right (Vote Abstain)

        it "rejects NoConfidence as last scheduled status" $ do
            vote NoConfidence (scheduled votingANoConfidence) True
                `shouldBe` Left (W.ErrAlreadyVoted NoConfidence)

        it "allows A when last DelegatingVoting is B" $ do
            vote drepA (scheduled delegatingVotingAB) True
                `shouldBe` Right (Vote drepA)

        it "rejects B when last DelegatingVoting is B" $ do
            vote drepB (scheduled delegatingVotingAB) True
                `shouldBe` Left (W.ErrAlreadyVoted drepB)

        it "returns VoteRegisteringKey without a stake key" $ do
            vote drepA (scheduled votingAB) False
                `shouldBe` Right (VoteRegisteringKey drepA)

        it "rejects a same vote when stake key is unregistered" $ do
            vote drepB (scheduled votingAB) False
                `shouldBe` Left (W.ErrAlreadyVoted drepB)

        it "voteRequestFor matches joinDRep on A -> B" $ do
            let dlg = scheduled votingAB
            WD.voteRequestFor drepA dlg
                `shouldBe` VotedDifferently
            vote drepA dlg True `shouldBe` Right (Vote drepA)
            WD.voteRequestFor drepB dlg
                `shouldBe` VotedSameAsBefore
            vote drepB dlg True
                `shouldBe` Left (W.ErrAlreadyVoted drepB)

        it "voteRequestFor matches joinDRepVotingAction arbitrarily"
            $ property prop_joinDRepParityWithVoteRequest

        it "effectiveDelegationStatus is last next, else active"
            $ property prop_effectiveDelegationStatus

    describe "joinDRepVotingAction with the ledger's vote delegation" $ do
        it "accepts the history's DRep while the ledger holds another" $ do
            voteWith (Just (VoteDelegatedTo drepB)) drepA votedA True
                `shouldBe` Right (Vote drepA)

        it "rejects the DRep that the ledger holds, set outside the wallet" $ do
            voteWith (Just (VoteDelegatedTo drepB)) drepB votedA True
                `shouldBe` Left (W.ErrAlreadyVoted drepB)
            voteWith (Just (VoteDelegatedTo drepB)) drepB notVoted True
                `shouldBe` Left (W.ErrAlreadyVoted drepB)

        it "ignores a vote delegation that a DRep retirement removed" $ do
            voteWith (Just VoteNotDelegated) drepA votedA True
                `shouldBe` Right (Vote drepA)

        it "rejects Abstain and NoConfidence when the ledger holds them" $ do
            voteWith (Just (VoteDelegatedTo Abstain)) Abstain notVoted True
                `shouldBe` Left (W.ErrAlreadyVoted Abstain)
            voteWith
                (Just (VoteDelegatedTo NoConfidence))
                NoConfidence
                votedA
                True
                `shouldBe` Left (W.ErrAlreadyVoted NoConfidence)

        it "accepts Abstain and NoConfidence when the ledger holds none" $ do
            voteWith (Just VoteNotDelegated) Abstain votedAbstain True
                `shouldBe` Right (Vote Abstain)
            voteWith (Just VoteNotDelegated) NoConfidence notVoted False
                `shouldBe` Right (VoteRegisteringKey NoConfidence)

        it "decides from the history while the ledger value is unread" $ do
            voteWith Nothing drepA votedA True
                `shouldBe` Left (W.ErrAlreadyVoted drepA)
            voteWith Nothing drepB votedA True
                `shouldBe` Right (Vote drepB)

    describe "guardDRepRegistered" $ do
        let afterBootstrap = WD.guardDRepRegistered False

        it "refuses a retired DRep, which the ledger no longer holds"
            $ afterBootstrap drepA (Just False)
            `shouldBe` Left (W.ErrDRepNotRegistered drepIdA)

        it "accepts a DRep that registered again after retiring"
            $ afterBootstrap drepA (Just True)
            `shouldBe` Right ()

        -- The registration query answers whether the ledger's DRep map holds
        -- the credential, which it does for a DRep past its expiry epoch.
        it "accepts an inactive DRep, which the ledger holds as registered"
            $ afterBootstrap drepA (Just True)
            `shouldBe` Right ()

        it "never refuses Abstain or NoConfidence" $ do
            afterBootstrap Abstain (Just False) `shouldBe` Right ()
            afterBootstrap NoConfidence (Just False) `shouldBe` Right ()

        it "does not refuse without an answer about the registration"
            $ afterBootstrap drepA Nothing
            `shouldBe` Right ()

        it "does not refuse in the bootstrap phase"
            $ WD.guardDRepRegistered True drepA (Just False)
            `shouldBe` Right ()

    describe "withdrawalPermitted" $ do
        let keyAccount = FromKeyHash (BS.replicate 28 3)
            scriptAccount = FromScriptHash (BS.replicate 28 4)
            permitted = W.withdrawalPermitted False

        it
            "refuses a retired DRep: the history names A, the ledger holds none"
            $ permitted keyAccount (Just VoteNotDelegated) (Just drepA)
            `shouldBe` False

        it "permits a vote delegation made outside the wallet"
            $ permitted keyAccount (Just (VoteDelegatedTo drepA)) Nothing
            `shouldBe` True

        it "permits a delegation to Abstain"
            $ permitted keyAccount (Just (VoteDelegatedTo Abstain)) Nothing
            `shouldBe` True

        it "permits a delegation to NoConfidence"
            $ permitted keyAccount (Just (VoteDelegatedTo NoConfidence)) Nothing
            `shouldBe` True

        it "refuses a key-hash account without a vote delegation"
            $ permitted keyAccount (Just VoteNotDelegated) Nothing
            `shouldBe` False

        it "permits a script-hash account without a vote delegation"
            $ permitted scriptAccount (Just VoteNotDelegated) Nothing
            `shouldBe` True

        it "permits a script-hash account whose ledger value is unread"
            $ permitted scriptAccount Nothing Nothing
            `shouldBe` True

        it "permits every account in the bootstrap phase"
            $ do
                W.withdrawalPermitted True keyAccount (Just VoteNotDelegated) Nothing
                    `shouldBe` True
                W.withdrawalPermitted True keyAccount Nothing Nothing
                    `shouldBe` True

        it "follows a recorded vote when the ledger value is unread"
            $ do
                permitted keyAccount Nothing (Just drepA) `shouldBe` True
                permitted keyAccount Nothing Nothing `shouldBe` False

        it "ignores the history when the ledger value is read"
            $ do
                permitted keyAccount (Just (VoteDelegatedTo drepB)) (Just drepA)
                    `shouldBe` True
                permitted keyAccount (Just VoteNotDelegated) (Just drepA)
                    `shouldBe` False

    describe "Agda #5350 law mirrors" $ do
        it "AGDA-5350-EMPTY: empty next selects active"
            $ property prop_effectiveDelegationStatusEmpty

        it "AGDA-5350-LAST: non-empty next selects its final status"
            $ property prop_effectiveDelegationStatusLast

        it "AGDA-5350-HISTORY: superseded history cannot change it"
            $ property prop_voteDecisionIgnoresHistory

        it "AGDA-5350-SAME: only the effective DRep is a duplicate vote"
            $ property prop_joinDRepVotingActionEffective

        it "control: the historical decision rejects a re-delegation" $ do
            historicalVoteRequestFor drepA supersededAToB
                `shouldBe` VotedSameAsBefore
            WD.voteRequestFor drepA supersededAToB
                `shouldBe` VotedDifferently
            vote drepA supersededAToB True
                `shouldBe` Right (Vote drepA)

        it "control: the historical decision depends on history" $ do
            let scheduledB = [at 1 (Voting drepB)]
            let withHistory = WalletDelegation (Voting drepA) scheduledB
            let withoutHistory = WalletDelegation NotDelegating scheduledB
            historicalVoteRequestFor drepA withHistory
                `shouldNotBe` historicalVoteRequestFor drepA withoutHistory
            WD.voteRequestFor drepA withHistory
                `shouldBe` WD.voteRequestFor drepA withoutHistory

    describe "Agda #5350 model assumptions" $ do
        it "INV-5350-DREP-EQ: Eq DRep matches structural identity"
            $ property prop_drepEqualityMatchesStructure

        it "control: DRep equality separates key from script" $ do
            let bytes = BS.replicate 28 7
            let asKey = FromDRepID (DRepFromKeyHash (DRepKeyHash bytes))
            let asScript =
                    FromDRepID (DRepFromScriptHash (DRepScriptHash bytes))
            (asKey == asScript) `shouldBe` False
            structurallyEqualDRep asKey asScript `shouldBe` False
            structurallyEqualDRep asKey asKey `shouldBe` True

    describe "Agda #5466 law mirrors" $ do
        it "AGDA-5466-WDRL-LEDGER: a read ledger value decides a withdrawal"
            $ prop_withdrawalPermittedFollowsLedger W.withdrawalPermitted

        it
            "AGDA-5466-WDRL-EXEMPT: bootstrap and script-hash accounts withdraw"
            $ prop_withdrawalPermittedExempt W.withdrawalPermitted

        it "AGDA-5466-WDRL-UNREAD: an unread ledger value defers to history"
            $ prop_withdrawalPermittedUnreadFollowsHistory W.withdrawalPermitted

        it
            "AGDA-5466-VOTE-LEDGER: a read ledger value decides a repeated vote"
            $ prop_ledgerVoteRequestFollowsLedger WD.ledgerVoteRequestFor

        it "AGDA-5466-VOTE-UNREAD: an unread ledger value defers to #5350"
            $ prop_ledgerVoteRequestUnreadIs5350 WD.ledgerVoteRequestFor

    describe "Agda #5466 negative controls" $ do
        it "WDRL-LEDGER refutes the decision from the history alone"
            $ expectFailure
            $ prop_withdrawalPermittedFollowsLedger
                historyOnlyWithdrawalPermitted

        it "WDRL-EXEMPT refutes the decision without exemptions"
            $ expectFailure
            $ prop_withdrawalPermittedExempt noExemptionWithdrawalPermitted

        it "WDRL-UNREAD refutes permitting every unread value"
            $ expectFailure
            $ prop_withdrawalPermittedUnreadFollowsHistory unreadIgnoresHistory

        it "VOTE-LEDGER refutes the #5350 decision from the history"
            $ expectFailure
            $ prop_ledgerVoteRequestFollowsLedger historyVoteRequestFor

        it "VOTE-UNREAD refutes a different vote for every unread value"
            $ expectFailure
            $ prop_ledgerVoteRequestUnreadIs5350 unreadAlwaysDifferent
  where
    pidA = PoolId "A"
    pidB = PoolId "B"
    pidUnknown = PoolId "unknown"
    knownPools = Set.fromList [pidA, pidB]
    noRetirementPlanned = Nothing
    drepIdA = DRepFromKeyHash (DRepKeyHash (BS.replicate 28 1))
    drepA = FromDRepID drepIdA
    drepB =
        FromDRepID
            (DRepFromKeyHash (DRepKeyHash (BS.replicate 28 2)))
    at epoch = WalletDelegationNext (EpochNo epoch)
    scheduled statuses =
        WalletDelegation NotDelegating (zipWith at [1 ..] statuses)
    activeOnly status = WalletDelegation status []
    votingAB = [Voting drepA, Voting drepB]
    votingANoConfidence = [Voting drepA, Voting NoConfidence]
    delegatingVotingAB =
        [ DelegatingVoting pidA drepA
        , DelegatingVoting pidB drepB
        ]
    supersededAToB =
        WalletDelegation (Voting drepA) [at 1 (Voting drepB)]
    supersededAToPool =
        WalletDelegation (Voting drepA) [at 1 (Delegating pidA)]
    supersededAToAbstain =
        WalletDelegation (Voting drepA) [at 1 (Voting Abstain)]
    notVoted = activeOnly NotDelegating
    votedA = activeOnly (Voting drepA)
    votedAbstain = activeOnly (Voting Abstain)
    -- The #5350 examples above decide with the ledger value unread.
    vote = voteWith Nothing
    voteWith ledger target =
        WD.joinDRepVotingAction Write.RecentEraConway target ledger

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

prop_guardJoinQuit
    :: GuardJoinFun
    -> [PoolId]
    -> WalletDelegation
    -> PoolId
    -> Withdrawal
    -> Maybe W.PoolRetirementEpochInfo
    -> Property
prop_guardJoinQuit guardJoin knownPoolsList dlg pid wdrl mRetirementInfo = checkCoverage
    $ cover
        10
        retirementNotPlanned
        "retirementNotPlanned"
    $ cover
        10
        retirementPlanned
        "retirementPlanned"
    $ cover
        10
        alreadyRetired
        "alreadyRetired"
    $ case guardJoin knownPools dlg pid mRetirementInfo NotVotedYet of
        Right () ->
            label "I can join"
                $ property
                $ alreadyRetired
                `shouldBe` False
        Left W.ErrNoSuchPool{} ->
            label "ErrNoSuchPool" $ property True
        Left W.ErrAlreadyDelegating{} ->
            label
                "ErrAlreadyDelegating"
                (WD.guardQuit dlg wdrl (Coin 0) False === Right ())
        Left W.ErrAlreadyDelegatingVoting{} ->
            label "ErrAlreadyDelegatingVoting" $ property True
  where
    knownPools = Set.fromList knownPoolsList
    retirementNotPlanned =
        isNothing mRetirementInfo
    retirementPlanned =
        (Just True ==) $ do
            info <- mRetirementInfo
            pure $ W.currentEpoch info < W.retirementEpoch info
    alreadyRetired =
        (Just True ==) $ do
            info <- mRetirementInfo
            pure $ W.currentEpoch info >= W.retirementEpoch info

prop_guardQuitJoin
    :: GuardJoinFun
    -> NonEmptyList PoolId
    -> WalletDelegation
    -> Word64
    -> Withdrawal
    -> Property
prop_guardQuitJoin guardJoin (NonEmpty knownPoolsList) dlg rewards wdrl =
    let knownPools = Set.fromList knownPoolsList
    in  let noRetirementPlanned = Nothing
        in  case WD.guardQuit dlg wdrl (Coin.fromWord64 rewards) False of
                Right () ->
                    label "I can quit" $ property True
                Left W.ErrNotDelegatingOrAboutTo ->
                    label "ErrNotDelegatingOrAboutTo"
                        $ guardJoin
                            knownPools
                            dlg
                            (last knownPoolsList)
                            noRetirementPlanned
                            NotVotedYet
                            === Right ()
                Left W.ErrNonNullRewards{} ->
                    label "ErrNonNullRewards"
                        $ property (rewards /= 0)
                        .&&. not (isSelfWdrl wdrl)
  where
    isSelfWdrl WithdrawalSelf{} = True
    isSelfWdrl _ = False

type GuardJoinFun =
    Set PoolId
    -> WalletDelegation
    -> PoolId
    -> Maybe PoolRetirementEpochInfo
    -> VoteRequest
    -> Either ErrCannotJoin ()

guardJoinConway :: GuardJoinFun
guardJoinConway = WD.guardJoin Write.RecentEraConway

-- Mirror of the Agda law AGDA-5350-SAME in
-- specifications/Cardano/Wallet/Delegation.agda: a request is rejected
-- exactly when the target equals the effective DRep. Independent D1
-- oracle: last scheduled status wins, else active.
prop_joinDRepVotingActionEffective
    :: DRep -> WalletDelegation -> Bool -> Property
prop_joinDRepVotingActionEffective target dlg registered =
    checkCoverage
        $ cover
            5
            historyMismatch
            "history-matches-effective-differs"
        $ WD.joinDRepVotingAction
            Write.RecentEraConway
            target
            Nothing
            dlg
            registered
            === expected
  where
    expected
        | statusDRep (effectiveStatus dlg) == Just target =
            Left (W.ErrAlreadyVoted target)
        | registered = Right (Vote target)
        | otherwise = Right (VoteRegisteringKey target)
    historyDreps = fmap statusDRep (historyStatuses dlg)
    historyMismatch =
        (Just target `elem` historyDreps)
            && (statusDRep (effectiveStatus dlg) /= Just target)

-- The F1/F2 parity surface, not a formal-law mirror: the pure
-- duplicate-vote verdict of 'WD.joinDRepVotingAction' must agree with the
-- verdict of 'WD.ledgerVoteRequestFor', which the transaction-building path
-- acts on, for every ledger value and history.
prop_joinDRepParityWithVoteRequest
    :: DRep -> WalletDelegation -> Bool -> Property
prop_joinDRepParityWithVoteRequest target dlg registered =
    forAll (genLedgerVote target) $ \ledger ->
        checkCoverage
            $ cover 10 (isNothing ledger) "ledger value unread"
            $ cover 10 (ledger == Just VoteNotDelegated) "no vote delegation"
            $ cover
                10
                (ledger == Just (VoteDelegatedTo target))
                "the target is delegated to"
            $ cover
                10
                (isDelegatedOtherThan ledger)
                "another DRep is delegated to"
            $ case (WD.ledgerVoteRequestFor target ledger dlg, action ledger) of
                (VotedSameAsBefore, Left (W.ErrAlreadyVoted drep)) ->
                    drep === target
                (VotedDifferently, Right _) -> property True
                _ -> property False
  where
    action ledger =
        WD.joinDRepVotingAction
            Write.RecentEraConway
            target
            ledger
            dlg
            registered
    isDelegatedOtherThan = \case
        Just (VoteDelegatedTo drep) -> drep /= target
        _ -> False

-- A ledger value that is unread, holds no vote delegation, holds the given
-- target, or holds another DRep.
genLedgerVote :: DRep -> Gen (Maybe VoteDelegation)
genLedgerVote target =
    oneof
        [ pure Nothing
        , pure (Just VoteNotDelegated)
        , pure (Just (VoteDelegatedTo target))
        , Just . VoteDelegatedTo <$> arbitrary
        ]

prop_effectiveDelegationStatus
    :: WalletDelegation -> Property
prop_effectiveDelegationStatus dlg =
    WD.effectiveDelegationStatus dlg === effectiveStatus dlg

-- Mirror of the Agda law AGDA-5350-EMPTY in
-- specifications/Cardano/Wallet/Delegation.agda: an empty 'next' schedule
-- selects 'active'. The oracle is the generated status itself, so the two
-- sides of the equation share no implementation expression.
prop_effectiveDelegationStatusEmpty
    :: WalletDelegationStatus -> Property
prop_effectiveDelegationStatusEmpty status =
    checkCoverage
        $ cover
            20
            (isJust (statusDRep status))
            "effective-status-carries-a-drep"
        $ WD.effectiveDelegationStatus (WalletDelegation status [])
            === status

-- Mirror of the Agda law AGDA-5350-LAST: a non-empty 'next' schedule selects
-- its final status, whatever 'active' and the superseded entries are. The
-- oracle reaches the final entry by reversing the generated list.
prop_effectiveDelegationStatusLast
    :: WalletDelegationStatus
    -> NonEmptyList WalletDelegationNext
    -> Property
prop_effectiveDelegationStatusLast status (NonEmpty scheduled) =
    checkCoverage
        $ cover
            20
            (length scheduled > 1)
            "superseded-entries-present"
        $ WD.effectiveDelegationStatus (WalletDelegation status scheduled)
            === finalStatus
  where
    finalStatus = case reverse scheduled of
        entry : _ -> nextStatus entry
        [] -> status

-- Mirror of the Agda law AGDA-5350-HISTORY: replacing 'active' and every
-- superseded 'next' entry, while keeping the final scheduled entry, cannot
-- change the duplicate-vote verdict. The pre-#5350 @active || any next@
-- decision violates exactly this law; 'historicalVoteRequestFor' is
-- registered above as the witness that it does.
prop_voteDecisionIgnoresHistory
    :: DRep
    -> (WalletDelegationStatus, [WalletDelegationNext])
    -> (WalletDelegationStatus, [WalletDelegationNext])
    -> WalletDelegationNext
    -> Property
prop_voteDecisionIgnoresHistory target one other final =
    checkCoverage
        $ cover
            5
            targetOnlyInHistory
            "target-in-superseded-history-only"
        $ (WD.voteRequestFor target (withFinal one) === expected)
        .&&. (WD.voteRequestFor target (withFinal other) === expected)
  where
    -- Oracle: derived from the final scheduled entry and the target
    -- alone, never from a second call into the implementation.
    expected =
        if statusDRep (nextStatus final) == Just target
            then VotedSameAsBefore
            else VotedDifferently
    withFinal (current, coming) =
        WalletDelegation current (coming ++ [final])
    supersededStatuses (current, coming) =
        current : fmap nextStatus coming
    supersededDReps =
        fmap statusDRep (concatMap supersededStatuses [one, other])
    targetOnlyInHistory =
        Just target `elem` supersededDReps
            && statusDRep (nextStatus final) /= Just target

-- Mirror of the Agda model's explicit 'eq-refl' and 'eq-sound' parameters in
-- specifications/Cardano/Wallet/Delegation.agda. Those are model assumptions,
-- not a fifth AGDA-5350-* law: 'DRep' is abstract in the model, so its
-- equality arrives as a parameter, and this property is what pins that
-- parameter to the real 'Eq DRep' instance (INV-5350-DREP-EQ).
prop_drepEqualityMatchesStructure :: DRep -> DRep -> Property
prop_drepEqualityMatchesStructure left right =
    checkCoverage
        $ cover
            10
            (structurallyEqualDRep left right)
            "structurally-identical"
        $ cover
            20
            (not (sameDRepConstructor left right))
            "different-constructors"
        $ ((left == right) === structurallyEqualDRep left right)
        .&&. property (left == left)
        .&&. property (structurallyEqualDRep left left)

-- The pre-#5350 duplicate-vote decision: reject whenever the target occurs in
-- 'active' or in any scheduled entry. It exists only as the negative control
-- for the mirrors above; production code never uses it.
historicalVoteRequestFor
    :: DRep
    -> WalletDelegation
    -> VoteRequest
historicalVoteRequestFor target dlg =
    if Just target `elem` fmap statusDRep (historyStatuses dlg)
        then VotedSameAsBefore
        else VotedDifferently

nextStatus :: WalletDelegationNext -> WalletDelegationStatus
nextStatus (WalletDelegationNext _ status) = status

-- Structural identity of a 'DRep', expressed by pattern matching instead of
-- by the derived instance under test. Credential bytes are compared as
-- '[Word8]', so no 'Eq DRep', 'Eq DRepID' or 'Eq ByteString' is involved.
structurallyEqualDRep :: DRep -> DRep -> Bool
structurallyEqualDRep Abstain Abstain = True
structurallyEqualDRep NoConfidence NoConfidence = True
structurallyEqualDRep (FromDRepID left) (FromDRepID right) =
    structurallyEqualDRepID left right
structurallyEqualDRep _ _ = False

structurallyEqualDRepID :: DRepID -> DRepID -> Bool
structurallyEqualDRepID (DRepFromKeyHash left) (DRepFromKeyHash right) =
    BS.unpack (getDRepKeyHash left) == BS.unpack (getDRepKeyHash right)
structurallyEqualDRepID (DRepFromScriptHash left) (DRepFromScriptHash right) =
    BS.unpack (getDRepScriptHash left)
        == BS.unpack (getDRepScriptHash right)
structurallyEqualDRepID _ _ = False

sameDRepConstructor :: DRep -> DRep -> Bool
sameDRepConstructor Abstain Abstain = True
sameDRepConstructor NoConfidence NoConfidence = True
sameDRepConstructor (FromDRepID _) (FromDRepID _) = True
sameDRepConstructor _ _ = False

effectiveStatus :: WalletDelegation -> WalletDelegationStatus
effectiveStatus (WalletDelegation current scheduled) =
    case reverse scheduled of
        WalletDelegationNext _ status : _ -> status
        [] -> current

historyStatuses :: WalletDelegation -> [WalletDelegationStatus]
historyStatuses (WalletDelegation current scheduled) =
    current
        : fmap
            (\(WalletDelegationNext _ status) -> status)
            scheduled

statusDRep :: WalletDelegationStatus -> Maybe DRep
statusDRep status = case status of
    Voting drep -> Just drep
    DelegatingVoting _ drep -> Just drep
    _ -> Nothing

{-------------------------------------------------------------------------------
                    Issue #5466: mirrors of the Agda laws
-------------------------------------------------------------------------------}

-- | The shape of 'W.withdrawalPermitted': bootstrap phase, reward account,
-- ledger value and the history's vote.
type WithdrawalPermittedFun =
    Bool -> RewardAccount -> Maybe VoteDelegation -> Maybe DRep -> Bool

-- | The shape of 'WD.ledgerVoteRequestFor': target, ledger value and history.
type LedgerVoteRequestFun =
    DRep -> Maybe VoteDelegation -> WalletDelegation -> VoteRequest

-- Mirror of the Agda law AGDA-5466-WDRL-LEDGER in
-- specifications/Cardano/Wallet/Delegation.agda: after the bootstrap phase, a
-- key-hash reward account whose ledger value has been read may withdraw
-- exactly when the ledger holds a vote delegation, whatever the history
-- records. The law's scope fixes the phase and the credential kind; the
-- oracle reads the constructor of the generated ledger value alone.
prop_withdrawalPermittedFollowsLedger
    :: WithdrawalPermittedFun -> Property
prop_withdrawalPermittedFollowsLedger permitted =
    checkCoverage $ forAll inputs $ \(account, ledger, history) ->
        cover 30 (not (holdsVoteDelegation ledger)) "no vote delegation"
            $ cover 30 (holdsVoteDelegation ledger) "a vote delegation"
            $ cover
                10
                (not (holdsVoteDelegation ledger) && isJust history)
                "the history records a vote that the ledger does not hold"
            $ cover
                10
                (holdsVoteDelegation ledger && isNothing history)
                "the ledger holds a vote that the history does not record"
            $ permitted False account (Just ledger) history
                === holdsVoteDelegation ledger
  where
    inputs =
        (,,)
            <$> genKeyHashAccount
            <*> genReadLedgerVote
            <*> genHistoryVote

-- Mirror of the Agda law AGDA-5466-WDRL-EXEMPT: every account in the
-- bootstrap phase, and a script-hash account in every phase, may withdraw,
-- whatever the ledger value and the history are.
prop_withdrawalPermittedExempt :: WithdrawalPermittedFun -> Property
prop_withdrawalPermittedExempt permitted =
    checkCoverage $ forAll inputs $ \((bootstrap, account), ledger, history) ->
        let keyHash = isKeyHashAccount account
            delegated = maybe False holdsVoteDelegation ledger
        in  cover 20 (bootstrap && keyHash) "bootstrap phase, key-hash account"
                $ cover
                    20
                    (bootstrap && not keyHash)
                    "bootstrap phase, script-hash account"
                $ cover
                    20
                    (not bootstrap && not keyHash)
                    "after the bootstrap phase, script-hash account"
                $ cover 20 (isNothing ledger) "ledger value unread"
                $ cover 20 (ledger == Just VoteNotDelegated) "no vote delegation"
                $ cover 20 delegated "a vote delegation"
                $ cover
                    5
                    (not keyHash && not delegated && isNothing history)
                    "script-hash account, no vote in the ledger or history"
                $ permitted bootstrap account ledger history === True
  where
    inputs =
        (,,)
            <$> genExemptAccount
            <*> genAnyLedgerVote
            <*> genHistoryVote

-- Mirror of the Agda law AGDA-5466-WDRL-UNREAD: after the bootstrap phase, a
-- key-hash reward account whose ledger value is unread may withdraw exactly
-- when the history records a vote. The law's scope fixes the phase, the
-- credential kind and the ledger value.
prop_withdrawalPermittedUnreadFollowsHistory
    :: WithdrawalPermittedFun -> Property
prop_withdrawalPermittedUnreadFollowsHistory permitted =
    checkCoverage $ forAll inputs $ \(account, history) ->
        cover 30 (isJust history) "the history records a vote"
            $ cover 30 (isNothing history) "the history records no vote"
            $ permitted False account Nothing history === isJust history
  where
    inputs = (,) <$> genKeyHashAccount <*> genHistoryVote

-- Mirror of the Agda law AGDA-5466-VOTE-LEDGER: with a read ledger value, a
-- request repeats the vote delegation in force exactly when the ledger holds
-- a delegation to the target, whatever the history records.
prop_ledgerVoteRequestFollowsLedger
    :: LedgerVoteRequestFun -> Property
prop_ledgerVoteRequestFollowsLedger decide =
    checkCoverage $ forAll inputs $ \(target, ledger, dlg) ->
        let inLedger = ledger == VoteDelegatedTo target
            inHistory = statusDRep (effectiveStatus dlg) == Just target
        in  cover 20 (ledger == VoteNotDelegated) "no vote delegation"
                $ cover 20 inLedger "a vote delegation to the target"
                $ cover
                    20
                    (holdsVoteDelegation ledger && not inLedger)
                    "a vote delegation to another DRep"
                $ cover
                    10
                    (inHistory && not inLedger)
                    "the history votes for the target, the ledger does not"
                $ cover
                    10
                    (inLedger && not inHistory)
                    "the ledger votes for the target, the history does not"
                $ decide target (Just ledger) dlg
                    === if inLedger then VotedSameAsBefore else VotedDifferently
  where
    inputs = do
        target <- arbitrary
        (,,) target
            <$> genReadLedgerVoteAround target
            <*> genDelegationAround target

-- Mirror of the Agda law AGDA-5466-VOTE-UNREAD: with the ledger value
-- unread, the decision is the #5350 one, from the effective delegation status
-- alone. The oracle is the independent D1 oracle of the #5350 mirrors.
prop_ledgerVoteRequestUnreadIs5350 :: LedgerVoteRequestFun -> Property
prop_ledgerVoteRequestUnreadIs5350 decide =
    checkCoverage $ forAll inputs $ \(target, dlg) ->
        let effective = statusDRep (effectiveStatus dlg) == Just target
            inHistory =
                Just target `elem` fmap statusDRep (historyStatuses dlg)
        in  cover 20 effective "the effective status votes for the target"
                $ cover
                    20
                    (not effective)
                    "the effective status does not vote for the target"
                $ cover
                    10
                    (inHistory && not effective)
                    "only a superseded status votes for the target"
                $ decide target Nothing dlg
                    === if effective then VotedSameAsBefore else VotedDifferently
  where
    inputs = do
        target <- arbitrary
        (,) target <$> genDelegationAround target

-- The pre-#5466 withdrawal decision: permitted exactly when the wallet's
-- history records a vote. It exists only as the negative control for
-- 'prop_withdrawalPermittedFollowsLedger', which a retired DRep separates
-- from 'W.withdrawalPermitted': the history names it, the ledger holds no
-- vote delegation.
historyOnlyWithdrawalPermitted :: WithdrawalPermittedFun
historyOnlyWithdrawalPermitted _bootstrap _rewardAccount _ledgerVote = isJust

-- 'W.withdrawalPermitted' without its bootstrap and script-hash clauses: the
-- negative control for 'prop_withdrawalPermittedExempt', which a script-hash
-- account with no vote delegation separates from it.
noExemptionWithdrawalPermitted :: WithdrawalPermittedFun
noExemptionWithdrawalPermitted _bootstrap _rewardAccount ledgerVote history
    | Just v <- ledgerVote = v /= VoteNotDelegated
    | otherwise = isJust history

-- 'W.withdrawalPermitted' with an unread clause that permits every
-- withdrawal: the negative control for
-- 'prop_withdrawalPermittedUnreadFollowsHistory', which an unread value with
-- no recorded vote separates from it.
unreadIgnoresHistory :: WithdrawalPermittedFun
unreadIgnoresHistory bootstrap rewardAccount ledgerVote _history
    | bootstrap = True
    | FromScriptHash _ <- rewardAccount = True
    | Just v <- ledgerVote = v /= VoteNotDelegated
    | otherwise = True

-- 'WD.voteRequestFor', the #5350 decision from the history alone, in the
-- shape of 'WD.ledgerVoteRequestFor': the negative control for
-- 'prop_ledgerVoteRequestFollowsLedger', which a retired DRep separates from
-- it when the target is the DRep that the history names.
historyVoteRequestFor :: LedgerVoteRequestFun
historyVoteRequestFor target _ledgerVote = WD.voteRequestFor target

-- 'WD.ledgerVoteRequestFor' with an unread clause that answers
-- 'VotedDifferently': the negative control for
-- 'prop_ledgerVoteRequestUnreadIs5350', which an unread value with the target
-- as the effective DRep separates from it.
unreadAlwaysDifferent :: LedgerVoteRequestFun
unreadAlwaysDifferent target ledgerVote _dlg = case ledgerVote of
    Just (VoteDelegatedTo drep)
        | drep == target -> VotedSameAsBefore
        | otherwise -> VotedDifferently
    Just VoteNotDelegated -> VotedDifferently
    Nothing -> VotedDifferently

-- Whether a ledger value holds a vote delegation, read from its constructor.
holdsVoteDelegation :: VoteDelegation -> Bool
holdsVoteDelegation = \case
    VoteDelegatedTo _ -> True
    VoteNotDelegated -> False

isKeyHashAccount :: RewardAccount -> Bool
isKeyHashAccount = \case
    FromKeyHash _ -> True
    FromScriptHash _ -> False

genKeyHashAccount :: Gen RewardAccount
genKeyHashAccount = FromKeyHash . BS.pack <$> vector 28

genScriptHashAccount :: Gen RewardAccount
genScriptHashAccount = FromScriptHash . BS.pack <$> vector 28

-- The bootstrap phase and reward accounts that the ledger exempts from
-- holding a vote delegation: either credential kind in the bootstrap phase,
-- and a script-hash account after it.
genExemptAccount :: Gen (Bool, RewardAccount)
genExemptAccount =
    oneof
        [ (,) True <$> genKeyHashAccount
        , (,) True <$> genScriptHashAccount
        , (,) False <$> genScriptHashAccount
        ]

-- A ledger value that is unread, holds no vote delegation, or holds one.
genAnyLedgerVote :: Gen (Maybe VoteDelegation)
genAnyLedgerVote = oneof [pure Nothing, Just <$> genReadLedgerVote]

-- A read ledger value: no vote delegation, or a delegation to any DRep.
genReadLedgerVote :: Gen VoteDelegation
genReadLedgerVote =
    oneof [pure VoteNotDelegated, VoteDelegatedTo <$> arbitrary]

-- A read ledger value: no vote delegation, a delegation to the target, or a
-- delegation to another DRep.
genReadLedgerVoteAround :: DRep -> Gen VoteDelegation
genReadLedgerVoteAround target =
    oneof
        [ pure VoteNotDelegated
        , pure (VoteDelegatedTo target)
        , VoteDelegatedTo <$> arbitrary `suchThat` (/= target)
        ]

-- The DRep of the latest vote in a wallet's delegation history, if any.
genHistoryVote :: Gen (Maybe DRep)
genHistoryVote = oneof [pure Nothing, Just <$> arbitrary]

-- A delegation history whose effective status votes for the target, one in
-- which only a superseded status may vote for it, or an arbitrary one.
genDelegationAround :: DRep -> Gen WalletDelegation
genDelegationAround target =
    oneof
        [ arbitrary
        , effective <$> arbitrary <*> arbitrary
        , superseded <$> arbitrary <*> arbitrary
        ]
  where
    effective current epoch =
        WalletDelegation current [WalletDelegationNext epoch (Voting target)]
    superseded epoch coming =
        WalletDelegation (Voting target) [WalletDelegationNext epoch coming]

{-------------------------------------------------------------------------------
                    Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary PoolId where
    arbitrary = PoolId <$> elements metasyntactic

instance Arbitrary WalletDelegation where
    shrink = genericShrink
    arbitrary =
        WalletDelegation
            <$> arbitrary
            <*> oneof [pure [], vector 1, vector 2]

instance Arbitrary WalletDelegationStatus where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary WalletDelegationNext where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary Withdrawal where
    arbitrary =
        oneof
            [ WithdrawalSelf <$> arbitrary <*> arbitrary <*> arbitrary
            , applyArbitrary4 WithdrawalExternal
            , pure NoWithdrawal
            ]

instance Arbitrary XPrv where
    arbitrary = fromJust . xprvFromBytes . BS.pack <$> vectorOf 96 arbitrary

instance Show XPrv where
    show = show . xprvToBytes

instance Eq XPrv where
    (==) = (==) `on` xprvToBytes

deriving instance Show Withdrawal

instance Arbitrary RewardAccount where
    arbitrary = FromKeyHash . BS.pack <$> vector 28

instance Arbitrary W.PoolRetirementEpochInfo where
    arbitrary = W.PoolRetirementEpochInfo <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = EpochNo <$> arbitrary

instance Arbitrary DerivationIndex where
    arbitrary = DerivationIndex <$> arbitrary

instance Arbitrary Coin where
    shrink _ = []
    arbitrary = genCoinPositive

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary a => Arbitrary (NonEmpty a) where
    arbitrary = (:|) <$> arbitrary <*> arbitrary
    shrink = genericShrink

instance Arbitrary DRepID where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        oneof
            [ pure $ DRepFromKeyHash $ DRepKeyHash $ BS.pack $ take 28 bytes
            , pure $ DRepFromScriptHash $ DRepScriptHash $ BS.pack $ take 28 bytes
            ]

instance Arbitrary DRep where
    arbitrary =
        oneof [pure Abstain, pure NoConfidence, FromDRepID <$> arbitrary]
