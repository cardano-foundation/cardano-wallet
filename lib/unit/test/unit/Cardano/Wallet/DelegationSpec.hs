{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    , InfiniteList (..)
    , NonEmptyList (..)
    , Property
    , applyArbitrary4
    , arbitrarySizedBoundedIntegral
    , checkCoverage
    , cover
    , elements
    , label
    , oneof
    , property
    , shrinkIntegral
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
  where
    pidA = PoolId "A"
    pidB = PoolId "B"
    pidUnknown = PoolId "unknown"
    knownPools = Set.fromList [pidA, pidB]
    noRetirementPlanned = Nothing
    drepA =
        FromDRepID
            (DRepFromKeyHash (DRepKeyHash (BS.replicate 28 1)))
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
    vote = WD.joinDRepVotingAction Write.RecentEraConway

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
-- duplicate-vote verdict consumed by the IO path must agree with the
-- verdict that the transaction-building path acts on.
prop_joinDRepParityWithVoteRequest
    :: DRep -> WalletDelegation -> Bool -> Property
prop_joinDRepParityWithVoteRequest target dlg registered =
    case (WD.voteRequestFor target dlg, action) of
        (VotedSameAsBefore, Left (W.ErrAlreadyVoted drep)) ->
            drep === target
        (VotedDifferently, Right _) -> property True
        _ -> property False
  where
    action =
        WD.joinDRepVotingAction
            Write.RecentEraConway
            target
            dlg
            registered

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

instance Arbitrary EpochNo => Arbitrary WalletDelegationNext where
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
