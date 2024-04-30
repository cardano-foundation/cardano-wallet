{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.DelegationSpec
    ( spec
    ) where

import Prelude

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

import qualified Cardano.Wallet as W
import qualified Cardano.Wallet.Delegation as WD
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Internal.Cardano.Write.Tx as Write

spec :: Spec
spec = describe "Cardano.Wallet.DelegationSpec" $ do

    describe "Join/Quit Stake pool properties" $ do
        it "You can quit if you cannot join Babbage" $ do
            property (prop_guardJoinQuit guardJoinBabbage)
        it "You can quit if you cannot join Conway" $ do
            property (prop_guardJoinQuit guardJoinConway)
        it "You can join if you cannot quit Babbage" $ do
            property (prop_guardQuitJoin guardJoinBabbage)
        it "You can join if you cannot quit Conway" $ do
            property (prop_guardQuitJoin guardJoinConway)

    describe "Join/Quit Stake pool unit mockEventSource" $ do
        it "Cannot join A, when active = A in Babbage" $ do
            let dlg = WalletDelegation {active = Delegating pidA, next = []}
            WD.guardJoin Write.RecentEraBabbage knownPools dlg pidA noRetirementPlanned NotVotedYet
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Can rejoin A, when active = A in Conway" $ do
            let dlg = WalletDelegation {active = Delegating pidA, next = []}
            WD.guardJoin Write.RecentEraConway knownPools dlg pidA noRetirementPlanned VotedDifferently
                `shouldBe` Right ()
        it "Cannot rejoin A, when active = A in Conway" $ do
            let dlg = WalletDelegation {active = Delegating pidA, next = []}
            WD.guardJoin Write.RecentEraConway knownPools dlg pidA noRetirementPlanned VotedSameAsBefore
                `shouldBe` Left (W.ErrAlreadyDelegatingVoting pidA)
        it "Cannot join A, when next = [A] in Babbage" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation {active = NotDelegating, next = [next1]}
            WD.guardJoin Write.RecentEraBabbage knownPools dlg pidA noRetirementPlanned NotVotedYet
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Can join A, when next = [A] in Conway" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation {active = NotDelegating, next = [next1]}
            WD.guardJoin Write.RecentEraConway knownPools dlg pidA noRetirementPlanned VotedDifferently
                `shouldBe` Right ()
        it "Can join A, when next = [A] in Conway" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation {active = NotDelegating, next = [next1]}
            WD.guardJoin Write.RecentEraConway knownPools dlg pidA noRetirementPlanned VotedSameAsBefore
                `shouldBe` Left (W.ErrAlreadyDelegatingVoting pidA)
        it "Can join A, when active = A, next = [B] in any era" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1]}
            WD.guardJoin Write.RecentEraBabbage knownPools dlg pidA noRetirementPlanned NotVotedYet
                `shouldBe` Right ()
            WD.guardJoin Write.RecentEraConway knownPools dlg pidA noRetirementPlanned VotedDifferently
                `shouldBe` Right ()
            WD.guardJoin Write.RecentEraConway knownPools dlg pidA noRetirementPlanned VotedSameAsBefore
                `shouldBe` Right ()
        it "Cannot join A, when active = A, next = [B, A] in Babbage" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let next2 = WalletDelegationNext (EpochNo 2) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            WD.guardJoin Write.RecentEraBabbage knownPools dlg pidA noRetirementPlanned NotVotedYet
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Can join A, when active = A, next = [B, A] in Conway" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let next2 = WalletDelegationNext (EpochNo 2) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            WD.guardJoin Write.RecentEraConway knownPools dlg pidA noRetirementPlanned VotedDifferently
                `shouldBe` Right ()
        it "Cannot join A, when active = A, next = [B, A] in Conway" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let next2 = WalletDelegationNext (EpochNo 2) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            WD.guardJoin Write.RecentEraConway knownPools dlg pidA noRetirementPlanned VotedSameAsBefore
                `shouldBe` Left (W.ErrAlreadyDelegatingVoting pidA)
        it "Cannot join when pool is unknown in any era" $ do
            let dlg = WalletDelegation {active = NotDelegating, next = []}
            WD.guardJoin Write.RecentEraBabbage knownPools dlg pidUnknown noRetirementPlanned NotVotedYet
                `shouldBe` Left (W.ErrNoSuchPool pidUnknown)
            WD.guardJoin Write.RecentEraConway knownPools dlg pidUnknown noRetirementPlanned VotedDifferently
                `shouldBe` Left (W.ErrNoSuchPool pidUnknown)
            WD.guardJoin Write.RecentEraConway knownPools dlg pidUnknown noRetirementPlanned VotedSameAsBefore
                `shouldBe` Left (W.ErrNoSuchPool pidUnknown)
        it "Cannot quit when active: not_delegating, next = []" $ do
            let dlg = WalletDelegation {active = NotDelegating, next = []}
            WD.guardQuit dlg NoWithdrawal (Coin 0) False
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [not_delegating]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) NotDelegating
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1]}
            WD.guardQuit dlg NoWithdrawal (Coin 0) False
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [B, not_delegating]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let next2 = WalletDelegationNext (EpochNo 2) NotDelegating
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            WD.guardQuit dlg NoWithdrawal (Coin 0) False
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Can quit when active: not_delegating, next = [A]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = NotDelegating, next = [next1]}
            WD.guardQuit dlg NoWithdrawal (Coin 0) False `shouldBe` Right ()
  where
    pidA = PoolId "A"
    pidB = PoolId "B"
    pidUnknown = PoolId "unknown"
    knownPools = Set.fromList [pidA, pidB]
    noRetirementPlanned = Nothing

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
    $ cover 10 retirementNotPlanned
        "retirementNotPlanned"
    $ cover 10 retirementPlanned
        "retirementPlanned"
    $ cover 10 alreadyRetired
        "alreadyRetired"
    $ case guardJoin knownPools dlg pid mRetirementInfo NotVotedYet of
        Right () ->
            label "I can join" $ property $
                alreadyRetired `shouldBe` False
        Left W.ErrNoSuchPool{} ->
            label "ErrNoSuchPool" $ property True
        Left W.ErrAlreadyDelegating{} ->
            label "ErrAlreadyDelegating"
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
    let knownPools = Set.fromList knownPoolsList in
    let noRetirementPlanned = Nothing in
    case WD.guardQuit dlg wdrl (Coin.fromWord64 rewards) False of
        Right () ->
            label "I can quit" $ property True
        Left W.ErrNotDelegatingOrAboutTo ->
            label "ErrNotDelegatingOrAboutTo" $
                guardJoin knownPools dlg (last knownPoolsList) noRetirementPlanned NotVotedYet
                    === Right ()
        Left W.ErrNonNullRewards{} ->
            label "ErrNonNullRewards" $
                property (rewards /= 0)
                    .&&. not (isSelfWdrl wdrl)
  where
    isSelfWdrl WithdrawalSelf{} = True
    isSelfWdrl _                = False

type GuardJoinFun = Set PoolId -> WalletDelegation -> PoolId -> Maybe PoolRetirementEpochInfo -> VoteRequest -> Either ErrCannotJoin ()

guardJoinBabbage :: GuardJoinFun
guardJoinBabbage = WD.guardJoin Write.RecentEraBabbage

guardJoinConway :: GuardJoinFun
guardJoinConway = WD.guardJoin Write.RecentEraConway

{-------------------------------------------------------------------------------
                    Arbitrary instances
-------------------------------------------------------------------------------}

instance Arbitrary PoolId where
    arbitrary = PoolId <$> elements metasyntactic

instance Arbitrary WalletDelegation where
    shrink = genericShrink
    arbitrary = WalletDelegation
        <$> arbitrary
        <*> oneof [ pure [], vector 1, vector 2 ]

instance Arbitrary WalletDelegationStatus where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary EpochNo => Arbitrary WalletDelegationNext where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary Withdrawal where
    arbitrary = oneof
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
        oneof [ pure $ DRepFromKeyHash $ DRepKeyHash $ BS.pack $ take 28 bytes
              , pure $ DRepFromScriptHash $ DRepScriptHash $ BS.pack $ take 28 bytes
              ]

instance Arbitrary DRep where
    arbitrary =
        oneof [pure Abstain, pure NoConfidence, FromDRepID <$> arbitrary]
