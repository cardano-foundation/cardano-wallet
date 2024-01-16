{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Cardano.Wallet.Address.Derivation
    ( DerivationIndex (..)
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
    , DRepKeyHash (..)
    , DRepScriptHash (..)
    , VoteAction (..)
    )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..)
    )
import Cardano.Wallet.Transaction
    ( Withdrawal (..)
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

spec :: Spec
spec = describe "Cardano.Wallet.DelegationSpec" $ do

    describe "Join/Quit Stake pool properties" $ do
        it "You can quit if you cannot join" $ do
            property prop_guardJoinQuit
        it "You can join if you cannot quit" $ do
            property prop_guardQuitJoin

    describe "Join/Quit Stake pool unit mockEventSource" $ do
        it "Cannot join A, when active = A" $ do
            let dlg = WalletDelegation {active = Delegating pidA, next = []}
            WD.guardJoin knownPools dlg pidA noRetirementPlanned
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Cannot join A, when next = [A]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation {active = NotDelegating, next = [next1]}
            WD.guardJoin knownPools dlg pidA noRetirementPlanned
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Can join A, when active = A, next = [B]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1]}
            WD.guardJoin knownPools dlg pidA noRetirementPlanned
                `shouldBe` Right ()
        it "Cannot join A, when active = A, next = [B, A]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let next2 = WalletDelegationNext (EpochNo 2) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            WD.guardJoin knownPools dlg pidA noRetirementPlanned
                `shouldBe` Left (W.ErrAlreadyDelegating pidA)
        it "Cannot join when pool is unknown" $ do
            let dlg = WalletDelegation {active = NotDelegating, next = []}
            WD.guardJoin knownPools dlg pidUnknown noRetirementPlanned
                `shouldBe` Left (W.ErrNoSuchPool pidUnknown)
        it "Cannot quit when active: not_delegating, next = []" $ do
            let dlg = WalletDelegation {active = NotDelegating, next = []}
            WD.guardQuit dlg NoWithdrawal (Coin 0)
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [not_delegating]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) NotDelegating
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1]}
            WD.guardQuit dlg NoWithdrawal (Coin 0)
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Cannot quit when active: A, next = [B, not_delegating]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidB)
            let next2 = WalletDelegationNext (EpochNo 2) NotDelegating
            let dlg = WalletDelegation
                    {active = Delegating pidA, next = [next1, next2]}
            WD.guardQuit dlg NoWithdrawal (Coin 0)
                `shouldBe` Left (W.ErrNotDelegatingOrAboutTo)
        it "Can quit when active: not_delegating, next = [A]" $ do
            let next1 = WalletDelegationNext (EpochNo 1) (Delegating pidA)
            let dlg = WalletDelegation
                    {active = NotDelegating, next = [next1]}
            WD.guardQuit dlg NoWithdrawal (Coin 0) `shouldBe` Right ()
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
    :: [PoolId]
    -> WalletDelegation
    -> PoolId
    -> Withdrawal
    -> Maybe W.PoolRetirementEpochInfo
    -> Property
prop_guardJoinQuit knownPoolsList dlg pid wdrl mRetirementInfo = checkCoverage
    $ cover 10 retirementNotPlanned
        "retirementNotPlanned"
    $ cover 10 retirementPlanned
        "retirementPlanned"
    $ cover 10 alreadyRetired
        "alreadyRetired"
    $ case WD.guardJoin knownPools dlg pid mRetirementInfo of
        Right () ->
            label "I can join" $ property $
                alreadyRetired `shouldBe` False
        Left W.ErrNoSuchPool{} ->
            label "ErrNoSuchPool" $ property True
        Left W.ErrAlreadyDelegating{} ->
            label "ErrAlreadyDelegating"
                (WD.guardQuit dlg wdrl (Coin 0) === Right ())
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
    :: NonEmptyList PoolId
    -> WalletDelegation
    -> Word64
    -> Withdrawal
    -> Property
prop_guardQuitJoin (NonEmpty knownPoolsList) dlg rewards wdrl =
    let knownPools = Set.fromList knownPoolsList in
    let noRetirementPlanned = Nothing in
    case WD.guardQuit dlg wdrl (Coin.fromWord64 rewards) of
        Right () ->
            label "I can quit" $ property True
        Left W.ErrNotDelegatingOrAboutTo ->
            label "ErrNotDelegatingOrAboutTo" $
                WD.guardJoin
                    knownPools dlg (last knownPoolsList) noRetirementPlanned
                    === Right ()
        Left W.ErrNonNullRewards{} ->
            label "ErrNonNullRewards" $
                property (rewards /= 0)
                    .&&. not (isSelfWdrl wdrl)
  where
    isSelfWdrl WithdrawalSelf{} = True
    isSelfWdrl _                = False

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

instance Arbitrary DRep where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        oneof [ pure $ DRepFromKeyHash $ DRepKeyHash $ BS.pack $ take 28 bytes
              , pure $ DRepFromScriptHash $ DRepScriptHash $ BS.pack $ take 28 bytes
              ]

instance Arbitrary VoteAction where
    arbitrary =
        oneof [pure Abstain, pure NoConfidence, VoteTo <$> arbitrary]

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
