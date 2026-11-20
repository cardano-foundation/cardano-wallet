{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Network.LocalStateQuery.RewardAccountSpec
    ( spec
    ) where

import Cardano.Wallet.Network.Implementation.Ouroboros
    ( LSQ (..)
    )
import Cardano.Wallet.Network.LocalStateQuery.RewardAccount
    ( RewardAccountState (..)
    , fetchRewardAccountStates
    , toRewardAccountStates
    )
import Cardano.Wallet.Primitive.Ledger.Convert
    ( toLedgerCoin
    , toLedgerDRep
    )
import Cardano.Wallet.Primitive.Ledger.Shelley
    ( toLedgerStakeCredential
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Coin.Gen
    ( genCoin
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
import Control.Monad
    ( forM_
    )
import Data.ByteString
    ( ByteString
    )
import Data.Map
    ( Map
    )
import Data.Maybe
    ( fromMaybe
    , isNothing
    )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Gen
    , Property
    , arbitrary
    , checkCoverage
    , cover
    , forAll
    , liftArbitrary
    , listOf
    , oneof
    , property
    , vectorOf
    , (===)
    )
import Prelude

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "toRewardAccountStates" $ do
        it
            "gives every requested account the reward balance and the vote \
            \delegation of the same read"
            $ property prop_statesFromOneRead
        it "gives every requested account no vote delegation before Conway"
            $ property prop_statesBeforeConway
        describe "converts the vote delegation of a requested account"
            $ forM_ voteDelegationExamples
            $ \(name, drep, expected) ->
                it name $ voteDelegationOf drep `shouldBe` Just (Just expected)
    describe "fetchRewardAccountStates"
        $ it "sends no query for an empty set of accounts"
        $ case fetchRewardAccountStates Set.empty of
            LSQPure states -> states `shouldBe` Map.empty
            _ -> expectationFailure "the query program sends a query"

-- | Requested reward accounts, each with the reward balance and the vote
-- delegation that the ledger holds for it. 'Nothing' stands for an account
-- that is absent from the ledger's answer.
type Requests = Map RewardAccount (Maybe Coin, Maybe DRep)

-- | Whether the ledger answers vote delegation queries.
data Era = BeforeConway | FromConway

prop_statesFromOneRead :: Property
prop_statesFromOneRead =
    forAll genRequests $ \requests ->
        checkCoverage
            $ cover 10 (hasVote isNothing requests) "no vote delegation"
            $ cover 10 (hasVote (== Just Abstain) requests) "Abstain"
            $ cover 10 (hasVote (== Just NoConfidence) requests) "NoConfidence"
            $ cover 10 (hasVote isDRepID requests) "a DRep credential"
            $ cover 10 (any (isNothing . fst) requests) "no reward balance"
            $ readStates FromConway requests
                === Map.map (expectedState FromConway) requests
  where
    hasVote p = any (p . snd)
    isDRepID = \case
        Just (FromDRepID _) -> True
        _ -> False

prop_statesBeforeConway :: Property
prop_statesBeforeConway =
    forAll genRequests $ \requests ->
        readStates BeforeConway requests
            === Map.map (expectedState BeforeConway) requests

-- | Convert the ledger's answers to the given requests.
readStates :: Era -> Requests -> Map RewardAccount RewardAccountState
readStates era requests =
    toRewardAccountStates (Map.keysSet requests) balances delegations
  where
    balances =
        Map.fromList
            [ (toLedgerStakeCredential account, toLedgerCoin balance)
            | (account, (Just balance, _)) <- Map.toList requests
            ]
    delegations = case era of
        BeforeConway -> Nothing
        FromConway ->
            Just
                $ Map.fromList
                    [ (toLedgerStakeCredential account, toLedgerDRep drep)
                    | (account, (_, Just drep)) <- Map.toList requests
                    ]

expectedState :: Era -> (Maybe Coin, Maybe DRep) -> RewardAccountState
expectedState era (balance, drep) =
    RewardAccountState
        { rewardBalance = fromMaybe (Coin 0) balance
        , voteDelegation = case era of
            BeforeConway -> Nothing
            FromConway -> Just $ maybe VoteNotDelegated VoteDelegatedTo drep
        }

voteDelegationOf :: Maybe DRep -> Maybe (Maybe VoteDelegation)
voteDelegationOf drep =
    voteDelegation
        <$> Map.lookup account (readStates FromConway requests)
  where
    account = FromKeyHash (BS.replicate 28 1)
    requests = Map.singleton account (Nothing, drep)

voteDelegationExamples :: [(String, Maybe DRep, VoteDelegation)]
voteDelegationExamples =
    [ ("absent from the answer", Nothing, VoteNotDelegated)
    , ("Abstain", Just Abstain, VoteDelegatedTo Abstain)
    , ("NoConfidence", Just NoConfidence, VoteDelegatedTo NoConfidence)
    , ("a key-hash DRep", Just keyHashDRep, VoteDelegatedTo keyHashDRep)
    ,
        ( "a script-hash DRep"
        , Just scriptHashDRep
        , VoteDelegatedTo scriptHashDRep
        )
    ]
  where
    keyHashDRep =
        FromDRepID $ DRepFromKeyHash $ DRepKeyHash $ BS.replicate 28 2
    scriptHashDRep =
        FromDRepID $ DRepFromScriptHash $ DRepScriptHash $ BS.replicate 28 3

genRequests :: Gen Requests
genRequests =
    Map.fromList
        <$> listOf
            ( (,)
                <$> genRewardAccount
                <*> ((,) <$> liftArbitrary genCoin <*> liftArbitrary genDRep)
            )

genRewardAccount :: Gen RewardAccount
genRewardAccount = oneof [FromKeyHash <$> genHash, FromScriptHash <$> genHash]

genDRep :: Gen DRep
genDRep =
    oneof
        [ pure Abstain
        , pure NoConfidence
        , FromDRepID . DRepFromKeyHash . DRepKeyHash <$> genHash
        , FromDRepID . DRepFromScriptHash . DRepScriptHash <$> genHash
        ]

genHash :: Gen ByteString
genHash = BS.pack <$> vectorOf 28 arbitrary
