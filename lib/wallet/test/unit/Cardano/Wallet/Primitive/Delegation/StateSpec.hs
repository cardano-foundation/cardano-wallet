{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Wallet.Primitive.Delegation.StateSpec where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    )
import Cardano.Wallet.Address.Derivation
    ( Depth (..)
    , DerivationType (..)
    , HardDerivation (..)
    , KeyFingerprint (..)
    , MkKeyFingerprint (..)
    , PaymentAddress (..)
    , RewardAccount (..)
    , SoftDerivation (..)
    , ToRewardAccount (..)
    )
import Cardano.Wallet.Primitive.Delegation.State
    ( Cert (..)
    , DelegationState (..)
    , Key0Status (..)
    , PointerUTxO (..)
    , State (..)
    , Tx (..)
    , activeKeys
    , applyTx
    , initialDelegationState
    , keyAtIx
    , presentableKeys
    , setPortfolioOf
    , usableKeys
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..)
    )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..)
    )
import Cardano.Wallet.Read.NetworkId
    ( SNetworkId (..)
    )
import Control.Arrow
    ( first
    )
import Crypto.Hash.Utils
    ( blake2b224
    )
import Data.Map
    ( Map
    )
import Data.Set
    ( Set
    )
import Fmt
    ( Buildable (..)
    , Builder
    , blockListF
    , blockListF'
    , blockMapF
    , fmt
    , listF
    , listF'
    , mapF'
    , pretty
    )
import GHC.Generics
    ( Generic
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (..)
    , Property
    , conjoin
    , counterexample
    , forAllShow
    , frequency
    , genericShrink
    , property
    , sublistOf
    , withMaxSuccess
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary
    )

import qualified Cardano.Wallet.Primitive.Types.TokenBundle as TB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = do
    describe "Goldens" $ do
        let acc = toRewardAccount @StakeKey' . toEnum
        let regAndDeleg i =
                applyTx
                    ( Tx
                        [ RegisterKey $ acc i
                        , Delegate $ acc i
                        ]
                        []
                        []
                    )
                    (error "hash not needed")

        let s0 = initialDelegationState accK
        describe "initialDelegationState" $ do
            it "presentableKeys is [0]" $ do
                (presentableKeys s0) `shouldBe` [toEnum 0]
            it "usableKeys is [0]" $ do
                usableKeys s0 `shouldBe` [toEnum 0]

        let s1 = regAndDeleg 0 s0
        describe "registering and delegating key 0" $ do
            it "presentableKeys == [0, 1]" $ do
                (presentableKeys s1) `shouldBe` [toEnum 0, toEnum 1]
            it "usableKeys is still [0]" $ do
                usableKeys s1 `shouldBe` [toEnum 0]

        let s2 = regAndDeleg 1 s1
        describe "registering and delegating key 1" $ do
            it "presentableKeys == [0, 1, 2]" $ do
                (presentableKeys s2) `shouldBe` [toEnum 0, toEnum 1, toEnum 2]
            it "usableKeys == [0, 1]" $ do
                usableKeys s2 `shouldBe` [toEnum 0, toEnum 1]

        let s3 = regAndDeleg 5 s2
        describe
            "Impossible gaps in stake keys (shouldn't happen unless\
            \ someone manually constructs txs to mess with the on-chain state)"
            $ do
                it "presentableKeys == [0, 1, 2] (doesn't find 5)" $ do
                    (presentableKeys s3)
                        `shouldBe` [toEnum 0, toEnum 1, toEnum 2]
                it "usableKeys == [0, 1]" $ do
                    usableKeys s3 `shouldBe` [toEnum 0, toEnum 1]

    it "(usableKeys s) are consecutive"
        $ property
        $ prop_keysConsecutive usableKeys

    it "(presentableKeys s) are consecutive"
        $ property
        $ prop_keysConsecutive presentableKeys

    it "(activeKeys s) are consecutive"
        $ property
        $ prop_keysConsecutive activeKeys

    it "pointer is only created and destroyed in specific cases"
        $ property
            prop_pointerRules

    it "adversaries can't affect usableKeys"
        $ property
            prop_usableKeysAdversaries

    it "adversaries can't affect pointer ix"
        $ property
            prop_pointerAdversaries

    it "(apply (cmds <> CmdSetPortfolioOf 0) s0) === s0"
        $ property
            prop_canAlwaysGoTo0

    it "no rejected txs, normally"
        $ property
            prop_noRejectedTxs

    -- Lots of weird things can happen when we consider concurrent user-actions
    -- on multiple wallet versions and rollbacks.
    --
    -- Whatever happens, we should be able to recover using a single
    -- @CmdSetPortfolioOf n@, and be consistent with the ledger.
    it "can recover from dropped transactions"
        $ withMaxSuccess
            2000
            prop_rollbacks

--
-- Properties
--

prop_keysConsecutive
    :: (DelegationState StakeKey' -> [StakeKey]) -> [Cmd] -> Property
prop_keysConsecutive f cmds = do
    let env = applyCmds env0 cmds
    let keys = map fromEnum $ f $ wallet env
    counterexample (pretty $ ledger env)
        $ isConsecutiveRange keys

-- | PointerUTxO should only be created/destroyed in very specific cases.
prop_pointerRules :: [Cmd] -> Property
prop_pointerRules cmds = do
    let env = applyCmds env0 cmds
    forAllTxs env $ \case
        Tx [] [] [_o] -> True -- Not the actual pointer
        Tx cs [] [_o] -> Delegate (acct 1) `elem` cs
        Tx cs [_i] [] -> DeRegisterKey (acct 1) `elem` cs
        Tx cs [_i] [_o] -> any ((> 1) . ixFromCert) cs
        Tx cs [] [] -> all ((< 1) . ixFromCert) $ filter notReg cs
        _ -> False
  where
    ixFromCert :: Cert -> Int
    ixFromCert (DeRegisterKey (FromKeyHash a)) = read . B8.unpack $ a
    ixFromCert (DeRegisterKey (FromScriptHash a)) = read . B8.unpack $ a
    ixFromCert (RegisterKey (FromKeyHash a)) = read . B8.unpack $ a
    ixFromCert (RegisterKey (FromScriptHash a)) = read . B8.unpack $ a
    ixFromCert (Delegate (FromKeyHash a)) = read . B8.unpack $ a
    ixFromCert (Delegate (FromScriptHash a)) = read . B8.unpack $ a

    -- For filtering away registrations from CmdAdversarialReg
    notReg :: Cert -> Bool
    notReg (RegisterKey _) = False
    notReg _ = True

    acct = toRewardAccount . StakeKey'

    forAllTxs e prop = conjoin . flip map (txs e)
        $ \tx -> counterexample (pretty tx) $ prop tx

-- | Tests that `isAdversarial` commands have no effect on the usableKeys of the
-- state.
prop_usableKeysAdversaries :: [Cmd] -> Property
prop_usableKeysAdversaries cmds = do
    counterexample "\nstate /= state without adversarial cmds" $ do
        let usableKeys' = usableKeys . wallet . applyCmds env0
        usableKeys' cmds
            === usableKeys' (filter (not . isAdversarial) cmds)

-- | Tests that `isAdversarial` commands have no effect on the `PointerUTxO`.
prop_pointerAdversaries :: NonNegative Int -> [RewardAccount] -> Property
prop_pointerAdversaries (NonNegative n) keys = do
    let env = applyCmds env0 [CmdSetPortfolioOf n]
    let env' = applyCmds env (map CmdMimicPointerOutput keys)
    state (wallet env') === state (wallet env)

-- | CmdSetPortfolioOf 0 returns us to the initial state.
prop_canAlwaysGoTo0 :: [Cmd] -> Property
prop_canAlwaysGoTo0 cmds = do
    let env = applyCmds env0 (cmds ++ [CmdSetPortfolioOf 0])
    counterexample (pretty env)
        $ wallet env === wallet env0

prop_noRejectedTxs :: [Cmd] -> Property
prop_noRejectedTxs cmds = do
    let env = applyCmds env0 cmds
    counterexample (pretty env)
        $ rejectedTxs env === []

-- | Rollbacks don't get us into an inconsistent or irrecoverable state.
prop_rollbacks :: NonNegative Int -> [Cmd] -> Property
prop_rollbacks (NonNegative n) cmds = do
    forAllSubchains (applyCmds env0 cmds) $ \env' -> do
        let env = applyCmds env' [CmdSetPortfolioOf n]

        let isSubsetOf a b =
                counterexample (show a <> " ⊄ " <> show b)
                    $ a `Set.isSubsetOf` b

        let allActiveKeysRegistered e =
                Set.map toRewardAccount (Set.fromList (activeKeys $ wallet e))
                    `isSubsetOf` regs (ledger e)

        counterexample (pretty env)
            $ length (activeKeys $ wallet env) === n
            .&&. allActiveKeysRegistered env

--
-- Helpers
--

-- | Take an arbitrary subset of the txs of an @Env@ to generate a new @Env@.
--
-- NOTE: Can drop txs, but not reorder them.
forAllSubchains :: Env -> (Env -> Property) -> Property
forAllSubchains env prop = do
    forAllShow (sublistOf (reverse $ txs env)) (fmt . blockListF) $ \subchain -> do
        counterexample
            ("Txs before dropping some:\n" <> (fmt . blockListF $ reverse $ txs env))
            $ prop
            $ applyTxs env0 subchain

accK :: StakeKey' 'AccountK XPub
accK = StakeKey' 0

isConsecutiveRange :: (Eq a, Num a) => [a] -> Bool
isConsecutiveRange [_] = True
isConsecutiveRange [] = True
isConsecutiveRange (a : b : t)
    | b == a + 1 = isConsecutiveRange (b : t)
    | otherwise = False

--
-- Mock Stake Keys
--

-- | Mock key type for testing.
--
-- FIXME: We should do /some/ testing with @ShelleyKey@ though.
newtype StakeKey' (depth :: Depth) key = StakeKey' Word
    deriving newtype (Eq, Enum, Ord, Show, Bounded)

type StakeKey = StakeKey' 'CredFromKeyK XPub

instance ToRewardAccount StakeKey' where
    toRewardAccount (StakeKey' i) = FromKeyHash . B8.pack $ show i
    someRewardAccount = error "someRewardAccount: not implemented"

instance HardDerivation StakeKey' where
    type AddressIndexDerivationType StakeKey' = 'Soft
    type AddressCredential StakeKey' = 'CredFromKeyK

    deriveAccountPrivateKey _ _ _ =
        error "deriveAccountPrivateKey: not implemented"
    deriveAddressPrivateKey _ _ _ _ =
        error "deriveAddressPrivateKey not implemented"

instance SoftDerivation StakeKey' where
    deriveAddressPublicKey _acc _role i = StakeKey' $ toEnum $ fromEnum i

instance MkKeyFingerprint StakeKey' Address where
    paymentKeyFingerprint (Address addr) = Right $ KeyFingerprint $ B8.drop 4 addr

instance PaymentAddress StakeKey' 'CredFromKeyK where
    liftPaymentAddress _ (KeyFingerprint fp) = Address fp
    paymentAddress _ k =
        let FromKeyHash bs = toRewardAccount k
        in  Address $ "addr" <> bs

instance MkKeyFingerprint StakeKey' (StakeKey' 'CredFromKeyK XPub) where
    paymentKeyFingerprint k =
        let FromKeyHash bs = toRewardAccount k
        in  Right $ KeyFingerprint bs

--
-- Mock chain of delegation certificates
--

instance Arbitrary RewardAccount where
    arbitrary = toRewardAccount @StakeKey' <$> arbitrary

instance Arbitrary Cert where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (StakeKey' depth key) where
    arbitrary = StakeKey' <$> arbitrary

--
-- Mock chain data
--

data Cmd
    = -- | Calls @setPortfolioOf@ which registers or de-registers keys to reach
      -- the new target.
      --
      -- If the target is already met, the command has no effect.
      --
      -- Delegation certificates are /not/ generated for existing keys.
      --
      -- TODO: Also test arbitrary re-delegations.
      CmdSetPortfolioOf Int
    | -- | A wallet implementation without multi-stake-key support could decide
      -- to either
      -- 1. register stake-key 0 without adding a pointer UTxO
      -- 2. de-register stake-key 0 despite despite e.g. key 1 being active
      -- depending on whether it is registered or not.
      --
      -- Having a "toggle"-command instead of two separate commands, makes
      -- generating valid arbitrary values easier.
      CmdOldWalletToggleFirstKey
    | -- | Someone could pay 2 ada to (re-)register your stake key. Your wallet
      -- shouldn't be affected negatively from it.
      CmdAdversarialReg RewardAccount
    | -- | Someone could send funds to the same UTxO
      CmdMimicPointerOutput RewardAccount
    deriving (Generic, Eq)

isAdversarial :: Cmd -> Bool
isAdversarial (CmdSetPortfolioOf _) = False
isAdversarial (CmdAdversarialReg _) = True
isAdversarial CmdOldWalletToggleFirstKey = False
isAdversarial (CmdMimicPointerOutput _) = True

instance Show Cmd where
    show (CmdSetPortfolioOf n) = "CmdSetPortfolioOf " <> show n
    show (CmdAdversarialReg (FromKeyHash a)) = "CmdAdversarialReg " <> B8.unpack a
    show (CmdAdversarialReg (FromScriptHash a)) = "CmdAdversarialReg " <> B8.unpack a
    show CmdOldWalletToggleFirstKey = "CmdOldWalletToggleFirstKey"
    show (CmdMimicPointerOutput (FromKeyHash a)) = "CmdMimicPointerOutput " <> B8.unpack a
    show (CmdMimicPointerOutput (FromScriptHash a)) = "CmdMimicPointerOutput " <> B8.unpack a

instance Buildable Tx where
    build (Tx cs [] []) = "Tx " <> listF cs
    build (Tx cs ins outs) =
        "Tx " <> listF cs <> " " <> listF' (inF . fst) ins <> " -> " <> listF' outF outs

inF :: TxIn -> Builder
inF (TxIn h ix) = build h <> "." <> build (fromEnum ix)

outF :: TxOut -> Builder
outF (TxOut (Address addr) _tb) = build $ B8.unpack addr

instance Buildable (DelegationState StakeKey') where
    build ds = case state ds of
        Zero -> "Zero"
        One -> "One"
        More ix p is0Reg ->
            blockMapF
                [ ("nextKeyIx" :: String, build $ fromEnum ix)
                , ("pointer", build p)
                , ("key0", key0F is0Reg)
                ]
      where
        key0F MissingKey0 = "missing"
        key0F ValidKey0 = "there"

instance Buildable PointerUTxO where
    build (PointerUTxO i _c) = "PointerUTxO " <> inF i

instance Buildable Env where
    build env =
        "Env:\n"
            <> blockMapF
                [ ("wallet" :: String, build $ wallet env)
                , ("txs", blockListF' "-" txF $ reverse $ txs env)
                , ("rejected txs", blockListF' "-" build $ reverse $ rejectedTxs env)
                , ("ledger", build $ ledger env)
                ]
      where
        txF tx = build (txid tx) <> ": " <> build tx

rewardAccountF :: RewardAccount -> Builder
rewardAccountF (FromKeyHash a) = build (B8.unpack a)
rewardAccountF (FromScriptHash a) = build (B8.unpack a)

instance Buildable Cert where
    build (RegisterKey k) = "RegKey " <> rewardAccountF k
    build (Delegate k) = "Deleg " <> rewardAccountF k
    build (DeRegisterKey k) = "DeReg " <> rewardAccountF k

instance Arbitrary Cmd where
    -- We don't want to generate too many adversarial registrations (we don't
    -- expect them to happen in reality), but at least some, and enough to cause
    -- consistent failures if something is wrong.
    arbitrary =
        frequency
            [ (80, CmdSetPortfolioOf . getNonNegative <$> arbitrary)
            , (5, CmdAdversarialReg <$> arbitrary)
            , (10, pure CmdOldWalletToggleFirstKey)
            , (5, CmdMimicPointerOutput <$> arbitrary)
            ]
    shrink = genericShrink

data Env = Env
    { wallet :: DelegationState StakeKey'
    , ledger :: Ledger
    , txs :: [Tx]
    -- ^ All accepted transactions so far in the chain. Newest first.
    , rejectedTxs :: [String]
    -- ^ User-generated txs that were rejected. I.e. not adversarial ones.
    -- Newest first.
    }
    deriving (Show, Eq)

env0 :: Env
env0 = Env (initialDelegationState accK) initialLedger [] []

stepCmd :: Cmd -> Env -> Env
stepCmd (CmdSetPortfolioOf n) env =
    case setPortfolioOf (wallet env) minUTxOVal mkAddr (acctIsReg (ledger env)) n of
        Just tx -> tryApplyTx tx env
        Nothing -> env
  where
    getKeyHash k =
        let bs' = case toRewardAccount k of
                FromKeyHash bs -> bs
                FromScriptHash bs -> bs
        in  bs'
    mkAddr k = Address $ "addr" <> getKeyHash k
    minUTxOVal = Coin 1
stepCmd (CmdAdversarialReg k) env
    | k `Set.member` regs (ledger env) =
        env -- We don't want tx errors to appear in @rejectedTxs@.
    | otherwise =
        tryApplyTx (Tx [RegisterKey k] [] []) env
stepCmd CmdOldWalletToggleFirstKey env =
    let
        key0 = toRewardAccount (keyAtIx (wallet env) minBound)
        isReg = key0 `Set.member` (regs (ledger env))
        tx = Tx [if isReg then DeRegisterKey key0 else RegisterKey key0] [] []
    in
        tryApplyTx tx env
stepCmd (CmdMimicPointerOutput (FromKeyHash acc)) env =
    let
        addr =
            liftPaymentAddress @StakeKey' @'CredFromKeyK SMainnet
                $ KeyFingerprint acc
        c = Coin 1
        out = TxOut addr (TB.fromCoin c)
        tx = Tx [] [] [out]
    in
        tryApplyTx tx env
stepCmd (CmdMimicPointerOutput (FromScriptHash acc)) env =
    let
        addr =
            liftPaymentAddress @StakeKey' @'CredFromKeyK SMainnet
                $ KeyFingerprint acc
        c = Coin 1
        out = TxOut addr (TB.fromCoin c)
        tx = Tx [] [] [out]
    in
        tryApplyTx tx env

tryApplyTx :: Tx -> Env -> Env
tryApplyTx tx env = case ledgerApplyTx tx h (ledger env) of
    Right l' ->
        env
            { ledger = l'
            , wallet = applyTx tx (txid tx) $ wallet env
            , txs = tx : txs env
            }
    Left e -> env{rejectedTxs = e : rejectedTxs env}
  where
    h = txid tx

applyCmds :: Env -> [Cmd] -> Env
applyCmds = foldl (flip stepCmd)

applyTxs :: Env -> [Tx] -> Env
applyTxs = foldl (flip tryApplyTx)

txid :: Tx -> Hash "Tx"
txid = Hash . BS.take 4 . blake2b224 . B8.pack . show

--
-- Mock ledger
--

data Ledger = Ledger
    { regs :: Set RewardAccount
    , utxo :: Map TxIn TxOut
    }
    deriving (Show, Eq)

instance Buildable Ledger where
    build l =
        blockMapF
            [ ("regs" :: String, listF' rewardAccountF (regs l))
            , ("utxo", mapF' inF outF (utxo l))
            ]

initialLedger :: Ledger
initialLedger = Ledger Set.empty Map.empty

acctIsReg :: Ledger -> RewardAccount -> Bool
acctIsReg l a = a `Set.member` (regs l)

ledgerApplyTx :: Tx -> (Hash "Tx") -> Ledger -> Either String Ledger
ledgerApplyTx tx h l' =
    (foldl (\x y -> x >>= ledgerApplyCert y) (Right l') (certs tx))
        -- Can be commented out to see the effects of no pointer UTxO on
        -- the tests:
        >>= ledgerApplyInsOus
  where
    ledgerApplyInsOus :: Ledger -> Either String Ledger
    ledgerApplyInsOus (Ledger r u) =
        let
            -- TODO: There could be duplicates, which we should forbid
            ins = Set.fromList $ map fst $ inputs tx
            newOuts =
                Map.fromList
                    $ zipWith
                        (curry $ first (TxIn h))
                        [0 ..]
                        (outputs tx)

            canSpend = ins `Set.isSubsetOf` Map.keysSet u
        in
            if canSpend
                then Right $ Ledger r $ Map.union newOuts $ u `Map.withoutKeys` ins
                else Left $ "invalid utxo spending in tx: " <> pretty tx

    ledgerApplyCert :: Cert -> Ledger -> Either String Ledger
    ledgerApplyCert (Delegate k) l
        | k `Set.member` (regs l) =
            pure l
        | otherwise =
            Left $ "Can't delegate: " <> show k <> " not in " <> show l
    ledgerApplyCert (RegisterKey k) l
        | k `Set.member` (regs l) =
            Left $ "Can't register: " <> show k <> " already in: " <> show l
        | otherwise =
            pure $ l{regs = Set.insert k (regs l)}
    ledgerApplyCert (DeRegisterKey k) l
        | k `Set.member` (regs l) =
            pure $ l{regs = Set.delete k (regs l)}
        | otherwise =
            Left $ "Can't deregister: " <> show k <> " not in " <> show l
