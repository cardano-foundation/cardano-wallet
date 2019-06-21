{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.TypesSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DummyTarget.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase (..), digest, generateKeyFromSeed, publicKey )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Direction (..)
    , Dom (..)
    , Hash (..)
    , SlotId (..)
    , TxIn (..)
    , TxMeta (TxMeta)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , WalletId (..)
    , WalletName (..)
    , balance
    , excluding
    , flatSlot
    , fromFlatSlot
    , isSubsetOf
    , isValidCoin
    , restrictedBy
    , restrictedTo
    , slotRatio
    , walletNameMaxLength
    , walletNameMinLength
    )
import Control.DeepSeq
    ( deepseq )
import Control.Monad
    ( replicateM )
import Crypto.Hash
    ( hash )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Set
    ( Set, (\\) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..), fromText )
import Fmt
    ( pretty )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryPrintableChar
    , checkCoverage
    , choose
    , cover
    , oneof
    , property
    , scale
    , vectorOf
    , (===)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Generators are valid" $ do
        it "Arbitrary Coin" $ property isValidCoin

    describe "Can perform roundtrip textual encoding & decoding" $ do
        textRoundtrip $ Proxy @AddressState
        textRoundtrip $ Proxy @WalletName
        textRoundtrip $ Proxy @WalletId

    describe "Buildable" $ do
        it "WalletId" $ do
            let seed = Passphrase (BA.convert @ByteString "0000000000000000")
            let xprv = generateKeyFromSeed (seed, mempty) mempty
            let wid = WalletId $ digest $ publicKey xprv
            "336c96f1...b8cac9ce" === pretty @_ @Text wid
        it "TxMeta (1)" $ do
            let txMeta = TxMeta Pending Outgoing (SlotId 14 42) (Quantity 1337)
            "-0.001337 pending since 14.42" === pretty @_ @Text txMeta
        it "TxMeta (2)" $ do
            let txMeta =
                    TxMeta InLedger Incoming (SlotId 14 0) (Quantity 13371442)
            "+13.371442 in ledger since 14.0" === pretty @_ @Text txMeta
        it "TxMeta (3)" $ do
            let txMeta = TxMeta Invalidated Incoming (SlotId 0 42) (Quantity 0)
            "+0.000000 invalidated since 0.42" === pretty @_ @Text txMeta

    describe "slotRatio" $ do
        it "works for any two slots" $ property $ \sl0 sl1 ->
            slotRatio sl0 sl1 `deepseq` ()
    describe "flatSlot" $ do
        it "flatSlot . fromFlatSlot == id" $ property $ \sl ->
            fromFlatSlot (flatSlot sl) === sl
        it "fromFlatSlot . flatSlot == id" $ property $ \n ->
            flatSlot (fromFlatSlot n) === n

    describe "Negative cases for types decoding" $ do
        it "fail fromText @AddressState \"unusedused\"" $ do
            let err = "Unable to decode the given value: \"unusedused\".\
                    \ Please specify one of the following values: used, unused."
            fromText @AddressState "unusedused" === Left (TextDecodingError err)
        it "fail fromText @WalletName \"\"" $ do
            let err = "name is too short: expected at least "
                      <> show walletNameMinLength <> " character"
            fromText @WalletName "" === Left (TextDecodingError err)
        it "fail fromText @WalletName > walletNameMaxLength" $ do
            let err = "name is too long: expected at most "
                      <> show walletNameMaxLength <> " characters"
            let walName = T.pack (replicate (walletNameMaxLength + 1) 'x')
            fromText @WalletName walName === Left (TextDecodingError err)
        it "fail fromText @WalletId \"101\"" $ do
            let err = "wallet id should be an hex-encoded string \
                      \of 40 characters"
            fromText @WalletId "101" === Left (TextDecodingError err)

    describe "Lemma 2.1 - Properties of UTxO operations" $ do
        it "2.1.1) ins⊲ u ⊆ u"
            (checkCoverage prop_2_1_1)
        it "2.1.2) ins⋪ u ⊆ u"
            (checkCoverage prop_2_1_2)
        it "2.1.3) u ⊳ outs ⊆ u"
            (checkCoverage prop_2_1_3)
        it "2.1.4) ins⊲ (u ⋃ v) = (ins⊲ u) ⋃ (ins⊲ v)"
            (checkCoverage prop_2_1_4)
        it "2.1.5) ins⋪ (u ⋃ v) = (ins⋪ u) ⋃ (ins⋪ v)"
            (checkCoverage prop_2_1_5)
        it "2.1.6) (dom u ⋂ ins) ⊲ u = ins⊲ u"
            (checkCoverage prop_2_1_6)
        it "2.1.7) (dom u ⋂ ins) ⋪ u = ins⋪ u"
            (checkCoverage prop_2_1_7)
        it "2.1.8) (dom u ⋃ ins) ⋪ (u ⋃ v) = (ins ⋃ dom u) ⋪ v"
            (checkCoverage prop_2_1_8)
        it "2.1.9) ins⋪ u = (dom u \\ ins)⊲ u"
            (checkCoverage prop_2_1_9)

    describe "Lemma 2.6 - Properties of balance" $ do
        it "2.6.1) dom u ⋂ dom v ==> balance (u ⋃ v) = balance u + balance v"
            (checkCoverage prop_2_6_1)
        it "2.6.2) balance (ins⋪ u) = balance u - balance (ins⊲ u)"
            (checkCoverage prop_2_6_2)

    describe "Slotting ordering" $ do
        it "Any Slot >= SlotId 0 0"
            (property (>= SlotId 0 0))
        it "SlotId 1 2 < SlotId 2 1"
            (property $ SlotId { epochNumber = 1, slotNumber = 2 } < SlotId 2 1)
        it "SlotId 1 1 < SlotId 1 2"
            (property $ SlotId { epochNumber = 1, slotNumber = 1 } < SlotId 1 2)
        it "SlotId 1 2 < SlotId 2 2"
            (property $ SlotId { epochNumber = 1, slotNumber = 2 } < SlotId 2 2)

{-------------------------------------------------------------------------------
       Wallet Specification - Lemma 2.1 - Properties of UTxO operations
-------------------------------------------------------------------------------}

prop_2_1_1 :: (Set TxIn, UTxO) -> Property
prop_2_1_1 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `restrictedBy` ins) `isSubsetOf` u

prop_2_1_2 :: (Set TxIn, UTxO) -> Property
prop_2_1_2 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `excluding` ins) `isSubsetOf` u

prop_2_1_3 :: (Set TxOut, UTxO) -> Property
prop_2_1_3 (outs, u) =
    cover 50 cond "u ⋂ outs ≠ ∅" (property prop)
  where
    cond = not $ Set.null $
        Set.fromList (Map.elems (getUTxO u)) `Set.intersection` outs
    prop = (u `restrictedTo` outs) `isSubsetOf` u

prop_2_1_4 :: (Set TxIn, UTxO, UTxO) -> Property
prop_2_1_4 (ins, u, v) =
    cover 50 cond "(dom u ⋃ dom v) ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ Set.union (dom u) (dom v) `Set.intersection` ins
    prop =
        ((u <> v) `restrictedBy` ins)
            ===
        (u `restrictedBy` ins) <> (v `restrictedBy` ins)

prop_2_1_5 :: (Set TxIn, UTxO, UTxO) -> Property
prop_2_1_5 (ins, u, v) =
    cover 50 cond "(dom u ⋃ dom v) ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ Set.union (dom u) (dom v) `Set.intersection` ins
    prop =
        ((u <> v) `excluding` ins)
            ===
        (u `excluding` ins) <> (v `excluding` ins)

prop_2_1_6 :: (Set TxIn, UTxO) -> Property
prop_2_1_6 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        (u `restrictedBy` (dom u `Set.intersection` ins))
            ===
        (u `restrictedBy` ins)

prop_2_1_7 :: (Set TxIn, UTxO) -> Property
prop_2_1_7 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        (u `excluding` (dom u `Set.intersection` ins))
            ===
        (u `excluding` ins)

prop_2_1_8 :: (Set TxIn, UTxO, UTxO) -> Property
prop_2_1_8 (ins, u, v) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        ((u <> v) `excluding` (dom u <> ins))
            ===
        v `excluding` (ins <> dom u)

prop_2_1_9 :: (Set TxIn, UTxO) -> Property
prop_2_1_9 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop = (u `excluding` ins) === u `restrictedBy` (dom u \\ ins)


{-------------------------------------------------------------------------------
       Wallet Specification - Lemma 2.6 - Properties of Balance
-------------------------------------------------------------------------------}

prop_2_6_1 :: (UTxO, UTxO) -> Property
prop_2_6_1 (u, v) =
    cover 50 cond "u ≠ ∅ , v ≠ ∅" (property prop)
  where
    -- NOTE:
    -- A precondition (u ⋂ v = ∅ ) is hard to satisfy because our generators
    -- are built in order to not be 'too entropic'. So, we better just create
    -- a v' that has no overlap with u.
    v' = v `excluding` dom u
    cond = not (u `isSubsetOf` mempty || v' `isSubsetOf` mempty)
    prop = balance (u <> v') === balance u + balance v'

prop_2_6_2 :: (Set TxIn, UTxO) -> Property
prop_2_6_2 (ins, u) =
    cover 50 cond "dom u ⋂ ins ≠ ∅" (property prop)
  where
    cond = not $ Set.null $ dom u `Set.intersection` ins
    prop =
        balance (u `excluding` ins)
            ===
        balance u - balance (u `restrictedBy` ins)

{-------------------------------------------------------------------------------
                            Arbitrary Instances

    Arbitrary instances define here aren't necessarily reflecting on real-life
    scenario, but they help test the property above by constructing data
    structures that don't have much entropy and therefore, allow us to even test
    something when checking for intersections and set restrictions!
-------------------------------------------------------------------------------}

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Hash "TXID01"
        , pure $ Hash "TXID02"
        , pure $ Hash "TXID03"
        ]

-- Same for addresses
instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        ]

instance Arbitrary AddressState where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (0, 3)

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a crazy high indexes

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (0, 10)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo

instance Arbitrary Tx where
    shrink (Tx ins outs) =
        (flip Tx outs <$> shrink ins) <> (Tx ins <$> shrink outs)
    arbitrary = do
        ins <- choose (1, 3) >>= flip vectorOf arbitrary
        outs <- choose (1, 3) >>= flip vectorOf arbitrary
        return $ Tx ins outs

instance Arbitrary BlockHeader where
    -- No Shrinking
    arbitrary = BlockHeader
        <$> arbitrary
        <*> oneof
            [ pure $ Hash "BLOCK01"
            , pure $ Hash "BLOCK02"
            , pure $ Hash "BLOCK03"
            ]

instance Arbitrary SlotId where
    shrink _ = []
    arbitrary = do
        ep <- choose (0, 10)
        sl <- choose (0, 100)
        return (SlotId ep sl)

instance Arbitrary (Block Tx) where
    shrink (Block h txs) = Block h <$> shrink txs
    arbitrary = do
        txs <- choose (0, 500) >>= flip vectorOf arbitrary
        Block <$> arbitrary <*> pure txs

instance Arbitrary WalletId where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> replicateM 16 arbitrary
        return $ WalletId (hash bytes)

instance Arbitrary WalletName where
    arbitrary = do
        nameLength <- choose (walletNameMinLength, walletNameMaxLength)
        WalletName . T.pack <$> replicateM nameLength arbitraryPrintableChar
    shrink (WalletName t)
        | T.length t <= walletNameMinLength = []
        | otherwise = [WalletName $ T.take walletNameMinLength t]
