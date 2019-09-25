{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.Arbitrary
    ( GenTxHistory (..)
    , KeyValPairs (..)
    , GenState
    , MockChain (..)
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( unXPrv )
import Cardano.Wallet.DB
    ( PrimaryKey (..) )
import Cardano.Wallet.DB.Model
    ( TxHistory, filterTxHistory )
import Cardano.Wallet.DB.Sqlite
    ( PersistTx (..) )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, genesisParameters )
import Cardano.Wallet.DummyTarget.Primitive.Types as DummyTarget
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , KeyToAddress (..)
    , Passphrase (..)
    , WalletKey (..)
    , XPrv
    , XPub
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( ChangeChain (..)
    , SeqKey (..)
    , deriveAddressPublicKey
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPool
    , AddressPoolGap (..)
    , SeqState (..)
    , accountPubKey
    , changeChain
    , gap
    , mkAddressPool
    , mkAddressPoolGap
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip, getState, unsafeInitWallet, utxo )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Direction (..)
    , EpochLength (..)
    , Hash (..)
    , ShowFmt (..)
    , SlotId (..)
    , SlotParameters (..)
    , SortOrder (..)
    , TxIn (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , UTxO (..)
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , flatSlot
    , isPending
    , slotSucc
    , wholeRange
    )
import Control.DeepSeq
    ( NFData )
import Crypto.Hash
    ( hash )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.Coerce
    ( coerce )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.List
    ( unfoldr )
import Data.Quantity
    ( Percentage, Quantity (..), mkPercentage )
import Data.Text.Class
    ( toText )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32 )
import Fmt
    ( Buildable (..), Builder, blockListF', prefixF, suffixF, tupleF )
import GHC.Generics
    ( Generic )
import Numeric.Natural
    ( Natural )
import System.IO.Unsafe
    ( unsafePerformIO )
import System.Random
    ( mkStdGen )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , NonEmptyList (..)
    , Positive (..)
    , arbitraryBoundedEnum
    , choose
    , elements
    , generate
    , genericShrink
    , liftArbitrary
    , oneof
    , scale
    , shrinkList
    , suchThat
    , vectorOf
    )
import Test.Utils.Time
    ( genUniformTime )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Sequential as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
                    Cross DB Specs Shared Arbitrary Instances
-------------------------------------------------------------------------------}

type GenState s = (NFData s, Show s, IsOurs s, Arbitrary s, Buildable s)

newtype KeyValPairs k v = KeyValPairs [(k, v)]
    deriving (Generic, Show, Eq)

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

instance (Arbitrary k, Arbitrary v) => Arbitrary (KeyValPairs k v) where
    shrink = genericShrink
    arbitrary = do
        pairs <- choose (1, 10) >>= flip vectorOf arbitrary
        pure $ KeyValPairs pairs

instance Arbitrary (PrimaryKey WalletId) where
    shrink _ = []
    arbitrary = do
        bytes <- B8.pack . pure <$> elements ['a'..'k']
        return $ PrimaryKey $ WalletId $ hash bytes

instance GenState s => Arbitrary (Wallet s DummyTarget) where
    shrink w =
        [ unsafeInitWallet u (currentTip w) s genesisParameters
        | (u, s) <- shrink (utxo w, getState w) ]
    arbitrary = unsafeInitWallet
        <$> arbitrary
        <*> pure (BlockHeader (SlotId 0 0) (Quantity 0) (Hash "hash"))
        <*> arbitrary
        <*> pure genesisParameters

instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        , pure $ Address "ADDR04"
        , pure $ Address "ADDR05"
        , pure $ Address "ADDR06"
        , pure $ Address "ADDR07"
        , pure $ Address "ADDR08"
        , pure $ Address "ADDR09"
        , pure $ Address "ADDR10"
        ]

instance Arbitrary (SeqState DummyTarget) where
    shrink (SeqState intPool extPool ixs) =
        (\(i, e, x) -> SeqState i e x) <$> shrink (intPool, extPool, ixs)
    arbitrary = do
        SeqState <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (Seq.PendingIxs) where
    shrink =
        map Seq.pendingIxsFromList . shrink . Seq.pendingIxsToList
    arbitrary =
        Seq.pendingIxsFromList . Set.toList <$> arbitrary

instance Typeable chain => Arbitrary (AddressPool DummyTarget chain) where
    shrink pool =
        let
            key = accountPubKey pool
            g = gap pool
            addrs = Seq.addresses pool
        in case length addrs of
            k | k == fromEnum g && g == minBound ->
                []
            k | k == fromEnum g && g > minBound ->
                [ mkAddressPool key minBound [] ]
            k ->
                [ mkAddressPool key minBound []
                , mkAddressPool key g []
                , mkAddressPool key g (take (k - (fromEnum g `div` 5)) addrs)
                ]
    arbitrary = do
        g <- unsafeMkAddressPoolGap <$> choose
            (getAddressPoolGap minBound, 2 * getAddressPoolGap minBound)
        n <- choose (0, 2 * fromEnum g)
        let addrs = take n (ourAddresses (changeChain @chain))
        return $ mkAddressPool ourAccount g addrs

instance Arbitrary (Index 'Soft 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

unsafeMkAddressPoolGap :: (Integral a, Show a) => a -> AddressPoolGap
unsafeMkAddressPoolGap g = case (mkAddressPoolGap $ fromIntegral g) of
    Right a -> a
    Left _ -> error $ "unsafeMkAddressPoolGap: bad argument: " <> show g

ourAccount
    :: SeqKey 'AccountK XPub
ourAccount = publicKey $ unsafeGenerateKeyFromSeed (seed, mempty) mempty
  where
    seed = Passphrase $ BA.convert $ BS.replicate 32 0

ourAddresses
    :: ChangeChain
    -> [Address]
ourAddresses cc =
    keyToAddress @DummyTarget . deriveAddressPublicKey ourAccount cc
        <$> [minBound..maxBound]

instance Arbitrary (RndState DummyTarget) where
    shrink (RndState k ix addrs pending g) =
        [ RndState k ix' addrs' pending' g
        | (ix', addrs', pending') <- shrink (ix, addrs, pending) ]
    arbitrary = RndState
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> (mkStdGen <$> arbitrary)

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance PersistTx DummyTarget where
    resolvedInputs = flip zip (repeat Nothing) . DummyTarget.inputs
    mkTx _ inps = DummyTarget.Tx (fst <$> inps)

instance Arbitrary DummyTarget.Tx where
    shrink (DummyTarget.Tx ins outs) =
        [DummyTarget.Tx ins' outs | ins' <- shrinkList' ins ] ++
        [DummyTarget.Tx ins outs' | outs' <- shrinkList' outs ]
      where
        shrinkList' xs  = filter (not . null)
            [ take n xs | Positive n <- shrink (Positive $ length xs) ]

    arbitrary = Tx
        <$> fmap (L.nub . L.take 5 . getNonEmpty) arbitrary
        <*> fmap (L.take 5 . getNonEmpty) arbitrary

instance Arbitrary TxMeta where
    shrink _ = []
    arbitrary = TxMeta
        <$> arbitrary
        <*> elements [Incoming, Outgoing]
        <*> arbitrary
        <*> fmap (Quantity . fromIntegral) (arbitrary @Word32)

instance Arbitrary TxStatus where
    arbitrary =
        elements [Pending, InLedger, Invalidated]

instance Arbitrary SlotId where
    shrink (SlotId ep sl) =
        uncurry SlotId <$> shrink (ep, sl)
    arbitrary = SlotId
        <$> choose (0, 100)
        <*> choose (0, ep)
      where
        EpochLength ep = genesisParameters ^. #getEpochLength

customizedGen :: Gen Percentage
customizedGen = do
    let (Right upperBound) = mkPercentage @Int 100
    arbitraryBoundedEnum `suchThat` (/= upperBound)

instance Arbitrary WalletMetadata where
    shrink _ = []
    arbitrary =  WalletMetadata
        <$> (WalletName <$> elements ["bulbazaur", "charmander", "squirtle"])
        <*> genUniformTime
        <*> (fmap WalletPassphraseInfo <$> liftArbitrary genUniformTime)
        <*> oneof [pure Ready, Restoring . Quantity <$> customizedGen]
        <*> pure NotDelegating

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (1, 100000)

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> (Hash . B8.pack <$> vectorOf 32 arbitrary)
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

newtype GenTxHistory = GenTxHistory { unGenTxHistory :: TxHistory DummyTarget }
    deriving stock (Show, Eq)
    deriving newtype (Semigroup, Monoid)

instance Arbitrary GenTxHistory where
    shrink (GenTxHistory h) = map GenTxHistory (shrinkList shrinkOneTx h)
      where
        shrinkOneTx
            :: (DummyTarget.Tx, TxMeta)
            -> [(DummyTarget.Tx, TxMeta)]
        shrinkOneTx (tx, meta) =
            [(tx', meta) | tx' <- shrink tx]

    -- Ensure unique transaction IDs within a given batch of transactions to add
    -- to the history.
    arbitrary = GenTxHistory . sortTxHistory <$> do
        -- NOTE
        -- We discard pending transaction from any 'GenTxHistory since,
        -- inserting a pending transaction actually has an effect on the
        -- checkpoint's pending transactions of the same wallet.
        filter (not . isPending . snd) <$> arbitrary
      where
        sortTxHistory = filterTxHistory @DummyTarget Descending wholeRange

instance Arbitrary UTxO where
    shrink (UTxO u) = UTxO <$> shrink u
    arbitrary = do
        n <- choose (1, 100)
        u <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList u

instance Arbitrary (SeqKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = elements rootKeysSeq

instance Arbitrary (RndKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = elements rootKeysRnd

instance Arbitrary (Hash "encryption") where
    shrink _ = []
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        return $ Hash $ BS.pack $ take 32 bytes

genRootKeysSeq :: Gen (SeqKey 'RootK XPrv)
genRootKeysSeq = do
    (s, g, e) <- (,,)
        <$> genPassphrase @"seed" (16, 32)
        <*> genPassphrase @"generation" (0, 16)
        <*> genPassphrase @"encryption" (0, 16)
    return $ Seq.generateKeyFromSeed (s, g) e

genRootKeysRnd :: Gen (RndKey 'RootK XPrv)
genRootKeysRnd = Rnd.generateKeyFromSeed
    <$> genPassphrase @"seed" (16, 32)
    <*> genPassphrase @"encryption" (0, 16)

genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
genPassphrase range = do
    n <- choose range
    InfiniteList bytes _ <- arbitrary
    return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

-- Properties above are quite heavy on the generation of values, althrough for
-- private keys, it isn't particularly useful / relevant to generate many of
-- them as they're really treated as an opaque type.
-- Instead, we generate them once, and picks from the list.
rootKeysSeq :: [SeqKey 'RootK XPrv]
rootKeysSeq = unsafePerformIO $ generate (vectorOf 10 genRootKeysSeq)
{-# NOINLINE rootKeysSeq #-}

rootKeysRnd :: [RndKey 'RootK XPrv]
rootKeysRnd = unsafePerformIO $ generate (vectorOf 10 genRootKeysRnd)
{-# NOINLINE rootKeysRnd #-}

newtype MockChain = MockChain
    { getMockChain :: [Block DummyTarget.Tx] }
    deriving stock (Eq, Show)
    deriving newtype (Buildable)

instance Arbitrary MockChain where
    shrink (MockChain chain) =
        [ MockChain chain'
        | chain' <- shrinkList shrinkBlock chain
        , not (null chain')
        ]
      where
        shrinkBlock (Block h txs) = Block h <$> shrinkList shrink txs
    arbitrary = do
        n0 <- choose (1, 10)
        slot0 <- arbitrary
        height0 <- fromIntegral <$> choose (0, flatSlot epochLength slot0)
        blocks <- sequence $ flip unfoldr (slot0, height0, n0) $ \(slot, height, n) ->
            if n <= (0 :: Int)
                then Nothing
                else Just
                    ( genBlock slot height
                    , (slotSucc sp slot, height + 1, n - 1)
                    )
        return (MockChain blocks)
      where
        mockHeaderHash :: SlotId -> Hash "BlockHeader"
        mockHeaderHash = Hash . convertToBase Base16 . B8.pack . show

        genBlock :: SlotId -> Natural -> Gen (Block DummyTarget.Tx)
        genBlock slot height = do
            let h = BlockHeader slot (Quantity height) (mockHeaderHash slot)
            Block h <$> (choose (1, 10) >>= \k -> vectorOf k arbitrary)

        epochLength :: EpochLength
        epochLength = genesisParameters ^. #getEpochLength

        sp :: SlotParameters
        sp = SlotParameters
            epochLength
            (genesisParameters ^. #getSlotLength)
            (genesisParameters ^. #getGenesisBlockDate)

{-------------------------------------------------------------------------------
                     Missing Instances for QC Tests
-------------------------------------------------------------------------------}

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = const "XPrv"

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

deriving instance Buildable a => Buildable (Identity a)

instance Buildable GenTxHistory where
    build (GenTxHistory txs) = blockListF' "-" tupleF txs

instance Buildable (SeqKey depth XPrv, Hash "encryption") where
    build (_, h) = tupleF (xprvF, prefixF 8 hF <> "..." <> suffixF 8 hF)
      where
        xprvF = "XPrv" :: Builder
        hF = build (toText (coerce @_ @(Hash "BlockHeader") h))

instance Buildable (PrimaryKey WalletId) where
    build (PrimaryKey wid) = build wid
