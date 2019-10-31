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
    , InitialCheckpoint (..)
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( unXPrv )
import Cardano.Wallet.DB
    ( PrimaryKey (..) )
import Cardano.Wallet.DB.Model
    ( TxHistory, filterTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget, genesisParameters )
import Cardano.Wallet.DummyTarget.Primitive.Types as DummyTarget
    ( Tx (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , WalletKey (..)
    , XPrv
    , XPub
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..), addrSingleSize, unsafeGenerateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPool, SeqState (..), mkAddressPool )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , blockchainParameters
    , currentTip
    , getState
    , unsafeInitWallet
    , utxo
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , Direction (..)
    , EpochLength (..)
    , EpochNo (..)
    , Hash (..)
    , ShowFmt (..)
    , SlotId (..)
    , SlotNo (..)
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
    , flatSlot
    , isPending
    , slotSucc
    , wholeRange
    )
import Control.Arrow
    ( second )
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
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.List
    ( unfoldr )
import Data.Quantity
    ( Quantity (..) )
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
    , scale
    , shrinkList
    , vectorOf
    )
import Test.Utils.Time
    ( genUniformTime )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                                 Modifiers
-------------------------------------------------------------------------------}

type GenState s = (NFData s, Show s, IsOurs s, Arbitrary s, Buildable s)

newtype KeyValPairs k v = KeyValPairs [(k, v)]
    deriving (Generic, Show, Eq)

newtype GenTxHistory = GenTxHistory { unGenTxHistory :: TxHistory DummyTarget }
    deriving stock (Show, Eq)
    deriving newtype (Semigroup, Monoid)

newtype MockChain = MockChain
    { getMockChain :: [Block DummyTarget.Tx] }
    deriving stock (Eq, Show)

-- | Generate arbitrary checkpoints, but that always have their tip at 0 0.
newtype InitialCheckpoint s =
    InitialCheckpoint { getInitialCheckpoint :: Wallet s DummyTarget }
    deriving newtype (Show, Eq, Buildable, NFData)

instance (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (KeyValPairs k v) where
    shrink = genericShrink
    arbitrary = do
        pairs <- choose (1, 10) >>= flip vectorOf arbitrary
        pure $ KeyValPairs $ L.sortOn fst pairs

-- | For checkpoints, we make sure to generate them in order.
instance {-# OVERLAPS #-} (Arbitrary k, Ord k, GenState s)
    => Arbitrary (KeyValPairs k (ShowFmt (Wallet s DummyTarget))) where
    shrink = genericShrink
    arbitrary = do
        pairs <- choose (1, 10) >>= flip vectorOf arbitrary
        pure $ KeyValPairs $ second ShowFmt
           <$> L.sortOn (\(k,cp) -> (k, view #slotId (currentTip cp))) pairs

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

        genBlock :: SlotId -> Word32 -> Gen (Block DummyTarget.Tx)
        genBlock slot height = do
            let h = BlockHeader slot (Quantity height) (mockHeaderHash slot) (mockHeaderHash slot)
            Block h <$> (choose (1, 10) >>= \k -> vectorOf k arbitrary)

        epochLength :: EpochLength
        epochLength = genesisParameters ^. #getEpochLength

        sp :: SlotParameters
        sp = SlotParameters
            epochLength
            (genesisParameters ^. #getSlotLength)
            (genesisParameters ^. #getGenesisBlockDate)

instance GenState s => Arbitrary (InitialCheckpoint s) where
    shrink (InitialCheckpoint cp) = InitialCheckpoint <$> shrink cp
    arbitrary = do
        cp <- arbitrary @(Wallet s DummyTarget)
        let tip0 = BlockHeader (SlotId 0 0) (Quantity 0) (Hash "block0") (Hash "genesis")
        pure $ InitialCheckpoint $ unsafeInitWallet
            (utxo cp)
            tip0
            (getState cp)
            (blockchainParameters cp)

{-------------------------------------------------------------------------------
                                   Wallets
-------------------------------------------------------------------------------}

instance GenState s => Arbitrary (Wallet s DummyTarget) where
    shrink w =
        [ unsafeInitWallet u (currentTip w) s (blockchainParameters w)
        | (u, s) <- shrink (utxo w, getState w) ]
    arbitrary = unsafeInitWallet
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure genesisParameters

instance Arbitrary (PrimaryKey WalletId) where
    shrink _ = []
    arbitrary = do
        bytes <- B8.pack . pure <$> elements ['a'..'k']
        return $ PrimaryKey $ WalletId $ hash bytes

instance Arbitrary WalletMetadata where
    shrink _ = []
    arbitrary =  WalletMetadata
        <$> (WalletName <$> elements ["bulbazaur", "charmander", "squirtle"])
        <*> genUniformTime
        <*> (fmap WalletPassphraseInfo <$> liftArbitrary genUniformTime)
        <*> pure NotDelegating

{-------------------------------------------------------------------------------
                                   Blocks
-------------------------------------------------------------------------------}

instance Arbitrary BlockHeader where
    arbitrary = do
        sid@(SlotId (EpochNo ep) (SlotNo sl)) <- arbitrary
        let h = fromIntegral sl + fromIntegral ep * arbitraryEpochLength
        bytes <- B8.pack <$> vectorOf 8 (elements ['a'..'f'])
        pure $ BlockHeader sid (Quantity h) (Hash bytes) (Hash bytes)

instance Arbitrary SlotId where
    shrink (SlotId (EpochNo ep) (SlotNo sl)) =
        uncurry SlotId <$> shrink (EpochNo ep, SlotNo sl)
    arbitrary = SlotId
        <$> (EpochNo <$> choose (0, fromIntegral arbitraryEpochLength))
        <*> (SlotNo <$> choose (0, fromIntegral arbitraryChainLength))

instance Arbitrary SlotNo where
    shrink (SlotNo x) = SlotNo <$> shrink x
    arbitrary = SlotNo <$> arbitrary

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = EpochNo <$> arbitrary

arbitraryEpochLength :: Word32
arbitraryEpochLength = 100

arbitraryChainLength :: Word32
arbitraryChainLength = 10

{-------------------------------------------------------------------------------
                                  Transactions
-------------------------------------------------------------------------------}

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

instance Arbitrary TxIn where
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary TxOut where
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary TxMeta where
    arbitrary = TxMeta
        <$> arbitrary
        <*> elements [Incoming, Outgoing]
        <*> arbitrary
        <*> fmap Quantity arbitrary
        <*> fmap (Quantity . fromIntegral) (arbitrary @Word32)

instance Arbitrary TxStatus where
    arbitrary = elements [Pending, InLedger]

instance Arbitrary Coin where
    arbitrary = Coin <$> choose (1, 100000)

instance Arbitrary UTxO where
    shrink (UTxO u) =
        UTxO <$> shrink u
    arbitrary = do
        n <- choose (1, 10)
        u <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList u

{-------------------------------------------------------------------------------
                                 Address
-------------------------------------------------------------------------------}

instance Arbitrary Address where
    arbitrary = Address . B8.pack <$> vectorOf addrSingleSize arbitrary

instance Arbitrary (Index 'Soft 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AccountK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened 'AddressK) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

{-------------------------------------------------------------------------------
                              Sequential State
-------------------------------------------------------------------------------}

instance Arbitrary (SeqState 'Testnet ShelleyKey) where
    shrink (SeqState intPool extPool ixs) =
        (\(i, e, x) -> SeqState i e x) <$> shrink (intPool, extPool, ixs)
    arbitrary = do
        SeqState <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (ShelleyKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = elements rootKeysSeq

-- FIXME:
-- Store pending change outside of the sequential state. Pending state is not
-- affected by rollbacks and therefore, managing with checkpoints makes testing
-- fairly difficult as many nice properties fail if we consider non empty change
-- indexes. For example:
--
-- - put checkpoint with pending indexes
-- - put checkpoint with empty pending indexes
-- - read latest checkpoint
--
-- We would expect the latest checkpoint to be the one we just inserted, but
-- currently, it'll also contains the pending indexes from the previously
-- inserted checkpoints!
--
--    shrink =
--        map Seq.pendingIxsFromList . shrink . Seq.pendingIxsToList
--    arbitrary =
--        Seq.pendingIxsFromList . Set.toList <$> arbitrary
instance Arbitrary (Seq.PendingIxs) where
    arbitrary = pure Seq.emptyPendingIxs

instance Typeable chain => Arbitrary (AddressPool 'Testnet chain ShelleyKey) where
    arbitrary = pure $ mkAddressPool arbitrarySeqAccount minBound mempty

-- Properties are quite heavy on the generation of values, although for
-- private keys, it isn't particularly useful / relevant to generate many of
-- them as they're really treated as an opaque type. Instead, we generate them
-- once, and picks from the list.
rootKeysSeq :: [ShelleyKey 'RootK XPrv]
rootKeysSeq = unsafePerformIO $ generate (vectorOf 10 genRootKeysSeq)
  where
    genRootKeysSeq :: Gen (ShelleyKey 'RootK XPrv)
    genRootKeysSeq = do
        (s, g, e) <- (,,)
            <$> genPassphrase @"seed" (16, 32)
            <*> genPassphrase @"generation" (0, 16)
            <*> genPassphrase @"encryption" (0, 16)
        return $ Seq.generateKeyFromSeed (s, g) e
{-# NOINLINE rootKeysSeq #-}

arbitrarySeqAccount
    :: ShelleyKey 'AccountK XPub
arbitrarySeqAccount =
    publicKey $ unsafeGenerateKeyFromSeed (seed, mempty) mempty
  where
    seed = Passphrase $ BA.convert $ BS.replicate 32 0

{-------------------------------------------------------------------------------
                                 Random State
-------------------------------------------------------------------------------}

instance Arbitrary (RndState 'Testnet) where
    shrink (RndState k ix addrs pending g) =
        [ RndState k ix' addrs' pending' g
        | (ix', addrs', pending') <- shrink (ix, addrs, pending)
        ]
    arbitrary = RndState
        <$> pure (Passphrase "passphrase")
        <*> pure minBound
        <*> arbitrary
        <*> (pure mempty) -- FIXME: see comment on 'Arbitrary Seq.PendingIxs'
        <*> pure (mkStdGen 42)

instance Arbitrary (ByronKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = elements rootKeysRnd

genRootKeysRnd :: Gen (ByronKey 'RootK XPrv)
genRootKeysRnd = Rnd.generateKeyFromSeed
    <$> genPassphrase @"seed" (16, 32)
    <*> genPassphrase @"encryption" (0, 16)

genPassphrase :: (Int, Int) -> Gen (Passphrase purpose)
genPassphrase range = do
    n <- choose range
    InfiniteList bytes _ <- arbitrary
    return $ Passphrase $ BA.convert $ BS.pack $ take n bytes

rootKeysRnd :: [ByronKey 'RootK XPrv]
rootKeysRnd = unsafePerformIO $ generate (vectorOf 10 genRootKeysRnd)
{-# NOINLINE rootKeysRnd #-}

{-------------------------------------------------------------------------------
                                 Miscellaneous
-------------------------------------------------------------------------------}

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

deriving instance Eq (SeqState 'Testnet ShelleyKey)

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = const "XPrv"

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

instance Arbitrary (Hash purpose) where
    arbitrary = do
        bytes <- BS.pack <$> vectorOf 32 arbitrary
        return $ Hash $ BS.take 8 $ convertToBase Base16 bytes

{-------------------------------------------------------------------------------
                                   Buildable
-------------------------------------------------------------------------------}

deriving instance Buildable a => Buildable (Identity a)

instance Buildable GenTxHistory where
    build (GenTxHistory txs) = blockListF' "-" tupleF txs

instance Buildable (ShelleyKey depth XPrv, Hash "encryption") where
    build (_, h) = tupleF (xprvF, prefixF 8 hF <> "..." <> suffixF 8 hF)
      where
        xprvF = "XPrv" :: Builder
        hF = build (toText (coerce @_ @(Hash "BlockHeader") h))

instance Buildable (PrimaryKey WalletId) where
    build (PrimaryKey wid) = build wid

instance Buildable MockChain where
    build (MockChain chain) = blockListF' mempty build chain
