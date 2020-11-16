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
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.DB.Arbitrary
    ( GenTxHistory (..)
    , KeyValPairs (..)
    , GenState
    , MockChain (..)
    , InitialCheckpoint (..)
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, XPub )
import Cardano.Crypto.Wallet
    ( unXPrv )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Wallet.DB
    ( PrimaryKey (..) )
import Cardano.Wallet.DB.Model
    ( TxHistory, filterTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types as DummyTarget
    ( block0, dummyGenesisParameters, mkTx, mockHash )
import Cardano.Wallet.Gen
    ( genMnemonic, genTxMetadata, shrinkSlotNo, shrinkTxMetadata )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , Passphrase (..)
    , WalletKey (..)
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey (..), addrSingleSize, unsafeGenerateKeyFromSeed )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPool
    , DerivationPrefix (..)
    , SeqState (..)
    , coinTypeAda
    , mkAddressPool
    , purposeCIP1852
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , blockchainParameters
    , currentTip
    , getState
    , unsafeInitWallet
    , utxo
    )
import Cardano.Wallet.Primitive.Slotting
    ( unsafeEpochNo )
import Cardano.Wallet.Primitive.Types
    ( AccountId (..)
    , Address (..)
    , AddressState (..)
    , Block (..)
    , BlockHeader (..)
    , ChimericAccount (..)
    , Coin (..)
    , DecentralizationLevel (..)
    , DelegationCertificate (..)
    , Direction (..)
    , EpochNo (..)
    , FeePolicy (..)
    , PassphraseScheme (..)
    , PoolId (..)
    , ProtocolParameters (..)
    , Range (..)
    , ShowFmt (..)
    , SlotInEpoch (..)
    , SlotNo (..)
    , SortOrder (..)
    , Tx (..)
    , TxIn (..)
    , TxMeta (..)
    , TxMetadata
    , TxOut (..)
    , TxParameters (..)
    , TxStatus (..)
    , UTxO (..)
    , WalletDelegation (..)
    , WalletDelegationStatus (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , isPending
    , rangeIsValid
    , wholeRange
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Unsafe
    ( someDummyMnemonic, unsafeMkPercentage )
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
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Percentage (..), Quantity (..) )
import Data.Ratio
    ( (%) )
import Data.Text.Class
    ( toText )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word32 )
import Data.Word.Odd
    ( Word31 )
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
    , arbitrarySizedBoundedIntegral
    , choose
    , elements
    , frequency
    , generate
    , genericShrink
    , oneof
    , scale
    , shrinkIntegral
    , shrinkList
    , vector
    , vectorOf
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )
import Test.Utils.Time
    ( genUniformTime )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Jormungandr as Jormungandr
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                                 Modifiers
-------------------------------------------------------------------------------}

type GenState s =
    ( Arbitrary s
    , Buildable s
    , IsOurs s Address
    , IsOurs s ChimericAccount
    , NFData s
    , Show s
    )

newtype KeyValPairs k v = KeyValPairs [(k, v)]
    deriving (Generic, Show, Eq)

newtype GenTxHistory = GenTxHistory { unGenTxHistory :: TxHistory }
    deriving stock (Show, Eq)
    deriving newtype (Semigroup, Monoid)

newtype MockChain = MockChain
    { getMockChain :: [Block] }
    deriving stock (Eq, Show)

-- | Generate arbitrary checkpoints, but that always have their tip at 0 0.
newtype InitialCheckpoint s =
    InitialCheckpoint { getInitialCheckpoint :: Wallet s }
    deriving newtype (Show, Eq, Buildable, NFData)

instance (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (KeyValPairs k v) where
    shrink = genericShrink
    arbitrary = do
        pairs <- choose (1, 10) >>= vector
        pure $ KeyValPairs $ L.sortOn fst pairs

-- | For checkpoints, we make sure to generate them in order.
instance {-# OVERLAPS #-} (Arbitrary k, Ord k, GenState s)
    => Arbitrary (KeyValPairs k (ShowFmt (Wallet s))) where
    shrink = genericShrink
    arbitrary = do
        pairs <- choose (1, 10) >>= vector
        pure $ KeyValPairs $ second ShowFmt
           <$> L.sortOn (\(k,cp) -> (k, view #slotNo (currentTip cp))) pairs

instance Arbitrary GenTxHistory where
    shrink (GenTxHistory txs) = GenTxHistory <$> shrinkList shrinkOne txs
      where
        shrinkOne (tx,meta)
            | length txs < 10 =
                [(tx', meta) | tx' <- shrink tx]
            | otherwise =
                []

    arbitrary = GenTxHistory . sortTxHistory <$> do
        -- NOTE
        -- We discard pending transaction from any 'GenTxHistory since,
        -- inserting a pending transaction actually has an effect on the
        -- checkpoint's pending transactions of the same wallet.
        filter (not . isPending . snd) <$> scale (`mod` 100) arbitrary
      where
        sortTxHistory = filterTxHistory Nothing Descending wholeRange

instance Arbitrary MockChain where
    shrink (MockChain chain) =
        [ MockChain chain'
        | chain' <- shrinkList shrinkBlock chain
        , not (null chain')
        ]
      where
        shrinkBlock (Block h txs _) =
            Block h <$> shrinkList shrink txs <*> pure []
    arbitrary = do
        n0 <- choose (1, 10)
        slot0 <- arbitrary
        height0 <- fromIntegral <$> choose (0, unSlotNo slot0)
        blocks <- sequence $ flip unfoldr (slot0, height0, n0) $
            \(slot, height, n) ->
                if n <= (0 :: Int)
                    then Nothing
                    else Just
                        ( genBlock slot height
                        , (SlotNo (unSlotNo slot + 1), height + 1, n - 1)
                        )
        return (MockChain blocks)
      where
        genBlock :: SlotNo -> Word32 -> Gen Block
        genBlock slot height = do
            let h = BlockHeader
                    slot
                    (Quantity height)
                    (mockHash slot)
                    (mockHash slot)
            Block h
                <$> (choose (1, 10) >>= vector)
                <*> pure []

instance GenState s => Arbitrary (InitialCheckpoint s) where
    shrink (InitialCheckpoint cp) = InitialCheckpoint <$> shrink cp
    arbitrary = do
        cp <- arbitrary @(Wallet s)
        pure $ InitialCheckpoint $ unsafeInitWallet
            (utxo cp)
            (block0 ^. #header)
            (getState cp)
            (blockchainParameters cp)

{-------------------------------------------------------------------------------
                                   Wallets
-------------------------------------------------------------------------------}

instance GenState s => Arbitrary (Wallet s) where
    shrink w =
        [ unsafeInitWallet u (currentTip w) s (blockchainParameters w)
        | (u, s) <- shrink (utxo w, getState w) ]
    arbitrary = unsafeInitWallet
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure dummyGenesisParameters

instance Arbitrary (PrimaryKey AccountId) where
    shrink _ = []
    arbitrary = do
        bytes <- B8.pack . pure <$> elements ['a'..'k']
        return $ PrimaryKey $ AccountId $ hash bytes

instance Arbitrary WalletMetadata where
    shrink _ = []
    arbitrary =  WalletMetadata
        <$> (WalletName <$> elements ["bulbazaur", "charmander", "squirtle"])
        <*> genUniformTime
        <*> oneof
            [ pure Nothing
            , Just <$> (WalletPassphraseInfo <$> genUniformTime <*> arbitrary)
            ]
        <*> pure (WalletDelegation NotDelegating [])

instance Arbitrary PassphraseScheme where
    arbitrary = genericArbitrary

{-------------------------------------------------------------------------------
                                   Blocks
-------------------------------------------------------------------------------}

instance Arbitrary BlockHeader where
    arbitrary = do
        EpochNo ep <- arbitrary
        SlotInEpoch sl <- arbitrary
        let h = fromIntegral sl + fromIntegral ep * arbitraryEpochLength
        blockH <- arbitrary
        let slot = SlotNo $ fromIntegral h
        pure $ BlockHeader slot (Quantity h) blockH (coerce blockH)

instance Arbitrary SlotNo where
    arbitrary = do
        SlotInEpoch sl <- arbitrary
        EpochNo ep <- arbitrary
        pure $ SlotNo $ fromIntegral $ fromIntegral ep * arbitraryChainLength + sl
    shrink = shrinkSlotNo

instance Arbitrary SlotInEpoch where
    shrink (SlotInEpoch x) = SlotInEpoch <$> shrink x
    arbitrary = SlotInEpoch <$> choose (0, fromIntegral arbitraryChainLength)

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = unsafeEpochNo <$> choose (0, arbitraryEpochLength)

arbitraryEpochLength :: Word32
arbitraryEpochLength = 100

arbitraryChainLength :: Word32
arbitraryChainLength = 10

{-------------------------------------------------------------------------------
                                  Transactions
-------------------------------------------------------------------------------}

instance Arbitrary Tx where
    shrink (Tx _tid ins outs wdrls md) = mconcat
        [ [ mkTx ins' outs  wdrls md
          | ins' <- shrinkList' ins
          ]

        , [ mkTx ins  outs' wdrls md
          | outs' <- shrinkList' outs
          ]

        , [ mkTx ins outs (Map.fromList wdrls') md
          | wdrls' <- shrinkList' (Map.toList wdrls)
          ]

        , [ mkTx ins  outs  wdrls md'
          | md' <- shrink md
          ]
        ]
      where
        shrinkList' xs  = filter (not . null)
            [ take n xs | Positive n <- shrink (Positive $ length xs) ]

    arbitrary = do
        ins <- fmap (L.nub . L.take 5 . getNonEmpty) arbitrary
        outs <- fmap (L.take 5 . getNonEmpty) arbitrary
        wdrls <- fmap (Map.fromList . L.take 5) arbitrary
        mkTx ins outs wdrls <$> arbitrary

instance Arbitrary TxIn where
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary TxOut where
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary TxMeta where
    arbitrary = do
        st <- arbitrary
        TxMeta st
            <$> elements [Incoming, Outgoing]
            <*> arbitrary
            <*> fmap Quantity arbitrary
            <*> fmap (Quantity . fromIntegral) (arbitrary @Word32)
            <*> (if st == Pending then Just <$> arbitrary else pure Nothing)

instance Arbitrary TxStatus where
    arbitrary = elements [Pending, InLedger]

instance Arbitrary TxMetadata where
    arbitrary = genTxMetadata
    shrink = shrinkTxMetadata

instance Arbitrary Coin where
    arbitrary = Coin <$> choose (1, 100000)

instance Arbitrary UTxO where
    shrink (UTxO u) =
        UTxO <$> shrink u
    arbitrary = do
        n <- choose (1, 10)
        u <- zip
            <$> vector n
            <*> vector n
        return $ UTxO $ Map.fromList u


instance (Ord a, Arbitrary a) => Arbitrary (Range a) where
    arbitrary = Range <$> arbitrary <*> arbitrary

    shrink (Range from to) = filter rangeIsValid $
        [Range from' to | from' <- shrink from]
        ++ [Range from to' | to' <- shrink to]

{-------------------------------------------------------------------------------
                                 Address
-------------------------------------------------------------------------------}

instance Arbitrary Address where
    arbitrary = Address . B8.pack <$> vector addrSingleSize

instance Arbitrary (Index 'Soft depth) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'Hardened depth) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary (Index 'WholeDomain depth) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

{-------------------------------------------------------------------------------
                              Sequential State
-------------------------------------------------------------------------------}

instance Arbitrary (SeqState 'Mainnet JormungandrKey) where
    shrink (SeqState intPool extPool ixs rwd prefix) =
        (\(i, e, x) -> SeqState i e x rwd prefix) <$> shrink (intPool, extPool, ixs)
    arbitrary = SeqState
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure arbitraryRewardAccount
        <*> pure defaultSeqStatePrefix

defaultSeqStatePrefix :: DerivationPrefix
defaultSeqStatePrefix = DerivationPrefix
    ( purposeCIP1852
    , coinTypeAda
    , minBound
    )

instance Arbitrary (JormungandrKey 'RootK XPrv) where
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

instance Typeable chain => Arbitrary (AddressPool chain JormungandrKey) where
    arbitrary = pure $ mkAddressPool @'Mainnet arbitrarySeqAccount minBound mempty

-- Properties are quite heavy on the generation of values, although for
-- private keys, it isn't particularly useful / relevant to generate many of
-- them as they're really treated as an opaque type. Instead, we generate them
-- once, and picks from the list.
rootKeysSeq :: [JormungandrKey 'RootK XPrv]
rootKeysSeq = unsafePerformIO $ generate (vectorOf 10 genRootKeysSeq)
  where
    genRootKeysSeq :: Gen (JormungandrKey 'RootK XPrv)
    genRootKeysSeq = do
        s <- SomeMnemonic <$> genMnemonic @12
        g <- frequency
                [ (1, return Nothing)
                , (3, Just . SomeMnemonic <$> genMnemonic @12)
                ]
        e <- genPassphrase @"encryption" (0, 16)
        return $ Jormungandr.generateKeyFromSeed (s, g) e
{-# NOINLINE rootKeysSeq #-}

arbitrarySeqAccount
    :: JormungandrKey 'AccountK XPub
arbitrarySeqAccount =
    publicKey $ unsafeGenerateKeyFromSeed (mw, Nothing) mempty
  where
    mw = someDummyMnemonic (Proxy @15)

arbitraryRewardAccount
    :: JormungandrKey 'AddressK XPub
arbitraryRewardAccount =
    publicKey $ unsafeGenerateKeyFromSeed (mw, Nothing) mempty
  where
    mw = someDummyMnemonic (Proxy @15)

{-------------------------------------------------------------------------------
                                 Random State
-------------------------------------------------------------------------------}

instance Arbitrary (RndState 'Mainnet) where
    shrink (RndState k ix addrs pending g) =
        [ RndState k ix' addrs' pending' g
        | (ix', addrs', pending') <- shrink (ix, addrs, pending)
        ]
    arbitrary = RndState
        (Passphrase "passphrase")
        minBound
        <$> arbitrary
        <*> (pure mempty) -- FIXME: see comment on 'Arbitrary Seq.PendingIxs'
        <*> pure (mkStdGen 42)

instance Arbitrary (ByronKey 'RootK XPrv) where
    shrink _ = []
    arbitrary = elements rootKeysRnd

genRootKeysRnd :: Gen (ByronKey 'RootK XPrv)
genRootKeysRnd = Byron.generateKeyFromSeed
    <$> arbitrary
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
                             Protocol Parameters
-------------------------------------------------------------------------------}

instance Arbitrary ProtocolParameters where
    arbitrary = ProtocolParameters
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
    shrink = genericShrink

instance Arbitrary TxParameters where
    arbitrary = TxParameters <$> arbitrary <*> arbitrary
    shrink (TxParameters fp mx) =
        [TxParameters fp' mx' | (fp', mx') <- shrink (fp, mx)]

instance Arbitrary FeePolicy where
    arbitrary = LinearFee <$> arbitrary <*> arbitrary <*> pure (Quantity 0)
    shrink (LinearFee a b c) = [LinearFee a' b' c | (a', b') <- shrink (a, b)]

deriving instance Arbitrary a => Arbitrary (Quantity n a)

{-------------------------------------------------------------------------------
                                 Miscellaneous
-------------------------------------------------------------------------------}

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

deriving instance Eq (SeqState 'Mainnet JormungandrKey)

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = const "XPrv"

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    a == b = unXPrv a == unXPrv b

instance Arbitrary Percentage where
    arbitrary = unsafeMkPercentage . (% upperLimit) <$> choose (0, upperLimit)
      where
        upperLimit = 10_000

instance Arbitrary DecentralizationLevel where
    arbitrary = DecentralizationLevel <$> arbitrary

instance Arbitrary ChimericAccount where
    arbitrary =
        ChimericAccount . BS.pack <$> vector 28

instance Arbitrary (Hash purpose) where
    arbitrary = do
        Hash . convertToBase Base16 . BS.pack <$> vector 16

instance Arbitrary PoolId where
    arbitrary = do
        PoolId . convertToBase Base16 . BS.pack <$> vector 16

instance Arbitrary DelegationCertificate where
    arbitrary = oneof
        [ CertDelegateNone <$> arbitraryChimericAccount
        , CertDelegateFull <$> arbitraryChimericAccount <*> arbitrary
        ]
      where
        arbitraryChimericAccount = pure $ ChimericAccount $ BS.replicate 32 0

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary AddressState where
    arbitrary = genericArbitrary

{-------------------------------------------------------------------------------
                                   Buildable
-------------------------------------------------------------------------------}

deriving instance Buildable a => Buildable (Identity a)

instance Buildable GenTxHistory where
    build (GenTxHistory txs) = blockListF' "-" tupleF txs

instance Buildable (JormungandrKey depth XPrv, Hash "encryption") where
    build (_, h) = tupleF (xprvF, prefixF 8 hF <> "..." <> suffixF 8 hF)
      where
        xprvF = "XPrv" :: Builder
        hF = build (toText (coerce @_ @(Hash "BlockHeader") h))

instance Buildable (PrimaryKey AccountId) where
    build (PrimaryKey wid) = build wid

instance Buildable MockChain where
    build (MockChain chain) = blockListF' mempty build chain

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12
