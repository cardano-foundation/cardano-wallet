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
{-# LANGUAGE TupleSections #-}

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
import Cardano.Address.Script
    ( Cosigner (..), Script (..), ScriptTemplate (..) )
import Cardano.Crypto.Wallet
    ( unXPrv )
import Cardano.Mnemonic
    ( SomeMnemonic (..) )
import Cardano.Pool.Types
    ( PoolId (..) )
import Cardano.Wallet.Address.Book
    ( AddressBookIso (..) )
import Cardano.Wallet.DB.Pure.Implementation
    ( TxHistory, filterTxHistory )
import Cardano.Wallet.DummyTarget.Primitive.Types as DummyTarget
    ( block0, mkTx )
import Cardano.Wallet.Gen
    ( genMnemonic, genSimpleTxMetadata, shrinkSlotNo, shrinkTxMetadata )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , DerivationType (..)
    , Index (..)
    , NetworkDiscriminant (..)
    , Role (..)
    , WalletKey (..)
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Shared
    ()
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey, purposeCIP1854 )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs, PendingIxs, emptyPendingIxs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState (..) )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( DerivationPrefix (..)
    , SeqState (..)
    , coinTypeAda
    , defaultAddressPoolGap
    , purposeCIP1852
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( SharedAddressPools (..), SharedState (..) )
import Cardano.Wallet.Primitive.Model
    ( Wallet, currentTip, getState, unsafeInitWallet, utxo )
import Cardano.Wallet.Primitive.Passphrase.Types
    ( Passphrase (..)
    , PassphraseHash (..)
    , PassphraseScheme (..)
    , WalletPassphraseInfo (..)
    )
import Cardano.Wallet.Primitive.Types
    ( Block (..)
    , BlockHeader (..)
    , DecentralizationLevel
    , DelegationCertificate (..)
    , EpochNo (..)
    , EraInfo (..)
    , ExecutionUnitPrices (..)
    , ExecutionUnits (..)
    , FeePolicy (..)
    , LinearFunction (LinearFunction)
    , ProtocolParameters (..)
    , Range (..)
    , Slot
    , SlotInEpoch (..)
    , SlotNo (..)
    , SortOrder (..)
    , TxParameters (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WithOrigin (..)
    , fromDecentralizationLevel
    , rangeIsValid
    , unsafeEpochNo
    , wholeRange
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..), AddressState (..) )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..), mockHash )
import Cardano.Wallet.Primitive.Types.MinimumUTxO.Gen
    ( genMinimumUTxO, shrinkMinimumUTxO )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount (..) )
import Cardano.Wallet.Primitive.Types.TokenBundle.Gen
    ( genTokenBundleSmallRange )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( TokenMap )
import Cardano.Wallet.Primitive.Types.TokenMap.Gen
    ( genTokenMap, shrinkTokenMap )
import Cardano.Wallet.Primitive.Types.Tx.Gen
    ( genTxScriptValidity, shrinkTxScriptValidity )
import Cardano.Wallet.Primitive.Types.Tx.Tx
    ( Tx (..), TxMetadata, TxScriptValidity (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxIn
    ( TxIn (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxMeta
    ( Direction (..), TxMeta (..), TxStatus (..), isPending )
import Cardano.Wallet.Primitive.Types.Tx.TxOut
    ( TxOut (..) )
import Cardano.Wallet.Primitive.Types.Tx.TxOut.Gen
    ( genTxOutCoin )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO (..) )
import Cardano.Wallet.Read.Eras.EraValue
    ( eraValueSerialize )
import Cardano.Wallet.Read.Eras.KnownEras
    ( knownEraIndices )
import Cardano.Wallet.Read.Tx.CBOR
    ( TxCBOR )
import Cardano.Wallet.Unsafe
    ( someDummyMnemonic, unsafeMkPercentage )
import Cardano.Wallet.Util
    ( ShowFmt (..) )
import Control.Arrow
    ( second )
import Control.DeepSeq
    ( NFData )
import Crypto.Hash
    ( hash )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.Either.Extra
    ( fromRight' )
import Data.Functor.Identity
    ( Identity (..) )
import Data.Generics.Internal.VL
    ( match )
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
    ( Word16, Word32 )
import Data.Word.Odd
    ( Word31 )
import Fmt
    ( Buildable (..), Builder, blockListF', prefixF, suffixF, tupleF )
import Generics.SOP
    ( NP (..) )
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
    , arbitraryBoundedEnum
    , arbitrarySizedBoundedIntegral
    , arbitrarySizedNatural
    , choose
    , elements
    , frequency
    , generate
    , genericShrink
    , liftArbitrary
    , oneof
    , scale
    , shrinkIntegral
    , shrinkList
    , vector
    , vectorOf
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )
import Test.QuickCheck.Extra
    ( genericRoundRobinShrink, (<:>), (<@>) )
import Test.Utils.Time
    ( genUniformTime )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.MintBurn as MintBurn
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shared as Shared
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Shelley
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Sequential as Seq
import qualified Cardano.Wallet.Primitive.AddressDiscovery.Shared as Shared
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
                                 Modifiers
-------------------------------------------------------------------------------}

-- | Convenient constraint alias for generating address discovery states
type GenState s =
    ( AddressBookIso s
    , Arbitrary s
    , Buildable s
    , Eq s
    , IsOurs s Address
    , IsOurs s RewardAccount
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
        shrinkOne (tx,meta) = [(tx', meta) | tx' <- shrink tx]

    arbitrary = GenTxHistory . sortTxHistory <$> do
        -- NOTE
        -- We discard pending transaction from any 'GenTxHistory since,
        -- inserting a pending transaction actually has an effect on the
        -- checkpoint's pending transactions of the same wallet.
        filter (not . isPending . snd) <$> scale (min 25) arbitrary
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
                    (Just $ mockHash slot)
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

{-------------------------------------------------------------------------------
                                   Wallets
-------------------------------------------------------------------------------}

instance GenState s => Arbitrary (Wallet s) where
    shrink w =
        [ unsafeInitWallet u (currentTip w) s
        | (u, s) <- shrink (utxo w, getState w) ]
    arbitrary = unsafeInitWallet
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary WalletId where
    shrink _ = []
    arbitrary = WalletId . hash . B8.pack . pure <$> elements ['a'..'k']

instance Arbitrary WalletMetadata where
    shrink _ = []
    arbitrary =  WalletMetadata
        <$> (WalletName <$> elements ["bulbazaur", "charmander", "squirtle"])
        <*> genUniformTime
        <*> oneof
            [ pure Nothing
            , Just <$> (WalletPassphraseInfo <$> genUniformTime <*> arbitrary)
            ]

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
        pure $ BlockHeader slot (Quantity h) blockH (Just blockH)

instance Arbitrary SlotNo where
    arbitrary = do
        SlotInEpoch sl <- arbitrary
        EpochNo ep <- arbitrary
        pure $ SlotNo $ fromIntegral $ fromIntegral ep * arbitraryChainLength + sl
    shrink = shrinkSlotNo

instance Arbitrary Slot where
    arbitrary = frequency
        [ ( 1, pure Origin)
        , (40, At <$> arbitrary)
        ]
    shrink Origin = [Origin]
    shrink (At slot) = At <$> shrinkSlotNo slot

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

instance Arbitrary TxCBOR where
    arbitrary = do
        c <- cbor
        n <- elements knownEraIndices
        pure $ fromRight' $ match eraValueSerialize (c,n)
      where
        cbor = do
            InfiniteList bytes _ <- arbitrary
            return $ BL.pack $ take 500 bytes

instance Arbitrary Tx where
    shrink (Tx _tid cbor fees ins cins outs cout wdrls md validity) =
        [ mkTx cbor fees ins' cins' outs' cout' wdrls' md' validity'
        | (ins', cins', outs', cout', wdrls', md', validity')
            <- shrink
                (ins, cins, outs, cout, wdrls, md, validity)
        ]

    arbitrary = do
        ins <- fmap (,Nothing) . L.nub . L.take 5 . getNonEmpty <$> arbitrary
        cins <- fmap (,Nothing) . L.nub . L.take 5 . getNonEmpty <$> arbitrary
        outs <- fmap (L.take 5 . getNonEmpty) arbitrary
        cout <- arbitrary
        wdrls <- fmap (Map.fromList . L.take 5) arbitrary
        fees <- arbitrary
        mkTx Nothing fees ins cins outs cout wdrls
            <$> arbitrary <*> liftArbitrary genTxScriptValidity

instance Arbitrary TokenMap where
    arbitrary = genTokenMap
    shrink = shrinkTokenMap

instance Arbitrary TxIn where
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary TxOut where
    arbitrary = TxOut
        <$> arbitrary
        <*> genTokenBundleSmallRange

instance Arbitrary TxMeta where
    arbitrary = do
        st <- arbitrary
        TxMeta st
            <$> elements [Incoming, Outgoing]
            <*> arbitrary
            <*> fmap Quantity arbitrary
            <*> arbitrary
            <*> (if st == Pending then Just <$> arbitrary else pure Nothing)

instance Arbitrary TxStatus where
    arbitrary = elements [Pending, InLedger]

instance Arbitrary TxMetadata where
    arbitrary = genSimpleTxMetadata
    shrink = shrinkTxMetadata

instance Arbitrary Coin where
    arbitrary = genTxOutCoin

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
    arbitrary = Address . B8.pack <$> vector 29

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

instance Arbitrary (SeqState 'Mainnet ShelleyKey) where
    shrink (SeqState intPool extPool ixs acc policy rwd prefix) =
            (\(i, e, x) -> SeqState i e x acc policy rwd prefix)
        <$> shrink (intPool, extPool, ixs)
    arbitrary = SeqState
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> pure arbitrarySeqAccount
        <*> pure (Just arbitraryPolicyKey)
        <*> pure arbitraryRewardAccount
        <*> pure defaultSeqStatePrefix

defaultSeqStatePrefix :: DerivationPrefix
defaultSeqStatePrefix = DerivationPrefix
    ( purposeCIP1852
    , coinTypeAda
    , minBound
    )

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
--        map pendingIxsFromList . shrink . pendingIxsToList
--    arbitrary =
--        pendingIxsFromList . Set.toList <$> arbitrary
instance Arbitrary (PendingIxs 'CredFromKeyK) where
    arbitrary = pure emptyPendingIxs

instance ( Typeable ( c :: Role ) )
    => Arbitrary (Seq.SeqAddressPool c ShelleyKey)
  where
    arbitrary = pure $ Seq.newSeqAddressPool @'Mainnet
        arbitrarySeqAccount defaultAddressPoolGap

-- Properties are quite heavy on the generation of values, although for
-- private keys, it isn't particularly useful / relevant to generate many of
-- them as they're really treated as an opaque type. Instead, we generate them
-- once, and picks from the list.
rootKeysSeq :: [ShelleyKey 'RootK XPrv]
rootKeysSeq = unsafePerformIO $ generate (vectorOf 10 genRootKeysSeq)
  where
    genRootKeysSeq :: Gen (ShelleyKey 'RootK XPrv)
    genRootKeysSeq = do
        s <- SomeMnemonic <$> genMnemonic @12
        g <- frequency
                [ (1, return Nothing)
                , (3, Just . SomeMnemonic <$> genMnemonic @12)
                ]
        e <- genPassphrase @"encryption" (0, 16)
        return $ Shelley.generateKeyFromSeed (s, g) e
{-# NOINLINE rootKeysSeq #-}

arbitrarySeqAccount
    :: ShelleyKey 'AccountK XPub
arbitrarySeqAccount =
    publicKey $ Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
  where
    mw = someDummyMnemonic (Proxy @15)

arbitraryRewardAccount
    :: ShelleyKey 'CredFromKeyK XPub
arbitraryRewardAccount =
    publicKey $ Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
  where
    mw = someDummyMnemonic (Proxy @15)

arbitraryPolicyKey
    :: ShelleyKey 'PolicyK XPub
arbitraryPolicyKey =
    publicKey $ liftRawKey $
    MintBurn.derivePolicyPrivateKey mempty rootXPrv minBound
  where
    rootXPrv:_ = map getRawKey $ take 1 rootKeysSeq

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
        <$> scale (min 10) arbitrary
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
                                 Shared State
-------------------------------------------------------------------------------}

-- Shared state is composed of static (ie., not depending on checkpoint) and
-- depending on checkpoint part. When in pending state all parts are static,
-- when in active state context address pool is depending on checkpoint the rest is static.
-- In order to proceed like for sequential/random wallet we will work with active state
-- and make

instance Arbitrary (SharedState 'Mainnet SharedKey) where
    arbitrary = do
        pt <- genScriptTemplateHardCoded
        pure $ SharedState
            defaultSharedStatePrefix
            arbitrarySharedAccount
            pt
            Nothing
            defaultAddressPoolGap
            (Shared.Active $ SharedAddressPools
                (Shared.newSharedAddressPool @'Mainnet defaultAddressPoolGap pt Nothing)
                (Shared.newSharedAddressPool @'Mainnet defaultAddressPoolGap pt Nothing)
                emptyPendingIxs
            )

defaultSharedStatePrefix :: DerivationPrefix
defaultSharedStatePrefix = DerivationPrefix
    ( purposeCIP1854
    , coinTypeAda
    , minBound @(Index 'Hardened 'AccountK)
    )

instance Arbitrary (Script Cosigner) where
    arbitrary = pure $
        RequireAllOf [RequireSignatureOf (Cosigner 0), RequireAnyOf [ActiveFromSlot 200, ActiveUntilSlot 100]]

genScriptTemplateHardCoded :: Gen ScriptTemplate
genScriptTemplateHardCoded =
    ScriptTemplate (Map.fromList [(Cosigner 0, getRawKey arbitrarySeqAccount)] ) <$> arbitrary

instance Arbitrary Seq.AddressPoolGap where
    arbitrary = pure defaultAddressPoolGap

arbitrarySharedAccount
    :: SharedKey 'AccountK XPub
arbitrarySharedAccount =
    publicKey $ Shared.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
  where
    mw = someDummyMnemonic (Proxy @15)

{-------------------------------------------------------------------------------
                             Protocol Parameters
-------------------------------------------------------------------------------}

instance Arbitrary ProtocolParameters where
    shrink = genericRoundRobinShrink
        <@> shrink
        <:> shrink
        <:> shrink
        <:> shrinkMinimumUTxO
        <:> shrink
        <:> shrink
        <:> shrink
        <:> shrink
        <:> shrink
        <:> const []
        <:> Nil
    arbitrary = ProtocolParameters
        <$> arbitrary
        <*> arbitrary
        <*> choose (0, 100)
        <*> genMinimumUTxO
        <*> arbitrary
        <*> arbitrary
        <*> genMaximumCollateralInputCount
        <*> genMinimumCollateralPercentage
        <*> arbitrary
        <*> pure Nothing
      where
        genMaximumCollateralInputCount :: Gen Word16
        genMaximumCollateralInputCount = arbitrarySizedNatural

        genMinimumCollateralPercentage :: Gen Natural
        genMinimumCollateralPercentage = arbitrarySizedNatural

instance Arbitrary Natural where
    arbitrary = arbitrarySizedNatural
    shrink = shrinkIntegral

instance Arbitrary ExecutionUnitPrices where
    shrink = genericShrink
    arbitrary = genericArbitrary

instance Arbitrary (EraInfo EpochNo) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TxParameters where
    shrink = genericShrink
    arbitrary = TxParameters
        <$> arbitrary
        <*> fmap Quantity (choose (0, 1_000))
        <*> arbitrary
        <*> arbitrary

instance Arbitrary ExecutionUnits where
    shrink = genericShrink
    arbitrary = ExecutionUnits
        <$> arbitrary
        <*> arbitrary

instance Arbitrary FeePolicy where
    arbitrary = (LinearFee . ) . LinearFunction
        <$> choose (0, 1_000)
        <*> choose (0, 100)

instance (Integral a, Arbitrary a) => Arbitrary (Quantity n a) where
    shrink (Quantity a) = Quantity <$> shrinkIntegral a
    arbitrary = Quantity <$> arbitrary

{-------------------------------------------------------------------------------
                                 Miscellaneous
-------------------------------------------------------------------------------}

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

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
    arbitrary = fromDecentralizationLevel <$> arbitrary

instance Arbitrary RewardAccount where
    arbitrary =
        RewardAccount . BS.pack <$> vector 28

instance Arbitrary (Hash purpose) where
    arbitrary = Hash . convertToBase Base16 . BS.pack <$> vector 16

instance Arbitrary PassphraseHash where
    arbitrary = PassphraseHash . convertToBase Base16 . BS.pack <$> vector 16

instance Arbitrary PoolId where
    arbitrary = PoolId . convertToBase Base16 . BS.pack <$> vector 16

instance Arbitrary DelegationCertificate where
    arbitrary = oneof
        [ CertDelegateNone <$> genArbitraryRewardAccount
        , CertDelegateFull <$> genArbitraryRewardAccount <*> arbitrary
        ]
      where
        genArbitraryRewardAccount = pure $ RewardAccount $ BS.replicate 32 0

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary AddressState where
    arbitrary = genericArbitrary

instance Arbitrary SomeMnemonic where
    arbitrary = SomeMnemonic <$> genMnemonic @12

instance Arbitrary TxScriptValidity where
    arbitrary = genTxScriptValidity
    shrink = shrinkTxScriptValidity

{-------------------------------------------------------------------------------
                                   Buildable
-------------------------------------------------------------------------------}

deriving instance Buildable a => Buildable (Identity a)

instance Buildable GenTxHistory where
    build (GenTxHistory txs) = blockListF' "-" tupleF txs

instance Buildable (ShelleyKey depth XPrv, PassphraseHash) where
    build (_, h) = tupleF (xprvF, prefixF 8 hF <> "..." <> suffixF 8 hF)
      where
        xprvF = "XPrv" :: Builder
        hF = build (toText (Hash @"BlockHeader" (BA.convert h)))

instance Buildable MockChain where
    build (MockChain chain) = blockListF' mempty build chain
