{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-x-partial #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- TODO: https://cardanofoundation.atlassian.net/browse/ADP-2841
{-# OPTIONS_GHC -fno-warn-star-is-type #-}

module Cardano.Wallet.Address.Discovery.SequentialSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    )
import Cardano.Wallet.Address.Derivation
    ( DelegationAddress (..)
    , Depth (..)
    , DerivationIndex
    , DerivationType (..)
    , HardDerivation (..)
    , Index
    , MkKeyFingerprint (..)
    , PaymentAddress (..)
    , Role (..)
    , SoftDerivation (..)
    )
import Cardano.Wallet.Address.Derivation.Icarus
    ( IcarusKey (..)
    )
import Cardano.Wallet.Address.Derivation.SharedKey
    ( SharedKey (..)
    )
import Cardano.Wallet.Address.Derivation.Shelley
    ( ShelleyKey (..)
    )
import Cardano.Wallet.Address.Discovery
    ( ChangeAddressMode (..)
    , CompareDiscovery (..)
    , GenChange (..)
    , GetPurpose
    , IsOurs (..)
    , KnownAddresses (..)
    , emptyPendingIxs
    , genChange
    )
import Cardano.Wallet.Address.Discovery.Sequential
    ( AddressPoolGap (..)
    , DerivationPrefix (..)
    , MkAddressPoolGapError (..)
    , SeqAddressPool (..)
    , SeqState (..)
    , SupportsDiscovery
    , coinTypeAda
    , defaultAddressPoolGap
    , mkAddressPoolGap
    , mkSeqStateFromAccountXPub
    , mkUnboundedAddressPoolGap
    , newSeqAddressPool
    , purposeCIP1852
    )
import Cardano.Wallet.Address.Keys.SequentialAny
    ( mkSeqStateFromRootXPrv
    )
import Cardano.Wallet.Address.Keys.WalletKey
    ( publicKey
    )
import Cardano.Wallet.Address.PoolSpec
    ( genPool
    , shrinkPool
    )
import Cardano.Wallet.Address.States.IsOwned
    ( isOwned
    )
import Cardano.Wallet.Flavor
    ( KeyFlavorS (..)
    , WalletFlavorS (ShelleyWallet)
    )
import Cardano.Wallet.Primitive.NetworkId
    ( NetworkDiscriminant (..)
    , SNetworkId (..)
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    , AddressState (..)
    )
import Cardano.Wallet.Primitive.Types.Credentials
    ( RootCredentials (..)
    )
import Cardano.Wallet.Unsafe
    ( someDummyMnemonic
    )
import Cardano.Wallet.Util
    ( ShowFmt (..)
    )
import Control.Arrow
    ( first
    )
import Control.Monad
    ( unless
    )
import Control.Monad.IO.Class
    ( liftIO
    )
import Data.Function
    ( (&)
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Maybe
    ( isJust
    , isNothing
    )
import Data.Proxy
    ( Proxy (..)
    )
import Data.Text.Class
    ( TextDecodingError (..)
    , fromText
    )
import Data.Type.Equality
    ( type (==)
    )
import Data.Typeable
    ( Typeable
    , typeRep
    )
import Data.Word
    ( Word8
    )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , conjoin
    , counterexample
    , cover
    , elements
    , expectFailure
    , frequency
    , genericShrink
    , label
    , property
    , suchThat
    , (.&&.)
    , (=/=)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary
    )
import Test.QuickCheck.Monadic
    ( monadicIO
    )
import Test.Text.Roundtrip
    ( textRoundtrip
    )

import qualified Cardano.Wallet.Address.Derivation.Icarus as Icarus
import qualified Cardano.Wallet.Address.Derivation.Shelley as Shelley
import qualified Cardano.Wallet.Address.Pool as AddressPool
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "AddressPoolGap" $ do
        it "'AddressPoolGap.succ maxBound' should result in a runtime err"
            (expectFailure prop_succMaxBoundGap)
        it "'AddressPoolGap.pred minBound' should result in a runtime err"
            (expectFailure prop_predMinBoundGap)
        it "FromEnum -> ToEnum roundtrip"
            (property prop_roundtripEnumGap)
        it "mkAddressPoolGap"
            (checkCoverage prop_mkAddressPoolGap)
        it "defaultAddressPoolGap is valid"
            (property prop_defaultValid)

    describe "DerivationPrefix" $ do
        textRoundtrip (Proxy @DerivationPrefix)

    describe "AddressPoolGap - Text Roundtrip" $ do
        textRoundtrip $ Proxy @AddressPoolGap
        let err = "An address pool gap must be a natural number between 10 and 100000."
        it "fail fromText @AddressPoolGap \"-10\"" $
            fromText @AddressPoolGap "-10" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"0\"" $
            fromText @AddressPoolGap "0" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"9\"" $
            fromText @AddressPoolGap "9" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"100001\"" $
            fromText @AddressPoolGap "100001" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"20eiei\"" $
            fromText @AddressPoolGap "20eiei" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"raczej nie\"" $
            fromText @AddressPoolGap "raczej nie" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"-1000\"" $
            fromText @AddressPoolGap "-1000" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"2.5\"" $
            fromText @AddressPoolGap "2.5" === Left (TextDecodingError err)

    describe "PendingIxs & GenChange" $ do
        it "Can always generate exactly `gap` different change addresses from rootXPrv"
            (property prop_genChangeGapFromRootXPrv)
        it "Can always generate exactly `gap` different change addresses from accXPub"
            (property prop_genChangeGapFromAccountXPub)
        it "After `gap` change addresses, the same one are yield in reverse order"
            (property prop_changeAddressRotation)
        it "Can generate new change addresses after discovering a pending one"
            (property prop_changeNoLock)

    describe "IsOwned" $ do
        it "Any discovered address has a corresponding private key!" $ do
            (property prop_lookupDiscovered)

    describe "CompareDiscovery" $ do
        it "Known addresses are always lesser than unknown ones" $ do
            (checkCoverage prop_compareKnownUnknown)
        it "compareDiscovery is anti-symmetric" $ do
            (checkCoverage prop_compareAntiSymmetric)

    describe "KnownAddresses" $ do
        it "Any known address is ours" $ do
            (property prop_knownAddressesAreOurs)
        it "There are at least gap known addresses" $ do
            (property prop_atLeastKnownAddresses)
        it "Change is only known after generation" $ do
            (property prop_changeIsOnlyKnownAfterGeneration)
        it "address that are discovered via isOurs are marked as 'Used'" $ do
            (property prop_oursAreUsed)
        it "addresses with wrong prefixes and our credentials are discovered via isOurs" $ do
            (property prop_oursUnexpectedPrefix)

{-------------------------------------------------------------------------------
                        Properties for AddressPoolGap
-------------------------------------------------------------------------------}

prop_mkAddressPoolGap
    :: Word8
    -> Property
prop_mkAddressPoolGap g =
    cover 25 isWithinBound "hits within bounds" prop
  where
    prop = case mkAddressPoolGap (fromIntegral g) of
        Left (ErrGapOutOfRange _) -> not isWithinBound
        Right _ -> isWithinBound
    isWithinBound =
        fromEnum g >= fromEnum (minBound @AddressPoolGap) &&
        fromEnum g <= fromEnum (maxBound @AddressPoolGap)

prop_defaultValid
    :: Property
prop_defaultValid =
    pure defaultAddressPoolGap
        ===
    mkAddressPoolGap (toEnum $ fromEnum defaultAddressPoolGap)

-- Failing property
prop_succMaxBoundGap :: Property
prop_succMaxBoundGap =
    property $ succ (maxBound @AddressPoolGap) `seq` ()

-- Failing property
prop_predMinBoundGap :: Property
prop_predMinBoundGap =
    property $ pred (minBound @AddressPoolGap) `seq` ()

prop_roundtripEnumGap
    :: AddressPoolGap
    -> Property
prop_roundtripEnumGap g =
    (toEnum . fromEnum) g === g

{-------------------------------------------------------------------------------
                    Properties for AddressScheme & PendingIxs
-------------------------------------------------------------------------------}

-- | We can always generate at exactly `gap` change addresses (on the internal
-- chain) using mkSeqStateFromRootXPrv
prop_genChangeGapFromRootXPrv :: AddressPoolGap -> Property
prop_genChangeGapFromRootXPrv g = property $
    length (fst $ changeAddresses [] s0) === fromEnum g
  where
    mw = someDummyMnemonic (Proxy @12)
    key = Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
    s0 = mkSeqStateFromRootXPrv ShelleyKeyS
        (RootCredentials key mempty) purposeCIP1852 g IncreasingChangeAddresses

-- | We can always generate at exactly `gap` change addresses (on the internal
-- chain) using mkSeqStateFromAccountXPub
prop_genChangeGapFromAccountXPub :: AddressPoolGap -> Property
prop_genChangeGapFromAccountXPub g = property $
    length (fst $ changeAddresses [] s0) === fromEnum g
  where
    mw = someDummyMnemonic (Proxy @12)
    rootXPrv = Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
    accIx = toEnum 0x80000000
    accXPub = publicKey ShelleyKeyS
        $ deriveAccountPrivateKey mempty rootXPrv accIx
    s0 = mkSeqStateFromAccountXPub accXPub Nothing purposeCIP1852 g IncreasingChangeAddresses

prop_changeAddressRotation
    :: SeqState 'Mainnet ShelleyKey
    -> Property
prop_changeAddressRotation s0 =
    property prop
  where
    (as, s') = changeAddresses [] s0
    prop =
        ShowFmt (fst $ changeAddresses [] s') === ShowFmt (reverse as)

prop_changeNoLock
    :: (SeqState 'Mainnet ShelleyKey, Int)
    -> Property
prop_changeNoLock (s0, ix) =
    ShowFmt xs =/= ShowFmt ys .&&. ShowFmt addr `notElem` (ShowFmt <$> ys)
  where
    g = AddressPool.gap . getPool $ internalPool s0
    (xs, s) = changeAddresses [] s0
    addr = xs !! (ix `mod` fromEnum g)
    (_, s') = isOurs addr s
    (ys, _) = changeAddresses [] s'

prop_lookupDiscovered
    :: (SeqState 'Mainnet ShelleyKey, Address)
    -> Property
prop_lookupDiscovered (s0, addr) =
    let (ours, s) = isOurs addr s0 in isJust ours ==> prop s
  where
    mw = someDummyMnemonic (Proxy @12)
    key = Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
    prop s = monadicIO $ liftIO $ do
        unless (isJust $ isOwned ShelleyWallet s (key, mempty) addr) $ do
            expectationFailure "couldn't find private key corresponding to addr"

{-------------------------------------------------------------------------------
                        Properties for CompareDiscovery
-------------------------------------------------------------------------------}

prop_compareKnownUnknown
    :: (SeqState 'Mainnet ShelleyKey, ShowFmt Address, ShowFmt Address)
    -> Property
prop_compareKnownUnknown (s, ShowFmt known, ShowFmt addr) =
    case (fst $ isOurs known s, fst $ isOurs addr s) of
        (Just{}, Nothing) -> cover 10 True "known-unknown" $ prop LT
        _ -> property True
  where
    prop ordering = compareDiscovery s known addr === ordering

prop_compareAntiSymmetric
    :: (SeqState 'Mainnet ShelleyKey, ShowFmt Address, ShowFmt Address)
    -> Property
prop_compareAntiSymmetric (s, ShowFmt a1, ShowFmt a2) =
    cover 90 (a1 /= a2) "a1 /= a2" prop
  where
    prop = compareDiscovery s a1 a2 === sym (compareDiscovery s a2 a1)
    sym = \case
        EQ -> EQ
        LT -> GT
        GT -> LT

{-------------------------------------------------------------------------------
                        Properties for KnownAddresses
-------------------------------------------------------------------------------}

fst' :: (Address, AddressState, NonEmpty DerivationIndex)
    -> Address
fst' (a,_,_) = a

pair' :: (Address, AddressState, NonEmpty DerivationIndex)
    -> (Address, AddressState)
pair' (a,s,_) = (a,s)

prop_knownAddressesAreOurs
    :: SeqState 'Mainnet ShelleyKey
    -> Property
prop_knownAddressesAreOurs s =
    map (\x -> (ShowFmt x, isJust $ fst $ isOurs x s)) (fst' <$> knownAddresses s)
    ===
    map (\x -> (ShowFmt x, True)) (fst' <$> knownAddresses s)

prop_atLeastKnownAddresses
    :: SeqState 'Mainnet ShelleyKey
    -> Property
prop_atLeastKnownAddresses s =
    property $ length (knownAddresses s) >= g (externalPool s)
  where
    g = fromEnum . AddressPool.gap . getPool

prop_changeIsOnlyKnownAfterGeneration
    :: ( SeqAddressPool 'UtxoInternal ShelleyKey
       , SeqAddressPool 'UtxoExternal ShelleyKey
       )
    -> ChangeAddressMode
    -> Property
prop_changeIsOnlyKnownAfterGeneration (intPool, extPool) changeAddr =
    let
        s0 :: SeqState 'Mainnet ShelleyKey
        s0 = SeqState intPool extPool emptyPendingIxs ourAccount
             Nothing rewardAccount defaultPrefix changeAddr
        addrs0 = pair' <$> knownAddresses s0
        (change, s1) = genChange (\k _ -> paymentAddress SMainnet k) s0
        addrs1 = fst' <$> knownAddresses s1
    in conjoin
        [ prop_addrsNotInInternalPool addrs0
        , prop_changeAddressIsKnown change addrs1
        ]
  where
    prop_addrsNotInInternalPool addrs =
        map (\(x, s) ->
                let notInPool = isNothing $ lookupAddress x intPool
                    isUsed = s == Used
                in (ShowFmt x, notInPool || isUsed))
            addrs
        ===
        map (\(x, _) -> (ShowFmt x, True)) addrs
    lookupAddress addrRaw (SeqAddressPool pool) =
        case paymentKeyFingerprint addrRaw of
            Left _ -> Nothing
            Right addr -> AddressPool.lookup addr pool
    prop_changeAddressIsKnown addr addrs =
        counterexample
            (show (ShowFmt addr) <> " not in " <> show (ShowFmt <$> addrs))
            (property (addr `elem` addrs))

prop_oursAreUsed
    :: SeqState 'Mainnet ShelleyKey
    -> Property
prop_oursAreUsed s =
    let
        (addr, status,_) = head $ knownAddresses s
        (True, s') = first isJust $ isOurs addr s
        (addr', status',_) = head $ knownAddresses s'
    in
        (status' == Used .&&. addr === addr')
        & label (show status)
        & counterexample (show (ShowFmt addr') <> ": " <> show status')

prop_oursUnexpectedPrefix
    :: SeqState 'Mainnet ShelleyKey
    -> UnexpectedPrefix
    -> Property
prop_oursUnexpectedPrefix s prefix =
    let
        (Address addr, _, _) = head $ knownAddresses s
        addr' = BS.cons (unWord8 prefix) (BS.tail addr)
    in
        first isJust (isOurs (Address addr') s) === (False, s)

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

class AddressPoolTest k where
    ourAccount
        :: k 'AccountK XPub
    ourAddresses
        :: Proxy k
        -> Role
        -> [Address]

instance AddressPoolTest IcarusKey where
    ourAccount = publicKey IcarusKeyS $
        Icarus.unsafeGenerateKeyFromSeed mw mempty
      where
        mw = someDummyMnemonic (Proxy @12)

    ourAddresses _proxy cc =
        mkAddress . deriveAddressPublicKey ourAccount cc <$> [minBound..maxBound]
      where
        mkAddress = paymentAddress @IcarusKey SMainnet

instance AddressPoolTest ShelleyKey where
    ourAccount = publicKey ShelleyKeyS $
        Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
      where
        mw = someDummyMnemonic (Proxy @15)

    ourAddresses _proxy cc =
        mkAddress . deriveAddressPublicKey ourAccount cc <$> [minBound..maxBound]
      where
        mkAddress k = delegationAddress SMainnet k rewardAccount

rewardAccount
    :: ShelleyKey 'CredFromKeyK XPub
rewardAccount = publicKey ShelleyKeyS $
    Shelley.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
  where
    mw = someDummyMnemonic (Proxy @15)

changeAddresses
    :: [Address]
    -> SeqState 'Mainnet ShelleyKey
    -> ([Address], SeqState 'Mainnet ShelleyKey)
changeAddresses as s =
    let (a, s') = genChange (\k _ -> paymentAddress SMainnet k) s
    in if a `elem` as then (as, s) else changeAddresses (a:as) s'

unsafeMkAddressPoolGap :: (Integral a, Show a) => a -> AddressPoolGap
unsafeMkAddressPoolGap g = case mkAddressPoolGap (fromIntegral g) of
    Right a -> a
    Left _ -> error $ "unsafeMkAddressPoolGap: bad argument: " <> show g

defaultPrefix :: DerivationPrefix
defaultPrefix = DerivationPrefix (purposeCIP1852, coinTypeAda, minBound)

{-------------------------------------------------------------------------------
                                Arbitrary Instances
-------------------------------------------------------------------------------}

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

instance Arbitrary AddressPoolGap where
    shrink _ = []
    arbitrary = mkUnboundedAddressPoolGap <$> choose (10, 20)

instance Arbitrary Role where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary AddressState where
    shrink _ = []
    arbitrary = genericArbitrary

instance Arbitrary DerivationPrefix where
    arbitrary = fmap DerivationPrefix $ (,,)
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary (Index 'Hardened depth) where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

-- | In this context, Arbitrary addresses are either some known addresses
-- derived from "our account key", or they just are some arbitrary addresses
-- that are unknown to us.
instance Arbitrary Address where
    shrink _ = []
    arbitrary = frequency
        [ (8, elements $ take 25 (ourAddresses (Proxy @ShelleyKey) UtxoExternal))
        , (8, elements $ take 25 (ourAddresses (Proxy @ShelleyKey) UtxoInternal))
        , (8, elements $ take 25 (ourAddresses (Proxy @ShelleyKey) MutableAccount))
        , (8, elements $ take 25 (ourAddresses (Proxy @IcarusKey) UtxoExternal))
        , (8, elements $ take 25 (ourAddresses (Proxy @IcarusKey) UtxoInternal))
        , (8, elements $ take 25 (ourAddresses (Proxy @IcarusKey) MutableAccount))
        ]

instance
    ( Typeable c
    , SupportsDiscovery 'Mainnet k
    , AddressPoolTest k
    , (k == SharedKey) ~ 'False
    ) => Arbitrary (SeqAddressPool c k) where
    shrink (SeqAddressPool pool) =
        SeqAddressPool <$> shrinkPool minGap pool
      where
        minGap = fromIntegral $ getAddressPoolGap minBound

    arbitrary = do
        gap <- unsafeMkAddressPoolGap <$> choose
            (getAddressPoolGap minBound, 2 * getAddressPoolGap minBound)
        let SeqAddressPool pool = newSeqAddressPool @'Mainnet @c ourAccount gap
        SeqAddressPool <$> genPool pool

instance Arbitrary (SeqState 'Mainnet ShelleyKey) where
    shrink (SeqState intPool extPool ixs acc policy rwd prefix change) =
        (\(i, e) -> SeqState i e ixs acc policy rwd prefix change)
            <$> shrink (intPool, extPool)
    arbitrary = do
        intPool <- arbitrary
        extPool <- arbitrary
        return $ SeqState intPool extPool emptyPendingIxs ourAccount
            Nothing rewardAccount defaultPrefix IncreasingChangeAddresses

-- | Wrapper to encapsulate keys.
data Key = forall (k :: Depth -> * -> *).
    ( Typeable k
    , Eq (k 'AccountK XPub)
    , Show (k 'AccountK XPub)
    , MkKeyFingerprint k (Proxy 'Mainnet, k 'CredFromKeyK XPub)
    , MkKeyFingerprint k Address
    , SoftDerivation k
    , AddressPoolTest k
    , GetPurpose k
    ) => Key (Proxy k)
instance Show Key where show (Key proxy) = show (typeRep proxy)

newtype UnexpectedPrefix = UnexpectedPrefix {unWord8 :: Word8}
    deriving (Eq, Show)

instance Arbitrary UnexpectedPrefix where
    arbitrary = do
        let baseAddr = 0b00000001       -- keyhash; keyhash, mainnet
            enterpriseAddr = 0b01100001 -- keyhash, mainnet
            rewardAcct = 0b11100001     -- keyhash, mainnet
            validPrefixesMainnet = [baseAddr, enterpriseAddr, rewardAcct]
        UnexpectedPrefix <$> arbitrary `suchThat` (`notElem` validPrefixesMainnet)

instance Arbitrary ChangeAddressMode where
    arbitrary = genericArbitrary
    shrink = genericShrink
