{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscovery.SequentialSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPub )
import Cardano.Wallet.Primitive.AddressDerivation
    ( AccountingStyle (..)
    , DelegationAddress (..)
    , Depth (..)
    , HardDerivation (..)
    , KeyFingerprint
    , MkKeyFingerprint (..)
    , NetworkDiscriminant (..)
    , PaymentAddress (..)
    , SoftDerivation (..)
    , WalletKey (..)
    )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey (..) )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey (..) )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    , genChange
    )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( AddressPool
    , AddressPoolGap (..)
    , MkAddressPoolGapError (..)
    , SeqState (..)
    , accountPubKey
    , accountingStyle
    , addresses
    , defaultAddressPoolGap
    , emptyPendingIxs
    , gap
    , lookupAddress
    , mkAddressPool
    , mkAddressPoolGap
    , mkSeqStateFromAccountXPub
    , mkSeqStateFromRootXPrv
    , mkUnboundedAddressPoolGap
    , shrinkPool
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), ShowFmt (..) )
import Cardano.Wallet.Unsafe
    ( someDummyMnemonic )
import Control.Monad
    ( forM, forM_, unless )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.State.Strict
    ( execState, state )
import Data.List
    ( elemIndex, (\\) )
import Data.Maybe
    ( isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..), fromText )
import Data.Typeable
    ( Typeable, typeRep )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, expectationFailure, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , Positive (..)
    , Property
    , arbitraryBoundedEnum
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , classify
    , conjoin
    , counterexample
    , cover
    , elements
    , expectFailure
    , frequency
    , label
    , property
    , (.&&.)
    , (=/=)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Icarus as Icarus
import qualified Cardano.Wallet.Primitive.AddressDerivation.Jormungandr as Jormungandr

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

    let styles =
            [ Style (Proxy @'UTxOExternal)
            , Style (Proxy @'UTxOInternal)
            , Style (Proxy @'MutableAccount)
            ]

    let keys =
            [ Key (Proxy @JormungandrKey)
            , Key (Proxy @IcarusKey)
            ]

    describe "AddressPool (Jormungandr)" $ do
        forM_ styles $ \s@(Style proxyS) -> forM_ keys $ \k@(Key proxyK) -> do
            describe (show k <> " " <> show s) $ do
                it "'lookupAddressPool' extends the pool by a maximum of 'gap'"
                    (checkCoverage (prop_poolGrowWithinGap (proxyS, proxyK)))
                it "'addresses' preserves the address order"
                    (checkCoverage (prop_roundtripMkAddressPool (proxyS, proxyK)))
                it "An AddressPool always contains at least 'gap pool' addresses"
                    (property (prop_poolAtLeastGapAddresses (proxyS, proxyK)))
                it "Our addresses are eventually discovered"
                    (property (prop_poolEventuallyDiscoverOurs (proxyS, proxyK)))
                it "Known addresses are still in a shrunk pool"
                    (property (prop_shrinkPreserveKnown (proxyS, proxyK)))
                it "Last address from a shrunk is the last known"
                    (property (prop_shrinkMaxIndex (proxyS, proxyK)))

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
                          Properties for AddressPool
-------------------------------------------------------------------------------}

-- | After a lookup, a property should never grow more than its gap value.
prop_poolGrowWithinGap
    :: forall (chain :: AccountingStyle) k.
        ( Typeable chain
        , Eq (k 'AccountK XPub)
        , Show (k 'AccountK XPub)
        , MkKeyFingerprint k (Proxy 'Mainnet, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , AddressPoolTest k
        )
    => (Proxy chain, Proxy k)
    -> (AddressPool chain k, Address)
    -> Property
prop_poolGrowWithinGap _proxy (pool, addr) =
    cover 7.5 (isJust $ fst res) "pool hit" prop
  where
    res = lookupAddress @'Mainnet addr pool
    prop = case res of
        (Nothing, pool') -> pool === pool'
        (Just _, pool') ->
            let k = length $ addresses liftAddress pool' \\ addresses liftAddress pool
            in conjoin
                [ gap pool === gap pool'
                , property (k >= 0 && k <= fromEnum (gap pool))
                ]

-- | A pool gives back its addresses in correct order and can be reconstructed
prop_roundtripMkAddressPool
    :: forall (chain :: AccountingStyle) k.
        ( Typeable chain
        , Eq (k 'AccountK XPub)
        , Show (k 'AccountK XPub)
        , MkKeyFingerprint k (Proxy 'Mainnet, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , AddressPoolTest k
        )
    => (Proxy chain, Proxy k)
    -> AddressPool chain k
    -> Property
prop_roundtripMkAddressPool _proxy pool =
    ( mkAddressPool @'Mainnet
        (accountPubKey pool)
        (gap pool)
        (addresses liftAddress pool)
    ) === pool

-- | A pool always contains a number of addresses at least equal to its gap
prop_poolAtLeastGapAddresses
    :: forall (chain :: AccountingStyle) k.
        ( AddressPoolTest k
        )
    => (Proxy chain, Proxy k)
    -> AddressPool chain k
    -> Property
prop_poolAtLeastGapAddresses _proxy pool =
    property prop
  where
    prop = length (addresses liftAddress pool) >= fromEnum (gap pool)

-- | Our addresses are eventually discovered
prop_poolEventuallyDiscoverOurs
    :: forall (chain :: AccountingStyle) k.
        ( Typeable chain
        , MkKeyFingerprint k (Proxy 'Mainnet, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , AddressPoolTest k
        )
    => (Proxy chain, Proxy k)
    -> (AddressPoolGap, Address)
    -> Property
prop_poolEventuallyDiscoverOurs _proxy (g, addr) =
    if addr `elem` ours then property $
        (fromEnum <$> fst (lookupAddress @'Mainnet addr pool)) === elemIndex addr ours
    else
        label "address not ours" (property True)
  where
    ours = take 25 (ourAddresses (Proxy @k) (accountingStyle @chain))
    pool = flip execState (mkAddressPool @'Mainnet @chain @k ourAccount g mempty) $
        forM ours (state . lookupAddress @'Mainnet)

{-------------------------------------------------------------------------------
                    Properties for AddressScheme & PendingIxs
-------------------------------------------------------------------------------}

-- | We can always generate at exactly `gap` change addresses (on the internal
-- chain) using mkSeqStateFromRootXPrv
prop_genChangeGapFromRootXPrv
    :: AddressPoolGap
    -> Property
prop_genChangeGapFromRootXPrv g =
    property prop
  where
    mw = someDummyMnemonic (Proxy @12)
    key = Jormungandr.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
    s0 = mkSeqStateFromRootXPrv (key, mempty) g
    prop =
        length (fst $ changeAddresses [] s0) === fromEnum g

-- | We can always generate at exactly `gap` change addresses (on the internal
-- chain) using mkSeqStateFromAccountXPub
prop_genChangeGapFromAccountXPub
    :: AddressPoolGap
    -> Property
prop_genChangeGapFromAccountXPub g =
    property prop
  where
    mw = someDummyMnemonic (Proxy @12)
    rootXPrv = Jormungandr.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
    accIx = toEnum 0x80000000
    accXPub = publicKey $ deriveAccountPrivateKey mempty rootXPrv accIx
    s0 = mkSeqStateFromAccountXPub accXPub g
    prop =
        length (fst $ changeAddresses [] s0) === fromEnum g

prop_changeAddressRotation
    :: SeqState 'Mainnet JormungandrKey
    -> Property
prop_changeAddressRotation s0 =
    property prop
  where
    (as, s') = changeAddresses [] s0
    prop =
        ShowFmt (fst $ changeAddresses [] s') === ShowFmt (reverse as)

prop_changeNoLock
    :: (SeqState 'Mainnet JormungandrKey, Int)
    -> Property
prop_changeNoLock (s0, ix) =
    ShowFmt xs =/= ShowFmt ys .&&. ShowFmt addr `notElem` (ShowFmt <$> ys)
  where
    g = gap $ internalPool s0
    (xs, s) = changeAddresses [] s0
    addr = xs !! (ix `mod` fromEnum g)
    (_, s') = isOurs addr s
    (ys, _) = changeAddresses [] s'

prop_lookupDiscovered
    :: (SeqState 'Mainnet JormungandrKey, Address)
    -> Property
prop_lookupDiscovered (s0, addr) =
    let (ours, s) = isOurs addr s0 in ours ==> prop s
  where
    mw = someDummyMnemonic (Proxy @12)
    key = Jormungandr.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
    prop s = monadicIO $ liftIO $ do
        unless (isJust $ isOwned s (key, mempty) addr) $ do
            expectationFailure "couldn't find private key corresponding to addr"


{-------------------------------------------------------------------------------
                        Properties for CompareDiscovery
-------------------------------------------------------------------------------}

prop_compareKnownUnknown
    :: (SeqState 'Mainnet JormungandrKey, ShowFmt Address, ShowFmt Address)
    -> Property
prop_compareKnownUnknown (s, ShowFmt known, ShowFmt addr) =
    case (fst $ isOurs known s, fst $ isOurs addr s) of
        (True, False) -> cover 10 True "known-unknown" $ prop LT
        _ -> property True
  where
    prop ordering = compareDiscovery s known addr === ordering

prop_compareAntiSymmetric
    :: (SeqState 'Mainnet JormungandrKey, ShowFmt Address, ShowFmt Address)
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

prop_knownAddressesAreOurs
    :: SeqState 'Mainnet JormungandrKey
    -> Property
prop_knownAddressesAreOurs s =
    map (\x -> (ShowFmt x, fst (isOurs x s))) (knownAddresses s)
    ===
    map (\x -> (ShowFmt x, True)) (knownAddresses s)

prop_atLeastKnownAddresses
    :: SeqState 'Mainnet JormungandrKey
    -> Property
prop_atLeastKnownAddresses s =
    property $ length (knownAddresses s) >= g (externalPool s)
  where
    g = fromEnum . getAddressPoolGap . gap

prop_changeIsOnlyKnownAfterGeneration
    :: ( AddressPool 'UTxOInternal JormungandrKey
       , AddressPool 'UTxOExternal JormungandrKey
       )
    -> Property
prop_changeIsOnlyKnownAfterGeneration (intPool, extPool) =
    let
        s0 :: SeqState 'Mainnet JormungandrKey
        s0 = SeqState intPool extPool emptyPendingIxs rewardAccount
        addrs0 = knownAddresses s0
        (change, s1) = genChange (\k _ -> paymentAddress @'Mainnet k) s0
        addrs1 = knownAddresses s1
    in conjoin
        [ prop_addrsNotInInternalPool addrs0
        , prop_changeAddressIsKnown change addrs1
        ]
  where
    prop_addrsNotInInternalPool addrs =
        map (\x -> (ShowFmt x, isNothing $ fst $ lookupAddress x intPool)) addrs
        ===
        map (\x -> (ShowFmt x, True)) addrs
    prop_changeAddressIsKnown addr addrs =
        counterexample
            (show (ShowFmt addr) <> " not in " <> show (ShowFmt <$> addrs))
            (property (addr `elem` addrs))

{-------------------------------------------------------------------------------
                        Properties for shrinkPool
-------------------------------------------------------------------------------}

-- Make sure that, for any cut we take from an existing pool, the addresses
-- from the cut are all necessarily in the pool.
prop_shrinkPreserveKnown
    :: forall (chain :: AccountingStyle) k.
        ( Typeable chain
        , MkKeyFingerprint k (Proxy 'Mainnet, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , AddressPoolTest k
        )
    => (Proxy chain, Proxy k)
    -> Positive Int
    -> AddressPool chain k
    -> Property
prop_shrinkPreserveKnown _proxy (Positive size) pool =
    property
        $ classify (length addrs' < length addrs) "pool is smaller"
        $ all (`elem` addrs') cut
  where
    pool'  = shrinkPool @'Mainnet liftAddress cut minBound pool
    addrs  = addresses liftAddress pool
    addrs' = addresses liftAddress pool'
    cut    = take size addrs

-- There's no address after the address from the cut with the highest index
prop_shrinkMaxIndex
    :: forall (chain :: AccountingStyle) k.
        ( Typeable chain
        , MkKeyFingerprint k (Proxy 'Mainnet, k 'AddressK XPub)
        , MkKeyFingerprint k Address
        , SoftDerivation k
        , AddressPoolTest k
        )
    => (Proxy chain, Proxy k)
    -> Positive Int
    -> AddressPool chain k
    -> Property
prop_shrinkMaxIndex _proxy (Positive size) pool =
    fromIntegral size > getAddressPoolGap minBound ==> last cut === last addrs'
  where
    pool'  = shrinkPool @'Mainnet liftAddress cut minBound pool
    addrs  = addresses liftAddress pool
    addrs' = addresses liftAddress pool'
    cut    = take size addrs

{-------------------------------------------------------------------------------
                                 Helpers
-------------------------------------------------------------------------------}

class AddressPoolTest k where
    ourAccount
        :: k 'AccountK XPub
    ourAddresses
        :: Proxy k
        -> AccountingStyle
        -> [Address]
    liftAddress
        :: KeyFingerprint "payment" k
        -> Address

instance AddressPoolTest IcarusKey where
    ourAccount = publicKey $
        Icarus.unsafeGenerateKeyFromSeed mw mempty
      where
        mw = someDummyMnemonic (Proxy @12)

    ourAddresses _proxy cc =
        mkAddress . deriveAddressPublicKey ourAccount cc <$> [minBound..maxBound]
      where
        mkAddress = paymentAddress @'Mainnet @IcarusKey

    liftAddress =
        liftPaymentAddress @'Mainnet

instance AddressPoolTest JormungandrKey where
    ourAccount = publicKey $
        Jormungandr.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
      where
        mw = someDummyMnemonic (Proxy @15)

    ourAddresses _proxy cc =
        mkAddress . deriveAddressPublicKey ourAccount cc <$> [minBound..maxBound]
      where
        mkAddress k = delegationAddress @'Mainnet k rewardAccount

    liftAddress fingerprint =
        liftDelegationAddress @'Mainnet fingerprint rewardAccount

rewardAccount
    :: JormungandrKey 'AddressK XPub
rewardAccount = publicKey $
    Jormungandr.unsafeGenerateKeyFromSeed (mw, Nothing) mempty
  where
    mw = someDummyMnemonic (Proxy @15)

changeAddresses
    :: [Address]
    -> SeqState 'Mainnet JormungandrKey
    -> ([Address], SeqState 'Mainnet JormungandrKey)
changeAddresses as s =
    let (a, s') = genChange (\k _ -> paymentAddress @'Mainnet k) s
    in if a `elem` as then (as, s) else changeAddresses (a:as) s'

unsafeMkAddressPoolGap :: (Integral a, Show a) => a -> AddressPoolGap
unsafeMkAddressPoolGap g = case (mkAddressPoolGap $ fromIntegral g) of
    Right a -> a
    Left _ -> error $ "unsafeMkAddressPoolGap: bad argument: " <> show g

{-------------------------------------------------------------------------------
                                Arbitrary Instances
-------------------------------------------------------------------------------}

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

instance Arbitrary AddressPoolGap where
    shrink _ = []
    arbitrary = mkUnboundedAddressPoolGap <$> choose (10, 20)

instance Arbitrary AccountingStyle where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

-- | In this context, Arbitrary addresses are either some known addresses
-- derived from "our account key", or they just are some arbitrary addresses
-- that are unknown to us.
instance Arbitrary Address where
    shrink _ = []
    arbitrary = frequency
        [ (8, elements $ take 25 (ourAddresses (Proxy @JormungandrKey) UTxOExternal))
        , (8, elements $ take 25 (ourAddresses (Proxy @JormungandrKey) UTxOInternal))
        , (8, elements $ take 25 (ourAddresses (Proxy @JormungandrKey) MutableAccount))
        , (8, elements $ take 25 (ourAddresses (Proxy @IcarusKey) UTxOExternal))
        , (8, elements $ take 25 (ourAddresses (Proxy @IcarusKey) UTxOInternal))
        , (8, elements $ take 25 (ourAddresses (Proxy @IcarusKey) MutableAccount))
        ]

instance
    ( Typeable chain
    , MkKeyFingerprint k (Proxy 'Mainnet, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , SoftDerivation k
    , AddressPoolTest k
    ) => Arbitrary (AddressPool chain k) where
    shrink pool =
        let
            key = accountPubKey pool
            g = gap pool
            addrs = addresses liftAddress pool
        in case length addrs of
            k | k == fromEnum g && g == minBound ->
                []
            k | k == fromEnum g && g > minBound ->
                [ mkAddressPool @'Mainnet key minBound [] ]
            k ->
                [ mkAddressPool @'Mainnet key minBound []
                , mkAddressPool @'Mainnet key g []
                , mkAddressPool @'Mainnet key g (take (k - (fromEnum g `div` 5)) addrs)
                ]

    arbitrary = do
        g <- unsafeMkAddressPoolGap <$> choose
            (getAddressPoolGap minBound, 2 * getAddressPoolGap minBound)
        n <- choose (0, 2 * fromEnum g)
        let addrs = take n (ourAddresses (Proxy @k) (accountingStyle @chain))
        return $ mkAddressPool @'Mainnet ourAccount g addrs

instance Arbitrary (SeqState 'Mainnet JormungandrKey) where
    shrink (SeqState intPool extPool ixs rwd) =
        (\(i, e) -> SeqState i e ixs rwd) <$> shrink (intPool, extPool)
    arbitrary = do
        intPool <- arbitrary
        extPool <- arbitrary
        return $ SeqState intPool extPool emptyPendingIxs rewardAccount

-- | Wrapper to encapsulate accounting style proxies that are so-to-speak,
-- different types in order to easily map over them and avoid duplicating
-- properties.
data Style =
    forall (chain :: AccountingStyle). Typeable chain => Style (Proxy chain)
instance Show Style where show (Style proxy) = show (typeRep proxy)

-- | Wrapper to encapsulate keys.
data Key = forall (k :: Depth -> * -> *).
    ( Typeable k
    , Eq (k 'AccountK XPub)
    , Show (k 'AccountK XPub)
    , MkKeyFingerprint k (Proxy 'Mainnet, k 'AddressK XPub)
    , MkKeyFingerprint k Address
    , SoftDerivation k
    , AddressPoolTest k
    ) => Key (Proxy k)
instance Show Key where show (Key proxy) = show (typeRep proxy)
