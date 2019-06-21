{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscoverySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.DummyTarget.Primitive.Types
    ( DummyTarget )
import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Depth (..)
    , Key
    , KeyToAddress (..)
    , Passphrase (..)
    , XPub
    , deriveAddressPublicKey
    , publicKey
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPool
    , AddressPoolGap (..)
    , CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    , MkAddressPoolGapError (..)
    , SeqState (..)
    , accountPubKey
    , addresses
    , changeChain
    , defaultAddressPoolGap
    , emptyPendingIxs
    , gap
    , genChange
    , lookupAddress
    , mkAddressPool
    , mkAddressPoolGap
    , mkSeqState
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), ShowFmt (..) )
import Control.Monad
    ( forM, unless )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.State.Strict
    ( execState, state )
import Data.ByteString
    ( ByteString )
import Data.List
    ( elemIndex, (\\) )
import Data.Maybe
    ( isJust, isNothing )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( TextDecodingError (..), fromText )
import Data.Typeable
    ( Typeable )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, expectationFailure, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
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
    , property
    , withMaxSuccess
    , (.&&.)
    , (=/=)
    , (===)
    , (==>)
    )
import Test.QuickCheck.Monadic
    ( monadicIO )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Data.ByteArray as BA
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

    describe "AddressPool ExternalChain" $ do
        it "'lookupAddressPool' extends the pool by a maximum of 'gap'"
            (checkCoverage (prop_poolGrowWithinGap @'ExternalChain))
        it "'addresses' preserves the address order"
            (checkCoverage (prop_roundtripMkAddressPool @'ExternalChain))
        it "An AddressPool always contains at least 'gap pool' addresses"
            (property (prop_poolAtLeastGapAddresses @'ExternalChain))
        it "Our addresses are eventually discovered"
            (property (prop_poolEventuallyDiscoverOurs @'ExternalChain))

    describe "AddressPool InternalChain" $ do
        it "'lookupAddressPool' extends the pool by a maximum of 'gap'"
            (checkCoverage (prop_poolGrowWithinGap @'InternalChain))
        it "'addresses' preserves the address order"
            (checkCoverage (prop_roundtripMkAddressPool @'InternalChain))
        it "An AddressPool always contains at least 'gap pool' addresses"
            (property (prop_poolAtLeastGapAddresses @'InternalChain))
        it "Our addresses are eventually discovered"
            (property (prop_poolEventuallyDiscoverOurs @'InternalChain))

    describe "AddressPoolGap - Text Roundtrip" $ do
        textRoundtrip $ Proxy @AddressPoolGap
        let err = "An address pool gap must be a natural number between 10 and 100."
        it "fail fromText @AddressPoolGap \"-10\"" $
            fromText @AddressPoolGap "-10" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"0\"" $
            fromText @AddressPoolGap "0" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"9\"" $
            fromText @AddressPoolGap "9" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"101\"" $
            fromText @AddressPoolGap "101" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"20eiei\"" $
            fromText @AddressPoolGap "20eiei" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"raczej nie\"" $
            fromText @AddressPoolGap "raczej nie" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"-1000\"" $
            fromText @AddressPoolGap "-1000" === Left (TextDecodingError err)
        it "fail fromText @AddressPoolGap \"2.5\"" $
            fromText @AddressPoolGap "2.5" === Left (TextDecodingError err)

    describe "PendingIxs & GenChange" $ do
        it "Can always generate exactly `gap` different change addresses"
            (property prop_genChangeGap)
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
    :: (Typeable chain)
    => (AddressPool DummyTarget chain, Address)
    -> Property
prop_poolGrowWithinGap (pool, addr) =
    cover 10 (isJust $ fst res) "pool hit" prop
  where
    res = lookupAddress addr pool
    prop = case res of
        (Nothing, pool') -> pool === pool'
        (Just _, pool') ->
            let k = length $ addresses pool' \\ addresses pool
            in conjoin
                [ gap pool === gap pool'
                , property (k >= 0 && k <= fromEnum (gap pool))
                ]

-- | A pool gives back its addresses in correct order and can be reconstructed
prop_roundtripMkAddressPool
    :: (Typeable chain)
    => AddressPool DummyTarget chain
    -> Property
prop_roundtripMkAddressPool pool =
    ( mkAddressPool
        (accountPubKey pool)
        (gap pool)
        (addresses pool)
    ) === pool

-- | A pool always contains a number of addresses at least equal to its gap
prop_poolAtLeastGapAddresses
    :: AddressPool DummyTarget chain
    -> Property
prop_poolAtLeastGapAddresses pool =
    property prop
  where
    prop = length (addresses pool) >= fromEnum (gap pool)

-- | Our addresses are eventually discovered
prop_poolEventuallyDiscoverOurs
    :: forall (chain :: ChangeChain). (Typeable chain)
    => (AddressPoolGap, Address)
    -> Property
prop_poolEventuallyDiscoverOurs (g, addr) =
    addr `elem` ours ==> withMaxSuccess 10 $ property prop
  where
    ours = take 25 (ourAddresses (changeChain @chain))
    pool = flip execState (mkAddressPool @DummyTarget @chain ourAccount g mempty) $
        forM ours (state . lookupAddress)
    prop = (fromEnum <$> fst (lookupAddress addr pool)) === elemIndex addr ours

{-------------------------------------------------------------------------------
                    Properties for AddressScheme & PendingIxs
-------------------------------------------------------------------------------}

-- | We can always generate at exactly `gap` change addresses (on the internal
-- chain)
prop_genChangeGap
    :: AddressPoolGap
    -> Property
prop_genChangeGap g =
    property prop
  where
    seed = Passphrase (BA.convert @ByteString "0000000000000000")
    key = unsafeGenerateKeyFromSeed (seed, mempty) mempty
    s0 = mkSeqState (key, mempty) g
    prop =
        length (fst $ changeAddresses [] s0) === fromEnum g

prop_changeAddressRotation
    :: SeqState DummyTarget
    -> Property
prop_changeAddressRotation s0 =
    property prop
  where
    (as, s') = changeAddresses [] s0
    prop =
        ShowFmt (fst $ changeAddresses [] s') === ShowFmt (reverse as)

prop_changeNoLock
    :: (SeqState DummyTarget, Int)
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
    :: (SeqState DummyTarget, Address)
    -> Property
prop_lookupDiscovered (s0, addr) =
    let (ours, s) = isOurs addr s0 in ours ==> prop s
  where
    key = unsafeGenerateKeyFromSeed (mempty, mempty) mempty
    prop s = monadicIO $ liftIO $ do
        unless (isJust $ isOwned s (key, mempty) addr) $ do
            expectationFailure "couldn't find private key corresponding to addr"


{-------------------------------------------------------------------------------
                        Properties for CompareDiscovery
-------------------------------------------------------------------------------}

prop_compareKnownUnknown
    :: (SeqState DummyTarget, ShowFmt Address, ShowFmt Address)
    -> Property
prop_compareKnownUnknown (s, ShowFmt known, ShowFmt addr) =
    case (fst $ isOurs known s, fst $ isOurs addr s) of
        (True, False) -> cover 10 True "known-unknown" $ prop LT
        _ -> property True
  where
    prop ordering = compareDiscovery s known addr === ordering

prop_compareAntiSymmetric
    :: (SeqState DummyTarget, ShowFmt Address, ShowFmt Address)
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
    :: SeqState DummyTarget
    -> Property
prop_knownAddressesAreOurs s =
    map (\x -> (ShowFmt x, fst (isOurs x s))) (knownAddresses s)
    ===
    map (\x -> (ShowFmt x, True)) (knownAddresses s)

prop_atLeastKnownAddresses
    :: SeqState DummyTarget
    -> Property
prop_atLeastKnownAddresses s =
    property $ length (knownAddresses s) >= g (externalPool s)
  where
    g = fromEnum . getAddressPoolGap . gap

prop_changeIsOnlyKnownAfterGeneration
    :: ( AddressPool DummyTarget 'InternalChain
       , AddressPool DummyTarget 'ExternalChain
       )
    -> Property
prop_changeIsOnlyKnownAfterGeneration (intPool, extPool) =
    let
        s0 = SeqState intPool extPool emptyPendingIxs
        addrs0 = knownAddresses s0
        (change, s1) = genChange s0
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
                                Arbitrary Instances
-------------------------------------------------------------------------------}

ourAccount
    :: Key 'AccountK XPub
ourAccount = publicKey $ unsafeGenerateKeyFromSeed (seed, mempty) mempty
  where
    seed = Passphrase $ BA.convert $ BS.replicate 32 0

ourAddresses
    :: ChangeChain
    -> [Address]
ourAddresses cc =
    keyToAddress @DummyTarget . deriveAddressPublicKey ourAccount cc
        <$> [minBound..maxBound]

changeAddresses
    :: [Address]
    -> SeqState DummyTarget
    -> ([Address], SeqState DummyTarget)
changeAddresses as s =
    let (a, s') = genChange s
    in if a `elem` as then (as, s) else changeAddresses (a:as) s'

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

instance Arbitrary AddressPoolGap where
    shrink _ = []
    arbitrary = arbitraryBoundedEnum

instance Arbitrary ChangeChain where
    shrink _ = []
    arbitrary = elements [InternalChain, ExternalChain]

-- | In this context, Arbitrary addresses are either some known addresses
-- derived from "our account key", or they just are some arbitrary addresses
-- that are unknown to us.
instance Arbitrary Address where
    shrink _ = []
    arbitrary = frequency
        [ (8, elements $ take 25 (ourAddresses ExternalChain))
        , (8, elements $ take 25 (ourAddresses InternalChain))
        , (1, notOurs)
        ]
      where
        notOurs = do
            bytes <- Passphrase . BA.convert . BS.pack . take 32 . getInfiniteList
                <$> arbitrary
            let xprv = unsafeGenerateKeyFromSeed (bytes, mempty) mempty
            return $ keyToAddress @DummyTarget $ publicKey xprv

instance Typeable chain => Arbitrary (AddressPool DummyTarget chain) where
    shrink pool =
        let
            key = accountPubKey pool
            g = gap pool
            addrs = addresses pool
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

instance Arbitrary (SeqState DummyTarget) where
    shrink (SeqState intPool extPool ixs) =
        (\(i, e) -> SeqState i e ixs) <$> shrink (intPool, extPool)
    arbitrary = do
        intPool <- arbitrary
        extPool <- arbitrary
        return $ SeqState intPool extPool emptyPendingIxs

unsafeMkAddressPoolGap :: (Integral a, Show a) => a -> AddressPoolGap
unsafeMkAddressPoolGap g = case (mkAddressPoolGap $ fromIntegral g) of
    Right a -> a
    Left _ -> error $ "unsafeMkAddressPoolGap: bad argument: " <> show g
