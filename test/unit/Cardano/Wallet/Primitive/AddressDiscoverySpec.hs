{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.AddressDiscoverySpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.AddressDerivation
    ( ChangeChain (..)
    , Depth (..)
    , Key
    , Passphrase (..)
    , XPub
    , deriveAddressPublicKey
    , keyToAddress
    , publicKey
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( AddressPool
    , AddressPoolGap
    , MkAddressPoolGapError (..)
    , accountPubKey
    , addresses
    , changeChain
    , defaultAddressPoolGap
    , gap
    , lookupAddress
    , mkAddressPool
    , mkAddressPoolGap
    )
import Cardano.Wallet.Primitive.Types
    ( Address )
import Control.Monad
    ( forM )
import Control.Monad.Trans.State.Strict
    ( execState, state )
import Data.List
    ( elemIndex, (\\) )
import Data.Maybe
    ( isJust )
import Data.Proxy
    ( Proxy (..) )
import Data.Word
    ( Word8 )
import Test.Hspec
    ( Spec, describe, it )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , Property
    , arbitraryBoundedEnum
    , checkCoverage
    , choose
    , conjoin
    , cover
    , elements
    , expectFailure
    , frequency
    , property
    , withMaxSuccess
    , (===)
    , (==>)
    )
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
        textRoundtrip $ Proxy @AddressPoolGap

    describe "AddressPool" $ do
        it "'lookupAddressPool' extends the pool by a maximum of 'gap'"
            (checkCoverage prop_poolGrowWithinGap)
        it "'addresses' preserves the address order"
            (checkCoverage prop_roundtripMkAddressPool)
        it "An AddressPool always contains at least 'gap pool' addresses"
            (property prop_poolAtLeastGapAddresses)
        it "Our addresses are eventually discovered"
            (property prop_poolEventuallyDiscoverOurs)


{-------------------------------------------------------------------------------
                        Properties for AddressPoolGap
-------------------------------------------------------------------------------}

prop_mkAddressPoolGap
    :: Word8
    -> Property
prop_mkAddressPoolGap g =
    cover 25 isWithinBound "hits within bounds" prop
  where
    prop = case mkAddressPoolGap g of
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
    :: (AddressPool, Address)
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
    :: AddressPool
    -> Property
prop_roundtripMkAddressPool pool =
    ( mkAddressPool
        (accountPubKey pool)
        (gap pool)
        (changeChain pool)
        (addresses pool)
    ) === pool

-- | A pool always contains a number of addresses at least equal to its gap
prop_poolAtLeastGapAddresses
    :: AddressPool
    -> Property
prop_poolAtLeastGapAddresses pool =
    property prop
  where
    prop = length (addresses pool) >= fromEnum (gap pool)

-- | Our addresses are eventually discovered
prop_poolEventuallyDiscoverOurs
    :: (AddressPoolGap, ChangeChain, Address)
    -> Property
prop_poolEventuallyDiscoverOurs (g, cc, addr) =
    addr `elem` ours ==> withMaxSuccess 10 $ property prop
  where
    ours = take 25 (ourAddresses cc)
    pool = flip execState (mkAddressPool ourAccount g cc mempty) $
        forM ours (state . lookupAddress)
    prop = (fromEnum <$> fst (lookupAddress addr pool)) === elemIndex addr ours


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
    keyToAddress . deriveAddressPublicKey ourAccount cc <$> [minBound..maxBound]

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
            return $ keyToAddress $ publicKey xprv

instance Arbitrary AddressPool where
    shrink pool =
        let
            key = accountPubKey pool
            g = gap pool
            cc = changeChain pool
            addrs = addresses pool
        in case length addrs of
            k | k == fromEnum g && g == minBound ->
                []
            k | k == fromEnum g && g > minBound ->
                [ mkAddressPool key minBound cc [] ]
            k ->
                [ mkAddressPool key minBound cc []
                , mkAddressPool key g cc []
                , mkAddressPool key g cc (take (k - (fromEnum g `div` 5)) addrs)
                ]
    arbitrary = do
        g <- arbitrary
        n <- choose (0, 2 * fromEnum g)
        cc <- arbitrary
        let addrs = take n (ourAddresses cc)
        return $ mkAddressPool ourAccount g cc addrs
