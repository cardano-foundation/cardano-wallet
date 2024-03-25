{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.Cardano.Write.TxSpec where

import Prelude

import Cardano.Api.Gen
    ( genTxIn
    , genTxOut
    )
import Cardano.Ledger.Api
    ( PParams
    , coinTxOutL
    , ppCoinsPerUTxOByteL
    )
import Control.Lens
    ( (&)
    , (.~)
    )
import Data.Default
    ( Default (..)
    )
import Internal.Cardano.Write.Tx
    ( AnyRecentEra
    , BabbageEra
    , ConwayEra
    , computeMinimumCoinForTxOut
    , datumHashFromBytes
    , datumHashToBytes
    , fromCardanoApiUTxO
    , isBelowMinimumCoinForTxOut
    , toCardanoApiUTxO
    )
import Test.Cardano.Ledger.Alonzo.Serialisation.Generators
    ()
import Test.Cardano.Ledger.Babbage.Arbitrary
    ()
import Test.Cardano.Ledger.Conway.Arbitrary
    ()
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldNotBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Arbitrary1 (liftArbitrary)
    , Property
    , arbitraryBoundedEnum
    , conjoin
    , counterexample
    , property
    , shrinkBoundedEnum
    , (===)
    )
import Test.QuickCheck.Classes
    ( boundedEnumLaws
    , eqLaws
    , showLaws
    )
import Test.Utils.Laws
    ( testLawsMany
    )

import qualified Cardano.Api as CardanoApi
import qualified Data.ByteString as BS
import qualified Data.Map as Map

spec :: Spec
spec = do
    describe "AnyRecentEra" $ do
        describe "Class instances obey laws" $ do
            testLawsMany @AnyRecentEra
                [ boundedEnumLaws
                , eqLaws
                , showLaws
                ]

    describe "DatumHash" $ do
        it "datumHashFromBytes . datumHashToBytes == Just"
            $ property $ \h -> do
                 let f = datumHashFromBytes . datumHashToBytes
                 f h === Just h

        describe "datumHashFromBytes goldens" $ do
            it "32 bytes -> Just" $ do
                datumHashFromBytes (BS.replicate 32 0)
                    `shouldNotBe` Nothing
            it "28 bytes -> Nothing" $ do
                datumHashFromBytes (BS.replicate 28 0)
                    `shouldBe` Nothing
            it "33 bytes -> Nothing" $ do
                datumHashFromBytes (BS.replicate 28 0)
                    `shouldBe` Nothing

    describe "TxOut" $ do
        describe "computeMinimumCoinForTxOut" $ do
            it "isBelowMinimumCoinForTxOut (setCoin (result <> delta)) \
               \ == False (Babbage)"
                $ property $ \out delta perByte -> do
                    let pp = (def :: PParams BabbageEra)
                            & ppCoinsPerUTxOByteL .~ perByte
                    let c = delta <> computeMinimumCoinForTxOut pp out
                    isBelowMinimumCoinForTxOut pp
                        (out & coinTxOutL .~ c )
                        === False

            it "isBelowMinimumCoinForTxOut (setCoin (result <> delta)) \
               \ == False (Conway)"
                $ property $ \out delta perByte -> do
                    let pp = (def :: PParams ConwayEra)
                            & ppCoinsPerUTxOByteL .~ perByte
                    let c = delta <> computeMinimumCoinForTxOut pp out
                    isBelowMinimumCoinForTxOut pp
                        (out & coinTxOutL .~ c)
                        === False

    describe "UTxO" $ do
        it "is isomorphic to CardanoApi.UTxO" $ do
            testIsomorphism
                (NamedFun
                    (toCardanoApiUTxO @BabbageEra)
                    "toCardanoApiUTxO")
                (NamedFun
                    (fromCardanoApiUTxO @BabbageEra)
                    "fromCardanoApiUTxO")
                id

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary AnyRecentEra where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

instance Arbitrary (CardanoApi.UTxO CardanoApi.BabbageEra) where
    arbitrary = CardanoApi.UTxO . Map.fromList <$> liftArbitrary genTxInOutEntry
      where
        genTxInOutEntry = (,)
            <$> genTxIn
            <*> genTxOut CardanoApi.BabbageEra

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Allows 'testIsomorphism' to be called more conveniently when short on
-- horizontal space, compared to a multiline "(a -> b, String)".
--
-- @@
--      (NamedFun
--          fun
--          "fun")
-- @@
--
-- vs
--
-- @@
--      ( fun
--      , "fun"
--      )
-- @@
data NamedFun a b = NamedFun (a -> b) String

-- | Tests @f . g == id@ and @g . f == id@
--
-- @@
--                 f
--      ┌───┐ ─────────▶ ┌───┐
--      │ a │            │ b │
--      │   │            │   │
--      └───┘ ◀───────── └───┘
--                 g
-- @@
testIsomorphism
    :: (Arbitrary a, Arbitrary b, Show a, Show b, Eq a, Eq b)
    => NamedFun a b
    -> NamedFun b a
    -> (b -> b) -- ^ Optional normalization, otherwise use @id@.
    -> Property
testIsomorphism (NamedFun f fName) (NamedFun g gName) normalize =
    conjoin
        [ counterexample
            (fName <> " . " <> gName <> " == id")
            (property $ \x -> f (g (normalize x)) === normalize x)
        , counterexample
            (gName <> " . " <> fName <> " == id")
            (property $ \x -> g (f x) === x)
        ]
