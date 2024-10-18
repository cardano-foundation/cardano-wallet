{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.Cardano.Write.TxSpec where

import Prelude

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
import Internal.Cardano.Write.Eras
    ( AnyRecentEra
    , Babbage
    , Conway
    )
import Internal.Cardano.Write.Tx
    ( computeMinimumCoinForTxOut
    , datumHashFromBytes
    , datumHashToBytes
    , isBelowMinimumCoinForTxOut
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

import qualified Data.ByteString as BS

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
                    let pp = (def :: PParams Babbage)
                            & ppCoinsPerUTxOByteL .~ perByte
                    let c = delta <> computeMinimumCoinForTxOut pp out
                    isBelowMinimumCoinForTxOut pp
                        (out & coinTxOutL .~ c )
                        === False

            it "isBelowMinimumCoinForTxOut (setCoin (result <> delta)) \
               \ == False (Conway)"
                $ property $ \out delta perByte -> do
                    let pp = (def :: PParams Conway)
                            & ppCoinsPerUTxOByteL .~ perByte
                    let c = delta <> computeMinimumCoinForTxOut pp out
                    isBelowMinimumCoinForTxOut pp
                        (out & coinTxOutL .~ c)
                        === False

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

instance Arbitrary AnyRecentEra where
    arbitrary = arbitraryBoundedEnum
    shrink = shrinkBoundedEnum

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
