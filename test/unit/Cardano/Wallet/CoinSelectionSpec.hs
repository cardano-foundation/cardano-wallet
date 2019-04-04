{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelectionSpec
    ( spec
    , Fixture(..)
    , coinSelectionUnitTest
    , CoveringCase(..)
    ) where

-- | This module contains shared logic between the coin selection tests. They
-- ought to share the same interface, and therefore, it makes sense for them to
-- also require the same arbitrary instances and instrument testing in a similar
-- way for both.

import Prelude

import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), CoinSelectionError (..), CoinSelectionOptions (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), TxIn (..), TxOut (..), UTxO (..) )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Word
    ( Word64, Word8 )
import Test.Hspec
    ( Spec, SpecWith, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose, generate, oneof, scale, vectorOf )

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map


spec :: Spec
spec = return ()

-- | A fixture for testing the coin selection
data Fixture = Fixture
    { maxNumOfInputs :: Word64
        -- ^ Maximum number of inputs that can be selected
    , utxoInputs :: [Word64]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , txOutputs :: NonEmpty Word64
        -- ^ Value (in Lovelace) & number of requested outputs
    } deriving Show

-- | Data for running
newtype CoveringCase = CoveringCase { getCoveringCase :: (UTxO, NonEmpty TxOut)}
    deriving Show

-- | Generate a 'UTxO' and 'TxOut' matching the given 'Fixture', and perform
-- given coin selection on it.
coinSelectionUnitTest
    :: ( CoinSelectionOptions
         -> UTxO
         -> NonEmpty TxOut
         -> ExceptT CoinSelectionError IO CoinSelection
       )
    -> String
    -> Either CoinSelectionError [Word64]
    -> Fixture
    -> SpecWith ()
coinSelectionUnitTest run lbl expected (Fixture n utxoCoins txOutsCoins) =
    it title $ do
        (utxo,txOuts) <- setup
        result <- runExceptT $ do
            CoinSelection inps _ _ <-
                run (CoinSelectionOptions n) utxo txOuts
            return $ map (getCoin . coin . snd) inps
        result `shouldBe` expected
  where
    title :: String
    title = mempty
        <> "max=" <> show n
        <> ", UTxO=" <> show utxoCoins
        <> ", Output=" <> show (NE.toList txOutsCoins)
        <> " --> " <> show expected
        <> if null lbl then "" else " (" <> lbl <> ")"

    setup :: IO (UTxO, NonEmpty TxOut)
    setup = do
        ins <- generate $ vectorOf (L.length utxoCoins) arbitrary
        addrs <- generate $ vectorOf (L.length utxoCoins) arbitrary
        let utxo = UTxO $ Map.fromList
                $ L.zip ins
                $ L.zipWith TxOut addrs
                $ map Coin utxoCoins
        txOutsAddrs <- generate $ vectorOf (L.length txOutsCoins) arbitrary
        let txOuts = NE.zipWith TxOut (NE.fromList txOutsAddrs)
                $ NE.map Coin txOutsCoins
        pure (utxo, txOuts)

{-------------------------------------------------------------------------------
                            Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary CoveringCase where
    arbitrary = do
        n <- choose (1, 10)
        txOutsNonEmpty <- NE.fromList <$> vectorOf n arbitrary
        utxo <- arbitrary
        return $ CoveringCase (utxo, txOutsNonEmpty)

instance Arbitrary Address where
    -- No Shrinking
    arbitrary = oneof
        [ pure $ Address "ADDR01"
        , pure $ Address "ADDR02"
        , pure $ Address "ADDR03"
        ]

instance Arbitrary Coin where
    -- No Shrinking
    arbitrary = Coin <$> choose (1, 100000)

instance Arbitrary TxIn where
    -- No Shrinking
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary (Hash "Tx") where
    -- No Shrinking
    arbitrary = do
        wds <- vectorOf 10 arbitrary :: Gen [Word8]
        let bs = BS.pack wds
        pure $ Hash bs

instance Arbitrary TxOut where
    -- No Shrinking
    arbitrary = TxOut
        <$> arbitrary
        <*> arbitrary

instance Arbitrary UTxO where
    shrink (UTxO utxo) = UTxO <$> shrink utxo
    arbitrary = do
        n <- choose (1, 100)
        utxo <- zip
            <$> vectorOf n arbitrary
            <*> vectorOf n arbitrary
        return $ UTxO $ Map.fromList utxo
