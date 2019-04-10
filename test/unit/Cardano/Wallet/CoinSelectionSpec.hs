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
    ( CoinSelection (..)
    , CoinSelectionError (..)
    , CoinSelectionOptions (..)
    , FeeError (..)
    , FeeOptions (..)
    , adjustForFees
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), TxIn (..), TxOut (..), UTxO (..) )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Word
    ( Word64, Word8 )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..), Gen, choose, generate, oneof, scale, vectorOf )

import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Fee calculation : unit tests" $ do
        feeUnitTest (FeeFixture
            { fInps = [20]
            , fOuts = [17]
            , fChngs = [3]
            , fExtraUtxo = []
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20]
            , csOuts = [17]
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [18,18]
            , fChngs = [2,2]
            , fExtraUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [18,18]
            , csChngs = [1,1]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [17,18]
            , fChngs = [3,2]
            , fExtraUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [17,18]
            , csChngs = [1,1]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [16,18]
            , fChngs = [4,2]
            , fExtraUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [16,18]
            , csChngs = [2,1]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [15,18]
            , fChngs = [5,2]
            , fExtraUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [15,18]
            , csChngs = [3,1]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [14,18]
            , fChngs = [6,2]
            , fExtraUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [14,18]
            , csChngs = [4,1]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [14,14]
            , fChngs = [6,6]
            , fExtraUtxo = []
            , fFee = 2
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [14,14]
            , csChngs = [5,5]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,18,19]
            , fChngs = [6,2,1]
            , fExtraUtxo = []
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,18,19]
            , csChngs = [4,1]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,18,19]
            , fChngs = [6,2,1]
            , fExtraUtxo = []
            , fFee = 3
            , fDust = 1
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,18,19]
            , csChngs = [3]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,17,19]
            , fChngs = [6,3,1]
            , fExtraUtxo = []
            , fFee = 3
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,17,19]
            , csChngs = [4,2]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,17,19]
            , fChngs = [6,3,1]
            , fExtraUtxo = []
            , fFee = 3
            , fDust = 1
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,17,19]
            , csChngs = [4,2]
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20,20]
            , fOuts = [14,17,19]
            , fChngs = [6,3,1]
            , fExtraUtxo = []
            , fFee = 3
            , fDust = 2
            }) (Right $ FeeOutput
            { csInps = [20,20,20]
            , csOuts = [14,17,19]
            , csChngs = [4]
            })

        feeUnitTest (FeeFixture
            { fInps = [20]
            , fOuts = [17]
            , fChngs = [3]
            , fExtraUtxo = []
            , fFee = 4
            , fDust = 0
            }) (Left $ CannotCoverFee 1)

        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [16,18]
            , fChngs = [4,2]
            , fExtraUtxo = []
            , fFee = 6
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [16,18]
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [16,18]
            , fChngs = [4,2]
            , fExtraUtxo = []
            , fFee = 6
            , fDust = 2
            }) (Left $ CannotCoverFee 2)

        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fExtraUtxo = [1]
            , fFee = 5
            , fDust = 0
            }) (Left $ CannotCoverFee 1)

        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fExtraUtxo = [2]
            , fFee = 5
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,2]
            , csOuts = [7]
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fExtraUtxo = [1,1]
            , fFee = 5
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,1,1]
            , csOuts = [7]
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fExtraUtxo = [3]
            , fFee = 5
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,3]
            , csOuts = [7]
            , csChngs = [1]
            })

        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fExtraUtxo = [2]
            , fFee = 8
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,2]
            , csOuts = [7,7]
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fExtraUtxo = [1,1]
            , fFee = 8
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,1,1]
            , csOuts = [7,7]
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fExtraUtxo = [2,2]
            , fFee = 9
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,2,2]
            , csOuts = [7,7]
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fExtraUtxo = [2,2]
            , fFee = 10
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,2,2]
            , csOuts = [7,7]
            , csChngs = []
            })

        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fExtraUtxo = [3,3]
            , fFee = 10
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,3,3]
            , csOuts = [7,7]
            , csChngs = [2]
            })

feeOptions
    :: Word64
    -> Word64
    -> FeeOptions
feeOptions fee dust = FeeOptions
    { estimate = \_num _outs ->
            Coin fee
    , dustThreshold =
            Coin dust
    }


feeUnitTest
    :: FeeFixture
    -> Either FeeError FeeOutput
    -> SpecWith ()
feeUnitTest (FeeFixture inps outs chngs extraUtxo fee dust) expected = it title $ do
    (utxo, coinSel) <- setup

    result <- runExceptT $ do
        (CoinSelection inps' outs'  chngs') <-
            adjustForFees (feeOptions fee dust) utxo coinSel
        return $ FeeOutput
            {
              csInps = map (getCoin . coin . snd) inps'
            , csOuts = map (getCoin . coin) outs'
            , csChngs = map getCoin chngs'
            }

    result `shouldBe` expected
    where
        setup :: IO (UTxO, CoinSelection)
        setup = do
            txUtxoIns <- generate $ vectorOf (L.length extraUtxo) arbitrary
            txUtxoOutsAddr <- generate $ vectorOf (L.length extraUtxo) arbitrary
            let utxo = UTxO $ Map.fromList $ L.zip txUtxoIns
                       $ L.zipWith TxOut txUtxoOutsAddr
                       $ map Coin extraUtxo

            coinSelIns <- generate $ vectorOf (L.length inps) arbitrary
            coinSelInsOutsAddr <- generate $ vectorOf (L.length inps) arbitrary
            let coinSelInps = L.zip coinSelIns
                              $ L.zipWith TxOut coinSelInsOutsAddr
                              $ map Coin inps


            coinSelOutsAddr <- generate $ vectorOf (L.length outs) arbitrary
            let coinSelOuts = L.zipWith TxOut coinSelOutsAddr
                              $ map Coin outs

            let coinSelChngs = map Coin chngs

            pure (utxo, CoinSelection coinSelInps coinSelOuts coinSelChngs)

        title :: String
        title = mempty
            <> "CoinSelection (inps=" <> show inps
            <> "outs=" <> show outs
            <> "chngs=" <> show chngs
            <> "), UTxO=" <> show extraUtxo
            <> "), fee=" <> show fee
            <> " --> " <> show expected


-- | A fixture for testing the fee calculation
data FeeFixture = FeeFixture
    { fInps :: [Word64]
        -- ^ Value (in Lovelace) & number of coins in inputs
    , fOuts :: [Word64]
        -- ^ Value (in Lovelace) & number of requested outputs
    , fChngs :: [Word64]
        -- ^ Value (in Lovelace) & number of changes
    , fExtraUtxo :: [Word64]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , fFee :: Word64
        -- ^ Value (in Lovelace) of rigid fee
    , fDust :: Word64
        -- ^ Value (in Lovelace) of dust
    } deriving Show

-- | A fee calculation output
data FeeOutput = FeeOutput
    { csInps :: [Word64]
        -- ^ Value (in Lovelace) & number of available coins in the UTxO
    , csOuts :: [Word64]
        -- ^ Value (in Lovelace) & number of requested outputs
    , csChngs :: [Word64]
        -- ^ Value (in Lovelace) & number of changes
    } deriving (Show, Eq)


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
            (CoinSelection inps _ _) <-
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
