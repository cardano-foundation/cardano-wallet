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
    , Fee (..)
    , FeeError (..)
    , FeeOptions (..)
    , adjustForFees
    )
import Cardano.Wallet.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), TxIn (..), TxOut (..), UTxO (..) )
import Control.Monad.Trans.Except
    ( ExceptT, runExceptT )
import Crypto.Random
    ( SystemDRG, getSystemDRG )
import Crypto.Random.Types
    ( withDRG )
import Data.Either
    ( isLeft, isRight, lefts )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Word
    ( Word64, Word8 )
import Test.Hspec
    ( Spec, SpecWith, before, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , generate
    , oneof
    , property
    , scale
    , vectorOf
    , (==>)
    )

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Fee calculation : unit tests" $ do
        feeUnitTest (FeeFixture
            { fInps = [20]
            , fOuts = [17]
            , fChngs = [3]
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
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
            , fUtxo = []
            , fFee = 4
            , fDust = 0
            }) (Left $ CannotCoverFee 1)

        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [16,18]
            , fChngs = [4,2]
            , fUtxo = []
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
            , fUtxo = []
            , fFee = 6
            , fDust = 2
            }) (Left $ CannotCoverFee 2)

        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [1]
            , fFee = 5
            , fDust = 0
            }) (Left $ CannotCoverFee 1)

        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [2]
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
            , fUtxo = [1,1]
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
            , fUtxo = [3]
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
            , fUtxo = [2]
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
            , fUtxo = [1,1]
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
            , fUtxo = [2,2]
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
            , fUtxo = [2,2]
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
            , fUtxo = [3,3]
            , fFee = 10
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10,3,3]
            , csOuts = [7,7]
            , csChngs = [1,1]
            })

        feeUnitTest (FeeFixture
            { fInps = [10,10]
            , fOuts = [7,7]
            , fChngs = [3,3]
            , fUtxo = [3,3]
            , fFee = 0
            , fDust = 0
            }) (Right $ FeeOutput
            { csInps = [10,10]
            , csOuts = [7,7]
            , csChngs = [3,3]
            })

    describe "Fee calculation properties" $ do
        it "forall CoinSelection,\
            \computing the fee of 0 ends up with the same CoinSelection"
            (property propTheSameCoinSelection)
        it "forall CoinSelection with UTxO empty,\
            \computing the fee is deterministic"
            (property propDeterministic)

    before getSystemDRG $ describe "Fee calculation properties" $ do
        it "forall CoinSelection with UTxO non-empty,\
            \computing the nonzero fee is deterministic when error arises"
            (property . propDeterministicError)
        it "forall CoinSelection with UTxO empty,\
            \computing the nonzero fee gives rise to reduced changes"
            (property . propReducedChanges)
        it "forall CoinSelection with UTxO empty,\
            \when computing the nonzero fee gives rise to error, \
            \then the same setup with UTxO non-empty gives rise increased inputs,\
            \if successful"
            (property . propIncreasedInputs)

propTheSameCoinSelection
    :: FeeCase
    -> Property
propTheSameCoinSelection (FeeCase (CoveringCase (utxo, txOuts)) extraUtxo _) = do
    isRight selection ==> let Right s = selection in prop (s, extraUtxo)
  where
    prop (coinSel, utxo') = do
        let feeOpt = feeOptions 0 0
        coinSel' <- runExceptT $ adjustForFees feeOpt utxo' coinSel
        coinSel' `shouldBe` (pure coinSel)
    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts

propDeterministic
    :: FeeCase
    -> Property
propDeterministic (FeeCase (CoveringCase (utxo, txOuts)) _ (fee, dust)) = do
    isRight selection ==> let Right s = selection in prop s
  where
    prop coinSel = do
        let feeOpt = feeOptions fee dust
        let utxo' = UTxO Map.empty
        resultOne <- runExceptT $ adjustForFees feeOpt utxo' coinSel
        resultTwo <- runExceptT $ adjustForFees feeOpt utxo' coinSel
        resultOne `shouldBe` resultTwo
    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts

propReducedChanges
    :: SystemDRG
    -> FeeCase
    -> Property
propReducedChanges drg (FeeCase (CoveringCase (utxo, txOuts)) _ (fee, dust)) = do
    isRight selection ==> let Right s = selection in prop s
  where
    prop coinSel = do
        isRight result ==> let Right ss = result in prop1 ss
            where
                prop1 coinSel' = do
                    let chgs' = sum $ map getCoin $ change coinSel'
                    let chgs = sum $ map getCoin $ change coinSel
                    chgs' `shouldSatisfy` (<= chgs)
                feeOpt = feeOptions fee dust
                utxo' = UTxO Map.empty
                (result,_) = withDRG drg
                    $ runExceptT $ adjustForFees feeOpt utxo' coinSel
    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts

propDeterministicError
    :: SystemDRG
    -> FeeCase
    -> Property
propDeterministicError drg (FeeCase (CoveringCase (utxo, txOuts)) _ (fee, dust)) = do
    isRight selection ==> let Right s = selection in prop s
  where
    prop coinSel = do
        isLeft result ==> let Left ss = result in prop1 ss
            where
                prop1 err = do
                    resultSecond <- runExceptT $ adjustForFees feeOpt utxo' coinSel
                    [err] `shouldBe` (lefts [resultSecond])
                feeOpt = feeOptions fee dust
                utxo' = UTxO Map.empty
                (result,_) = withDRG drg
                    $ runExceptT $ adjustForFees feeOpt utxo' coinSel
    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts

propIncreasedInputs
    :: SystemDRG
    -> FeeCase
    -> Property
propIncreasedInputs drg (FeeCase (CoveringCase (utxo, txOuts)) extraUtxo (fee, dust)) = do
    isRight selection ==> let Right s = selection in prop s
  where
    prop coinSel = do
        isLeft result ==> let Left ss = result in prop1 ss
            where
                prop1 _err = do
                    resultSecond <- runExceptT $ adjustForFees feeOpt extraUtxo coinSel
                    case resultSecond of
                        Right coinSel'' -> do
                            let computeInps = sum . map (getCoin . coin . snd ) . inputs
                            let inps = computeInps coinSel
                            let inps' = computeInps coinSel''
                            inps `shouldSatisfy` (<= inps')
                        Left _ ->
                            -- just tautology
                            result `shouldBe` result
                feeOpt = feeOptions fee dust
                utxo' = UTxO Map.empty
                (result,_) = withDRG drg
                    $ runExceptT $ adjustForFees feeOpt utxo' coinSel
    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts

feeOptions
    :: Word64
    -> Word64
    -> FeeOptions
feeOptions fee dust = FeeOptions
    { estimate = \_num _outs ->
            Fee fee
    , dustThreshold =
            Coin dust
    }

feeUnitTest
    :: FeeFixture
    -> Either FeeError FeeOutput
    -> SpecWith ()
feeUnitTest (FeeFixture inpsF outsF chngsF utxoF feeF dustF) expected = it title $ do
    (utxo, sel) <- setup
    result <- runExceptT $ do
        (CoinSelection inps outs chngs) <-
            adjustForFees (feeOptions feeF dustF) utxo sel
        return $ FeeOutput
            { csInps = map (getCoin . coin . snd) inps
            , csOuts = map (getCoin . coin) outs
            , csChngs = map getCoin chngs
            }
    result `shouldBe` expected
  where
    setup :: IO (UTxO, CoinSelection)
    setup = do
        utxo <- generate (genUTxO utxoF)
        inps <- (Map.toList . getUTxO) <$> generate (genUTxO inpsF)
        outs <- generate (genTxOut outsF)
        let chngs = map Coin chngsF
        pure (utxo, CoinSelection inps outs chngs)

    title :: String
    title = mempty
        <> "CoinSelection (inps=" <> show inpsF
        <> "outs=" <> show outsF
        <> "chngs=" <> show chngsF
        <> "), UTxO=" <> show utxoF
        <> "), fee=" <> show feeF
        <> " --> " <> show expected

-- | A fixture for testing the fee calculation
data FeeFixture = FeeFixture
    { fInps :: [Word64]
        -- ^ Value (in Lovelace) & number of coins in inputs
    , fOuts :: [Word64]
        -- ^ Value (in Lovelace) & number of requested outputs
    , fChngs :: [Word64]
        -- ^ Value (in Lovelace) & number of changes
    , fUtxo :: [Word64]
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
coinSelectionUnitTest run lbl expected (Fixture n utxoF outsF) =
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
        <> ", UTxO=" <> show utxoF
        <> ", Output=" <> show (NE.toList outsF)
        <> " --> " <> show expected
        <> if null lbl then "" else " (" <> lbl <> ")"

    setup :: IO (UTxO, NonEmpty TxOut)
    setup = do
        utxo <- generate (genUTxO utxoF)
        outs <- generate (genTxOut $ NE.toList outsF)
        pure (utxo, NE.fromList outs)


-- | Data for running fee calculation properties
data FeeCase = FeeCase
    { coveringCase :: CoveringCase
     -- ^ inputs from wich largestFirst can be calculated
    , availableUtxo :: UTxO
     -- ^ additional UTxO from which fee calculation will pick needed coins to cover fee
    , feeDust :: (Word64, Word64)
     -- ^ constant fee and dust threshold
    }
    deriving Show

{-------------------------------------------------------------------------------
                            Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary FeeCase where
    arbitrary = do
        cc <- arbitrary
        utxo <- arbitrary
        fee <- choose (100000, 500000)
        dust <- choose (0, 10000)
        return $ FeeCase cc utxo (fee, dust)

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

genUTxO :: [Word64] -> Gen UTxO
genUTxO coins = do
    let n = length coins
    inps <- vectorOf n arbitrary
    outs <- genTxOut coins
    return $ UTxO $ Map.fromList $ zip inps outs

genTxOut :: [Word64] -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vectorOf n arbitrary
    return $ zipWith TxOut outs (map Coin coins)
