{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.FeeSpec
    ( spec
    ) where

import Prelude

import Cardano.Environment
    ( Network (..) )
import Cardano.Wallet.Binary
    ( encodeSignedTx )
import Cardano.Wallet.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.CoinSelection.Fee
    ( Fee (..)
    , FeeError (..)
    , FeeOptions (..)
    , TxSizeLinear (..)
    , adjustForFee
    , cardanoPolicy
    , estimateFee
    )
import Cardano.Wallet.CoinSelection.Policy.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , ShowFmt (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    , UTxO (..)
    )
import Codec.CBOR.Write
    ( toLazyByteString )
import Control.Arrow
    ( left )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Random
    ( SystemDRG, getSystemDRG )
import Crypto.Random.Types
    ( withDRG )
import Data.Digest.CRC32
    ( crc32 )
import Data.Either
    ( isRight )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), nameF, tupleF )
import Test.Hspec
    ( Spec, SpecWith, before, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , InfiniteList (..)
    , Property
    , choose
    , disjoin
    , generate
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( monadicIO )

import qualified Cardano.Wallet.Binary as CBOR
import qualified Cardano.Wallet.CoinSelection as CS
import qualified Codec.CBOR.Encoding as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Fee calculation : unit tests" $ do
        -- Change covers fee exactly, single change output
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

        -- Total change covers fee, multiple change outputs
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

        -- Fee split evenly across change outputs
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

        -- Fee split evenly across change outputs, with rounding 'issues'
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

        -- Fee divvied, dust removed (dust = 0)
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

        -- Fee divvied, dust removed (dust = 1)
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
            , csChngs = [4]
            })

        -- Cannot cover fee, no extra inputs
        feeUnitTest (FeeFixture
            { fInps = [20]
            , fOuts = [17]
            , fChngs = [3]
            , fUtxo = []
            , fFee = 4
            , fDust = 0
            }) (Left $ CannotCoverFee 1)

        -- Cannot cover fee even with an extra (too small) inputs
        feeUnitTest (FeeFixture
            { fInps = [10]
            , fOuts = [7]
            , fChngs = [3]
            , fUtxo = [1]
            , fFee = 5
            , fDust = 0
            }) (Left $ CannotCoverFee 1)

        -- Can select extra inputs to exactly cover fee, no change back
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

        -- Can select extra inputs to cover for fee, and leave a change back
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

        -- Multiple change output, can select extra inputs to cover fee, no change
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

        -- Multiple outputs, extra inputs selected, resulting change
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

        -- Multiple change outputs, some bigger than actual Dust
        feeUnitTest (FeeFixture
            { fInps = [20,20]
            , fOuts = [16,18]
            , fChngs = [4,2]
            , fUtxo = []
            , fFee = 6
            , fDust = 2
            }) (Right $ FeeOutput
            { csInps = [20,20]
            , csOuts = [16,18]
            , csChngs = []
            })

        -- Selection with no fee
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

    describe "Fee Calculation: Generators" $ do
        it "Arbitrary CoinSelection" $ property $ \(ShowFmt cs) ->
            property $ isValidSelection cs

    before getSystemDRG $ describe "Fee Adjustment properties" $ do
        it "No fee gives back the same selection"
            (\_ -> property propSameSelection)
        it "Fee adjustment is deterministic when there's no extra inputs"
            (\_ -> property propDeterministic)
        it "Adjusting for fee (/= 0) reduces the change outputs or increase inputs"
            (property . propReducedChanges)

    describe "Fee Estimation properties" $ do
        it "Estimated fee is the same as taken by encodeSignedTx"
            (withMaxSuccess 1000 $ property propFeeEstimation)

{-------------------------------------------------------------------------------
                         Fee Adjustment - Properties
-------------------------------------------------------------------------------}

-- Check whether a selection is valid
isValidSelection :: CoinSelection -> Bool
isValidSelection (CoinSelection i o c) =
    let
        oAmt = sum $ map (fromIntegral . getCoin . coin) o
        cAmt = sum $ map (fromIntegral . getCoin) c
        iAmt = sum $ map (fromIntegral . getCoin . coin . snd) i
    in
        (iAmt :: Integer) >= (oAmt + cAmt)

-- | Data for running fee calculation properties
data FeeProp = FeeProp
    { selection :: CoinSelection
     -- ^ inputs from wich largestFirst can be calculated
    , availableUtxo :: UTxO
     -- ^ additional UTxO from which fee calculation will pick needed coins
    , feeDust :: (Word64, Word64)
     -- ^ constant fee and dust threshold
    } deriving Show

instance Buildable FeeProp where
    build (FeeProp cc utxo opt) = mempty
        <> nameF "selection" (build cc)
        <> build utxo
        <> nameF "options" (tupleF opt)

propSameSelection
    :: ShowFmt FeeProp
    -> Property
propSameSelection (ShowFmt (FeeProp coinSel utxo _)) = monadicIO $ liftIO $ do
    let feeOpt = feeOptions 0 0
    coinSel' <- runExceptT (adjustForFee feeOpt utxo coinSel)
    fmap ShowFmt coinSel' `shouldBe` Right (ShowFmt coinSel)

propDeterministic
    :: ShowFmt FeeProp
    -> Property
propDeterministic (ShowFmt (FeeProp coinSel _ (fee, dust))) = monadicIO $ liftIO $ do
    let feeOpt = feeOptions fee dust
    let utxo = mempty
    resultOne <- runExceptT $ adjustForFee feeOpt utxo coinSel
    resultTwo <- runExceptT $ adjustForFee feeOpt utxo coinSel
    resultOne `shouldBe` resultTwo

propReducedChanges
    :: SystemDRG
    -> ShowFmt FeeProp
    -> Property
propReducedChanges drg (ShowFmt (FeeProp coinSel utxo (fee, dust))) = do
    isRight coinSel' ==> let Right s = coinSel' in prop s
  where
    prop s = do
        let chgs' = sum $ map getCoin $ change s
        let chgs = sum $ map getCoin $ change coinSel
        let inps' = CS.inputs s
        let inps = CS.inputs coinSel
        disjoin
            [ chgs' `shouldSatisfy` (<= chgs)
            , length inps' `shouldSatisfy` (>= length inps)
            ]
    feeOpt = feeOptions fee dust
    coinSel' = left show $ fst $ withDRG drg $ runExceptT $
        adjustForFee feeOpt utxo coinSel

propFeeEstimation
    :: (ShowFmt CoinSelection, Network, InfiniteList (Network -> Address))
    -> Property
propFeeEstimation (ShowFmt sel, network, InfiniteList chngAddrs _) =
    let
        (Fee calcFee) = estimateFee cardanoPolicy network sel
        (TxSizeLinear (Quantity a) (Quantity b)) = cardanoPolicy
        tx = fromCoinSelection sel
        size = BL.length $ toLazyByteString $ encodeSignedTx tx
        -- We always go for the higher bound for change address payload's size,
        -- so, we may end up with up to 4 extra bytes per change address in our
        -- estimation.
        margin = 4 * fromIntegral (length $ CS.change sel)
        realFeeSup = ceiling (a + b*(fromIntegral size + margin))
        realFeeInf = ceiling (a + b*(fromIntegral size))
    in
        property (calcFee >= realFeeInf && calcFee <= realFeeSup)
  where
    dummyWitness = PublicKeyWitness
        "\130X@\226E\220\252\DLE\170\216\210\164\155\182mm$ePG\252\186\195\225_\b=\v\241=\255 \208\147[\239\RS\170|\214\202\247\169\229\205\187O_)\221\175\155?e\198\248\170\157-K\155\169z\144\174\ENQhX@\193\151*,\NULz\205\234\&1tL@\211\&2\165\129S\STXP\164C\176 Xvf\160|;\CANs{\SYN\204<N\207\154\130\225\229\t\172mbC\139\US\159\246\168x\163Mq\248\145)\160|\139\207-\SI"
    dummyInput = Hash
        "`\219\178g\158\233 T\f\CAN\EMZ=\146\238\155\229\n\238n\213\248\145\217-Q\219\138v\176,\210"
    fromCoinSelection (CoinSelection inps outs chngs) =
        let
            txIns = zipWith TxIn
                (replicate (length inps) dummyInput)
                [0..]
            txChngs = zipWith TxOut
                (take (length chngs) (chngAddrs <*> pure network))
                chngs
            wits = replicate (length inps) dummyWitness
        in
            (Tx txIns (outs <> txChngs), wits)

{-------------------------------------------------------------------------------
                         Fee Adjustment - Unit Tests
-------------------------------------------------------------------------------}

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
            adjustForFee (feeOptions feeF dustF) utxo sel
        return $ FeeOutput
            { csInps = map (getCoin . coin . snd) inps
            , csOuts = map (getCoin . coin) outs
            , csChngs = map getCoin chngs
            }
    result `shouldBe` expected
  where
    setup :: IO (UTxO, CoinSelection)
    setup = do
        utxo <- generate (genUTxO $ Coin <$> utxoF)
        inps <- (Map.toList . getUTxO) <$> generate (genUTxO $ Coin <$> inpsF)
        outs <- generate (genTxOut $ Coin <$> outsF)
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

{-------------------------------------------------------------------------------
                            Arbitrary Instances
-------------------------------------------------------------------------------}

deriving newtype instance Arbitrary a => Arbitrary (ShowFmt a)

instance Show (Network -> Address) where
    show _ = "<Change Address Generator>"

genUTxO :: [Coin] -> Gen UTxO
genUTxO coins = do
    let n = length coins
    inps <- vectorOf n arbitrary
    outs <- genTxOut coins
    return $ UTxO $ Map.fromList $ zip inps outs

genTxOut :: [Coin] -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vectorOf n arbitrary
    return $ zipWith TxOut outs coins

instance Arbitrary TxIn where
    shrink _ = []
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary Coin where
    shrink (Coin c) = Coin <$> shrink (fromIntegral c)
    arbitrary = Coin <$> choose (1, 200000)

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> vectorOf 32 arbitrary
        pure $ Hash bytes

instance Arbitrary Address where
    shrink _ = []
    arbitrary = genAddress (30, 100)

instance Arbitrary Network where
    shrink = genericShrink
    arbitrary = genericArbitrary

-- | Generate change addresses for the given network. We consider that change
-- addresses are always following a sequential scheme.
instance {-# OVERLAPS #-} Arbitrary (Network -> Address) where
    shrink _ = []
    arbitrary = do
        mainnetA <- genAddress (39, 43)
        testnetA <- genAddress (46, 50)
        return $ \case
            Mainnet -> mainnetA
            Staging -> mainnetA
            Testnet -> testnetA
            Local -> testnetA

-- | Generate a valid address with a payload of the given size. As pointers,
-- the sizes of payloads are as follows:
--
--     | Network | Scheme     | Size (bytes)   |
--     | ---     | ---        | ---            |
--     | Mainnet | Random     | 72,73,74 or 76 |
--     | Mainnet | Sequential | 39,40,41 or 43 |
--     | Testnet | Random     | 79,80,81 or 83 |
--     | Testnet | Sequential | 46,47,48 or 50 |
--
-- The address format on 'Staging' is the same as 'Mainnet'.
-- The address format on 'Local' is the same as 'Testnet'.
genAddress :: (Int, Int) -> Gen Address
genAddress range = do
    n <- choose range
    let prefix = BS.pack
            [ 130       -- Array(2)
            , 216, 24   -- Tag 24
            , 88, fromIntegral n -- Bytes(n), n > 23 && n < 256
            ]
    payload <- BS.pack <$> vectorOf n arbitrary
    let crc = CBOR.toByteString (CBOR.encodeWord32 $ crc32 payload)
    return $ Address (prefix <> payload <> crc)

instance Arbitrary CoinSelection where
    shrink sel@(CoinSelection inps outs chgs) = case (inps, outs, chgs) of
        ([_], [_], []) ->
            []
        _ ->
            let
                inps' = take (max 1 (length inps `div` 2)) inps
                outs' = take (max 1 (length outs `div` 2)) outs
                chgs' = take (length chgs `div` 2) chgs
                inps'' = if length inps > 1 then drop 1 inps else inps
                outs'' = if length outs > 1 then drop 1 outs else outs
                chgs'' = drop 1 chgs
            in
                filter (\s -> s /= sel && isValidSelection s)
                    [ CoinSelection inps' outs' chgs'
                    , CoinSelection inps' outs chgs
                    , CoinSelection inps outs chgs'
                    , CoinSelection inps outs' chgs
                    , CoinSelection inps'' outs'' chgs''
                    , CoinSelection inps'' outs chgs
                    , CoinSelection inps outs'' chgs
                    , CoinSelection inps outs chgs''
                    ]
    arbitrary = do
        outs <- choose (1, 10)
            >>= \n -> vectorOf n arbitrary
            >>= genTxOut
        genSelection (NE.fromList outs)
      where
        genSelection :: NonEmpty TxOut -> Gen CoinSelection
        genSelection outs = do
            let opts = CS.CoinSelectionOptions 100
            utxo <- vectorOf (NE.length outs * 3) arbitrary >>= genUTxO
            case runIdentity $ runExceptT $ largestFirst opts utxo outs of
                Left _ -> genSelection outs
                Right s -> return s

instance Arbitrary FeeProp where
    shrink (FeeProp cs utxo opts) =
        case Map.toList $ getUTxO utxo  of
            [] ->
                map (\cs' -> FeeProp cs' utxo opts) (shrink cs)
            us ->
                concatMap (\cs' ->
                    [ FeeProp cs' mempty opts
                    , FeeProp cs' (UTxO $ Map.fromList (drop 1 us)) opts
                    ]
                ) (shrink cs)
    arbitrary = do
        cs <- arbitrary
        utxo <- choose (0, 50)
            >>= \n -> vectorOf n arbitrary
            >>= genUTxO
        fee <- choose (100000, 500000)
        dust <- choose (0, 10000)
        return $ FeeProp cs utxo (fee, dust)
