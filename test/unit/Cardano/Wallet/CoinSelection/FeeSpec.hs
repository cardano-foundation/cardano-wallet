{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.CoinSelection.FeeSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Binary
    ( encodeSignedTx )
import Cardano.Wallet.CoinSelection
    ( CoinSelection (..), CoinSelectionOptions (..) )
import Cardano.Wallet.CoinSelection.Fee
    ( AddressScheme (..)
    , Fee (..)
    , FeeError (..)
    , FeeOptions (..)
    , Network (..)
    , TxSizeLinear (..)
    , adjustForFees
    , cardanoPolicy
    , estimateCardanoFee
    )
import Cardano.Wallet.CoinSelection.Policy.LargestFirst
    ( largestFirst )
import Cardano.Wallet.CoinSelectionSpec
    ( CoinSelProp (..), genTxOut, genUTxO )
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
import Control.Monad.Trans.Except
    ( runExceptT )
import Crypto.Random
    ( SystemDRG, getSystemDRG )
import Crypto.Random.Types
    ( withDRG )
import Data.Either
    ( isRight )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Text.Class
    ( TextDecodingError (..), fromText )
import Data.Word
    ( Word64 )
import Fmt
    ( Buildable (..), nameF, tupleF )
import GHC.Generics
    ( Generic )
import Test.Hspec
    ( Spec, SpecWith, before, describe, it, shouldBe, shouldSatisfy )
import Test.QuickCheck
    ( Arbitrary (..), Property, choose, disjoin, generate, property, (==>) )

import qualified Cardano.Wallet.CoinSelection as CS
import qualified Data.ByteString.Lazy as BL
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

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [1]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 170543
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [1]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Sequential
            , expectedFee = 168697
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [1]
            , csChanges = [1]
            , csNetwork = Mainnet
            , csAddressScheme = Random
            , expectedFee = 169840
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [1]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Sequential
            , expectedFee = 168697
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [23]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 170543
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [24]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 170587
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [255]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 170587
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [256]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 170631
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [65535]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 170631
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000]
            , csOutputs = [65536]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 170719
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [500000,500000]
            , csOutputs = [750000]
            , csChanges = [1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 182101
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [330000,330000,330000]
            , csOutputs = [750000]
            , csChanges = [1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 193483
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [250000,250000,250000,250000]
            , csOutputs = [750000]
            , csChanges = [1,1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 204865
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [200000,200000,200000,200000,200000]
            , csOutputs = [750000]
            , csChanges = [1,1,1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 216247
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [185000,185000,185000,185000,185000,185000]
            , csOutputs = [750000]
            , csChanges = [1,1,1,1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 227629
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [142000,142000,142000,142000,142000,142000,142000]
            , csOutputs = [750000]
            , csChanges = [1,1,1,1,1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 239011
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [125000,125000,125000,125000,125000,125000,125000,125000]
            , csOutputs = [750000]
            , csChanges = [1,1,1,1,1,1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 250393
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [250000,250000]
            , csOutputs = [100000,23]
            , csChanges = [1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 185528
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [250000,250000]
            , csOutputs = [100000,24]
            , csChanges = [1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 185572
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [250000,250000]
            , csOutputs = [100000,256]
            , csChanges = [1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 185616
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [250000,250000]
            , csOutputs = [100000,65536]
            , csChanges = [1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 185704
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [120000,120000,120000]
            , csOutputs = [100000,23]
            , csChanges = [1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 193483
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [120000,120000,120000]
            , csOutputs = [100000,24]
            , csChanges = [1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 193527
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [120000,120000,120000]
            , csOutputs = [100000,256]
            , csChanges = [1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 196998
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [120000,120000,120000]
            , csOutputs = [100000,65536]
            , csChanges = [1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 197086
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [100000,100000,100000]
            , csOutputs = [1,1,1]
            , csChanges = [1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 200162
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [60000,60000,60000,60000]
            , csOutputs = [1,1,1]
            , csChanges = [1,1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 211544
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [50000,50000,50000,50000,50000]
            , csOutputs = [1,1,1]
            , csChanges = [1,1,1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 222927
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [100000,100000,100000]
            , csOutputs = [24,1,1]
            , csChanges = [1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 200206
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [100000,100000,100000]
            , csOutputs = [24,24,1]
            , csChanges = [1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 200250
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [100000,100000,100000]
            , csOutputs = [24,256,1]
            , csChanges = [1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 200294
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [100000,100000,100000]
            , csOutputs = [24,256,256]
            , csChanges = [1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 200382
            })

        feeEstimationUnitTest (FeeCase
            { csInputs = [100000,100000,100000]
            , csOutputs = [65536,256,256]
            , csChanges = [1,1,1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 200514
            })

        -- 23 inps => 23*42 = 966
        -- 1+1 outs+chngs => 78*2 = 156
        -- totalOutSize = 6 + 966 + 156 =  1128
        feeEstimationUnitTest (FeeCase
            { csInputs = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
            , csOutputs = [1]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 345536
            })

        -- 24 inps => 24*43 = 1032
        -- 1+1 outs+chngs => 78*2 = 156
        -- totalOutSize = 7 + 1032 + 156 =  1195
        feeEstimationUnitTest (FeeCase
            { csInputs = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
            , csOutputs = [1]
            , csChanges = [1]
            , csNetwork = Testnet
            , csAddressScheme = Random
            , expectedFee = 354589
            })


    before getSystemDRG $ describe "Fee calculation properties" $ do
        it "No fee gives back the same selection"
            (\_ -> property propSameSelection)
        it "Fee adjustment is deterministic when there's no extra inputs"
            (\_ -> property propDeterministic)
        it "Adjusting for fee (/= 0) reduces the change outputs or increase inputs"
            (property . propReducedChanges)
        it "Estimated fee is the same as taken by encodeSignedTx"
            (\_ -> property propFeeEstimation)


{-------------------------------------------------------------------------------
                         Fee Adjustment - Properties
-------------------------------------------------------------------------------}

-- | Data for running fee calculation properties
data FeeProp = FeeProp
    { coveringCase :: CoinSelProp
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
propSameSelection (ShowFmt (FeeProp (CoinSelProp utxo txOuts) utxo' _)) = do
    isRight selection ==> let Right s = selection in prop s
  where
    prop coinSel = do
        let feeOpt = feeOptions 0 0
        coinSel' <- runExceptT (adjustForFees feeOpt utxo' coinSel)
        fmap ShowFmt coinSel' `shouldBe` Right (ShowFmt coinSel)
    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts

propDeterministic
    :: ShowFmt FeeProp
    -> Property
propDeterministic (ShowFmt (FeeProp (CoinSelProp utxo txOuts) _ (fee, dust))) = do
    isRight selection ==> let Right s = selection in prop s
  where
    prop coinSel = do
        let feeOpt = feeOptions fee dust
        let utxo' = mempty
        resultOne <- runExceptT $ adjustForFees feeOpt utxo' coinSel
        resultTwo <- runExceptT $ adjustForFees feeOpt utxo' coinSel
        resultOne `shouldBe` resultTwo
    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts

propReducedChanges
    :: SystemDRG
    -> ShowFmt FeeProp
    -> Property
propReducedChanges drg (ShowFmt (FeeProp (CoinSelProp utxo txOuts) utxo' (fee, dust))) = do
    isRight selection' ==>
        let (Right s, Right s') = (selection, selection') in prop s s'
  where
    prop coinSel coinSel' = do
        let chgs' = sum $ map getCoin $ change coinSel'
        let chgs = sum $ map getCoin $ change coinSel
        let inps' = CS.inputs coinSel'
        let inps = CS.inputs coinSel
        disjoin
            [ chgs' `shouldSatisfy` (<= chgs)
            , length inps' `shouldSatisfy` (>= length inps)
            ]
    selection = left show $ runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts
    selection' = selection >>= adjust
    feeOpt = feeOptions fee dust
    adjust s = left show $ fst $ withDRG drg $ runExceptT $
        adjustForFees feeOpt utxo' s

propFeeEstimation
    :: FeeProp
    -> Property
propFeeEstimation (FeeProp (CoinSelProp utxo txOuts) _ _) = do
    isRight selection ==> let Right s = selection in prop s
    where
    prop coinSel@(CoinSelection inps outs chngs) = do
        let (Fee estFee) = estimateCardanoFee cardanoPolicy Random Testnet coinSel

        -- | Make a Hash from a Base16 encoded string, without error handling.
        let hash16 :: Text -> Hash "Tx"
            hash16 txt = case fromText @(Hash "Tx") txt of
                Left (TextDecodingError err) ->
                    error ("Could not decode test string: " <> err)
                Right res -> res

        -- | Make an Address from a Base58 encoded string, without error handling.
        let addr58 :: Text -> Address
            addr58 txt = case fromText @Address txt of
                Left (TextDecodingError err) ->
                    error ("addr58: Could not decode because " <> err)
                Right res -> res

        let inputId0 = hash16 "60dbb2679ee920540c18195a3d92ee9be50aee6ed5f891d92d51db8a76b02cd2"
        let address0 = addr58 "DdzFFzCqrhsug8jKBMV5Cr94hKY4DrbJtkUpqptoGEkovR2QSkcA\
                              \cRgjnUyegE689qBX6b2kyxyNvCL6mfqiarzRB9TRq8zwJphR31pr"
        let pkWitness = "\130X@\226E\220\252\DLE\170\216\210\164\155\182mm$ePG\252\186\195\225_\b=\v\241=\255 \208\147[\239\RS\170|\214\202\247\169\229\205\187O_)\221\175\155?e\198\248\170\157-K\155\169z\144\174\ENQhX@\193\151*,\NULz\205\234\&1tL@\211\&2\165\129S\STXP\164C\176 Xvf\160|;\CANs{\SYN\204<N\207\154\130\225\229\t\172mbC\139\US\159\246\168x\163Mq\248\145)\160|\139\207-\SI"
        let txIns = zipWith TxIn (replicate (length inps) inputId0) [0..]
        let coins = map coin outs ++ chngs
        let txOuts' = zipWith TxOut (replicate (length coins) address0) coins
        let tx = Tx txIns txOuts'
        let calculatedSize = BL.length $ toLazyByteString $ encodeSignedTx (tx, replicate (length inps) (PublicKeyWitness pkWitness))
        let (TxSizeLinear (Quantity a) (Quantity b)) = cardanoPolicy
        let calcFee = ceiling (a + b*(fromIntegral calculatedSize))

        estFee `shouldBe` calcFee

    selection = runIdentity $ runExceptT $
        largestFirst (CoinSelectionOptions 100) utxo txOuts


{-------------------------------------------------------------------------------
                         Fee Adjustment - Unit Tests
-------------------------------------------------------------------------------}

feeEstimationUnitTest
    :: FeeCase
    -> SpecWith ()
feeEstimationUnitTest (FeeCase inps' outs' chngs' net scheme expected) = it title $ do
    inps <- (Map.toList . getUTxO) <$> generate (genUTxO inps')
    outs <- generate (genTxOut (outs' ++ chngs'))
    let coinSel = CoinSelection inps outs []
    let (Fee estFee) = estimateCardanoFee cardanoPolicy scheme net coinSel
    estFee `shouldBe` expected
        where
    title :: String
    title = mempty
        <> "FeeCase (inps=" <> show inps'
        <> " outs=" <> show outs'
        <> " outs=" <> show outs'
        <> " chngs=" <> show chngs'
        <> " address scheme=" <> show scheme
        <> " " <> show net
        <> ") --> " <> show expected

data FeeCase = FeeCase
    { csInputs :: [Word64]
    , csOutputs :: [Word64]
    , csChanges :: [Word64]
    , csNetwork :: Network
    , csAddressScheme :: AddressScheme
    , expectedFee :: Word64
    } deriving (Show, Generic)

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

{-------------------------------------------------------------------------------
                            Arbitrary Instances
-------------------------------------------------------------------------------}

instance Arbitrary FeeProp where
    shrink (FeeProp cc utxo opts) =
        (\(cc', utxo') -> FeeProp cc' utxo' opts)
            <$> zip (shrink cc) (shrink utxo)
    arbitrary = do
        cc <- arbitrary
        utxo <- arbitrary
        fee <- choose (100000, 500000)
        dust <- choose (0, 10000)
        return $ FeeProp cc utxo (fee, dust)
