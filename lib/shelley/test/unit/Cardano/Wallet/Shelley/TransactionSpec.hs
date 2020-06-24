{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Shelley.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv, xprvFromBytes, xprvToBytes )
import Cardano.Wallet
    ( ErrSelectForPayment (..)
    , FeeEstimation (..)
    , coinSelOpts
    , estimateFeeForCoinSelection
    , feeOpts
    , handleCannotCover
    )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..)
    , Passphrase (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    , preparePassphrase
    )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelectionOptions )
import Cardano.Wallet.Primitive.Fee
    ( Fee (..), FeeOptions (..), FeePolicy (..), adjustForFee )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , EpochLength (..)
    , Hash (..)
    , ProtocolMagic (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    )
import Cardano.Wallet.Shelley.Compatibility
    ( toSealed )
import Cardano.Wallet.Shelley.Transaction
    ( mkUnsignedTx
    , mkWitness
    , newTransactionLayer
    , _decodeSignedTx
    , _estimateMaxNumberOfInputs
    )
import Cardano.Wallet.Transaction
    ( WithDelegation (..) )
import Control.Monad
    ( replicateM )
import Control.Monad.Trans.Except
    ( catchE, runExceptT, withExceptT )
import Data.Function
    ( on )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word8 )
import Ouroboros.Network.Block
    ( SlotNo (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Hspec.QuickCheck
    ( prop )
import Test.QuickCheck
    ( Arbitrary (..)
    , InfiniteList (..)
    , Property
    , arbitraryPrintableChar
    , choose
    , scale
    , vectorOf
    , withMaxSuccess
    , (===)
    , (==>)
    )

import qualified Cardano.Api as Cardano
import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Cardano.Wallet.Primitive.CoinSelection.Random as CS
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Tx as SL

spec :: Spec
spec = do
    describe "decodeSignedTx testing" $
        prop "roundtrip" prop_decodeSignedTxRoundtrip

    describe "estimateMaxNumberOfInputs" $ do
        it "order of magnitude, nOuts = 1" $
            _estimateMaxNumberOfInputs (Quantity 4096) 1 `shouldBe` 27
        it "order of magnitude, nOuts = 10" $
            _estimateMaxNumberOfInputs (Quantity 4096) 10 `shouldBe` 19
        it "order of magnitude, nOuts = 20" $
            _estimateMaxNumberOfInputs (Quantity 4096) 20 `shouldBe` 10
        it "order of magnitude, nOuts = 30" $
            _estimateMaxNumberOfInputs (Quantity 4096) 30 `shouldBe` 1

        prop "more outputs ==> less inputs" prop_moreOutputsMeansLessInputs
        prop "less outputs ==> more inputs" prop_lessOutputsMeansMoreInputs
        prop "bigger size  ==> more inputs" prop_biggerMaxSizeMeansMoreInputs

    it "regression #1740 - fee estimation at the boundaries" $ do
        let utxo = UTxO $ Map.fromList
                [ ( TxIn dummyTxId 0
                  , TxOut dummyAddress (Coin 5000000)
                  )
                ]
        let recipients = NE.fromList
                [ TxOut dummyAddress (Coin 4834720)
                ]

        let selectCoins = flip catchE (handleCannotCover utxo recipients) $ do
                (sel, utxo') <- withExceptT ErrSelectForPaymentCoinSelection $ do
                    CS.random testCoinSelOpts recipients utxo
                withExceptT ErrSelectForPaymentFee $
                    (Fee . CS.feeBalance) <$> adjustForFee testFeeOpts utxo' sel
        res <- runExceptT $ estimateFeeForCoinSelection selectCoins

        res `shouldBe` Right (FeeEstimation 165281 165281)

prop_decodeSignedTxRoundtrip
    :: DecodeSetup
    -> Property
prop_decodeSignedTxRoundtrip (DecodeSetup utxo outs slotNo pairs) = do
    let ownedIns = Map.toList $ getUTxO utxo
    let unsigned = mkUnsignedTx slotNo ownedIns outs []
    let addrWits = Set.fromList $ map (mkWitness unsigned) pairs
    let metadata = SL.SNothing
    let wits = SL.WitnessSet addrWits mempty mempty
    let ledgerTx = SL.Tx unsigned wits metadata

    _decodeSignedTx (Cardano.txSignedToCBOR (Cardano.TxSignedShelley ledgerTx))
        === Right (toSealed ledgerTx)

-- | Increasing the number of outputs reduces the number of inputs.
prop_moreOutputsMeansLessInputs
    :: Quantity "byte" Word16
    -> Word8
    -> Property
prop_moreOutputsMeansLessInputs size nOuts = withMaxSuccess 1000 $
    nOuts < maxBound ==>
        _estimateMaxNumberOfInputs size nOuts
        >=
        _estimateMaxNumberOfInputs size (nOuts + 1)

-- | REducing the number of outputs increases the number of inputs.
prop_lessOutputsMeansMoreInputs
    :: Quantity "byte" Word16
    -> Word8
    -> Property
prop_lessOutputsMeansMoreInputs size nOuts = withMaxSuccess 1000 $
    nOuts > minBound ==>
        _estimateMaxNumberOfInputs size (nOuts - 1)
        >=
        _estimateMaxNumberOfInputs size nOuts


-- | Increasing the max size automatically increased the number of inputs
prop_biggerMaxSizeMeansMoreInputs
    :: Quantity "byte" Word16
    -> Word8
    -> Property
prop_biggerMaxSizeMeansMoreInputs (Quantity size) nOuts = withMaxSuccess 1000 $
    size < maxBound `div` 2 ==>
        _estimateMaxNumberOfInputs (Quantity size) nOuts
        <=
        _estimateMaxNumberOfInputs (Quantity (size * 2)) nOuts

testCoinSelOpts :: CoinSelectionOptions ()
testCoinSelOpts = coinSelOpts tl (Quantity 4096)
  where
    pm        = ProtocolMagic 1
    epLength  = EpochLength 42 -- irrelevant here
    tl        = newTransactionLayer @_ @ShelleyKey (Proxy @'Mainnet) pm epLength

testFeeOpts :: FeeOptions
testFeeOpts = feeOpts tl (WithDelegation False) feePolicy
  where
    pm        = ProtocolMagic 1
    epLength  = EpochLength 42 -- irrelevant here
    tl        = newTransactionLayer @_ @ShelleyKey (Proxy @'Mainnet) pm epLength
    feePolicy = LinearFee (Quantity 155381) (Quantity 44) (Quantity 0)

data DecodeSetup = DecodeSetup
    { inputs :: UTxO
    , outputs :: [TxOut]
    , ttl :: SlotNo
    , keyPasswd :: [(XPrv, Passphrase "encryption")]
    } deriving Show

instance Arbitrary DecodeSetup where
    arbitrary = do
        utxo <- arbitrary
        n <- choose (1,10)
        outs <- vectorOf n arbitrary
        slot <- arbitrary
        let numInps = Map.size $ getUTxO utxo
        pairs <- vectorOf numInps arbitrary
        pure $ DecodeSetup utxo outs slot pairs

instance Arbitrary SlotNo where
    arbitrary = SlotNo <$> choose (1, 1000)

instance Arbitrary TxIn where
    arbitrary = do
        ix <- scale (`mod` 3) arbitrary
        txId <- arbitrary
        pure $ TxIn txId ix

instance Arbitrary (Hash "Tx") where
    arbitrary = do
        bs <- vectorOf 32 arbitrary
        pure $ Hash $ BS.pack bs

instance Arbitrary Coin where
    arbitrary =
        Coin <$> choose (1, 200000)

instance Arbitrary TxOut where
    arbitrary = do
        let addr = Address $ BS.pack (1:replicate 64 0)
        TxOut addr <$> arbitrary

instance Arbitrary UTxO where
    arbitrary = do
        n <- choose (1,10)
        inps <- vectorOf n arbitrary
        let addr = Address $ BS.pack (1:replicate 64 0)
        coins <- vectorOf n arbitrary
        let outs = map (TxOut addr) coins
        pure $ UTxO $ Map.fromList $ zip inps outs

instance Arbitrary XPrv where
    arbitrary = do
        InfiniteList bytes _ <- arbitrary
        let (Just xprv) = xprvFromBytes $ BS.pack $ take 96 bytes
        pure xprv

-- Necessary unsound Show instance for QuickCheck failure reporting
instance Show XPrv where
    show = show . xprvToBytes

-- Necessary unsound Eq instance for QuickCheck properties
instance Eq XPrv where
    (==) = (==) `on` xprvToBytes

instance Arbitrary (Passphrase "raw") where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy "raw"

    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy "raw"

instance Arbitrary (Passphrase "encryption") where
    arbitrary = preparePassphrase EncryptWithPBKDF2
        <$> arbitrary @(Passphrase "raw")

instance Arbitrary (Quantity "byte" Word16) where
    arbitrary = Quantity <$> choose (128, 2048)
    shrink (Quantity size)
        | size <= 1 = []
        | otherwise = Quantity <$> shrink size

dummyAddress :: Address
dummyAddress = Address $ BS.pack $ 1 : replicate 64 0

dummyTxId :: Hash "Tx"
dummyTxId = Hash $ BS.pack $ replicate 32 0
