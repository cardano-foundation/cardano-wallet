{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.BinarySpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.Jormungandr.Binary
    ( Fragment (..)
    , MkFragment (..)
    , StakeDelegationType (..)
    , TaxParameters (..)
    , TxWitnessTag (..)
    , getBlock
    , getBlockHeader
    , getFragment
    , maxNumberOfInputs
    , maxNumberOfOutputs
    , putFragment
    , runGet
    , runPut
    )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Passphrase, hex )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , ChimericAccount (..)
    , Coin (..)
    , PoolId (..)
    , PoolOwner (..)
    , SealedTx (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeFromHexFile, unsafeXPrv )
import Control.DeepSeq
    ( force )
import Control.Exception
    ( SomeException, evaluate, try )
import Control.Monad
    ( forM_, void )
import Control.Monad.Loops
    ( whileM )
import Data.Binary.Get as Get
    ( getWord16be, isEmpty, isolate, label )
import Data.Either
    ( isRight )
import Data.List
    ( isSuffixOf )
import Data.Maybe
    ( isNothing )
import Data.Word
    ( Word8 )
import GHC.Generics
    ( Generic )
import System.Directory
    ( getDirectoryContents )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec
    , describe
    , expectationFailure
    , it
    , runIO
    , shouldBe
    , shouldContain
    , shouldSatisfy
    )
import Test.QuickCheck as QC
    ( Arbitrary (..)
    , choose
    , counterexample
    , elements
    , label
    , oneof
    , property
    , vector
    , withMaxSuccess
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary, genericShrink )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor, run )
import Test.Utils.Paths
    ( getTestData )

import qualified Cardano.Crypto.Wallet as CC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL

spec :: Spec
spec = do
    describe "Decoding" $ do
        it "should decode the block0.bin used for integration tests" $ do
            bs <- BL.readFile ($(getTestData) </> "jormungandr/block0.bin")
            res <- try' (runGet getBlock bs)
            res `shouldSatisfy` isRight

        describe "golden block0s generated in jormungandr-lib" $ do
            let dir = $(getTestData) </> "block0s"
            files <- runIO $ filter (".bin" `isSuffixOf`)
                <$> getDirectoryContents dir
            forM_ files $ \filename -> do
                it ("should decode " ++ filename) $ do
                    bs <- BL.readFile (dir </> filename)
                    res <- try' (runGet getBlock bs)
                    force res `shouldSatisfy` isRight

        describe "whileM (not <$> isEmpty)" $ do
            it "should fail immediately when the decoder errors" $ do
                -- Context: We use whileM not <$> isEmpty to decode multiple
                -- block-fragments/messages. If a message-decoder is wrong, we
                -- want to know that clearly.
                let getFragment' = Get.label "getFragment" $ isolate 3 getWord16be
                let getBlock' = whileM (not <$> isEmpty) getFragment'
                res <- try' (runGet getBlock' $ BL.pack [0,0,0])
                case res of
                    Right _ -> expectationFailure
                        "Faulty decoder scenario should not succeed"
                    Left e -> do
                        e `shouldContain`
                            "the decoder consumed 2 bytes which is less than \
                            \the expected 3 bytes"
                        e `shouldContain` "getFragment"

        it "Regression #1025" $ do
            -- Blockheader taken from current `block0.bin` in out integration
            -- test data, with a modified slot number `ffffffff` making sure it
            -- overflows way beyond Word16
            let bytes = BL.fromStrict $ unsafeFromHex
                    "005200000002d5d800000000ffffffff000000003a2954d0068a79b7fa\
                    \44d9b75d81a78e0c527b3060e923b28d6c981e9024ca28000000000000\
                    \0000000000000000000000000000000000000000000000000000"
            res <- try' (runGet getBlockHeader bytes)
            force res `shouldSatisfy` isRight

    describe "Fragment Encoding/Decoding" $ do
        it "decode (encode fragment) == fragment" $
          withMaxSuccess 1000 $ property $ \test -> monadicIO $ do
            let fragment = void $ putFragment
                    (fragmentGenesis test)
                    (zip (fragmentInputs test) (fragmentCredentials test))
                    (fragmentOutputs test)
                    (fragmentTag test)

            let fragment' = runGet getFragment (runPut fragment)

            monitor (counterexample $ show fragment')

            case fragment' of
                Transaction (Tx _ inps outs _ _) -> do
                    monitor (QC.label "Transaction")
                    assert (inps == fragmentInputs test)
                    assert (outs == fragmentOutputs test)

                StakeDelegation (DlgFull poolId, accountId, (Tx _ inps outs _ _)) -> do
                    monitor (QC.label "StakeDelegation (Full)")
                    assert (inps == fragmentInputs test)
                    assert (outs == fragmentOutputs test)
                    assert (Just accountId == fragmentAccountId test)
                    assert (Just poolId == fragmentPoolId test)

                StakeDelegation (DlgNone, accountId, (Tx _ inps outs _ _)) -> do
                    monitor (QC.label "StakeDelegation (None)")
                    assert (inps == fragmentInputs test)
                    assert (outs == fragmentOutputs test)
                    assert (Just accountId == fragmentAccountId test)
                    assert (isNothing $ fragmentPoolId test)

                _ -> run $ expectationFailure
                    "unexpected fragment after serializing a transaction?"

    describe "Stake Pool Registration golden tests" $ do
        let dataDir = $(getTestData) </> "jormungandr/stake_pools"
        -- this is "jcli key to-bytes < owner.pub"
        let owner = PoolOwner $ unsafeFromHex
                "2ab7522ff4b469ecabfad689fcf1d2ff07b4bd86912f5eacc2e536a5035bc9\
                \bf"

        forM_ ["a", "b", "c"] $ \p -> it ("Message fragment " ++ p) $ do
            let poolDir = dataDir </> p
            tx <- unsafeFromHexFile (poolDir </> "registration_no_fees.msg")
            poolId' <- PoolId <$> unsafeFromHexFile
                (poolDir </> "stake_pool.id")
            txId' <- Hash <$> unsafeFromHexFile
                (poolDir </> "registration_no_fees.txid")

            let taxes = TaxParameters 0 0 Nothing

            runGet getFragment (BL.fromStrict tx)
                `shouldBe`
                PoolRegistration (poolId', [owner], taxes, Tx txId' [] [] mempty Nothing)

    describe "Decode External Tx" $ do
        let tl = newTransactionLayer @JormungandrKey (Hash "genesis")

        it "decodeExternalTx works ok with properly constructed binary blob" $
            property $ \test -> monadicIO $ do
                let bytes = BL.toStrict $ runPut $ void $ putFragment
                        (fragmentGenesis test)
                        (zip (fragmentInputs test) (fragmentCredentials test))
                        (fragmentOutputs test)
                        (fragmentTag test)

                monitor (counterexample $ show $ hex bytes)

                case fragmentTag test of
                    MkFragmentSimpleTransaction{} -> do
                        monitor (QC.label "Simple Transaction")
                        case decodeSignedTx tl bytes of
                            Left err ->
                                run $ expectationFailure $ show err
                            Right (tx@(Tx _ inps outs _ _), SealedTx bytes') -> do
                                monitor (counterexample $ show tx)
                                assert (inps == fragmentInputs test)
                                assert (outs == fragmentOutputs test)
                                assert (hex bytes == hex bytes')

                    MkFragmentStakeDelegation{} -> do
                        monitor (QC.label "Not Simple Transaction")
                        case decodeSignedTx tl bytes of
                            Left _ -> pure ()
                            Right (tx, _) -> do
                                monitor (counterexample $ show tx)
                                run $ expectationFailure "expected failure"
  where
    try' :: a -> IO (Either String a)
    try' = fmap (either (Left . show) Right)
        . (try @SomeException) . evaluate

data TestFragment = TestFragment
    { fragmentGenesis :: Hash "Genesis"
    , fragmentInputs :: [(TxIn, Coin)]
    , fragmentCredentials  :: [(XPrv, Passphrase "encryption")]
    , fragmentOutputs :: [TxOut]
    , fragmentTag :: MkFragment
    } deriving (Eq, Show, Generic)

fragmentAccountId :: TestFragment -> Maybe ChimericAccount
fragmentAccountId test = case fragmentTag test of
    MkFragmentStakeDelegation _ _ (ChimericAccount accountId) _ ->
        Just $ ChimericAccount accountId
    _ ->
        Nothing

fragmentPoolId :: TestFragment -> Maybe PoolId
fragmentPoolId test = case fragmentTag test of
    MkFragmentStakeDelegation _ dlgTag _ _ -> case dlgTag of
        DlgFull poolId -> Just poolId
        _ -> Nothing
    _ -> Nothing

instance Arbitrary TestFragment where
    shrink test
        | length (fragmentInputs test) <= 1
            || length (fragmentOutputs test) <= 1 = []
        | otherwise =
            [ TestFragment genesis inps' (take nInps creds) outs' tag
            | let (TestFragment genesis inps creds outs tag) = test
            , inps' <- shrinkHard inps
            , let nInps = length inps', nInps > 0
            , outs' <- shrinkHard outs
            , let nOuts = length outs', nOuts > 0
            ]
      where
        shrinkHard :: Arbitrary a => [a] -> [[a]]
        shrinkHard xs
            | null xs = []
            | length xs <= 3 = shrink xs
            | otherwise = [take 3 xs]

    arbitrary = oneof
        [ genSimpleTransactionFragment
        , genStakeDelegationFragment
        ]
      where
        block0H = Hash $ B8.replicate 32 '0'

        genSimpleTransactionFragment = do
            inps <- choose (1, maxNumberOfInputs) >>= vector
            creds <- map (,mempty) <$> vector (length inps)
            outs <- choose (1, maxNumberOfOutputs) >>= vector
            witTag <- elements [TxWitnessUTxO, TxWitnessLegacyUTxO]
            pure $ TestFragment
                { fragmentGenesis = block0H
                , fragmentInputs = inps
                , fragmentCredentials = creds
                , fragmentOutputs = outs
                , fragmentTag = MkFragmentSimpleTransaction witTag
                }

        genStakeDelegationFragment = do
            fragment <- genSimpleTransactionFragment
            witTag <- elements [TxWitnessUTxO, TxWitnessLegacyUTxO]
            dlgTag <- oneof [pure DlgNone, DlgFull <$> arbitrary]
            account <- arbitrary
            creds <- (,mempty) <$> arbitrary
            pure $ fragment
                { fragmentTag = MkFragmentStakeDelegation
                    witTag
                    dlgTag
                    account
                    creds
                }

instance Arbitrary ChimericAccount where
    arbitrary = ChimericAccount . BS.pack <$> vector 32

instance Arbitrary XPrv where
    arbitrary = unsafeXPrv . BS.pack <$> vector 128

instance Arbitrary Address where
    arbitrary = Address <$> oneof
        [ BS.pack . (3:) <$> vector 32
        , BS.pack . (4:) <$> vector 64
        , BS.pack . (5:) <$> vector 32
        , BS.pack . (6:) <$> vector 32
        ]

instance Arbitrary (Hash "Tx") where
    arbitrary = Hash . B8.pack <$> vector 32

instance Arbitrary Coin where
    arbitrary = do
        n <- choose (0, 100)
        oneof [
            return $ Coin n
            , return $ Coin (getCoin (maxBound :: Coin) - n)
            , Coin <$> choose (0, getCoin (maxBound :: Coin))
            ]
    shrink (Coin c) = map Coin (shrink c)

instance Arbitrary TxIn where
    arbitrary = TxIn
        <$> arbitrary
        <*> (fromIntegral <$> arbitrary @Word8)
    shrink = genericShrink

instance Arbitrary TxOut where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PoolId where
    arbitrary = PoolId . B8.pack <$> vector 32

-- Necessary unsound 'Show' and 'Eq' instances for QuickCheck failure reporting
instance Show XPrv where
    show = const "XPrv"
instance Eq XPrv where
    a == b = CC.unXPrv a == CC.unXPrv b

deriving instance Show MkFragment
deriving instance Eq MkFragment
