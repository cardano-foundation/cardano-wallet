{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( putSignedTx, runPut )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Jormungandr.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , Key
    , Passphrase (..)
    , XPrv
    , keyToAddress
    , publicKey
    , unsafeGenerateKeyFromSeed
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.CoinSelection.LargestFirst
    ( largestFirst )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , ShowFmt (..)
    , TxIn (..)
    , TxOut (..)
    , UTxO (..)
    )
import Cardano.Wallet.Transaction
    ( TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Functor.Identity
    ( Identity (runIdentity) )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , property
    , scale
    , vectorOf
    , withMaxSuccess
    , (===)
    )

import qualified Cardano.Wallet.Primitive.CoinSelection as CS
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map

spec :: Spec
spec = do
    estimateSizeSpec
    mkStdTxSpec

{-------------------------------------------------------------------------------
                                Size Estimation
-------------------------------------------------------------------------------}

estimateSizeSpec :: Spec
estimateSizeSpec = do
    describe "estimateSize" $ do
        it "Estimated size is zero"
            (withMaxSuccess 2500 $ property $ propSizeEstimation $ Proxy @'Mainnet)
        it "Estimated size is zero"
            (withMaxSuccess 2500 $ property $ propSizeEstimation $ Proxy @'Testnet)

propSizeEstimation
    :: forall n. (KnownNetwork n)
    => Proxy n
    -> (ShowFmt CoinSelection)
    -> Property
propSizeEstimation _ (ShowFmt sel) =
    let
        blockHash =
            Hash {getHash = "\216OY\rX\199\234\188.<O\\\244Y\211\210\254\224`i\216\DC3\167\132\139\154\216\161T\174\247\155"}
        tl = newTransactionLayer blockHash :: TransactionLayer (Jormungandr n)
        calcSize = estimateSize tl sel
    in calcSize === Quantity 0

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

deriving instance Arbitrary a => Arbitrary (ShowFmt a)

-- Check whether a selection is valid
isValidSelection :: CoinSelection -> Bool
isValidSelection (CoinSelection i o c) =
    let
        oAmt = sum $ map (fromIntegral . getCoin . coin) o
        cAmt = sum $ map (fromIntegral . getCoin) c
        iAmt = sum $ map (fromIntegral . getCoin . coin . snd) i
    in
        (iAmt :: Integer) >= (oAmt + cAmt)

genTxOut :: [Coin] -> Gen [TxOut]
genTxOut coins = do
    let n = length coins
    outs <- vectorOf n arbitrary
    return $ zipWith TxOut outs coins

genSelection :: NonEmpty TxOut -> Gen CoinSelection
genSelection outs = do
    let opts = CS.CoinSelectionOptions 100
    utxo <- vectorOf (NE.length outs * 3) arbitrary >>= genUTxO
    case runIdentity $ runExceptT $ largestFirst opts outs utxo of
        Left _ -> genSelection outs
        Right (s,_) -> return s

genUTxO :: [Coin] -> Gen UTxO
genUTxO coins = do
    let n = length coins
    inps <- vectorOf n arbitrary
    outs <- genTxOut coins
    return $ UTxO $ Map.fromList $ zip inps outs

instance Arbitrary Coin where
    shrink (Coin c) = Coin <$> shrink (fromIntegral c)
    arbitrary = Coin <$> choose (1, 200000)

instance Arbitrary Address where
    shrink _ = []
    arbitrary =
        pure $ Address "\131\&3$\195xi\193\"h\154\&5\145}\245:O\"\148\163\165/h^\ENQ\245\248\229;\135\231\234E/"

instance Arbitrary TxIn where
    shrink _ = []
    arbitrary = TxIn
        <$> arbitrary
        <*> scale (`mod` 3) arbitrary -- No need for a high indexes

instance Arbitrary (Hash "Tx") where
    shrink _ = []
    arbitrary = do
        bytes <- BS.pack <$> vectorOf 32 arbitrary
        pure $ Hash bytes

{-------------------------------------------------------------------------------
                                  mkStdTx
-------------------------------------------------------------------------------}

mkStdTxSpec :: Spec
mkStdTxSpec = do
    let (xprv0, pwd0) =
            xprvFromSeed "arbitrary-seed-0"
        -- ^ 30c8422fd3cbaf54449df9f627a1f88cf85d4fee84083a91cdb6f0dbdb09c24d
        --   ed29409c6a8275a643a2e79d280d97a898a3366706f3f677434b75c9d98680d0
    let (xprv1, pwd1) =
            xprvFromSeed "arbitrary-seed-1"
        -- ^ b81e217576bf2683f3359f50d0bf938ca3c61fdf7a2d0c1b2b35f7fb174dc042
        --   6bb7377c9ea9cb481b4c3df601379fbf69033add18c1d272d7975c43682afc48
    let (xprv2, pwd2) =
            xprvFromSeed "arbitrary-seed-2"
        -- ^ e01bfb39e3e595fce0b9b19e386a82816e0cef8aa823c75a32bc0f39bcf7c14e
        --   8a03d255df0440b6d0fcf5d5199a582d1df7d858bd7556d4941ebf6223fa66d1
    let txin0 = Hash $ unsafeFromHex
            "666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4"
    let txin1 = Hash $ unsafeFromHex
            "1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1"
    let block0 = Hash $ unsafeFromHex
            "13c3d835c53a198f7c8513b04d99eeb23c745c0a73364c2f0e802fa38eec9dba"

    describe "mkStdTx 'Mainnet" $ do
        let proxy = Proxy @(Jormungandr 'Mainnet)
        let keyToAddress' = keyToAddress @(Jormungandr 'Mainnet)
        let addr0 = keyToAddress' (publicKey xprv0)
        -- ^ ca1qvk32hg8rppc0wn0lzpkq996pd3xkxqguel8tharwrpdch6czu2luh3truq
        let addr1 = keyToAddress' (publicKey xprv1)
        -- ^ ca1qwedmnalvvgqqgt2dczppejvyrn2lydmk2pxya4dd076wal8v6eykzfapdx
        let addr2 = keyToAddress' (publicKey xprv2)
        -- ^ ca1qvp2m296efkn4zy63y769x4g52g5vrt7e9dnvszt7z2vrfe0ya66vznknfn

        let keystore = mkKeystore
                [ (addr0, (xprv0, pwd0))
                , (addr1, (xprv1, pwd1))
                , (addr2, (xprv2, pwd2))
                ]

        -- echo $XPRV0 \
        --   | jcli key from-bytes --type ed25519Extended \
        --   | jcli transaction make-witness $TXIN0 \
        --      --genesis-block-hash $BLOCK0 --type utxo > wit0.bin
        --
        -- jcli transaction new \
        --   | jcli transaction add-input $TXIN0 0 10000 \
        --   | jcli transaction add-output $ADDR1 14 \
        --   | jcli transaction finalize $ADDR2 \
        --   | jcli transaction add-witness wit0.bin \
        --   | jcli transaction seal \
        --   | jcli transaction to-message
        goldenTestStdTx proxy keystore block0
            [ (TxIn txin0 0, TxOut addr0 (Coin 10000)) ]
            [ TxOut addr1 (Coin 14)
            , TxOut addr2 (Coin 9986)
            ]
            "00bf020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f403b2ddcfbf631000216a6e0410e64c20e6af91bb\
            \b2826276ad6bfda777e766b24b000000000000000e0302ada8baca6d3a889a893d\
            \a29aa8a291460d7ec95b36404bf094c1a72f2775a60000000000002702011d6870\
            \fa98199f5b826fa30a3d888334d84671ac62d8bad8afd3c2aa1ece4995db1015f5\
            \9b3c1b784ef5a8c7f6aaa68eb814a19809da6d74b55168100b6ac901"

        -- echo $XPRV0 \
        --   | jcli key from-bytes --type ed25519Extended \
        --   | jcli transaction make-witness $TXIN0 --genesis-block-hash $BLOCK0 --type utxo > wit0.bin
        --
        -- echo $XPRV1 \
        --   | jcli key from-bytes --type ed25519Extended \
        --   | jcli transaction make-witness $TXIN1 --genesis-block-hash $BLOCK0 --type utxo > wit1.bin
        --
        -- jcli transaction new \
        --   | jcli transaction add-input $TXIN0 0 10000 \
        --   | jcli transaction add-input $TXIN1 1 999999999 \
        --   | jcli transaction add-output $ADDR0 99999 \
        --   | jcli transaction add-output $ADDR1 42 \
        --   | jcli transaction add-output $ADDR2 1337 \
        --   | jcli transaction finalize \
        --   | jcli transaction add-witness wit0.bin \
        --   | jcli transaction add-witness wit1.bin \
        --   | jcli transaction seal \
        --   | jcli transaction to-message
        goldenTestStdTx proxy keystore block0
            [ (TxIn txin0 0, TxOut addr0 (Coin 10000))
            , (TxIn txin1 1, TxOut addr1 (Coin 999999999))
            ]
            [ TxOut addr0 (Coin 99999)
            , TxOut addr1 (Coin 42)
            , TxOut addr2 (Coin 1337)
            ]
            "0152020203000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f401000000003b9ac9ff1323856bc91c49e928f6f3\
            \0f4e8d665d53eb4ab6028bd0ac971809d514c92db1032d155d07184387ba6ff883\
            \6014ba0b626b1808e67e75dfa370c2dc5f581715fe000000000001869f03b2ddcf\
            \bf631000216a6e0410e64c20e6af91bbb2826276ad6bfda777e766b24b00000000\
            \0000002a0302ada8baca6d3a889a893da29aa8a291460d7ec95b36404bf094c1a7\
            \2f2775a60000000000000539011d6870fa98199f5b826fa30a3d888334d84671ac\
            \62d8bad8afd3c2aa1ece4995db1015f59b3c1b784ef5a8c7f6aaa68eb814a19809\
            \da6d74b55168100b6ac901018d6e44b7d33951c1ba690388fd274c784eac9504f9\
            \18d3df7b186cee7e4e593e8029953054f6da1f21e012bdce6eac708f4183052175\
            \a5d933e8b719c53eed06"

    describe "mkStdTx 'Testnet" $ do
        let proxy = Proxy @(Jormungandr 'Testnet)
        let keyToAddress' = keyToAddress @(Jormungandr 'Testnet)
        let addr0 = keyToAddress' (publicKey xprv0)
        -- ^ ta1svk32hg8rppc0wn0lzpkq996pd3xkxqguel8tharwrpdch6czu2luue5k36
        let addr1 = keyToAddress' (publicKey xprv1)
        -- ^ ta1swedmnalvvgqqgt2dczppejvyrn2lydmk2pxya4dd076wal8v6eykfpz5qu
        let addr2 = keyToAddress' (publicKey xprv2)
        -- ^ ta1svp2m296efkn4zy63y769x4g52g5vrt7e9dnvszt7z2vrfe0ya66vfmfxyf

        let keystore = mkKeystore
                [ (addr0, (xprv0, pwd0))
                , (addr1, (xprv1, pwd1))
                , (addr2, (xprv2, pwd2))
                ]

        -- See 'Mainnet description
        goldenTestStdTx proxy keystore block0
            [ (TxIn txin0 0, TxOut addr0 (Coin 10000)) ]
            [ TxOut addr1 (Coin 14)
            , TxOut addr2 (Coin 9986)
            ]
            "00bf020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f483b2ddcfbf631000216a6e0410e64c20e6af91bb\
            \b2826276ad6bfda777e766b24b000000000000000e8302ada8baca6d3a889a893d\
            \a29aa8a291460d7ec95b36404bf094c1a72f2775a60000000000002702011d6870\
            \fa98199f5b826fa30a3d888334d84671ac62d8bad8afd3c2aa1ece4995db1015f5\
            \9b3c1b784ef5a8c7f6aaa68eb814a19809da6d74b55168100b6ac901"

        -- See 'Mainnet description
        goldenTestStdTx proxy keystore block0
            [ (TxIn txin0 0, TxOut addr0 (Coin 10000))
            , (TxIn txin1 1, TxOut addr1 (Coin 999999999))
            ]
            [ TxOut addr0 (Coin 99999)
            , TxOut addr1 (Coin 42)
            , TxOut addr2 (Coin 1337)
            ]
            "0152020203000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f401000000003b9ac9ff1323856bc91c49e928f6f3\
            \0f4e8d665d53eb4ab6028bd0ac971809d514c92db1832d155d07184387ba6ff883\
            \6014ba0b626b1808e67e75dfa370c2dc5f581715fe000000000001869f83b2ddcf\
            \bf631000216a6e0410e64c20e6af91bbb2826276ad6bfda777e766b24b00000000\
            \0000002a8302ada8baca6d3a889a893da29aa8a291460d7ec95b36404bf094c1a7\
            \2f2775a60000000000000539011d6870fa98199f5b826fa30a3d888334d84671ac\
            \62d8bad8afd3c2aa1ece4995db1015f59b3c1b784ef5a8c7f6aaa68eb814a19809\
            \da6d74b55168100b6ac901018d6e44b7d33951c1ba690388fd274c784eac9504f9\
            \18d3df7b186cee7e4e593e8029953054f6da1f21e012bdce6eac708f4183052175\
            \a5d933e8b719c53eed06"

goldenTestStdTx
    :: forall (n :: Network). ()
    => Proxy (Jormungandr n)
    -> (Address -> Maybe (Key 'AddressK XPrv, Passphrase "encryption"))
    -> Hash "Genesis"
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> ByteString
    -> SpecWith ()
goldenTestStdTx _ keystore block0 inps outs bytes' = it title $ do
    let tx = mkStdTx tl keystore  inps outs
    let bytes = hex . BL.toStrict . runPut . putSignedTx <$> tx
    bytes `shouldBe` Right bytes'
  where
    tl = newTransactionLayer @n block0
    title = "golden test mkStdTx: " <> show inps <> show outs

xprvFromSeed
    :: ByteString
    -> (Key depth XPrv, Passphrase "encryption")
xprvFromSeed seed =
    ( unsafeGenerateKeyFromSeed (Passphrase (BA.convert seed), mempty) pwd
    , pwd
    )
  where
    pwd = mempty

mkKeystore :: Ord k => [(k,v)] -> (k -> Maybe v)
mkKeystore pairs k =
    Map.lookup k (Map.fromList pairs)

hex :: ByteString -> ByteString
hex = convertToBase Base16
