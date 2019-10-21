{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Jormungandr.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( MessageType (..), putSignedTx, runPut, withHeader )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork (..), Network (..) )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Jormungandr.Transaction
    ( ErrExceededInpsOrOuts (..), newTransactionLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , KeyToAddress (..)
    , Passphrase (..)
    , XPrv
    , keyToAddress
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Random
    ( RndKey )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( SeqKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), Hash (..), TxIn (..), TxOut (..) )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), TransactionLayer (..) )
import Cardano.Wallet.TransactionSpecShared
    ( propMaxNumberOfInputsEstimation )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( toText )
import Test.Hspec
    ( Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( property )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Random as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Sequential as Seq
import qualified Data.ByteArray as BA
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Text as T

spec :: Spec
spec = do
    estimateMaxNumberOfInputsSpec
    mkStdTxSpec

{-------------------------------------------------------------------------------
                             Max inputs estimation
-------------------------------------------------------------------------------}

estimateMaxNumberOfInputsSpec :: Spec
estimateMaxNumberOfInputsSpec = describe "estimateMaxNumberOfInputs" $ do
    it "Property for mainnet addresses" $
        property $ propMaxNumberOfInputsEstimation
        (newTransactionLayer (Hash "") :: TransactionLayer (Jormungandr 'Mainnet) SeqKey)
    it "Property for testnet addresses" $
        property $ propMaxNumberOfInputsEstimation
        (newTransactionLayer (Hash "") :: TransactionLayer (Jormungandr 'Testnet) SeqKey)

{-------------------------------------------------------------------------------
                                  mkStdTx
-------------------------------------------------------------------------------}

mkStdTxSpec :: Spec
mkStdTxSpec = do
    let (xprv0, pwd0) =
            xprvSeqFromSeed "arbitrary-seed-0"
        -- ^ 30c8422fd3cbaf54449df9f627a1f88cf85d4fee84083a91cdb6f0dbdb09c24d
        --   ed29409c6a8275a643a2e79d280d97a898a3366706f3f677434b75c9d98680d0
    let (xprv1, pwd1) =
            xprvSeqFromSeed "arbitrary-seed-1"
        -- ^ b81e217576bf2683f3359f50d0bf938ca3c61fdf7a2d0c1b2b35f7fb174dc042
        --   6bb7377c9ea9cb481b4c3df601379fbf69033add18c1d272d7975c43682afc48
    let (xprv2, pwd2) =
            xprvSeqFromSeed "arbitrary-seed-2"
        -- ^ e01bfb39e3e595fce0b9b19e386a82816e0cef8aa823c75a32bc0f39bcf7c14e
        --   8a03d255df0440b6d0fcf5d5199a582d1df7d858bd7556d4941ebf6223fa66d1

    let (xprvRnd0, pwdRnd0) =
            xprvRndFromSeed "arbitrary-seed-0"
        -- ^ 183b26b1127ea29c2f053ee8f9d8c0a90c5251235e0fd2bfda401da318045a4d
        --   8fe398f1d0898ee4505e6b2546c1ac5c4b201e0833708312489e26142892a4ae
    let (xprvRnd1, pwdRnd1) =
            xprvRndFromSeed "arbitrary-seed-1"
        -- ^ 80330dc0e8382d72313a050add1fdaf8ad3f99f8a0bd56b4a27af497d43d0c53
        --   acdbfa4f24a8e04aa2298ddc623ebbb4b61fd61f6076d269aa6a30ffb6bb7574

    let txin0 = Hash $ unsafeFromHex
            "666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4"
    let txin1 = Hash $ unsafeFromHex
            "1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1"
    let block0 = Hash $ unsafeFromHex
            "13c3d835c53a198f7c8513b04d99eeb23c745c0a73364c2f0e802fa38eec9dba"

    describe "mkStdTx 'Mainnet" $ do
        let tl = newTransactionLayer @'Mainnet block0
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

        -- jcli transaction new \
        --   | jcli transaction add-input $TXIN0 0 10000 \
        --   | jcli transaction add-output $ADDR1 14 \
        --   | jcli transaction finalize $ADDR2 \
        --   > tx
        --
        -- echo $XPRV0 \
        --   | jcli key from-bytes --type ed25519Extended \
        --   | jcli transaction make-witness $(jcli transaction id --staging tx) \
        --      --genesis-block-hash $BLOCK0 --type utxo > wit0.bin
        --
        -- cat tx \
        --   | jcli transaction add-witness wit0.bin \
        --   | jcli transaction seal \
        --   | jcli transaction to-message
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addr0 (Coin 10000)) ]
            [ TxOut addr1 (Coin 14)
            , TxOut addr2 (Coin 9986)
            ]
            "00bf020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f403b2ddcfbf631000216a6e0410e64c20e6af91bb\
            \b2826276ad6bfda777e766b24b000000000000000e0302ada8baca6d3a889a893d\
            \a29aa8a291460d7ec95b36404bf094c1a72f2775a60000000000002702012c36bb\
            \92280d2f627e85bf4ce363214f3a20f35944867ea74f38559ac6f34906fdbc9c33\
            \84cb86e49ea8c2aacb066d75db18171deaed7323665aef25505e5701"

        -- jcli transaction new \
        --   | jcli transaction add-input $TXIN0 0 10000 \
        --   | jcli transaction add-input $TXIN1 1 999999999 \
        --   | jcli transaction add-output $ADDR0 99999 \
        --   | jcli transaction add-output $ADDR1 42 \
        --   | jcli transaction add-output $ADDR2 1337 \
        --   | jcli transaction finalize \
        --   > tx
        --
        -- echo $XPRV0 \
        --   | jcli key from-bytes --type ed25519Extended \
        --   | jcli transaction make-witness $(jcli transaction id --staging tx) \
        --      --genesis-block-hash $BLOCK0 --type utxo > wit0.bin
        --
        -- echo $XPRV1 \
        --   | jcli key from-bytes --type ed25519Extended \
        --   | jcli transaction make-witness $(jcli transaction id --staging tx) \
        --      --genesis-block-hash $BLOCK0 --type utxo > wit1.bin
        --
        -- cat tx \
        --   | jcli transaction add-witness wit0.bin \
        --   | jcli transaction add-witness wit1.bin \
        --   | jcli transaction seal \
        --   | jcli transaction to-message
        goldenTestStdTx tl keystore
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
            \2f2775a6000000000000053901815cc9a2ad72e0bc5dc76f6152c38761fc294325\
            \29c55b48973b540c63eb8e5597d8b59de5adc25c2e4e33212ef326e044e6266c3d\
            \8e5e65d110abd63d39460501d28ca6334756ddd335eaada4a7491373b4d7206978\
            \080f5ac10474c7f6decbbc35f1620817b15bf1594981c034b7f6a15d0a24ec743d\
            \332f1774eb8733a5d40c"

    describe "mkStdTx (legacy) 'Mainnet" $ do
        let tl = newTransactionLayer @'Mainnet block0
        let addrRnd0 = keyToAddress @(Jormungandr 'Mainnet) (publicKey xprvRnd0)
        -- ^ DdzFFzCqrhsyySN2fbNnZf4kh2gg9t4mZzcnCiiw1EFG4ynvCGi35qgdUPh1DJp5Z28SVQxsxfNn7CaRB6DbvvvXZzdtMJ4ML2RrXvrG
        let addrRnd1 = keyToAddress @(Jormungandr 'Mainnet) (publicKey xprvRnd1)
        -- ^ DdzFFzCqrhsrqGWaofA6TmXeChoV5YGk8GyvLE2DCyTib8YpQn4qxsomw4oagtcpa321iQynEtT2D31xG5XGLSWTLHe9CZz26CwZZBQf
        let addr0 = keyToAddress @(Jormungandr 'Mainnet) (publicKey xprv0)
        -- ^ ca1qvk32hg8rppc0wn0lzpkq996pd3xkxqguel8tharwrpdch6czu2luh3truq
        let addr1 = keyToAddress @(Jormungandr 'Mainnet) (publicKey xprv1)
        -- ^ ca1qwedmnalvvgqqgt2dczppejvyrn2lydmk2pxya4dd076wal8v6eykzfapdx
        let addr2 = keyToAddress @(Jormungandr 'Mainnet) (publicKey xprv2)
        -- ^ ca1qvp2m296efkn4zy63y769x4g52g5vrt7e9dnvszt7z2vrfe0ya66vznknfn

        let keystore = mkKeystore
                [ (addrRnd0, (xprvRnd0, pwdRnd0))
                , (addrRnd1, (xprvRnd1, pwdRnd1))
                ]

        -- See 'Mainnet description
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addrRnd0 (Coin 10000)) ]
            [ TxOut addr1 (Coin 14)
            , TxOut addr2 (Coin 9986)
            ]
            "00ff020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f403b2ddcfbf631000216a6e0410e64c20e6af91bb\
            \b2826276ad6bfda777e766b24b000000000000000e0302ada8baca6d3a889a893d\
            \a29aa8a291460d7ec95b36404bf094c1a72f2775a6000000000000270200260e3f\
            \0867d341caf86f9b7bd9689f1b21f04dad05711ea3594cc441f159a723537824ae\
            \e5526bd50c6cc32113911bd591e3a3601326513ed88151ce8dbdfa488754e00ab7\
            \e4f1e8d7534cd654b82b3eab841bb387113e46f9902519a94f9296b83a5fb99793\
            \4c1cba5ccefc42e55b59c70752b356a77307f1a32e686a42970c"

        -- See 'Mainnet description
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addrRnd0 (Coin 10000))
            , (TxIn txin1 1, TxOut addrRnd1 (Coin 999999999))
            ]
            [ TxOut addr0 (Coin 99999)
            , TxOut addr1 (Coin 42)
            , TxOut addr2 (Coin 1337)
            ]
            "01d2020203000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f401000000003b9ac9ff1323856bc91c49e928f6f3\
            \0f4e8d665d53eb4ab6028bd0ac971809d514c92db1032d155d07184387ba6ff883\
            \6014ba0b626b1808e67e75dfa370c2dc5f581715fe000000000001869f03b2ddcf\
            \bf631000216a6e0410e64c20e6af91bbb2826276ad6bfda777e766b24b00000000\
            \0000002a0302ada8baca6d3a889a893da29aa8a291460d7ec95b36404bf094c1a7\
            \2f2775a6000000000000053900260e3f0867d341caf86f9b7bd9689f1b21f04dad\
            \05711ea3594cc441f159a723537824aee5526bd50c6cc32113911bd591e3a36013\
            \26513ed88151ce8dbdfa4817c80704772729b5c0b2583abe0a3e675e2227879a74\
            \52882ba9733baa03bfb9ec2ff75e086fdf8f8fd89c2c3ef9b9c4052e7096dd9fc1\
            \2fc22541394a0b3d0d000205b66108c02e4d6dbc4c70adaa27d22ea2adeffe0de0\
            \09c099ac230b520917d2c23ec4bf897f4fd236c676988b62877259f81bd9f08d43\
            \55d5fe6b96cda06db052067dafcc5b525efe2bd9d03d21c356aa1b82887126a218\
            \5231c749a117d70641583fe739d6b59fab049028ad68cbe935910251eb4aacb803\
            \5353e609e400"

    describe "mkStdTx 'Testnet" $ do
        let tl = newTransactionLayer @'Testnet block0
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
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addr0 (Coin 10000)) ]
            [ TxOut addr1 (Coin 14)
            , TxOut addr2 (Coin 9986)
            ]
            "00bf020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f483b2ddcfbf631000216a6e0410e64c20e6af91bb\
            \b2826276ad6bfda777e766b24b000000000000000e8302ada8baca6d3a889a893d\
            \a29aa8a291460d7ec95b36404bf094c1a72f2775a6000000000000270201c2c564\
            \f812120d6c6efdaeb1781b5db14d0f109d749db39083a65d28c123eb1fc7b7762a\
            \dd66f34c185212b4a4133b9fdc857487cc4497fc356ed01e91726c0d"

        -- See 'Mainnet description
        goldenTestStdTx tl keystore
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
            \2f2775a60000000000000539015e70794cd622240a1482f7aae5cd2abd4054e106\
            \f0d590e37f0a4555a58ebcedbae7ba8de6bf952d480311c7f325e99f84b1b312b0\
            \255545d62c97b00a4b16040137bd006254850d77ac2fcb273d6b9b6076133a3660\
            \d6bd5c813d6f3f454f4935b009eabbac1ffd768a07e37773e5b568e8c76c57e53d\
            \a6b26d3da9f466b78200"

    describe "mkStdTx (legacy) 'Testnet" $ do
        let tl = newTransactionLayer @'Testnet block0

        let addrRnd0 = keyToAddress @(Jormungandr 'Mainnet) (publicKey xprvRnd0)
        -- ^ DdzFFzCqrhsyySN2fbNnZf4kh2gg9t4mZzcnCiiw1EFG4ynvCGi35qgdUPh1DJp5Z28SVQxsxfNn7CaRB6DbvvvXZzdtMJ4ML2RrXvrG
        let addrRnd1 = keyToAddress @(Jormungandr 'Mainnet) (publicKey xprvRnd1)
        -- ^ DdzFFzCqrhsrqGWaofA6TmXeChoV5YGk8GyvLE2DCyTib8YpQn4qxsomw4oagtcpa321iQynEtT2D31xG5XGLSWTLHe9CZz26CwZZBQf
        let addr0 = keyToAddress @(Jormungandr 'Testnet) (publicKey xprv0)
        -- ^ ta1svk32hg8rppc0wn0lzpkq996pd3xkxqguel8tharwrpdch6czu2luue5k36
        let addr1 = keyToAddress @(Jormungandr 'Testnet) (publicKey xprv1)
        -- ^ ta1swedmnalvvgqqgt2dczppejvyrn2lydmk2pxya4dd076wal8v6eykfpz5qu
        let addr2 = keyToAddress @(Jormungandr 'Testnet) (publicKey xprv2)
        -- ^ ta1svp2m296efkn4zy63y769x4g52g5vrt7e9dnvszt7z2vrfe0ya66vfmfxyf

        let keystore = mkKeystore
                [ (addrRnd0, (xprvRnd0, pwdRnd0))
                , (addrRnd1, (xprvRnd1, pwdRnd1))
                ]

        -- See 'Mainnet description
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addrRnd0 (Coin 10000)) ]
            [ TxOut addr1 (Coin 14)
            , TxOut addr2 (Coin 9986)
            ]
            "00ff020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f483b2ddcfbf631000216a6e0410e64c20e6af91bb\
            \b2826276ad6bfda777e766b24b000000000000000e8302ada8baca6d3a889a893d\
            \a29aa8a291460d7ec95b36404bf094c1a72f2775a6000000000000270200260e3f\
            \0867d341caf86f9b7bd9689f1b21f04dad05711ea3594cc441f159a723537824ae\
            \e5526bd50c6cc32113911bd591e3a3601326513ed88151ce8dbdfa486fb2c7b4bf\
            \45910e1a07f26209654a00541d918a49f0339bb820da724f165df1c647ccfc6e5c\
            \de86dd31e154b7cc6beef06d561359a2d102f8b3de15b5990007"

        -- See 'Mainnet description
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addrRnd0 (Coin 10000))
            , (TxIn txin1 1, TxOut addrRnd1 (Coin 999999999))
            ]
            [ TxOut addr0 (Coin 99999)
            , TxOut addr1 (Coin 42)
            , TxOut addr2 (Coin 1337)
            ]
            "01d2020203000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f401000000003b9ac9ff1323856bc91c49e928f6f3\
            \0f4e8d665d53eb4ab6028bd0ac971809d514c92db1832d155d07184387ba6ff883\
            \6014ba0b626b1808e67e75dfa370c2dc5f581715fe000000000001869f83b2ddcf\
            \bf631000216a6e0410e64c20e6af91bbb2826276ad6bfda777e766b24b00000000\
            \0000002a8302ada8baca6d3a889a893da29aa8a291460d7ec95b36404bf094c1a7\
            \2f2775a6000000000000053900260e3f0867d341caf86f9b7bd9689f1b21f04dad\
            \05711ea3594cc441f159a723537824aee5526bd50c6cc32113911bd591e3a36013\
            \26513ed88151ce8dbdfa48b1c5041340c009641039e1dcdf436ecc2e7f219d3dd3\
            \aa93b5841662b45e04e8232050f3c42cb64ca45219deb841ecef026488fbf0d653\
            \2589c4e50fd7a11d00000205b66108c02e4d6dbc4c70adaa27d22ea2adeffe0de0\
            \09c099ac230b520917d2c23ec4bf897f4fd236c676988b62877259f81bd9f08d43\
            \55d5fe6b96cda06db22f7d78f0ff0ec3a7c7fbc3da777a6aea6aeddd8ffef0beb8\
            \0afd3ba2dd9972585ae74b5876cca3301cee381daf5f9e10b5f8da2f2b9eabba26\
            \bc5c0f01b803"

    describe "mkStdTx unknown input" $ do
        unknownInputTest (Proxy @'Mainnet) block0
        unknownInputTest (Proxy @'Testnet) block0

    describe "validateSelection cannot accept selection that violates maxNumberOfInputs" $ do
        tooNumerousInpsTest (Proxy @'Mainnet) block0
        tooNumerousInpsTest (Proxy @'Testnet) block0

    describe "validateSelection cannot accept selection that violates maxNumberOfOutputs" $ do
        tooNumerousOutsTest (Proxy @'Mainnet) block0
        tooNumerousOutsTest (Proxy @'Testnet) block0

goldenTestStdTx
    :: forall t n k. (t ~ Jormungandr n)
    => TransactionLayer t k
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> ByteString
    -> SpecWith ()
goldenTestStdTx tl keystore inps outs bytes' = it title $ do
    let tx = mkStdTx tl keystore inps outs
    let bytes = fmap
            (\(Tx _ i o, w) -> hex
                $ BL.toStrict
                $ runPut
                $ withHeader MsgTypeTransaction
                $ putSignedTx i o w)
            tx
    bytes `shouldBe` Right bytes'
  where
    title = "golden test mkStdTx: " <> show inps <> show outs

xprvSeqFromSeed
    :: ByteString
    -> (SeqKey depth XPrv, Passphrase "encryption")
xprvSeqFromSeed seed =
    ( Seq.unsafeGenerateKeyFromSeed (Passphrase (BA.convert seed), mempty) pwd
    , pwd
    )
  where
    pwd = mempty

xprvRndFromSeed
    :: ByteString
    -> (RndKey 'AddressK XPrv, Passphrase "encryption")
xprvRndFromSeed seed =
    ( Rnd.unsafeGenerateKeyFromSeed derPath (Passphrase $ BA.convert seed) pwd
    , pwd
    )
  where
    pwd = mempty
    derPath = (minBound, minBound)

mkKeystore :: Ord k => [(k,v)] -> (k -> Maybe v)
mkKeystore pairs k =
    Map.lookup k (Map.fromList pairs)

hex :: ByteString -> ByteString
hex = convertToBase Base16

unknownInputTest
    :: forall n. (KnownNetwork n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
unknownInputTest _ block0 = it title $ do
    let addr = keyToAddress @(Jormungandr n) $ publicKey $ fst $
            xprvSeqFromSeed "address-number-0"
    let res = mkStdTx tl keyFrom inps outs
          where
            tl = newTransactionLayer @n @SeqKey block0
            keyFrom = const Nothing
            inps =
                [ ( TxIn (Hash "arbitrary") 0
                  , TxOut addr (Coin 0)
                  )
                ]
            outs = []
    res `shouldBe` Left (ErrKeyNotFoundForAddress addr)
  where
    title = "Unknown input address yields an error ("
        <> T.unpack (toText (networkVal @n))
        <> ")"

tooNumerousInpsTest
    :: forall n. (KnownNetwork n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
tooNumerousInpsTest _ block0 = it title $ do
    let addr = keyToAddress @(Jormungandr n) $ publicKey $ fst $
            xprvSeqFromSeed "address-number-0"
    let res = validateSelection tl (CoinSelection inps outs chngs)
          where
            tl = newTransactionLayer @n @SeqKey block0
            inps = replicate 256
                ( TxIn (Hash "arbitrary") 0
                , TxOut addr (Coin 1)
                )
            outs = []
            chngs = []
    res `shouldBe` Left ErrExceededInpsOrOuts
  where
    title = "Too numerous inputs yields an error ("
        <> T.unpack (toText (networkVal @n))
        <> ")"

tooNumerousOutsTest
    :: forall n. (KnownNetwork n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
tooNumerousOutsTest _ block0 = it title $ do
    let addr = keyToAddress @(Jormungandr n) $ publicKey $ fst $
            xprvSeqFromSeed "address-number-0"
    let res = validateSelection tl (CoinSelection inps outs chngs)
          where
            tl = newTransactionLayer @n @SeqKey block0
            inps = replicate 255
                ( TxIn (Hash "arbitrary") 0
                , TxOut addr (Coin 10)
                )
            outs = replicate 256 (TxOut addr (Coin 9))
            chngs = replicate 256 (Coin 9)
    res `shouldBe` Left ErrExceededInpsOrOuts
  where
    title = "Too numerous outputs yields an error ("
        <> T.unpack (toText (networkVal @n))
        <> ")"
