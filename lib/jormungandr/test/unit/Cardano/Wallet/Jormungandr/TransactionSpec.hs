{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
    , Key
    , KeyToAddress (..)
    , Passphrase (..)
    , XPrv
    , keyToAddress
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Sequential
    ( unsafeGenerateKeyFromSeed )
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
        (newTransactionLayer (Hash "") :: TransactionLayer (Jormungandr 'Mainnet))
    it "Property for testnet addresses" $
        property $ propMaxNumberOfInputsEstimation
        (newTransactionLayer (Hash "") :: TransactionLayer (Jormungandr 'Testnet))

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
        goldenTestStdTx proxy keystore block0
            [ (TxIn txin0 0, TxOut addr0 (Coin 10000)) ]
            [ TxOut addr1 (Coin 14)
            , TxOut addr2 (Coin 9986)
            ]
            "00bf020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f403b2ddcfbf631000216a6e0410e64c20e6af91bb\
            \b2826276ad6bfda777e766b24b000000000000000e0302ada8baca6d3a889a893d\
            \a29aa8a291460d7ec95b36404bf094c1a72f2775a6000000000000270201bf524c\
            \8f2be5b1adb7f4b6de2cfd90e226f48fa1d1943bda8e6f0140f8cf7f3a6f4b3797\
            \649b2bc92fa7e3cb6c32f2299899ba7cbd18abb8252786cd212a6f08"

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
            \2f2775a600000000000005390141a47dc3e1246215cd4f78b82444a623c507a1de\
            \d0eb74f641cc77879fc99d6464ac64b63c6ea3a678da99e0028e6de97ad72c5f92\
            \19624f873c7ca4fdb15209012399890d409217c80744dd7a1409d5d53ca735e4bc\
            \044b712c59e612858993b95b47ec7d94ded00044a34fdd9686fe3dea3a62bbf7eb\
            \1ddc2a576bd5e1133207"

    describe "mkStdTx unknown input" $ do
        unknownInputTest (Proxy @'Mainnet) block0
        unknownInputTest (Proxy @'Testnet) block0

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
            \a29aa8a291460d7ec95b36404bf094c1a72f2775a60000000000002702017eeb55\
            \f63214db6bed534d2a623e2b6c2d9b8e54d68c8d77b0328d84d2e7de816046e3ae\
            \56d252a9284f96b512159593f469b771e211348e1c1b4deee987b606"

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
            \2f2775a60000000000000539012334a13af31af13490fa3da215f0aa14731ccac7\
            \2573f28d0778525e847b3700824548dfa2cdddc0a71376de9b7a4867ac93a4dfcf\
            \d51516e63cbf23fa59810c01ecad1eec04b45f8a0405b38e242f05868d339ca91d\
            \9cebdc18021656f17413e35efed90cc701c5066ea423c16a2e2b85ad2fc314ad6b\
            \ff21f5f9ebc2d2a5e201"

    describe "validateSelection cannot accept selection that violates maxNumberOfInputs" $ do
        tooNumerousInpsTest (Proxy @'Mainnet) block0
        tooNumerousInpsTest (Proxy @'Testnet) block0

    describe "validateSelection cannot accept selection that violates maxNumberOfOutputs" $ do
        tooNumerousOutsTest (Proxy @'Mainnet) block0
        tooNumerousOutsTest (Proxy @'Testnet) block0

goldenTestStdTx
    :: forall n. (KnownNetwork n)
    => Proxy (Jormungandr n)
    -> (Address -> Maybe (Key 'AddressK XPrv, Passphrase "encryption"))
    -> Hash "Genesis"
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> ByteString
    -> SpecWith ()
goldenTestStdTx _ keystore block0 inps outs bytes' = it title $ do
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

unknownInputTest
    :: forall n. (KnownNetwork n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
unknownInputTest _ block0 = it title $ do
    let addr = keyToAddress @(Jormungandr n) $ publicKey $ xprv "address-number-0"
    let res = mkStdTx tl keyFrom inps outs
          where
            tl = newTransactionLayer @n block0
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
    let addr = keyToAddress @(Jormungandr n) $ publicKey $ xprv "address-number-0"
    let res = validateSelection tl (CoinSelection inps outs chngs)
          where
            tl = newTransactionLayer @n block0
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
    let addr = keyToAddress @(Jormungandr n) $ publicKey $ xprv "address-number-0"
    let res = validateSelection tl (CoinSelection inps outs chngs)
          where
            tl = newTransactionLayer @n block0
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


xprv :: ByteString -> Key depth XPrv
xprv seed =
    unsafeGenerateKeyFromSeed (Passphrase (BA.convert seed), mempty) mempty
