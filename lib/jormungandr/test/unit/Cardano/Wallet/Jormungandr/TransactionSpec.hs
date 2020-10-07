{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- HLINT ignore "Unused LANGUAGE pragma" -}

module Cardano.Wallet.Jormungandr.TransactionSpec
    ( spec
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Wallet.Jormungandr.Transaction
    ( ErrValidateSelection (..), newTransactionLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , Passphrase (..)
    , PaymentAddress (..)
    , WalletKey
    , getRawKey
    , networkDiscriminantVal
    , paymentAddress
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Jormungandr
    ( JormungandrKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , PoolId (..)
    , SealedTx (..)
    , SlotNo (..)
    , TxIn (..)
    , TxOut (..)
    )
import Cardano.Wallet.Transaction
    ( ErrMkTx (..), TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex, unsafeMkSomeMnemonicFromEntropy )
import Data.ByteArray.Encoding
    ( Base (Base16), convertToBase )
import Data.ByteString
    ( ByteString )
import Data.Function
    ( (&) )
import Data.Proxy
    ( Proxy (..) )
import Data.Text.Class
    ( toText )
import Test.Hspec
    ( HasCallStack, Spec, SpecWith, describe, it, shouldBe )
import Test.QuickCheck
    ( counterexample )

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Byron
import qualified Cardano.Wallet.Primitive.AddressDerivation.Jormungandr as Jormungandr
import qualified Data.ByteString.Char8 as B8
import qualified Data.Map as Map
import qualified Data.Text as T

spec :: Spec
spec = do
    mkStdTxSpec

{-------------------------------------------------------------------------------
                                  mkStdTx
-------------------------------------------------------------------------------}

mkStdTxSpec :: Spec
mkStdTxSpec = do
    let (xprv0, pwd0) =
            xprvSeqFromSeed "arbitrary-seed-0"
        -- ^ (Private key only, i.e. first 64 bytes)
        -- 30c8422fd3cbaf54449df9f627a1f88cf85d4fee84083a91cdb6f0dbdb09c24d
        -- ed29409c6a8275a643a2e79d280d97a898a3366706f3f677434b75c9d98680d0
    let (xprv1, pwd1) =
            xprvSeqFromSeed "arbitrary-seed-1"
        -- ^ (Private key only, i.e. first 64 bytes)
        -- b81e217576bf2683f3359f50d0bf938ca3c61fdf7a2d0c1b2b35f7fb174dc042
        -- 6bb7377c9ea9cb481b4c3df601379fbf69033add18c1d272d7975c43682afc48
    let (xprv2, pwd2) =
            xprvSeqFromSeed "arbitrary-seed-2"
        -- ^ (Private key only, i.e. first 64 bytes)
        -- e01bfb39e3e595fce0b9b19e386a82816e0cef8aa823c75a32bc0f39bcf7c14e
        -- 8a03d255df0440b6d0fcf5d5199a582d1df7d858bd7556d4941ebf6223fa66d1

    let (xprvRnd0, pwdRnd0) =
            xprvRndFromSeed "arbitrary-seed-0"
        -- ^ (Extended private key, i.e. private key + chain code = 96 bytes)
        -- xprv1rqajdvgj063fctc98m50nkxq4yx9y5frtc8a9076gqw6xxqytfxclcuc78g
        -- gnrhy2p0xkf2xcxk9cjeqrcyrxuyrzfyfufs59zf2ftjn0qj2ae2jd02scmxryyf
        -- ezx74j836xcqnyegnakyp288gm006fq3xyn72
    let (xprvRnd1, pwdRnd1) =
            xprvRndFromSeed "arbitrary-seed-1"
        -- ^ (Extended private key, i.e. private key + chain code = 96 bytes)
        -- xprv1sqesms8g8qkhyvf6q59d6876lzknlx0c5z74dd9z0t6f04pap3f6ekl6fuj
        -- 23cz25g5cmhrz86amfdsl6c0kqakjdx4x5v8lk6ah2axjcglvf0uf0a8aydkxw6v
        -- gkc58wfvlsx7e7zx5x4w4le4edndqd5k72xrz

    let txin0 = Hash $ unsafeFromHex
            "666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4"
    let txin1 = Hash $ unsafeFromHex
            "1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1"
    let block0 = Hash $ unsafeFromHex
            "13c3d835c53a198f7c8513b04d99eeb23c745c0a73364c2f0e802fa38eec9dba"

    -- Bash equivalent of above:
    -- XPRV0HEX=30c8422fd3cbaf54449df9f627a1f88cf85d4fee84083a91cdb6f0dbdb09c24ded29409c6a8275a643a2e79d280d97a898a3366706f3f677434b75c9d98680d0
    -- XPRV1HEX=b81e217576bf2683f3359f50d0bf938ca3c61fdf7a2d0c1b2b35f7fb174dc0426bb7377c9ea9cb481b4c3df601379fbf69033add18c1d272d7975c43682afc48
    -- XPRV2HEX=e01bfb39e3e595fce0b9b19e386a82816e0cef8aa823c75a32bc0f39bcf7c14e8a03d255df0440b6d0fcf5d5199a582d1df7d858bd7556d4941ebf6223fa66d1
    -- XPRV0=$(echo $XPRV0HEX | jcli key from-bytes --type ed25519Extended)
    -- XPRV1=$(echo $XPRV1HEX | jcli key from-bytes --type ed25519Extended)
    -- XPRV2=$(echo $XPRV2HEX | jcli key from-bytes --type ed25519Extended)
    --
    -- TXIN0=666984dec4bc0ff1888be97bfe0694a96b35c58d025405ead51d5cc72a3019f4
    -- TXIN1=1323856bc91c49e928f6f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1
    -- BLOCK0=13c3d835c53a198f7c8513b04d99eeb23c745c0a73364c2f0e802fa38eec9dba

    describe "mkStdTx 'Mainnet" $ do
        let tl = newTransactionLayer block0
        let paymentAddress' = paymentAddress @'Mainnet
        let addr0 = paymentAddress' (publicKey xprv0)
        -- ^ ca1qvk32hg8rppc0wn0lzpkq996pd3xkxqguel8tharwrpdch6czu2luh3truq
        let addr1 = paymentAddress' (publicKey xprv1)
        -- ^ ca1qwedmnalvvgqqgt2dczppejvyrn2lydmk2pxya4dd076wal8v6eykzfapdx
        let addr2 = paymentAddress' (publicKey xprv2)
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
        --   | jcli transaction make-witness $(jcli transaction data-for-witness --staging tx) \
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
            "00c000020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35\
            \c58d025405ead51d5cc72a3019f403b2ddcfbf631000216a6e0410e64c20e6af91\
            \bbb2826276ad6bfda777e766b24b000000000000000e0302ada8baca6d3a889a89\
            \3da29aa8a291460d7ec95b36404bf094c1a72f2775a60000000000002702010d05\
            \360e86f1cdd145f03275b1703e7eb34112c00b6dfc81cdbf4eb1d936fa0d016191\
            \3c818a33bb1b91835736fd142dd6d88e77ce2aff5a9437703325747e0c"

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
        --   | jcli transaction make-witness $(jcli transaction data-for-witness --staging tx) \
        --      --genesis-block-hash $BLOCK0 --type utxo > wit0.bin
        --
        -- echo $XPRV1 \
        --   | jcli key from-bytes --type ed25519Extended \
        --   | jcli transaction make-witness $(jcli transaction data-for-witness --staging tx) \
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
            "015300020203000000000000002710666984dec4bc0ff1888be97bfe0694a96b35\
            \c58d025405ead51d5cc72a3019f401000000003b9ac9ff1323856bc91c49e928f6\
            \f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1032d155d07184387ba6ff8\
            \836014ba0b626b1808e67e75dfa370c2dc5f581715fe000000000001869f03b2dd\
            \cfbf631000216a6e0410e64c20e6af91bbb2826276ad6bfda777e766b24b000000\
            \000000002a0302ada8baca6d3a889a893da29aa8a291460d7ec95b36404bf094c1\
            \a72f2775a600000000000005390168f81e53613b36e6aeae7ed33c57648d2fa5fa\
            \8bf2b5a8c15f80e6171e4e2779718a5fd267dc239770c242a7aab186e804e5290a\
            \b5dded082f7a2a309b18aa0201a0bb8e90c086e59680faa9c414236deff83f9cda\
            \268e2fae6cdf6fad382f2d00ae20857916797f4a4687daaa544bde5cd9f0ffaa6a\
            \ba33ca696125f5a2e06a07"

        -- Delegation tx (without fees)
        --
        -- POOLID=5aac0894709438314ec1c8b0697820fc2efaac914768b53e54d68b0bce17a9a8
        --
        -- jcli certificate new stake-delegation \
        --   $(echo $XPRV0 | jcli key from-bytes --type ed25519extended | jcli key to-public) \
        --   $POOLID > deleg.cert
        --
        -- echo $XPRV0 | jcli key from-bytes --type ed25519extended > xprv0.prv
        --
        -- jcli certificate sign -c deleg.cert -k xprv0.prv -o deleg.signedcert
        --
        --
        -- echo "Delegation tx without fees:"
        -- jcli transaction new \
        --     | jcli transaction add-certificate $(jcli utils bech32-convert $(cat deleg.signedcert) cert) \
        --     | jcli transaction finalize \
        --     | jcli transaction seal \
        --     | jcli transaction auth -k xprv0.prv \
        --     | jcli transaction to-message

        let poolId = PoolId $ unsafeFromHex
                "5aac0894709438314ec1c8b0697820fc\
                \2efaac914768b53e54d68b0bce17a9a8"
        goldenTestDelegationCertTx tl keystore
            poolId
            (xprv0, pwd0)
            [] -- no inputs; we assume 0 fees here for simplicity
            [] -- no outputs; no change
            "008600042d155d07184387ba6ff8836014ba0b626b1808e67e75dfa370c2dc5f58\
            \1715fe015aac0894709438314ec1c8b0697820fc2efaac914768b53e54d68b0bce\
            \17a9a800000105458a17be5c072b8981f43a272b1d8136f880ff91ff87d1b34aef\
            \3e860300df40997b2130b7db70796b30684ea99d5dd7de1767bd6602da7138864e\
            \30a3d202"

        -- Delegation certificate tx /with/ fees:
        --
        -- jcli transaction new \
        --     | jcli transaction add-certificate $(jcli utils bech32-convert $(cat deleg.signedcert) cert) \
        --     | jcli transaction add-input $TXIN0 0 42 \
        --     | jcli transaction finalize > tx
        --
        -- echo $XPRV0 \
        --     | jcli transaction make-witness $(jcli transaction data-for-witness --staging tx) \
        --     --genesis-block-hash $BLOCK0 --type utxo > wit0.bin
        --
        -- cat tx \
        --     | jcli transaction add-witness wit0.bin \
        --     | jcli transaction seal \
        --     | jcli transaction auth -k xprv0.prv \
        --     | jcli transaction to-message

        goldenTestDelegationCertTx tl keystore
            poolId
            (xprv0, pwd0)
            [(TxIn txin0 0, TxOut addr0 (Coin 42))]
            [] -- no outputs; no change
            "00f000042d155d07184387ba6ff8836014ba0b626b1808e67e75dfa370c2dc5f58\
            \1715fe015aac0894709438314ec1c8b0697820fc2efaac914768b53e54d68b0bce\
            \17a9a8010000000000000000002a666984dec4bc0ff1888be97bfe0694a96b35c5\
            \8d025405ead51d5cc72a3019f40188fdf7b9d26be460d9b90a3af4b8f3d1c13504\
            \2f5b3134504061df289f017fae296a47e899ca2746ec79eca7d270694872e6bdec\
            \d63fb7dc7bb7f1256ac7e909014fb2d07451991b6b5a1dcae8dabecaaaa920b5b1\
            \7b2402291d22019571e180ccf88d906eb53ba71d897362542014ef21ae0e463310\
            \1e196c98c25bf5937a1c06"

    describe "mkStdTx (legacy) 'Mainnet" $ do
        let tl = newTransactionLayer block0
        let addrRnd0 = paymentAddress @'Mainnet (publicKey xprvRnd0)
        -- ^ DdzFFzCqrhsyySN2fbNnZf4kh2gg9t4mZzcnCiiw1EFG4ynvCGi35qgdUPh1DJp5Z28SVQxsxfNn7CaRB6DbvvvXZzdtMJ4ML2RrXvrG
        let addrRnd1 = paymentAddress @'Mainnet (publicKey xprvRnd1)
        -- ^ DdzFFzCqrhsrqGWaofA6TmXeChoV5YGk8GyvLE2DCyTib8YpQn4qxsomw4oagtcpa321iQynEtT2D31xG5XGLSWTLHe9CZz26CwZZBQf
        let addr0 = paymentAddress @'Mainnet (publicKey xprv0)
        -- ^ ca1qvk32hg8rppc0wn0lzpkq996pd3xkxqguel8tharwrpdch6czu2luh3truq
        let addr1 = paymentAddress @'Mainnet (publicKey xprv1)
        -- ^ ca1qwedmnalvvgqqgt2dczppejvyrn2lydmk2pxya4dd076wal8v6eykzfapdx
        let addr2 = paymentAddress @'Mainnet (publicKey xprv2)
        -- ^ ca1qvp2m296efkn4zy63y769x4g52g5vrt7e9dnvszt7z2vrfe0ya66vznknfn

        let keystore = mkKeystore
                [ (addrRnd0, (xprvRnd0, pwdRnd0))
                , (addrRnd1, (xprvRnd1, pwdRnd1))
                ]

        -- jcli transaction new \
        --   | jcli transaction add-input $TXIN0 0 10000 \
        --   | jcli transaction add-output $ADDR1 14 \
        --   | jcli transaction finalize $ADDR2 \
        --   > tx
        --
        -- echo $XPRVRND0 \
        --   | jcli transaction make-witness $(jcli transaction data-for-witness --staging tx) \
        --      --genesis-block-hash $BLOCK0 --type legacy-utxo > wit0.bin
        --
        -- cat tx \
        --   | jcli transaction add-witness wit0.bin \
        --   | jcli transaction seal \
        --   | jcli transaction to-message
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addrRnd0 (Coin 10000)) ]
            [ TxOut addr1 (Coin 14)
            , TxOut addr2 (Coin 9986)
            ]
            "010000020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35\
            \c58d025405ead51d5cc72a3019f403b2ddcfbf631000216a6e0410e64c20e6af91\
            \bbb2826276ad6bfda777e766b24b000000000000000e0302ada8baca6d3a889a89\
            \3da29aa8a291460d7ec95b36404bf094c1a72f2775a6000000000000270200260e\
            \3f0867d341caf86f9b7bd9689f1b21f04dad05711ea3594cc441f159a723537824\
            \aee5526bd50c6cc32113911bd591e3a3601326513ed88151ce8dbdfa48d1a5bc78\
            \8fd6186fc9924146d32ce687a80ea67c95e03fac6389dfe008cdba37f062174ddb\
            \01e30555c2c5aa07ecb618d15be4a96a4837d2296cdf51dd310808"

        -- jcli transaction new \
        --   | jcli transaction add-input $TXIN0 0 10000 \
        --   | jcli transaction add-input $TXIN1 1 999999999 \
        --   | jcli transaction add-output $ADDR0 99999 \
        --   | jcli transaction add-output $ADDR1 42 \
        --   | jcli transaction add-output $ADDR2 1337 \
        --   | jcli transaction finalize \
        --   > tx
        --
        -- echo $XPRVRND0 \
        --   | jcli transaction make-witness $(jcli transaction data-for-witness --staging tx) \
        --      --genesis-block-hash $BLOCK0 --type legacy-utxo > wit0.bin
        --
        -- echo $XPRVRND1 \
        --   | jcli transaction make-witness $(jcli transaction data-for-witness --staging tx) \
        --      --genesis-block-hash $BLOCK0 --type legacy-utxo > wit1.bin
        --
        -- cat tx \
        --   | jcli transaction add-witness wit0.bin \
        --   | jcli transaction add-witness wit1.bin \
        --   | jcli transaction seal \
        --   | jcli transaction to-message
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addrRnd0 (Coin 10000))
            , (TxIn txin1 1, TxOut addrRnd1 (Coin 999999999))
            ]
            [ TxOut addr0 (Coin 99999)
            , TxOut addr1 (Coin 42)
            , TxOut addr2 (Coin 1337)
            ]
            "01d300020203000000000000002710666984dec4bc0ff1888be97bfe0694a96b35\
            \c58d025405ead51d5cc72a3019f401000000003b9ac9ff1323856bc91c49e928f6\
            \f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1032d155d07184387ba6ff8\
            \836014ba0b626b1808e67e75dfa370c2dc5f581715fe000000000001869f03b2dd\
            \cfbf631000216a6e0410e64c20e6af91bbb2826276ad6bfda777e766b24b000000\
            \000000002a0302ada8baca6d3a889a893da29aa8a291460d7ec95b36404bf094c1\
            \a72f2775a6000000000000053900260e3f0867d341caf86f9b7bd9689f1b21f04d\
            \ad05711ea3594cc441f159a723537824aee5526bd50c6cc32113911bd591e3a360\
            \1326513ed88151ce8dbdfa4801136baa5ec7f8e4cc0ca35b2f0c71aa8b0f3fb91f\
            \0117f07e644f31347baa198585aa7f5c86e707586a4c8faaa0c27bd024f95ada08\
            \ea32cd2ac1a8e9942807000205b66108c02e4d6dbc4c70adaa27d22ea2adeffe0d\
            \e009c099ac230b520917d2c23ec4bf897f4fd236c676988b62877259f81bd9f08d\
            \4355d5fe6b96cda06d2b5c3c589a20d24586a63c56373b21e8cc900b606dd88851\
            \93e2a68a1d9f3fd685a492f66b33f72f5194d7ff8bb2e2c020ba4ed287ee9b296d\
            \db65a57699fe09"

    describe "mkStdTx 'Testnet" $ do
        let tl = newTransactionLayer block0
        let paymentAddress' = paymentAddress @('Testnet 0)

        let addr0 = paymentAddress' (publicKey xprv0)
        -- ^ ta1svk32hg8rppc0wn0lzpkq996pd3xkxqguel8tharwrpdch6czu2luue5k36
        let addr1 = paymentAddress' (publicKey xprv1)
        -- ^ ta1swedmnalvvgqqgt2dczppejvyrn2lydmk2pxya4dd076wal8v6eykfpz5qu
        let addr2 = paymentAddress' (publicKey xprv2)
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
            "00c000020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35\
            \c58d025405ead51d5cc72a3019f483b2ddcfbf631000216a6e0410e64c20e6af91\
            \bbb2826276ad6bfda777e766b24b000000000000000e8302ada8baca6d3a889a89\
            \3da29aa8a291460d7ec95b36404bf094c1a72f2775a600000000000027020135f1\
            \2dab4dcc757f9f6aceac7c0a9913dc2523aeb09e8657f7978556046dd352e1e2c2\
            \ba899d5f2ad209c05aadd2e3c4306add4a6fcec9a3491804f667a0b708"

        -- See 'Mainnet description
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addr0 (Coin 10000))
            , (TxIn txin1 1, TxOut addr1 (Coin 999999999))
            ]
            [ TxOut addr0 (Coin 99999)
            , TxOut addr1 (Coin 42)
            , TxOut addr2 (Coin 1337)
            ]
            "015300020203000000000000002710666984dec4bc0ff1888be97bfe0694a96b35\
            \c58d025405ead51d5cc72a3019f401000000003b9ac9ff1323856bc91c49e928f6\
            \f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1832d155d07184387ba6ff8\
            \836014ba0b626b1808e67e75dfa370c2dc5f581715fe000000000001869f83b2dd\
            \cfbf631000216a6e0410e64c20e6af91bbb2826276ad6bfda777e766b24b000000\
            \000000002a8302ada8baca6d3a889a893da29aa8a291460d7ec95b36404bf094c1\
            \a72f2775a6000000000000053901d1399f9728cb4b14a10afba14bd18a80a536ad\
            \3eba863c4b0557b2965c15c3945f9505e282a2d7281485fb65cfca4ab266ab9509\
            \4122ed3d5d62669ed0eb4e060113cb632d99a59f7b8c5fb5d819bbde8f75991078\
            \388269acb895cd9595b9fed743b77c545efc9627d06181b229d33485b3475349c2\
            \9bba975d1d26e3dc1bba05"

    describe "mkStdTx (legacy) 'Testnet" $ do
        let tl = newTransactionLayer block0

        let addrRnd0 = paymentAddress @'Mainnet (publicKey xprvRnd0)
        -- ^ DdzFFzCqrhsyySN2fbNnZf4kh2gg9t4mZzcnCiiw1EFG4ynvCGi35qgdUPh1DJp5Z28SVQxsxfNn7CaRB6DbvvvXZzdtMJ4ML2RrXvrG
        let addrRnd1 = paymentAddress @'Mainnet (publicKey xprvRnd1)
        -- ^ DdzFFzCqrhsrqGWaofA6TmXeChoV5YGk8GyvLE2DCyTib8YpQn4qxsomw4oagtcpa321iQynEtT2D31xG5XGLSWTLHe9CZz26CwZZBQf
        let addr0 = paymentAddress @('Testnet 0) (publicKey xprv0)
        -- ^ ta1svk32hg8rppc0wn0lzpkq996pd3xkxqguel8tharwrpdch6czu2luue5k36
        let addr1 = paymentAddress @('Testnet 0) (publicKey xprv1)
        -- ^ ta1swedmnalvvgqqgt2dczppejvyrn2lydmk2pxya4dd076wal8v6eykfpz5qu
        let addr2 = paymentAddress @('Testnet 0) (publicKey xprv2)
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
            "010000020102000000000000002710666984dec4bc0ff1888be97bfe0694a96b35\
            \c58d025405ead51d5cc72a3019f483b2ddcfbf631000216a6e0410e64c20e6af91\
            \bbb2826276ad6bfda777e766b24b000000000000000e8302ada8baca6d3a889a89\
            \3da29aa8a291460d7ec95b36404bf094c1a72f2775a6000000000000270200260e\
            \3f0867d341caf86f9b7bd9689f1b21f04dad05711ea3594cc441f159a723537824\
            \aee5526bd50c6cc32113911bd591e3a3601326513ed88151ce8dbdfa4866949112\
            \0c811864e854f8d773bc7cda8ed18919a598e443814abbd0a463873668a47322be\
            \62f343985002343ea3f7b0b8b11afc57def4d3820198f666b17306"

        -- See 'Mainnet description
        goldenTestStdTx tl keystore
            [ (TxIn txin0 0, TxOut addrRnd0 (Coin 10000))
            , (TxIn txin1 1, TxOut addrRnd1 (Coin 999999999))
            ]
            [ TxOut addr0 (Coin 99999)
            , TxOut addr1 (Coin 42)
            , TxOut addr2 (Coin 1337)
            ]
            "01d300020203000000000000002710666984dec4bc0ff1888be97bfe0694a96b35\
            \c58d025405ead51d5cc72a3019f401000000003b9ac9ff1323856bc91c49e928f6\
            \f30f4e8d665d53eb4ab6028bd0ac971809d514c92db1832d155d07184387ba6ff8\
            \836014ba0b626b1808e67e75dfa370c2dc5f581715fe000000000001869f83b2dd\
            \cfbf631000216a6e0410e64c20e6af91bbb2826276ad6bfda777e766b24b000000\
            \000000002a8302ada8baca6d3a889a893da29aa8a291460d7ec95b36404bf094c1\
            \a72f2775a6000000000000053900260e3f0867d341caf86f9b7bd9689f1b21f04d\
            \ad05711ea3594cc441f159a723537824aee5526bd50c6cc32113911bd591e3a360\
            \1326513ed88151ce8dbdfa481da1640d1d25733ee2935877bfba2b109ac34bba1c\
            \2b109a2303e8a76267ee0b890ae01e7cb8e0128b85469f4c279017657a49b72daf\
            \e9c6930cd040dc675008000205b66108c02e4d6dbc4c70adaa27d22ea2adeffe0d\
            \e009c099ac230b520917d2c23ec4bf897f4fd236c676988b62877259f81bd9f08d\
            \4355d5fe6b96cda06dbb3810284f7d1924149ac69a1e097532225aaec68ce5245c\
            \1cc0ceba2ed3d8ecb45280e8d7130ffaebee86fe3db7b03acbeaacebd444557709\
            \652ae7b5566604"

    describe "mkStdTx unknown input" $ do
        unknownInputTest (Proxy @'Mainnet) block0
        unknownInputTest (Proxy @('Testnet 0)) block0

    describe "validateSelection cannot accept selection that violates maxNumberOfInputs" $ do
        tooNumerousInpsTest (Proxy @'Mainnet) block0
        tooNumerousInpsTest (Proxy @('Testnet 0)) block0

    describe "validateSelection cannot accept selection that violates maxNumberOfOutputs" $ do
        tooNumerousOutsTest (Proxy @'Mainnet) block0
        tooNumerousOutsTest (Proxy @('Testnet 0)) block0

goldenTestStdTx
    :: TransactionLayer t k
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> ByteString
    -> SpecWith ()
goldenTestStdTx tl keystore inps outs bytes' = it title $ do
    let cs = mempty { inputs = inps, outputs = outs }
    let rewardAcnt = error "unused"
    let tx = mkStdTx tl rewardAcnt keystore (SlotNo 0) Nothing cs
    let bytes = hex . getSealedTx . snd <$> tx
    bytes `shouldBe` Right bytes'
  where
    title = "golden test mkStdTx: " <> show inps <> show outs

goldenTestDelegationCertTx
    :: forall t k. (HasCallStack, WalletKey k)
    => TransactionLayer t k
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> PoolId
    -> (k 'AddressK XPrv, Passphrase "encryption")
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> ByteString
    -> SpecWith ()
goldenTestDelegationCertTx tl keystore pool (accountXPrv, pass) inputs outputs bytes' = it title $ do
    let res = mkDelegationJoinTx tl
            pool
            (getRawKey accountXPrv, pass)
            keystore
            (SlotNo 0)
            (mempty { inputs, outputs })
    let sealed = getSealedTx . snd <$> res
    sealed `shouldBe` Right (unsafeFromHex bytes')
    & counterexample ("poolId = " <> showHex (getPoolId pool))
  where
    title = "golden test mkCertificateTx: " <> show pool
        <> show inputs <> show outputs
    showHex = B8.unpack . hex

xprvSeqFromSeed
    :: ByteString
    -> (JormungandrKey depth XPrv, Passphrase "encryption")
xprvSeqFromSeed bytes =
    ( Jormungandr.unsafeGenerateKeyFromSeed (seed, Nothing) pwd
    , pwd
    )
  where
    pwd = mempty
    seed = unsafeMkSomeMnemonicFromEntropy (Proxy @12) bytes

xprvRndFromSeed
    :: ByteString
    -> (ByronKey 'AddressK XPrv, Passphrase "encryption")
xprvRndFromSeed bytes =
    ( Byron.unsafeGenerateKeyFromSeed derPath seed pwd
    , pwd
    )
  where
    pwd = mempty
    derPath = (minBound, minBound)
    seed = unsafeMkSomeMnemonicFromEntropy (Proxy @12) bytes

mkKeystore :: Ord k => [(k,v)] -> (k -> Maybe v)
mkKeystore pairs k =
    Map.lookup k (Map.fromList pairs)

hex :: ByteString -> ByteString
hex = convertToBase Base16

unknownInputTest
    :: forall n. (PaymentAddress n JormungandrKey, NetworkDiscriminantVal n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
unknownInputTest _ block0 = it title $ do
    let addr = paymentAddress @n $ publicKey $ fst $
            xprvSeqFromSeed "address-number-0"
    let res = mkStdTx tl rewardAcnt keyFrom (SlotNo 0) Nothing cs
          where
            tl = newTransactionLayer @JormungandrKey block0
            rewardAcnt = error "unused"
            keyFrom = const Nothing
            cs = mempty
                { inputs =
                    [ ( TxIn (Hash "arbitrary") 0
                      , TxOut addr (Coin 0)
                      )
                    ]
                , outputs =
                    []
                }
    res `shouldBe` Left (ErrKeyNotFoundForAddress addr)
  where
    title = "Unknown input address yields an error ("
        <> T.unpack (toText (networkDiscriminantVal @n))
        <> ")"

tooNumerousInpsTest
    :: forall n. (PaymentAddress n JormungandrKey, NetworkDiscriminantVal n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
tooNumerousInpsTest _ block0 = it title $ do
    let addr = paymentAddress @n $ publicKey $ fst $
            xprvSeqFromSeed "address-number-0"
    let res = validateSelection tl $ mempty { inputs, outputs, change }
          where
            tl = newTransactionLayer @JormungandrKey block0
            inputs = replicate 256
                ( TxIn (Hash "arbitrary") 0
                , TxOut addr (Coin 1)
                )
            outputs = []
            change  = []
    res `shouldBe` Left ErrExceededInpsOrOuts
  where
    title = "Too numerous inputs yields an error ("
        <> T.unpack (toText (networkDiscriminantVal @n))
        <> ")"

tooNumerousOutsTest
    :: forall n. (PaymentAddress n JormungandrKey, NetworkDiscriminantVal n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
tooNumerousOutsTest _ block0 = it title $ do
    let addr = paymentAddress @n $ publicKey $ fst $
            xprvSeqFromSeed "address-number-0"
    let res = validateSelection tl $ mempty { inputs, outputs, change }
          where
            tl = newTransactionLayer @JormungandrKey block0
            inputs = replicate 255
                ( TxIn (Hash "arbitrary") 0
                , TxOut addr (Coin 10)
                )
            outputs = replicate 256 (TxOut addr (Coin 9))
            change  = replicate 256 (Coin 9)
    res `shouldBe` Left ErrExceededInpsOrOuts
  where
    title = "Too numerous outputs yields an error ("
        <> T.unpack (toText (networkDiscriminantVal @n))
        <> ")"
