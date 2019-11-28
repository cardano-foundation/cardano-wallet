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

import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Transaction
    ( ErrExceededInpsOrOuts (..), newTransactionLayer )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , Passphrase (..)
    , PaymentAddress (..)
    , XPrv
    , networkDiscriminantVal
    , paymentAddress
    , publicKey
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , PoolId (..)
    , SealedTx (..)
    , TxIn (..)
    , TxOut (..)
    )
import Cardano.Wallet.Transaction
    ( ErrMkTx (..), TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
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

import qualified Cardano.Wallet.Primitive.AddressDerivation.Byron as Rnd
import qualified Cardano.Wallet.Primitive.AddressDerivation.Shelley as Seq
import qualified Data.ByteArray as BA
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
            \3da29aa8a291460d7ec95b36404bf094c1a72f2775a60000000000002702012c36\
            \bb92280d2f627e85bf4ce363214f3a20f35944867ea74f38559ac6f34906fdbc9c\
            \3384cb86e49ea8c2aacb066d75db18171deaed7323665aef25505e5701"

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
            \a72f2775a6000000000000053901815cc9a2ad72e0bc5dc76f6152c38761fc2943\
            \2529c55b48973b540c63eb8e5597d8b59de5adc25c2e4e33212ef326e044e6266c\
            \3d8e5e65d110abd63d39460501d28ca6334756ddd335eaada4a7491373b4d72069\
            \78080f5ac10474c7f6decbbc35f1620817b15bf1594981c034b7f6a15d0a24ec74\
            \3d332f1774eb8733a5d40c"

        -- Delegation tx (without fees)
        --
        -- POOLID=5aac0894709438314ec1c8b0697820fc2efaac914768b53e54d68b0bce17a9a8
        --
        -- jcli certificate new stake-delegation \
        --   $(echo $XPRV0 | jcli key to-public) \
        --   $POOLID > deleg.cert
        --
        -- echo $XPRV0 > xprv0.prv
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
            \8d025405ead51d5cc72a3019f40179e6b8dc3885f26bea38de7e48adeb985e1d70\
            \b79aa1f6e150c2b61795dbb8d2e2fb76ae59ca862f28431ab318ef622999c25a4e\
            \d4947f299b5020a74317e70e0162aee0d27ff232fa09b588483ba861fc374df790\
            \0836e07eb80bd2fc88684eb32d9b500db025cd0c3f8b83a86c6ac61d9426e61e99\
            \99a80e7ea19df8fe49c701"

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
            \aee5526bd50c6cc32113911bd591e3a3601326513ed88151ce8dbdfa488754e00a\
            \b7e4f1e8d7534cd654b82b3eab841bb387113e46f9902519a94f9296b83a5fb997\
            \934c1cba5ccefc42e55b59c70752b356a77307f1a32e686a42970c"

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
            \1326513ed88151ce8dbdfa4817c80704772729b5c0b2583abe0a3e675e2227879a\
            \7452882ba9733baa03bfb9ec2ff75e086fdf8f8fd89c2c3ef9b9c4052e7096dd9f\
            \c12fc22541394a0b3d0d000205b66108c02e4d6dbc4c70adaa27d22ea2adeffe0d\
            \e009c099ac230b520917d2c23ec4bf897f4fd236c676988b62877259f81bd9f08d\
            \4355d5fe6b96cda06db052067dafcc5b525efe2bd9d03d21c356aa1b82887126a2\
            \185231c749a117d70641583fe739d6b59fab049028ad68cbe935910251eb4aacb8\
            \035353e609e400"

    describe "mkStdTx 'Testnet" $ do
        let tl = newTransactionLayer block0
        let paymentAddress' = paymentAddress @'Testnet

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
            \3da29aa8a291460d7ec95b36404bf094c1a72f2775a6000000000000270201c2c5\
            \64f812120d6c6efdaeb1781b5db14d0f109d749db39083a65d28c123eb1fc7b776\
            \2add66f34c185212b4a4133b9fdc857487cc4497fc356ed01e91726c0d"

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
            \a72f2775a60000000000000539015e70794cd622240a1482f7aae5cd2abd4054e1\
            \06f0d590e37f0a4555a58ebcedbae7ba8de6bf952d480311c7f325e99f84b1b312\
            \b0255545d62c97b00a4b16040137bd006254850d77ac2fcb273d6b9b6076133a36\
            \60d6bd5c813d6f3f454f4935b009eabbac1ffd768a07e37773e5b568e8c76c57e5\
            \3da6b26d3da9f466b78200"

    describe "mkStdTx (legacy) 'Testnet" $ do
        let tl = newTransactionLayer block0

        let addrRnd0 = paymentAddress @'Mainnet (publicKey xprvRnd0)
        -- ^ DdzFFzCqrhsyySN2fbNnZf4kh2gg9t4mZzcnCiiw1EFG4ynvCGi35qgdUPh1DJp5Z28SVQxsxfNn7CaRB6DbvvvXZzdtMJ4ML2RrXvrG
        let addrRnd1 = paymentAddress @'Mainnet (publicKey xprvRnd1)
        -- ^ DdzFFzCqrhsrqGWaofA6TmXeChoV5YGk8GyvLE2DCyTib8YpQn4qxsomw4oagtcpa321iQynEtT2D31xG5XGLSWTLHe9CZz26CwZZBQf
        let addr0 = paymentAddress @'Testnet (publicKey xprv0)
        -- ^ ta1svk32hg8rppc0wn0lzpkq996pd3xkxqguel8tharwrpdch6czu2luue5k36
        let addr1 = paymentAddress @'Testnet (publicKey xprv1)
        -- ^ ta1swedmnalvvgqqgt2dczppejvyrn2lydmk2pxya4dd076wal8v6eykfpz5qu
        let addr2 = paymentAddress @'Testnet (publicKey xprv2)
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
            \aee5526bd50c6cc32113911bd591e3a3601326513ed88151ce8dbdfa486fb2c7b4\
            \bf45910e1a07f26209654a00541d918a49f0339bb820da724f165df1c647ccfc6e\
            \5cde86dd31e154b7cc6beef06d561359a2d102f8b3de15b5990007"

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
            \1326513ed88151ce8dbdfa48b1c5041340c009641039e1dcdf436ecc2e7f219d3d\
            \d3aa93b5841662b45e04e8232050f3c42cb64ca45219deb841ecef026488fbf0d6\
            \532589c4e50fd7a11d00000205b66108c02e4d6dbc4c70adaa27d22ea2adeffe0d\
            \e009c099ac230b520917d2c23ec4bf897f4fd236c676988b62877259f81bd9f08d\
            \4355d5fe6b96cda06db22f7d78f0ff0ec3a7c7fbc3da777a6aea6aeddd8ffef0be\
            \b80afd3ba2dd9972585ae74b5876cca3301cee381daf5f9e10b5f8da2f2b9eabba\
            \26bc5c0f01b803"

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
    :: TransactionLayer t k
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> ByteString
    -> SpecWith ()
goldenTestStdTx tl keystore inps outs bytes' = it title $ do
    let tx = mkStdTx tl keystore inps outs
    let bytes = hex . getSealedTx . snd <$> tx
    bytes `shouldBe` Right bytes'
  where
    title = "golden test mkStdTx: " <> show inps <> show outs

goldenTestDelegationCertTx
    :: forall t k. (t ~ Jormungandr, HasCallStack)
    => TransactionLayer t k
    -> (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
    -> PoolId
    -> (k 'AddressK XPrv, Passphrase "encryption")
    -> [(TxIn, TxOut)]
    -> [TxOut]
    -> ByteString
    -> SpecWith ()
goldenTestDelegationCertTx tl keystore pool (accountXPrv, pass) inps outs bytes' = it title $ do
    let res = mkDelegationCertTx tl pool (accountXPrv, pass) keystore inps outs
    let sealed = getSealedTx . snd <$> res
    sealed `shouldBe` (Right $ unsafeFromHex bytes')
    & counterexample ("poolId = " <> showHex (getPoolId pool))
  where
    title = "golden test mkCertificateTx: " <> show pool
        <> show inps <> show outs
    showHex = B8.unpack . hex

xprvSeqFromSeed
    :: ByteString
    -> (ShelleyKey depth XPrv, Passphrase "encryption")
xprvSeqFromSeed seed =
    ( Seq.unsafeGenerateKeyFromSeed (Passphrase (BA.convert seed), mempty) pwd
    , pwd
    )
  where
    pwd = mempty

xprvRndFromSeed
    :: ByteString
    -> (ByronKey 'AddressK XPrv, Passphrase "encryption")
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
    :: forall n. (PaymentAddress n ShelleyKey, NetworkDiscriminantVal n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
unknownInputTest _ block0 = it title $ do
    let addr = paymentAddress @n $ publicKey $ fst $
            xprvSeqFromSeed "address-number-0"
    let res = mkStdTx tl keyFrom inps outs
          where
            tl = newTransactionLayer @ShelleyKey block0
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
        <> T.unpack (toText (networkDiscriminantVal @n))
        <> ")"

tooNumerousInpsTest
    :: forall n. (PaymentAddress n ShelleyKey, NetworkDiscriminantVal n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
tooNumerousInpsTest _ block0 = it title $ do
    let addr = paymentAddress @n $ publicKey $ fst $
            xprvSeqFromSeed "address-number-0"
    let res = validateSelection tl (CoinSelection inps outs chngs)
          where
            tl = newTransactionLayer @ShelleyKey block0
            inps = replicate 256
                ( TxIn (Hash "arbitrary") 0
                , TxOut addr (Coin 1)
                )
            outs = []
            chngs = []
    res `shouldBe` Left ErrExceededInpsOrOuts
  where
    title = "Too numerous inputs yields an error ("
        <> T.unpack (toText (networkDiscriminantVal @n))
        <> ")"

tooNumerousOutsTest
    :: forall n. (PaymentAddress n ShelleyKey, NetworkDiscriminantVal n)
    => Proxy n
    -> Hash "Genesis"
    -> SpecWith ()
tooNumerousOutsTest _ block0 = it title $ do
    let addr = paymentAddress @n $ publicKey $ fst $
            xprvSeqFromSeed "address-number-0"
    let res = validateSelection tl (CoinSelection inps outs chngs)
          where
            tl = newTransactionLayer @ShelleyKey block0
            inps = replicate 255
                ( TxIn (Hash "arbitrary") 0
                , TxOut addr (Coin 10)
                )
            outs = replicate 256 (TxOut addr (Coin 9))
            chngs = replicate 256 (Coin 9)
    res `shouldBe` Left ErrExceededInpsOrOuts
  where
    title = "Too numerous outputs yields an error ("
        <> T.unpack (toText (networkDiscriminantVal @n))
        <> ")"
