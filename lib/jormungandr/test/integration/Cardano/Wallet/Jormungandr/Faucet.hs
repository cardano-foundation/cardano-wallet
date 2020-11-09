{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Jormungandr.Faucet
    ( initFaucet
    ) where

import Prelude

import Cardano.Wallet.Jormungandr.Binary
    ( MkFragment (..), TxWitnessTag (..), fragmentId, putFragment )
import Cardano.Wallet.Primitive.Fee
    ( FeePolicy (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..), Coin (..), TxIn (..), TxOut (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Control.Concurrent.MVar
    ( newMVar )
import Data.ByteString
    ( ByteString )
import System.FilePath
    ( FilePath, (</>) )
import System.IO.Temp
    ( withSystemTempDirectory )
import Test.Integration.Faucet
    ( Faucet (..), icaMnemonics, mirMnemonics, rndMnemonics, seqMnemonics )
import Test.Integration.Jcli
    ( argHex, argInt, getBlock0H, jcli, jcli_, sinkAddress )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Codec.Binary.Bech32.TH as Bech32
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO

-- | Initialize a bunch of faucet wallets and make them available for the
-- integration tests scenarios.
initFaucet :: FeePolicy -> IO Faucet
initFaucet policy = Faucet
    <$> newMVar seqMnemonics
    <*> newMVar icaMnemonics
    <*> newMVar rndMnemonics
    <*> newMVar mirMnemonics
    <*> newMVar (mkTxBuilder policy <$> externalAddresses)

-- | Prepare externally signed Tx for Jormungandr
mkTxBuilder
    :: FeePolicy
    -> (TxIn, String)
    -> (Address, Coin)
    -> IO ByteString
mkTxBuilder (LinearFee cst coeff _) (TxIn inpTx inpIx, key) (addr, Coin amt) =
    withSystemTempDirectory "cardano-wallet-jormungandr" $ \d -> do
        let txFile = d </> "trans.tx"
        let witnessFile = d </> "witness"
        let keyFile = d </> "key.prv"

        TIO.writeFile keyFile (T.pack key)
        prepareTx txFile
        signTx txFile keyFile witnessFile
        toMessage txFile
  where
    -- Amount associated with each input in the genesis file, in Lovelace
    inputAmt :: Int
    inputAmt =
        100000000000

    prepareTx :: FilePath -> IO ()
    prepareTx txFile = do
        let hrp = [Bech32.humanReadablePart|addr|]
        let dp  = Bech32.dataPartFromBytes (unAddress addr)
        jcli_
            [ "transaction"
            , "new"
            , "--staging"
            , txFile
            ]
        jcli_
            [ "transaction"
            , "add-input"
            , argHex inpTx
            , show inpIx
            , show inputAmt
            , "--staging"
            , txFile
            ]
        jcli_
            [ "transaction"
            , "add-output"
            , T.unpack (Bech32.encodeLenient hrp dp)
            , show amt
            , "--staging"
            , txFile
            ]
        jcli_
            [ "transaction"
            , "finalize"
            , sinkAddress
            , "--fee-constant", argInt cst
            , "--fee-coefficient", argInt coeff
            , "--staging"
            , txFile
            ]

    signTx :: FilePath -> FilePath -> FilePath -> IO ()
    signTx txFile keyFile witnessFile = do
        txId <- jcli
            [ "transaction"
            , "data-for-witness"
            , "--staging"
            , txFile
            ]
        block0H <- argHex <$> getBlock0H
        jcli_
            [ "transaction"
            , "make-witness"
            , T.unpack . T.strip . T.pack $ txId
            , "--genesis-block-hash"
            , block0H
            , "--type", "utxo"
            , witnessFile
            , keyFile
            ]
        jcli_
            [ "transaction"
            , "add-witness"
            , witnessFile
            , "--staging"
            , txFile
            ]

    toMessage :: FilePath -> IO ByteString
    toMessage txFile = do
        jcli_
            [ "transaction"
            , "seal"
            , "--staging"
            , txFile
            ]
        bytes <- jcli
            [ "transaction"
            , "to-message"
            , "--staging"
            , txFile
            ]
        return (unsafeFromHex $ T.encodeUtf8 $ T.strip $ T.pack bytes)

externalAddresses :: [(TxIn, String)]
externalAddresses =
    [ ( unsafeMkTxIn
        "external1s0c3kr37th47lcajtcdcmj6z954ylg537msupdcxxwsdrnmuclxfqr9cqau"
      , "ed25519_sk1qmgkz5d4c0swl2uwfvaxuxyd7zmuq95s22xxwkel30sx9l9m6ffqa4hdxg"
      )
    , ( unsafeMkTxIn
        "external1sdqse0sme69dfrrlwshhkwqwwqtrp9h6g4h7qzdgfe0ws27t746ludqrs69"
      , "ed25519_sk1j7gw3p942jh78ck87f0zknx5ts87cjmw7qrc3paa9wkwhznmajcsm72d5v"
      )

    , ( unsafeMkTxIn
        "external1s0h6kelrx0kukrg80p66z63dkycr200taxa8vscgvt07r3heyet97jztehn"
      , "ed25519_sk1vtvxq5ya43zygslw46n05pjtqp686mph2kq6xlh74qcut7rk0jcsd4h40r"
      )

    , ( unsafeMkTxIn
        "external1swe7gkjfh5us4ndh05qs4l5zzr9326gmx90n4wyyqhhcd9z6d2wu7r4altz"
      , "ed25519_sk1zyqdl00vkkkjdnqcgcmkfqr3m5vr7fg4qm000zsv70kunzzzw5cqjmqhza"
      )

    , ( unsafeMkTxIn
        "external1s0df8z9qrqc7mvv2v6uk0th930km5rgj00arzp8pmhm86z9srncd73dzqer"
      , "ed25519_sk1lkv5g5anapcpaud3ny9wsl6d6smdrjj0v2jx70jnkmtzc07u2vksyyheqw"
      )

    , ( unsafeMkTxIn
        "external1swch34gwdh8c5ly5u9872knkkjekvlhtvkxgg0uryr3qy3arxuhr76ks8ha"
      , "ed25519_sk1lek3s5nzv67kn3dqp7tzz5xreh2954e6npa02pct94fpqg7s34rsccyah8"
      )

    , ( unsafeMkTxIn
        "external1swnfgruhxu2y6986vaq8kmnz4q2aa2j82unc52mjtm9mha79axmrzwua468"
      , "ed25519_sk1lq9ccgjz6cmkc203xu5efz0nyhvngea4uapzt0nqwxq6c349ky2qzvr7wf"
      )

    , ( unsafeMkTxIn
        "external1sd3w9tlg6ln8vn8fc8peu8xtavsehdvpx38h44lw782m7lpap4xzjd9dwsc"
      , "ed25519_sk16yqxp28e4lsj00klyrutnf9eld879s7gww8h58chdnvhngvqgz4sf95z86"
      )

    , ( unsafeMkTxIn
        "external1sdmtjwmdqsz2nl2pypgsdxypvf97f7y0dppw5ypg2vyyrhzfpazcckq7557"
      , "ed25519_sk1y8zzc7dh36p54jw8j6cc6awpymhrfs8flczf2xt2zg798zx2qhrscq7myx"
      )

    , ( unsafeMkTxIn
        "external1s0vn7nhenkn8lfx2twydelw7wlzq5rxtkxqzhcu53anjgz2gjl4tkc438l5"
      , "ed25519_sk19aj3wm4ztwz6mjf33knq3u30am0exhntu76qees7zjryc4hzugpspmjz5w"
      )
    ]
  where
    unsafeMkTxIn out =
        let
            Right (_, dp) = Bech32.decodeLenient out
            Just addr = Address <$> Bech32.dataPartToBytes dp
            sourceId = fragmentId $ putFragment
                (Hash $ BS.replicate 32 0)
                []
                [TxOut addr (Coin 100000000000)]
                (MkFragmentSimpleTransaction TxWitnessUTxO)
        in
            TxIn sourceId 0
