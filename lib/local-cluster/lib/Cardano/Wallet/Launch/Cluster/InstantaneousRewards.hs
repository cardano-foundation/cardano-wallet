{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.Launch.Cluster.InstantaneousRewards
    ( moveInstantaneousRewardsTo
    , Credential (..)
    )
 where

import Prelude

import Cardano.Address.Derivation
    ( XPub
    , xpubPublicKey
    )
import Cardano.Launcher.Node
    ( CardanoNodeConn
    )
import Cardano.Wallet.Launch.Cluster.CardanoCLI
    ( cli
    , cliLine
    )
import Cardano.Wallet.Launch.Cluster.ClusterEra
    ( clusterEraToString
    )
import Cardano.Wallet.Launch.Cluster.Config
    ( Config (..)
    , TestnetMagic (testnetMagicToNatural)
    )
import Cardano.Wallet.Launch.Cluster.Faucet
    ( depositAmt
    , faucetAmt
    , takeFaucet
    )
import Cardano.Wallet.Launch.Cluster.SinkAddress
    ( genSinkAddress
    )
import Cardano.Wallet.Launch.Cluster.StakeCertificates
    ( issueStakeScriptCert
    , issueStakeVkCert
    )
import Cardano.Wallet.Launch.Cluster.Tx
    ( signAndSubmitTx
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Util
    ( HasCallStack
    )
import Control.Monad
    ( unless
    , when
    )
import Data.Aeson
    ( (.=)
    )
import Data.ByteArray.Encoding
    ( Base (..)
    , convertToBase
    )
import Data.ByteString
    ( ByteString
    )
import Data.Generics.Labels
    ()
import Data.Tagged
    ( Tagged (..)
    , retag
    , untag
    )
import System.FilePath
    ( (</>)
    )

import qualified Cardano.Codec.Cbor as CBOR
import qualified Cardano.Wallet.Primitive.Types.Coin as Coin
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Credential
    = KeyCredential XPub
    | ScriptCredential ByteString
    deriving stock (Eq, Show)

moveInstantaneousRewardsTo
    :: HasCallStack
    => Config
    -> CardanoNodeConn
    -> [(Credential, Coin)]
    -> IO ()
moveInstantaneousRewardsTo config@Config{..} conn targets = unless (null targets) $ do
    let clusterDir = cfgClusterDir
    let clusterConfigs = cfgClusterConfigs
    let outputDir = retag @"cluster" @_ @"output" clusterDir
    certs <- mapM (mkCredentialCerts outputDir cfgTestnetMagic ) targets
    (faucetInput, faucetPrv) <- takeFaucet clusterConfigs
    let txFile = untag clusterDir </> "mir-tx.raw"

    let total = sum $ map (Coin.toInteger . snd) targets
    let totalDeposit = fromIntegral (length targets) * depositAmt
    when (total > faucetAmt) $
        error "moveInstantaneousRewardsTo: too much to pay"

    sink <- genSinkAddress
        config
        (retag @"cluster" @_ @"output" clusterDir)
        Nothing -- stake pub

    cli cfgTracer
        $ [ clusterEraToString cfgLastHardFork
          , "transaction"
          , "build-raw"
          , "--tx-in"
          , untag faucetInput
          , "--ttl"
          , "999999999"
          , "--fee"
          , show (faucetAmt - 1_000_000 - totalDeposit)
          , "--tx-out"
          , sink <> "+" <> "1000000"
          , "--out-file"
          , txFile
          ]
            ++ concatMap (\x -> ["--certificate-file", untag x]) (mconcat certs)

    {- There is a ledger rule that disallows submitting MIR certificates
    "too late in Epoch" e.g. less that stability window slots before beginning
    of a next epoch. See the MIRCertificateTooLateinEpochDELEG error.

    This problem is worked around by retrying the transaction submission until
    it succeeds.  (This is not ideal as it pollutes logs with error messages)
    -}
    signAndSubmitTx
        config
        conn
        outputDir
        (Tagged @"tx-body" txFile)
        [ retag @"faucet-prv" @_ @"signing-key" faucetPrv
        , Tagged @"signing-key" $ untag clusterConfigs
            </> "delegate-keys/shelley.000.skey"
        ]
        "MIR certificates"
  where
    mkCredentialCerts
        :: Tagged "output" FilePath
        -> TestnetMagic
        -> (Credential, Coin)
        -> IO [Tagged "cert" FilePath]
    mkCredentialCerts outputDir testnetMagic = \case
        (KeyCredential xpub, coin) -> do
            (prefix, vkFile) <- mkVerificationKey xpub
            stakeAddr <-
                cliLine
                    cfgTracer
                    [ "stake-address"
                    , "build"
                    , "--testnet-magic"
                    , show (testnetMagicToNatural testnetMagic)
                    , "--stake-verification-key-file"
                    , vkFile
                    ]
            stakeCert <-
                issueStakeVkCert cfgTracer outputDir prefix (Tagged @"stake-pub" vkFile)
            mirCert <- mkMIRCertificate (stakeAddr, coin)
            pure [retag stakeCert, retag mirCert]
        (ScriptCredential script, coin) -> do
            (prefix, scriptFile) <- mkScript script
            stakeAddr <-
               cliLine
                    cfgTracer
                    [ "stake-address"
                    , "build"
                    , "--testnet-magic"
                    , show (testnetMagicToNatural testnetMagic)
                    , "--stake-script-file"
                    , scriptFile
                    ]
            stakeCert <- issueStakeScriptCert cfgTracer outputDir prefix scriptFile
            mirCert <- mkMIRCertificate (stakeAddr, coin)
            pure [retag stakeCert, retag mirCert]

    mkVerificationKey :: XPub -> IO (Tagged "prefix" String, FilePath)
    mkVerificationKey xpub = do
        let base16 =
                T.unpack . T.decodeUtf8 . convertToBase Base16
                    $ xpubPublicKey xpub
        let json =
                Aeson.object
                    [ "type" .= Aeson.String "StakeVerificationKeyShelley_ed25519"
                    , "description" .= Aeson.String "Stake Verification Key"
                    , "cborHex" .= Aeson.String ("5820" <> T.pack base16)
                    ]
        let file = untag cfgClusterDir </> base16 <> ".vk"
        BL8.writeFile file (Aeson.encode json)
        pure (Tagged base16, file)

    mkScript :: ByteString -> IO (Tagged "prefix" String, FilePath)
    mkScript bytes = do
        let base16 =
                T.decodeUtf8 . convertToBase Base16
                    $ CBOR.toStrictByteString $ CBOR.encodeBytes bytes
        let json =
                Aeson.object
                    [ "type" .= Aeson.String "PlutusScriptV1"
                    , "description" .= Aeson.String ""
                    , "cborHex" .= Aeson.String base16
                    ]
        let prefix = take 100 (T.unpack base16)
        let file = untag cfgClusterDir </> prefix <> ".plutus"
        BL8.writeFile file (Aeson.encode json)
        pure (Tagged prefix, file)

    mkMIRCertificate :: (String, Coin) -> IO (Tagged "mir-cert" FilePath)
    mkMIRCertificate (stakeAddr, Coin reward) = do
        let mirCert = untag cfgClusterDir </> stakeAddr <> ".mir"
        cli
            cfgTracer
            [ "governance"
            , "create-mir-certificate"
            , "--reserves"
            , "--reward"
            , show reward
            , "--stake-address"
            , stakeAddr
            , "--out-file"
            , mirCert
            ]
        pure $ Tagged @"mir-cert" mirCert
